#====================================================================================
# Determine where differences are occurring for stations like Tacoma
#
# Notes:
#  1. See comparisons of data output by noaa, rtide, mtide in scripts/Rtide_comparison
#
# Result: Need to set time_meridian to zero for all harmonic stations.
#         Then output will be correct. Need to test on all improper harmonics.
#
# AS 2024-02-07
#====================================================================================

# Load libraries
library(data.table)
library(DBI)
library(glue)

#===============================================================================
# NOAA predictions functions
#===============================================================================

# Construct function to get one days worth of predictions at one station from noaa
noaa_tides = function(station_code, start_date, end_date, time_interval) {
  st_code = station_code
  st_date = gsub("-", "", start_date)
  nd_date = gsub("-", "", end_date)
  time_int = time_interval
  coops_url = "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?"
  pred_url = "product=predictions&application=NOS.COOPS.TAC.WL&"
  dt_url = glue::glue("begin_date={st_date}&end_date={nd_date}&datum=MLLW&")
  st_url = glue::glue("station={st_code}&time_zone=gmt&")
  units_url = glue::glue("units=metric&interval={time_int}&format=csv")
  full_url = paste0(coops_url, pred_url, dt_url, st_url, units_url)
  noaa_tides = tryCatch(read.csv(full_url), error = function(e) {return(NA)})
  while(all(is.na(noaa_tides))) {
    Sys.sleep(10)
    noaa_tides = tryCatch(read.csv(full_url), error = function(e) {return(NA)})
  }
  noaa_tides$station_code = st_code
  names(noaa_tides) = c("tide_time", "tide_level", "station_code")
  tides_out = noaa_tides[, c("tide_time", "station_code", "tide_level")]
  return(tides_out)
}

#===============================================================================
# MTide Functions
#===============================================================================

# Function to identify station code. THIS COULD BE WAY BETTER !!!!!!!!!!!!!!!!!!!!!!!!
identify_station = function(station, verbose, harms = harmonics) {
  stations_dt = as.data.table(harms$st_data)
  # Pull out possible stations and ids
  first_try = stations_dt[station_name == station, list(station_name, station_code)]
  if ( nrow(first_try) == 1L ) {
    station_code = first_try$station_code
    alt_names = stations_dt[station_name %ilike% station, list(station_name, station_code)]
    if ( verbose == TRUE ) {
      alt_stations = alt_names$station_name[!alt_names$station_name == station]
      alt_stations = paste0(alt_stations, collapse = "; ")
      cat(glue::glue("Since you asked, stations listed below have similar names:\n{alt_stations}", "\n"))
    }
  } else if ( nrow(first_try) == 0L ) {
    station_codes = stations_dt[station_name %ilike% station, list(station_name, station_code)]
    if ( nrow(station_codes) > 1L ) {
      # Message in case more than one station matches
      n_stations = station_codes$station_name
      n_stations = paste0(n_stations, collapse = "; ")
      cat(glue::glue("Stations listed below have similar names:\n{n_stations}", "\n",
                     "Please enter one specific station"), "\n\n")
      station_code = NA_character_
    } else if ( nrow(station_codes) == 0L ) {
      cat(glue::glue("Please try again: \n'{station}' did not match any existing station names"), "\n\n")
      station_code = NA_character_
    }
  } else {
    station_code = NA_character_
  }
  return(station_code)
}

# Function to get reference station
get_reference_station = function(station_code, verbose, harms = harmonics) {
  station_cd = station_code
  stations_dt = data.table(harms$st_data)
  offsets_dt = data.table(harms$st_offsets)
  station_dt = stations_dt[station_code == station_cd]
  station_type = station_dt[, station_type]
  station_name = station_dt[, station_name]
  station_timezone = station_dt[, timezone]
  if ( station_type == "H" ) {
    ref_station_code = list(station_cd, station_type, station_timezone)
    if ( verbose == TRUE ) {
      cat(glue::glue("{station_dt$station_name} is a reference station. Tide heights are", "\n ",
                     "calculated from {station_dt$station_name} harmonic constituents."), "\n\n")
    }
  } else {
    ref_station_code = offsets_dt[station_code == station_cd, reference_station_code]
    ref_station_name = stations_dt[station_code == ref_station_code, station_name]
    ref_station_code = list(ref_station_code, station_type, station_timezone)
    if ( verbose == TRUE ) {
      cat(glue::glue("{station_name} is a subordinate station. Tide levels are", "\n ",
                     "calculated from {ref_station_name} harmonic constituents at one minute", "\n ",
                     "increments. Values for high and low tide are extracted, then time and", "\n ",
                     "height corrections from {ref_station_name} are applied to the {station_name} ", "\n ",
                     "predictions."), "\n\n")
    }
  }
  return(ref_station_code)
}

# Function to generate prediction span vector
get_prediction_range = function(start_date, end_date, pred_inc, timezone) {
  start = ISOdatetime(
    year = data.table::year(start_date),
    month = data.table::month(start_date),
    day = data.table::mday(start_date),
    hour = 0, min = 0, sec = 0,
    tz = timezone)
  end = ISOdatetime(
    year = data.table::year(end_date),
    month = data.table::month(end_date),
    day = data.table::mday(end_date),
    hour = 23, min = 59, sec = 59,
    tz = timezone)
  pred_span = seq(start, end, by = paste(pred_inc, "min"))
  attr(pred_span, "tzone") = "UTC"
  return(pred_span)
}

# Function to calculate the number of hours since start of the year for a given datetime.
hours_from_newyear <- function(datetime) {
  year = data.table::year(datetime)
  newyear = ISOdate(year, 1, 1, 0, tz = "UTC")
  hours = difftime(datetime, newyear, units = 'hours')
  hours = as.numeric(hours)
  return(hours)
}

# Function to get range of years in dts
get_year_range = function(start_date, end_date) {
  year_rng = c(data.table::year(start_date),
               data.table::year(end_date)) |>
    unique() |>
    as.integer()
}

# Function to generate harmonic tides
harmonic_tides = function(station_code, station_info,
                          start_date, end_date,
                          prediction_dts,
                          timezone,
                          verbose,
                          harms) {
  # Convert harmonics data to DT...maybe preconfigure in harms?
  stations_dt = data.table(harms$st_data)
  offsets_dt = data.table(harms$st_offsets)
  stconsts_dt = data.table(harms$st_constituents)
  yrconsts_dt = data.table(harms$yr_constituents)
  speeds_dt = data.table(harms$ct_speed)
  # Get unique station data
  station_dt = stations_dt[station_code == station_info[[1]]]
  datum = station_dt[, station_datum]
  meridian = station_dt[, station_meridian]
  # Get station_constituents data: amplitude, speed, phase
  consts = stconsts_dt[station_code == station_info[[1]], .(order, code, amplitude, phase)]
  consts = speeds_dt[consts, .(order, code, amplitude, phase, speed), on = "order"]
  amplitude = consts[, amplitude]
  speed = consts[, speed]
  phase = consts[, phase]
  # Pull out year constants
  year_range = get_year_range(start_date, end_date)
  yr_dat = yrconsts_dt[node_year %in% year_range & order %in% consts[,order]]
  yr_args = data.table(node_year = year_range,
                       year_factor = NA_real_,
                       equil_arg = NA_real_)
  for (i in seq_along(year_range) ) yr_args$year_factor[i] =
    list(yr_dat$year_factor[yr_dat$node_year == year_range[i]])
  for (i in seq_along(year_range) ) yr_args$equil_arg[i] =
    list(yr_dat$equil_arg[yr_dat$node_year == year_range[i]])
  td_hts = data.table(tide_time = prediction_dts, datum = datum, meridian = meridian)
  set(td_hts, j = "amplitude", value = list(amplitude))
  set(td_hts, j = "speed", value = list(speed))
  set(td_hts, j = "phase", value = list(phase))
  set(td_hts, j = "node_year", value = year(td_hts$tide_time))
  set(td_hts, j = "pred_hour", value = hours_from_newyear(td_hts$tide_time))
  td_hts = td_hts[yr_args, on = "node_year"]
  for ( i in seq_along(td_hts$tide_time) ) {
    set(td_hts, i, j = "tide_height",
        value = (td_hts$datum[i] +
                   sum(unlist(td_hts$year_factor[[i]]) * unlist(td_hts$amplitude[[i]]) *
                         cos((unlist(td_hts$speed[[i]]) * (td_hts$pred_hour[[i]] - td_hts$meridian[[i]]) +
                                unlist(td_hts$equil_arg[[i]]) - unlist(td_hts$phase[[i]])) * pi/180))))
  }
  tide_hts = data.frame(station_code = station_code,
                        station_name = station_dt$station_name,
                        reference_station_code = station_info[[1]],
                        station_type = station_info[[2]],
                        tide_type = "P",
                        tide_time = td_hts$tide_time,
                        tide_level = td_hts$tide_height)
  attr(tide_hts$tide_time, "tzone") = timezone
  return(tide_hts)
}

#=============================================================================
# Load harmonics data: 4.5 MB uncompressed.
#=============================================================================

# Load harmonics data
load("data/harmonics.rda")

#============================================================================================
# Pull out variables for testing
#============================================================================================

# Varibles
tide_station = "Tacoma"
start_date = "2024-02-06"
end_date = "2024-02-06"
data_interval = "60-min"
timezone = "UTC"
verbose = TRUE
harms = harmonics

# Station info
station_code = identify_station(tide_station, verbose = TRUE)
station_info = get_reference_station(station_code, verbose)
pred_inc = as.integer(sub("-min", "", data_interval))
if ( is.null(timezone) ) {
  timezone = station_info[[3]]
}
start_date = anytime::anydate(start_date, tz = timezone)
end_date = anytime::anydate(end_date, tz = timezone)
prediction_dts = get_prediction_range(start_date, end_date, pred_inc, timezone)

# # Date info.........Might be some issues with timezone being reset twice when manual to UTC
# final_dts = get_prediction_range(start_date, end_date, pred_inc, timezone)
# attr(final_dts, "tzone") = timezone
# final_dts
# if ( station_info[[2]] == "S" | data_interval %in% c("high-low", "high-only", "low-only") ) {
#   prediction_dts = get_prediction_range(start_date, end_date, pred_inc, timezone)
# } else {
#   prediction_dts = final_dts
# }

# From tide_harmonics
# Convert harmonics data to DT...maybe preconfigure in harms?
stations_dt = data.table(harms$st_data)
offsets_dt = data.table(harms$st_offsets)
stconsts_dt = data.table(harms$st_constituents)
yrconsts_dt = data.table(harms$yr_constituents)
speeds_dt = data.table(harms$ct_speed)
# Get unique station data
station_dt = stations_dt[station_code == station_info[[1]]]
datum = station_dt[, station_datum]

# Editing HERE TO ZERO !!!!
# meridian = station_dt[, station_meridian]
meridian = 0L

# Get station_constituents data: amplitude, speed, phase
consts = stconsts_dt[station_code == station_info[[1]], .(order, code, amplitude, phase)]
consts = speeds_dt[consts, .(order, code, amplitude, phase, speed), on = "order"]
amplitude = consts[, amplitude]
speed = consts[, speed]
phase = consts[, phase]
# Pull out year constants
year_range = get_year_range(start_date, end_date)
yr_dat = yrconsts_dt[node_year %in% year_range & order %in% consts[,order]]
yr_args = data.table(node_year = year_range,
                     year_factor = NA_real_,
                     equil_arg = NA_real_)
for (i in seq_along(year_range) ) yr_args$year_factor[i] =
  list(yr_dat$year_factor[yr_dat$node_year == year_range[i]])
for (i in seq_along(year_range) ) yr_args$equil_arg[i] =
  list(yr_dat$equil_arg[yr_dat$node_year == year_range[i]])
td_hts = data.table(tide_time = prediction_dts, datum = datum, meridian = meridian)
set(td_hts, j = "amplitude", value = list(amplitude))
set(td_hts, j = "speed", value = list(speed))
set(td_hts, j = "phase", value = list(phase))
set(td_hts, j = "node_year", value = year(td_hts$tide_time))
set(td_hts, j = "pred_hour", value = hours_from_newyear(td_hts$tide_time))
td_hts = td_hts[yr_args, on = "node_year"]

# Core predictions
for ( i in seq_along(td_hts$tide_time) ) {
  set(td_hts, i, j = "tide_height",
      value = (td_hts$datum[i] +
                 sum(unlist(td_hts$year_factor[[i]]) * unlist(td_hts$amplitude[[i]]) *
                       cos((unlist(td_hts$speed[[i]]) * (td_hts$pred_hour[[i]] - td_hts$meridian[[i]]) +
                              unlist(td_hts$equil_arg[[i]]) - unlist(td_hts$phase[[i]])) * pi/180))))
}

# Some major differences between RTide and MTide up to here (converted rtide to meters)
#  1. datum:               mtide = 2.085, rtide = 2.091995        website: MSL = 2.085
#  2. time_meridian:       mtide = -120,  rtide = 0               website: 120
#  3. amplitude M2 [1]     mtide = 1.133  rtide = 1.138733        website: 1.133
#  4. phase M2 [1]         mtide = 11.8   rtide = 11.8            website: 11.8
#  5. speed M2             mtide = 28.984 rtide = 28.984          website: 28.984
#  6. year_factor M2 [1]   mtide = 0.9639 rtide = 0.9639          not on site
#  7. equil_arg M2 [1]     mtide = 247.82 rtide = 247.82          not on site

# RESULT: The only difference is the time_meridian: rtide = 0, mtide = -120

# Calculate with meridian manually set to 0L
tide_hts = data.frame(station_code = station_code,
                      station_name = station_dt$station_name,
                      reference_station_code = station_info[[1]],
                      station_type = station_info[[2]],
                      tide_type = "P",
                      tide_time = td_hts$tide_time,
                      tide_level = td_hts$tide_height)
attr(tide_hts$tide_time, "tzone") = timezone

# Compare to NOAA
st_code = tide_hts$station_code[1]
noaa_hts = noaa_tides(station_code = st_code,
                      start_date = "2024-02-06",
                      end_date = "2024-02-06",
                      time_interval = 60L)

# Plot....MANUALLY SETTING MERIDIAN TO ZERO FIXED ISSUES WITH TACOMA !!!
noaa_hts$tide_time = as.POSIXct(noaa_hts$tide_time, tz = "UTC")
plot(noaa_hts$tide_time, noaa_hts$tide_level, type = "l", col = "blue")
lines(tide_hts$tide_time, tide_hts$tide_level, col = "red")












