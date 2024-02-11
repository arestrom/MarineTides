#====================================================================================
# Core functions for MarineTides package
#
# Notes:
#  1. For this version update to have all timezones come from station data: -- Done
#  2. See if I can get no differences with NOAA by adding a 30-sec option: --Did not improve
#  3. Use Sys.date as default: --Done
#  5. See if I can get timezone from harms --Done
#
# ToDo:
#  1. Fix the identify_stations function. Way too convoluted, and not fully working.     --Done
#  2. Why are Tacoma tides off...both RTide and Mine....similar outputs. Look at         --Done
#     time_meridian and timezone offsets first. See how many have non-zero meridians.
#     RESULT: Issue fixed when time-meridian set to zero.
#  3. Dump start and end dates from harmonic_tides()? No, data is needed.                --Done
#  4. Add warning for harmonic stations that have been removed!
#  5. Fix database to eliminate duplicate station names.                                 --Done
#  6. Add distance to reference station to harms. Output as warning. In cases such
#     as Christmas Island to Honolulu it should be a note to the user.
#
#  NEXT:
#  1. Create function to plot high-low data, using spline, or combo?
#     Possibly time from HL function, then spline?
#  2. Create a tide calendar function?
#  3. Create a colored, clickable squares, annual calendar function?
#
#  Packaging issues:
#  1 https://stackoverflow.com/questions/10527072/using-data-table-package-inside-my-own-package
#
# AS 2024-02-03
#====================================================================================

# Load libraries
library(data.table)
library(glue)
library(anytime)
library(DBI)
library(RPostgres)

#===============================================================================
# Data Functions
#===============================================================================

# Function to get user for database
pg_user <- function(user_label) {
  Sys.getenv(user_label)
}

# Function to get pw for database
pg_pw <- function(pwd_label) {
  Sys.getenv(pwd_label)
}

# Function to get pw for database
pg_host <- function(host_label) {
  Sys.getenv(host_label)
}

# Function to connect to postgres
pg_con_local = function(dbname, port = '5433') {
  con <- dbConnect(
    RPostgres::Postgres(),
    host = pg_host("pg_host_local"),
    dbname = dbname,
    user = pg_user("pg_user"),
    password = pg_pw("pg_pwd_local"),
    port = port)
  con
}

#=============================================================================
# tideslite Functions:
#=============================================================================

# # Load needed libraries
# library(rtide)        # Needed for harmonics data
# library(magrittr)     # Will not be needed later
# library(datacheckr)   # Will not be essential later# Load needed libraries
# library(chk)
#
# # harmonics = rtide::harmonics
# # tide_stations()
#
# tide_stations <- function(stations = ".*", harmonics = rtide::harmonics) {
#   stations = "San Mateo Bridge (west end), San Francisco Bay, California"
#   stations <- gsub("[(]", "[(]", stations)
#   stations <- gsub("[)]", "[)]", stations)
#   stations <- paste0("(", paste(stations, collapse = ")|("), ")")
#   match <- grepl(stations, harmonics$Station$Station)
#   match <- which(match)
#   if (!length(match)) stop("no matching stations", call. = FALSE)
#   harmonics$Station$Station[sort(unique(match))]
# }
#
# harms = MarineTides::harmonics
# # stations_dt = as.data.table(harms$st_data)
# # unique(stations_dt$station_name)
# # grep("Folly", value = TRUE, stations_dt$station_name)
# verbose = TRUE
# station = "Tacoma"
# #station = "Whit"

# Function to identify station code. THIS COULD BE WAY BETTER !!!!!!!!!!!!!!!!!!!!!!!!
identify_station = function(station, verbose, harms = MarineTides::harmonics) {
  stations_dt = as.data.table(harms$st_data)
  # Pull out possible stations and ids
  first_try = stations_dt[station_name == station, list(station_name, station_code)]
  if ( nrow(first_try) == 1L ) {
    station_code = first_try$station_code
    alt_names = stations_dt[station_name %ilike% station, list(station_name, station_code)]
    if ( verbose == TRUE ) {
      alt_stations = alt_names$station_name[!alt_names$station_name == station]
      if ( length(alt_stations) > 1L ) {
        alt_stations = paste0(alt_stations, collapse = "; ")
        cat(glue::glue("Stations listed below have similar names:\n{alt_stations}", "\n"))
      }
    }
  } else if ( !nrow(first_try) == 1L ) {
    station_codes = stations_dt[station_name %ilike% station, list(station_name, station_code)]
    if ( nrow(station_codes) > 1L ) {
      # Message in case more than one station matches
      n_stations = station_codes$station_name
      n_stations = paste0(n_stations, collapse = "; ")
      cat(glue::glue("Stations listed below have similar names:\n{n_stations}", "\n",
                     "Please enter one specific station"), "\n\n")
      station_code = NA_character_
    } else if ( nrow(station_codes) == 1L ) {
      station_code = station_codes$station_code
    } else if ( nrow(station_codes) == 0L ) {
      cat(glue::glue("Please try again: \n'{station}' did not match any existing station names"), "\n\n")
      station_code = NA_character_
    }
  }
  return(station_code)
}

# # Test
# (station_code = identify_station(station = "ABERDEEN", verbose = TRUE))

# Function to get reference station
get_reference_station = function(station_code, verbose, harms = harmonics) {
  station_cd = station_code
  stations_dt = data.table(harms$st_data)
  offsets_dt = data.table(harms$st_offsets)
  station_dt = stations_dt[station_code == station_cd]
  station_type = station_dt[, station_type]
  station_name = station_dt[, station_name]
  station_timezone = station_dt[, timezone]
  station_removed = station_dt[, removed]
  if ( station_type == "H" ) {
    ref_station_code = list(station_cd, station_type, station_timezone)
    if ( !is.na(station_removed) ) {
      station_removed = format(station_removed, format = '%m/%d/%Y')
      warning(cat(glue::glue("{station_dt$station_name} tide station is no longer active. It was removed on {station_removed}.", "\n ",
                     "USE WITH CAUTION!"), "\n\n"))
    }
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

# # Test
# (ref_station_code = get_reference_station(station_code, verbose = FALSE))

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

# Function to find peaks from:
# https://stats.stackexchange.com/questions/22974/how-to-find-local-peaks-valleys-in-a-series-of-data
find_peaks = function (x, m = 3){
  shape = diff(sign(diff(x, na.pad = FALSE)))
  pks = sapply(which(shape < 0), FUN = function(i){
    z = i - m + 1
    z = ifelse(z > 0, z, 1)
    w = i + m + 1
    w = ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks = unlist(pks)
  return(pks)
}

# Function to generate harmonic tides
harmonic_tides = function(station_code, station_info,
                          start_date, end_date,
                          prediction_dts, timezone,
                          verbose, harms) {
  # Convert harmonics data to DT...maybe preconfigure in harms?
  stations_dt = data.table(harms$st_data)
  offsets_dt = data.table(harms$st_offsets)
  stconsts_dt = data.table(harms$st_constituents)
  yrconsts_dt = data.table(harms$yr_constituents)
  speeds_dt = data.table(harms$ct_speed)
  # Get unique station data
  station_dt = stations_dt[station_code == station_info[[1]]]
  datum = station_dt[, station_datum]
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

# Function to generate high_low tide values.
high_low_tides = function(tide_pred, data_interval, tide_station) {
  tide_pred = data.table(tide_pred)
  high_peaks = find_peaks(tide_pred$tide_level, m = 5)
  low_peaks = find_peaks(-tide_pred$tide_level, m = 5)
  # Assign H and L values to tide_level using find_peaks()
  for ( i in seq_along(tide_pred$tide_time) ) {
    set(tide_pred, i, j = "tide_type",
        value = if ( i %in% high_peaks ) {"H"} else if (i %in% low_peaks) {"L"} else {"P"})
  }
  if ( data_interval == "high-only" ) {
    tide_hl = tide_pred[tide_type == "H"]
  } else if (data_interval == "low-only" ) {
    tide_hl = tide_pred[tide_type == "L"]
  } else if ( data_interval == "high-low" ) {
    tide_hl = tide_pred[tide_type %in% c("H", "L")]
  } else {
    tide_hl = tide_pred
  }
  return(tide_hl)
}

# Apply offsets for subordinate stations
subordinate_tides = function(hl_tides, harms) {
  offsets_dt = data.table(harms$st_offsets)
  offsets_dt = offsets_dt[station_code == hl_tides$station_code[1]]
  height_offset_type = offsets_dt[, height_offset_type]
  tide_hl = cbind(offsets_dt, hl_tides[, .(tide_type, tide_time, tide_level)])
  if ( height_offset_type == "R" ) {
    tide_hl[tide_type == "H", ':=' (offset_time = tide_time + (time_offset_high_tide_minutes * 60),
                                    offset_level = tide_level * height_offset_high_tide)]
    tide_hl[tide_type == "L", ':=' (offset_time = tide_time + (time_offset_low_tide_minutes * 60),
                                    offset_level = tide_level * height_offset_low_tide)]
  } else {
    tide_hl[tide_type == "H", ':=' (offset_time = tide_time + (time_offset_high_tide_minutes * 60),
                                    offset_level = tide_level + height_offset_high_tide)]
    tide_hl[tide_type == "L", ':=' (offset_time = tide_time + (time_offset_low_tide_minutes * 60),
                                    offset_level = tide_level + height_offset_low_tide)]
  }
  tide_hl = tide_hl[, .(station_code, station_name, reference_station_code, tide_type,
                        tide_time = offset_time, tide_level = offset_level)]
  return(tide_hl)
}

# # Test
# tide_station = "Asau Harbor, Savaii Island"
# start_date = "2024-02-06"
# end_date = "2024-02-06"
# data_interval = "high-low"
# timezone = NULL
# verbose = TRUE
# harms = MarineTides::harmonics


# Initial wrapper function
tide_level = function(tide_station,
                      start_date = Sys.Date(),
                      end_date = Sys.Date() + 1,
                      data_interval = "15-min",
                      timezone = NULL,
                      verbose = FALSE,
                      harms = harmonics) {
  # Get station info
  station_code = identify_station(tide_station, verbose)
  if ( is.na(station_code) ) {
    stop("Station name is ambiguous")
  } else {
    station_info = get_reference_station(station_code, verbose)
  }
  if ( !data_interval %in% c("1-min", "6-min", "15-min", "30-min", "60-min",
                             "high-low", "high-only", "low-only") ) {
    stop(glue::glue("Please select one of the allowable options for data_interval:\n ",
                    "1-min, 6-min, 15-min, 30-min, 60-min, high-low, high-only, low-only"))
  } else if ( data_interval %in% c("1-min", "6-min", "15-min", "30-min", "60-min") ) {
    pred_inc = as.integer(sub("-min", "", data_interval))
  } else if ( data_interval %in% c("high-low", "high-only", "low-only") ) {
    pred_inc = 1L
  } else {
    pred_inc = 1L
  }
  if ( is.null(timezone) ) {
    timezone = station_info[[3]]
  }
  start_date = anytime::anydate(start_date, tz = timezone)
  end_date = anytime::anydate(end_date, tz = timezone)
  if ( verbose == TRUE ) {
    cat(glue::glue("\nTides will be predicted from {start_date} to {end_date}\n\n"))
  }
  prediction_dts = get_prediction_range(start_date, end_date, pred_inc, timezone)
  # Update data_interval to high-low if minutes are requested and its a subordinate station
  if ( station_info[[2]] == "S" & data_interval %in% c("1-min", "6-min", "15-min", "30-min", "60-min") ) {
    data_interval = "high-low"
    warning("\nFor subordinate stations, only high and low tide values will be computed.\n")
  }
  # Generate harmonic tides, either for reference or subordinate station
  tide_pred = harmonic_tides(station_code, station_info,
                             start_date, end_date,
                             prediction_dts, timezone,
                             verbose, harms)
  # Generate high_low values for subordinate stations or harmonic stations where high_low requested
  if ( station_info[[2]] == "S" | data_interval %in% c("high-low", "high-only", "low-only") ) {
    hl_tides = high_low_tides(tide_pred, data_interval, tide_station)
  }
  # Correct tides for subordinate station offsets and output accordingly
  if ( station_info[[2]] == "S" ) {
    tide_out = subordinate_tides(hl_tides, harms)
  } else if ( station_info[[2]] == "H" & data_interval %in% c("high-low", "high-only", "low-only") ) {
    tide_out = hl_tides
  } else if (station_info[[2]] == "H" & data_interval %in% c("1-min", "6-min", "15-min", "30-min", "60-min") ) {
    tide_pred = data.table(tide_pred)
    tide_pred = tide_pred[, .(station_code, station_name,
                              reference_station_code,
                              tide_type, tide_time, tide_level)]
    tide_out = tide_pred
  }
  return(tide_out)
}

#=============================================================================
# Load harmonics data: 4.5 MB uncompressed.
#=============================================================================

# Load harmonics data
load("data/harmonics.rda")
harms = harmonics

# Test high-low Christmas Island
tm = Sys.time()
harm_xmas = tide_level(
  tide_station = "Christmas Island",
  start_date = "2024-02-06",
  end_date = "2024-02-06",
  data_interval = "high-low",
  timezone = "UTC",
  verbose = TRUE
)
nd = Sys.time(); nd - tm  # 0.12431 secs

#====================================================================
# Test various combinations of inputs for harmonic station
#====================================================================

# Test one min harmonic
tm = Sys.time()
harm_seattle = tide_level(
  tide_station = "Seattle",
  start_date = as.Date("2024-01-25"),
  end_date = as.Date("2024-01-26"),
  data_interval = "1-min",
  verbose = TRUE
)
nd = Sys.time(); nd - tm  # 0.12431 secs

# Test one min harmonic Tacoma
tm = Sys.time()
harm_tacoma = tide_level(
  tide_station = "Tacoma",
  start_date = as.Date("2024-01-25"),
  end_date = as.Date("2024-01-26"),
  data_interval = "1-min",
  verbose = TRUE
)
nd = Sys.time(); nd - tm  # 0.12431 secs

# # Plot Seattle vs Tacoma
# plot(harm_seattle$tide_time, harm_seattle$tide_level, type = "l", col = "blue")
# lines(harm_tacoma$tide_time, harm_tacoma$tide_level, col = "red")

# Test harmonic
tm = Sys.time()
harm_point_6 = tide_level(
  tide_station = "Seattle",
  start_date = as.Date("2024-01-25"),
  end_date = as.Date("2024-01-26"),
  data_interval = "6-min",
  verbose = FALSE
)
nd = Sys.time(); nd - tm  # 0.06150699 secs

# Test harmonic
tm = Sys.time()
harm_point_15 = tide_level(
  tide_station = "Seattle",
  start_date = as.Date("2024-01-25"),
  end_date = as.Date("2024-01-26"),
  data_interval = "15-min",
  verbose = FALSE
)
nd = Sys.time(); nd - tm  # 0.05088711 secs

# Test harmonic
tm = Sys.time()
harm_point_30 = tide_level(
  tide_station = "Seattle",
  start_date = as.Date("2024-01-25"),
  end_date = as.Date("2024-01-26"),
  data_interval = "30-min",
  verbose = FALSE
)
nd = Sys.time(); nd - tm  # 0.04704094 secs

# Test harmonic
tm = Sys.time()
harm_point_60 = tide_level(
  tide_station = "Seattle",
  start_date = as.Date("2024-01-25"),
  end_date = as.Date("2024-01-26"),
  data_interval = "60-min",
  verbose = FALSE
)
nd = Sys.time(); nd - tm  # 0.04612899 secs

# Test harmonic
tm = Sys.time()
harm_high_low = tide_level(
  tide_station = "Seattle",
  start_date = "2024-01-25",
  end_date = "2024-01-26",
  data_interval = "high-low",
  verbose = FALSE
)
nd = Sys.time(); nd - tm  # 0.1358869 secs

# Test harmonic
tm = Sys.time()
harm_high = tide_level(
  tide_station = "Seattle",
  start_date = "2024-01-25",
  end_date = "2024-01-26",
  data_interval = "high-only",
  verbose = FALSE
)
nd = Sys.time(); nd - tm  # 0.1377492 secs

# Test harmonic
tm = Sys.time()
harm_low = tide_level(
  tide_station = "Seattle",
  start_date = "2024-01-25",
  end_date = "2024-01-26",
  data_interval = "low-only",
  verbose = FALSE
)
nd = Sys.time(); nd - tm  # 0.132844 secs

# Test harmonic high_low for six months
tm = Sys.time()
harm_year_high_low = tide_level(
  tide_station = "Seattle",
  start_date = "2024-01-01",
  end_date = "2024-06-30",
  data_interval = "high-low",
  verbose = FALSE
)
nd = Sys.time(); nd - tm  # 8.841941 secs

# Test harmonic high-low for a full year at one minute resolution
tm = Sys.time()
harm_year_high_low = tide_level(
  tide_station = "Seattle",
  start_date = "2024-01-01",
  end_date = "2024-12-31",
  data_interval = "high-low",
  verbose = TRUE
)
nd = Sys.time(); nd - tm  # 18.34023 secs

# #====================================================================
# # Test all highs and lows for year vs tides table in ps_shellfish
# # RESULT: All were just off by one minute...all were plus one minute
# #         on my side. NOAA was always one minute earlier when values
# #         did not agree. Might be able to avoid if predicted to 30 sec.
# #         Issue is probably with rounding of datetime values.
# #         Times differed by one minute in 10% of cases over 2024
# #====================================================================
#
# # Get all low tides for Seattle for year 2024
# qry = glue("select loc.location_name as tide_station, td.low_tide_datetime as ps_tide_time, ",
#            "td.tide_height_feet as ps_tide_level, st.tide_strata_code as tide_strata ",
#            "from tide as td ",
#            "inner join location as loc on td.tide_station_location_id = loc.location_id ",
#            "inner join tide_strata_lut as st on td.tide_strata_id = st.tide_strata_id ",
#            "where location_name = 'Seattle' ",
#            "and date_part('year', low_tide_datetime) = 2024 ",
#            "order by td.low_tide_datetime")
# pg_con = pg_con_local(dbname = "ps_shellfish")
# ps_year_24 = dbGetQuery(pg_con, qry)
# dbDisconnect(pg_con)
#
# # Convert to meters
# ps_year_24 = data.table(ps_year_24)
# ps_year_24 = ps_year_24[, ':=' (ps_tide_level = ps_tide_level / 3.28084)]
# attr(ps_year_24$ps_tide_time, "tzone") = "America/Los_Angeles"
# ps_year_24 = ps_year_24[2:nrow(ps_year_24),]
#
# # # Simplest way to truncate without the dependencies:
# # (x = as.POSIXct(Sys.time()))
# # (xx = round(x, "mins"))
# # (y = as.POSIXct(format(x, "%Y-%m-%d %H:%M:00")))
#
# # Combine ps_year_24 with harm_year_high_low
# comp_ps = merge(harm_year_high_low, ps_year_24, by.x = "tide_time", by.y = "ps_tide_time", all.x = TRUE)
#
# # Pull out cases that agree
# comp_ps = data.table(comp_ps)
# comp_agree = comp_ps[!is.na(ps_tide_level)]
# comp_diff = comp_ps[is.na(ps_tide_level)]
#
# # RESULT: It makes no difference to use higher resolution (30-sec, 20-sec)
#           in quest to match exact NOAA times for highs and low
# nrow(comp_diff) / nrow(comp_agree)
# # 0.1003891

#====================================================================
# Test various combinations of inputs for subordinate station
#====================================================================

# Test subordinate 1-min increment...get warning!
tm = Sys.time()
sub_point_1 = tide_level(
  tide_station = "Whitney Point, Dabob Bay",
  start_date = "2024-01-25",
  end_date = "2024-01-26",
  data_interval = "1-min",
  verbose = FALSE
)
nd = Sys.time(); nd - tm  # 0.1433229 secs

# Test subordinate 6-min warning, verbose true
tm = Sys.time()
sub_point_6 = tide_level(
  tide_station = "Whitney Point, Dabob Bay",
  start_date = "2024-01-25",
  end_date = "2024-01-26",
  data_interval = "6-min",
  verbose = TRUE
)
nd = Sys.time(); nd - tm  # 0.145246 secs

# Test subordinate
tm = Sys.time()
sub_high_low = tide_level(
  tide_station = "Whitney Point, Dabob Bay",
  start_date = "2024-01-25",
  end_date = "2024-01-26",
  data_interval = "high-low",
  verbose = FALSE
)
nd = Sys.time(); nd - tm  # 0.1502721 secs

# Test subordinate
tm = Sys.time()
sub_high = tide_level(
  tide_station = "Whitney Point, Dabob Bay",
  start_date = "2024-01-25",
  end_date = "2024-01-26",
  data_interval = "high-only",
  verbose = FALSE
)
nd = Sys.time(); nd - tm  # 0.153089 secs

# Test subordinate
tm = Sys.time()
sub_low = tide_level(
  tide_station = "Whitney Point, Dabob Bay",
  start_date = "2024-01-25",
  end_date = "2024-01-26",
  data_interval = "low-only",
  verbose = FALSE
)
nd = Sys.time(); nd - tm  # 0.144172 secs


#====================================================================
# Test time to compute
#====================================================================

# Test one month at one hour increments
tm = Sys.time()
tide_one = tide_level(
  tide_station = "Seattle",
  start_date = "2023-12-08",
  end_date = "2024-01-08",
  data_interval = "60-min"
)
nd = Sys.time(); nd - tm  # 0.3623669 secs pre: 0.06596899 secs DT

# Test one year at one hour increments
tm = Sys.time()
tide_two = tide_level(
  tide_station = "Seattle",
  start_date = "2023-12-08",
  end_date = "2024-12-08",
  data_interval = "60-min"
)
nd = Sys.time(); nd - tm  # 2.842646 secs pre: 0.2789869 secs DT

# Test one month at six minute increments
tm = Sys.time()
tide_three = tide_level(
  tide_station = "Seattle",
  start_date = "2023-12-08",
  end_date = "2024-01-08",
  data_interval = "6-min"

)
nd = Sys.time(); nd - tm  # 2.774406 secs pre: 0.246902 secs DT

# Test one year at six minute increments
tm = Sys.time()
tide_four = tide_level(
  tide_station = "Seattle",
  start_date = "2023-12-08",
  end_date = "2024-12-08",
  data_interval = "6-min"
)
nd = Sys.time(); nd - tm  # 1.287391 mins: 2.282157 secs DT
                          # (1.287391 * 60) / 2.282157 secs = 34 times faster with DT

#====================================================================
# Testing time to compute...rtide
#====================================================================

# Test one month at one hour increments
tm = Sys.time()
rtide_today_one = rtide::tide_height(
  stations = "Seattle",
  minutes = 60L,
  from = as.Date("2023-12-08"),
  to = as.Date("2024-01-08"),
  tz = "America/Los_Angeles"
)
nd = Sys.time(); nd - tm  # 0.328156 secs

# Test one year at one hour increments
tm = Sys.time()
rtide_today_two = rtide::tide_height(
  stations = "Seattle",
  minutes = 60L,
  from = as.Date("2023-12-08"),
  to = as.Date("2024-12-08"),
  tz = "America/Los_Angeles"
)
nd = Sys.time(); nd - tm  # 3.64111 secs

# Test one month at six minute increments
tm = Sys.time()
rtide_today_three = rtide::tide_height(
  stations = "Seattle",
  minutes = 6L,
  from = as.Date("2023-12-08"),
  to = as.Date("2024-01-08"),
  tz = "America/Los_Angeles"
)
nd = Sys.time(); nd - tm  # 3.093035 secs

# Test one year at six minute increments
tm = Sys.time()
rtide_four = rtide::tide_height(
  stations = "Seattle",
  minutes = 6L,
  from = as.Date("2023-12-08"),
  to = as.Date("2024-12-08"),
  tz = "America/Los_Angeles"
)
nd = Sys.time(); nd - tm  # 1.287391 mins

#====================================================================
# Results: For one year at 6 minute resolution:
#          My best is about 34 times faster
#====================================================================

#====================================================================
# Test to see if output is comparable
#====================================================================

# Combine tide height columns
tide_four = data.frame(tide_four)
tide_comparison = cbind(tide_four, rtide_four$TideHeight)
names(tide_comparison) = c("station_code", "station_name", "reference_station_code",
                           "tide_type", "tide_time", "mytide_ht", "rtide_ht")

# Compute difference
tide_comparison$mytide_ht = round(tide_comparison$mytide_ht, digits = 2)
tide_comparison$rtide_ht = round(tide_comparison$rtide_ht, digits = 2)

# Pull out cases where heights differ
tide_differences = subset(tide_comparison, !mytide_ht == rtide_ht)
tide_differences$diff = abs(tide_differences$mytide_ht - tide_differences$rtide_ht)

# Compute the maximum difference from rtide: max = 0.03, about 30 + cases. None at 0.1.
max(tide_differences$diff)

# Get one month of noaa data to see where mine differs from noaa. One month is max allowed.
coops_url = "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?"
pred_url = "product=predictions&application=NOS.COOPS.TAC.WL&"
dt_url = "begin_date=20240101&end_date=20240130&datum=MLLW&"
st_url = "station=9447130&time_zone=lst_ldt&units=metric&interval=6&format=csv"
full_url = paste0(coops_url, pred_url, dt_url, st_url)
noaa_tides = read.csv(full_url)

noaa_tides$tide_time = as.POSIXct(noaa_tides$Date.Time)
noaa_tides$Prediction = round(noaa_tides$Prediction, digits = 2)
noaa_comparison = noaa_tides |>
  dplyr::select(tide_time, noaa_ht = Prediction) |>
  dplyr::left_join(tide_differences, by = "tide_time") |>
  subset(!is.na(station_code))

# Pull out cases where mine matches noaa
my_tides_match = subset(noaa_comparison, noaa_ht == mytide_ht)
rtides_match = subset(noaa_comparison, noaa_ht == rtide_ht)

# Percent of time mine vs rtide matches noaa output for one month at 6 min increments
(my_match = nrow(my_tides_match) / nrow(noaa_comparison))  # 79% of the time
(rtide_match = nrow(rtides_match) / nrow(noaa_comparison)) # 21% of the time

#============================================================================
# Result: Mine matches:  79% of the time
#         rtide matches: 21% of the time
# But if rounded to one digit, both would likely always match noaa
#============================================================================

































