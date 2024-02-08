#====================================================================================
# Check data in Harmonics DB
#
#  Notes:
#  1. I checked Station info for Tacoma. No notes about station being removed
#
#
#  Questions:
#  1. Do subs all have ref codes and corrections?
#  2. Do harms all have at least 37 constituents and have any been removed?
#  3. Any documentation on harms that have been removed?
#  4. What's happening with Tacoma? Will it be removed?
#  5. Create a leaflet interface to identify stations?
#  6. Do a better job with identify_station() function
#  7. For now, filter out stations with meridians other than 0?
#  8. Redo harmonics data to eliminate unneeded harmonic stations
#  9. Rewrite docs to give new counts.
# 10 Add high offsets to output above for added info.
# 11 Use regex to id stations? Also, lower case?
# 12 Use region to get added info?
# 13 Could use leaflet tools to find stations in search box
# 14 Maybe just use a markdown page an an interface?
#
# AS 2024-02-05
#====================================================================================

# Load libraries
library(data.table)
library(glue)
library(DBI)
library(RPostgres)

#===============================================================================
# Functions to access database
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
# Load harmonics data: 4.5 MB uncompressed.
#=============================================================================

# Load harmonics data
load("data/harmonics.rda")

# #====================================================================
# # Check on specific stations around here. Why do I not get Tacoma?
# #====================================================================
#
# # Get all basic info
# qry = glue("select s.station_code, s.station_name, st.station_type_code, s.time_meridian, ",
#            "s.tide_type, s.datum_msl_meter, so.height_offset_factor_low_tide as low_factor ",
#            "from station as s ",
#            "left join station_type_lut as st on s.station_type_id = st.station_type_id ",
#            "left join station_offsets as so on s.station_id = so.station_id ",
#            "where s.station_code in ('9446484', '9445246','9447130','9446486')")
# pg_con = pg_con_local(dbname = "harmonics")
# local_st = dbGetQuery(pg_con, qry)
# dbDisconnect(pg_con)
#
# # Get harmonics info
# qry = glue("select s.station_code, count (sc.constituent_id) as n_consts ",
#            "from station_constituent as sc ",
#            "left join station as s on sc.station_id = s.station_id ",
#            "where s.station_code in ('9446484', '9445246','9447130','9446486') ",
#            "group by s.station_code")
# pg_con = pg_con_local(dbname = "harmonics")
# const_count = dbGetQuery(pg_con, qry)
# dbDisconnect(pg_con)
#
# # Combine
# local_st = merge(local_st, const_count, by = "station_code", all.x = TRUE)
# local_st = as.data.table(local_st)
# tacoma_st = local_st[station_name %ilike% "Tacoma"]
#
# #====================================================================
# # Some tests on the identify_station() function
# #====================================================================
#
# # See if I can pull tacoma from harms
# mt_st = MarineTides::identify_station("Tacoma;", harms = harmonics)
#
# # See if I can pull tacoma from harms
# MarineTides::identify_station("Sea", harms = harmonics)
#
# # Example of problem
# station_list = data.table(stations = c("Old Tacoma", "Tacoma", "Tacoma Narrows Bridge", "Tacoma, Commencement Bay"))
#
# # First try exact match, then do like
# match_try = station_list[stations == "Tacoma"]
# part_try = station_list[stations == "S Tacoma"]
# station_list[stations %ilike% "Tacoma"]

#====================================================================
# Checks on all stations in DB
#====================================================================

# Get all basic info
qry = glue("select s.station_code, ss.station_code as ref_station_code, s.station_name, ",
           "ST_Y (s.geog::geometry) as lat, ST_X (s.geog::geometry) as lng, ",
           "rg.region_code, st.station_type_code, s.time_meridian, s.tide_type, s.datum_msl_meter, ",
           "s.dst_observed, s.established, s.removed, s.epoch_start, s.epoch_end, ",
           "so.height_offset_factor_high_tide as high_factor, so.time_offset_low_tide_minutes as high_time, ",
           "so.height_offset_factor_low_tide as low_factor, so.time_offset_low_tide_minutes as low_time ",
           "from station as s ",
           "left join station_type_lut as st on s.station_type_id = st.station_type_id ",
           "left join region_lut as rg on s.region_id = rg.region_id ",
           "left join station_offsets as so on s.station_id = so.station_id ",
           "left join station as ss on so.reference_station_id = ss.station_id ",
           "where s.station_code is not null")
pg_con = pg_con_local(dbname = "harmonics")
all_stations = dbGetQuery(pg_con, qry)
dbDisconnect(pg_con)

# Get harmonics info
qry = glue("select s.station_code, count (sc.constituent_id) as n_consts ",
           "from station_constituent as sc ",
           "left join station as s on sc.station_id = s.station_id ",
           "where s.station_code is not null ",
           "group by s.station_code")
pg_con = pg_con_local(dbname = "harmonics")
const_counts = dbGetQuery(pg_con, qry)
dbDisconnect(pg_con)

# Combine
all_stations = merge(all_stations, const_counts, by = "station_code", all.x = TRUE)
all_stations_dt = as.data.table(all_stations)

# Pull out harmonic stations where time_meridian is zero
proper_harmonics = all_stations_dt[time_meridian == 0L & station_type_code == "H"]
improper_harmonics = all_stations_dt[!time_meridian == 0L & station_type_code == "H"]

# Pull out the proper subordinate stations
proper_subordinates = all_stations_dt[!is.na(low_factor) & station_type_code == "S"]
improper_subordinates = all_stations_dt[is.na(low_factor) & station_type_code == "S"]

#=========================================================================
# Questions:
#=========================================================================

# Check subordinate stations
all_subs = all_stations_dt[station_type_code == "S"]

# Do subordinate stations all have ref_station_codes? --Result: All have refs
nrow(all_subs[is.na(ref_station_code)])

# Do subordinate stations all have ref_station_codes and corrections? --Result: All have full set of offsets
nrow(all_subs[is.na(high_factor) | is.na(high_time) | is.na(low_factor) | is.na(low_time)])

# Check harmonic stations
all_harms = all_stations_dt[station_type_code == "H"]

# Do harms all have at least 37 constituents?
# --Result: All stations with less than 37 have been removed, except Atka and Lake Worth Pier
# -- Only Atka looks like it would be worthwhile keeping?
harms_missing_consts = all_harms[is.na(n_consts) | n_consts < 37L]
nrow(all_harms[is.na(n_consts) | n_consts < 37L])

# How many harms have not been removed?
harms_now = all_harms[is.na(removed)]; nrow(harms_now) # N = 266
any(is.na(harms_now$time_meridian)) # All have time_meridian values

# Of all current harms, how many have time_meridian zero
harms_now_timezero = harms_now[time_meridian == 0L]; nrow(harms_now_timezero)

# Of all current harms, how many have time_meridian not zero
harms_now_time_off = harms_now[!time_meridian == 0L]; nrow(harms_now_time_off)

#===============================================================================
# Compare rtide output for Tacoma, and other improper harmonic stations, to NOAA
# Use harms_now_time_off dataset
#===============================================================================

# # Seattle
# station_code = "9447130"
# start_date = "2024-02-06"
# end_date = "2024-02-06"
# time_interval = "60"

#==================================================================================
# Loop functions
#==================================================================================

# NOAA functions ==============================

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


# Construct function to loop through all entries in harms_now_time_off
noaa_tides_loop = function(station_list, start_date, end_date, time_interval = "60") {
  all_tides = NULL
  for ( i in seq_along(station_list$station_code) ) {
    station_code = station_list$station_code[i]
    st_tide = noaa_tides(station_code, start_date, end_date, time_interval)
    all_tides = rbind(st_tide, all_tides)
    Sys.sleep(2L)
  }
  return(all_tides)
}

# rtide functions ==============================

# rtide function
rtide_tides_loop = function(station_list, start_date, end_date, time_interval = 60L) {
  all_tides = NULL
  for ( i in seq_along(station_list$station_code) ) {
    st_namei = station_list$station_name[i]
    st_codei = station_list$station_code[i]
    station = tryCatch(rtide::tide_stations(st_namei), error = function(e) {return(NA)})
    if ( is.na(station[1]) ) {
      next
    }
    rtidei = rtide::tide_height(stations = st_namei,
                                from = as.Date(start_date),
                                to = as.Date(end_date),
                                minutes = time_interval,
                                tz = "UTC")
    rtidei$station_code = st_codei
    names(rtidei) = c("rstation_name", "tide_time", "rtide_level", "station_code")
    all_tides = rbind(rtidei, all_tides)
  }
  return(all_tides)
}

# rtide functions ==============================

# Useful to test ability to get names
mtide_tides_loop = function(station_list, start_date, end_date, data_interval = "60-min") {
  all_tides = NULL
  for ( i in seq_along(station_list$station_code) ) {
    st_namei = station_list$station_name[i]
    station_code = tryCatch(MarineTides::identify_station(st_namei, verbose = FALSE), error = function(e) {return(NA)})
    if ( is.na(station_code[1]) ) {
      next
    }
    mtidei = MarineTides::tide_level(tide_station = st_namei,
                                     start_date,
                                     end_date,
                                     data_interval,
                                     timezone = "UTC")
    names(mtidei) = c("station_code", "mstation_name", "reference_station_code",
                      "tide_type", "tide_time", "mtide_level")
    all_tides = rbind(mtidei, all_tides)
  }
  return(all_tides)
}

# # Also useful, but will go directly to code
# mtide_tides_loop = function(station_list, start_date, end_date, data_interval = "60-min") {
#   all_tides = NULL
#   for ( i in seq_along(station_list$station_code) ) {
#     st_namei = station_list$station_name[i]
#     st_codei = station_list$station_code[i]
#     station_code = tryCatch(MarineTides::identify_station(st_namei, verbose = FALSE), error = function(e) {return(NA)})
#     if ( is.na(station_code[1]) ) {
#       harms = MarineTides::harmonics
#       stations_dt = as.data.table(harms$st_data)
#       st_names = stations_dt[station_code == st_codei, list(station_name, station_code)]
#       st_namei = st_names$station_code
#     }
#     mtidei = MarineTides::tide_level(tide_station = st_namei,
#                                      start_date,
#                                      end_date,
#                                      data_interval,
#                                      timezone = "UTC")
#     names(mtidei) = c("station_code", "mstation_name", "reference_station_code",
#                       "tide_type", "tide_time", "mtide_level")
#     all_tides = rbind(mtidei, all_tides)
#   }
#   return(all_tides)
# }

#==================================================================================
# Run the loop functions
#==================================================================================

# Pull out subset with names and codes
harms_time_off = harms_now_time_off[,c("station_code", "station_name")]

# Get noaa predictions
harms_noaa = noaa_tides_loop(harms_time_off,
                             start_date = "2024-02-06",
                             end_date = "2024-02-06",
                             time_interval = "60")

# Get all rtide predictions
harms_rtide = rtide_tides_loop(harms_time_off,
                               start_date = "2024-02-06",
                               end_date = "2024-02-06")

# Get all mtide predictions
harms_mtide = mtide_tides_loop(harms_time_off,
                               start_date = "2024-02-06",
                               end_date = "2024-02-06")

# Get the initial data back
harms_noaa = merge(harms_now_time_off, harms_noaa,
                   by = "station_code", all.x = TRUE)

# Join rtide and noaa as comb_tide
harms_noaa$tide_time = as.POSIXct(harms_noaa$tide_time, tz = "UTC")
comb_tide = merge(harms_noaa, harms_rtide, by = c("station_code", "tide_time"), all.x = TRUE)

# Join mtide to comb_tide
harms_mtide = harms_mtide[, .(station_code, tide_time, mtide_level)]
comb_tide = merge(comb_tide, harms_mtide, by = c("station_code", "tide_time"), all.x = TRUE)
comb_tide = comb_tide[, c("station_name", "station_code", "time_meridian", "established", "removed",
                          "epoch_start", "epoch_end", "n_consts", "tide_time", "tide_level",
                          "rtide_level", "mtide_level")]

# Print station names
unique(comb_tide$station_name)

#==================================================================================
# Plot Tacoma as a check
#==================================================================================

# Plot Tacoma
check_tide = comb_tide[station_name == "Tacoma"]
plot(check_tide$tide_time, check_tide$tide_level, type = "l", col = "blue")
lines(check_tide$tide_time, check_tide$rtide_level, col = "red")
lines(check_tide$tide_time, check_tide$mtide_level, col = "green")

#==================================================================================
# Identify differences
#==================================================================================

# Round tide hts   MTIDES LOOKS EXCELLENT !!!!
comb_tide$rtide_level = round(comb_tide$rtide_level, digits = 3)
comb_tide$mtide_level = round(comb_tide$mtide_level, digits = 3)
comb_tide$difr = abs(comb_tide$tide_level - comb_tide$rtide_level)
comb_tide$difm = abs(comb_tide$tide_level - comb_tide$mtide_level)

# Check the one missing Charleston, Oregon....Need to re-import harmonics
charelston = MarineTides::tide_level("Charleston",
                                     start_date = "2024-02-06",
                                     end_date = "2024-02-06",
                                     data_interval = "60-min",
                                     timezone = "UTC",
                                     verbose = TRUE)

# Check if any other station_names clash
dup_names = all_stations$station_name[duplicated(all_stations$station_name)]
dup_stations = subset(all_stations, station_name %in% dup_names)
dup_stations = dup_stations[order(dup_stations$station_name),]














# Test Atka: < 37 consts
rtide::tide_height("Atka",
                   minutes = 60L)



# # Some SQL tests
# SELECT ty.station_type_code, count( ty.station_type_code)
# FROM station as st
# LEFT JOIN station_type_lut as ty on ty.station_type_id = st.station_type_id
# group by ty.station_type_code






















