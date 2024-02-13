#====================================================================================
# Check all station data in Harmonics DB
#
#  Notes:
#  1. I checked Station info for Tacoma. No notes about station being removed
#     Using time_meridian = 0, getting perfect match with NOAA predictions
#  2. For stations with < 37 constants, mtide matched noaa, rtide somewhat worse.
#  3. Needed to remove 9450623 Big Salt Lake, AK. No harmonics data even though
#     it is listed as a harmonic station. No predictions on NOAA website. Got
#     error about invalid datum input.
#
#
#  Questions:
#  1. Do subs all have ref codes and corrections? --Done: All have ref codes and full set of offsets.
#  2. Do harms all have at least 37 constituents, and have any been removed? --No, some have less,
#     Of stations with < 37 constituents, all but Atka and Lake Worth Pier have had stations removed.
#  5. Create a leaflet interface to identify stations?
#  6. Do a better job with identify_station() function  -- Done
#  7. For now, filter out stations with meridians other than 0?  --No, when set to zero, output correct.
#  8. Redo harmonics data to eliminate unneeded harmonic stations. --No will keep, but warn if removed!
#  9. Rewrite docs to give new counts.
# 10. Use leaflet tools to find stations in search box?
# 11. Use a markdown page an an interface?
# 12. Do foreign station predictions match NOAA web output?
#
# AS 2024-02-05
#====================================================================================

# Load libraries
library(MarineTides)
library(data.table)
library(glue)
library(DBI)
library(RPostgres)
library(TideHarmonics)

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

#====================================================================
# Get data for all stations in DB
#====================================================================

# Get all basic info
qry = glue("select s.station_code, ss.station_code as ref_station_code, s.station_name, ",
           "ST_Y (s.geog::geometry) as lat, ST_X (s.geog::geometry) as lng, ",
           "rg.region_code, rg.country_name, st.station_type_code, s.time_meridian, s.tide_type, ",
           "s.datum_msl_meter, s.dst_observed, s.established, s.removed, s.epoch_start, s.epoch_end, ",
           "so.height_offset_high_tide as high_offset, so.time_offset_low_tide_minutes as high_time, ",
           "so.height_offset_low_tide as low_offset, so.time_offset_low_tide_minutes as low_time ",
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
all_harms = all_stations_dt[station_type_code == "H"]

# Pull out the proper subordinate stations
all_subs = all_stations_dt[station_type_code == "S"]

#=========================================================================
# Questions:
#=========================================================================

# Do subordinate stations all have ref_station_codes? --Result: All have refs
nrow(all_subs[is.na(ref_station_code)])

# Do subordinate stations all have ref_station_codes and corrections? --Result: All have full set of offsets
nrow(all_subs[is.na(high_offset) | is.na(high_time) | is.na(low_offset) | is.na(low_time)])

# How many harms have not been removed?
harms_now = all_harms[is.na(removed)]; nrow(harms_now) # N = 266
any(is.na(harms_now$time_meridian)) # All have time_meridian values

#==================================================================================
# Loop functions
#==================================================================================

# NOAA functions ==============================

# station_code = "1778000"
# #station_code = "9441187"
# start_date = "2024-02-06"
# end_date = "2024-02-06"
# time_interval = 60L
#
# # Test
# noaa_tides(station_code, start_date, end_date, time_interval = "60")

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
  if ( !time_interval == "hilo" ) {
    noaa_tides$tide_type = "P"
    names(noaa_tides) = c("tide_time", "tide_level",  "station_code", "tide_type")
  } else {
    names(noaa_tides) = c("tide_time", "tide_level", "tide_type", "station_code")
  }
  tides_out = noaa_tides[, c("tide_time", "station_code", "tide_type", "tide_level")]
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

# mtide functions ==============================

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

#==================================================================================
# Run the loop functions on first set of harm stations
#==================================================================================

# Pull out subset with names and codes
is.data.table(all_harms)
harms_one = all_harms[1:250, .(station_code, station_name)]

# =========================================================

# Get noaa predictions
tm = Sys.time()
harms_noaa_one = noaa_tides_loop(harms_one,
                                 start_date = "2024-02-06",
                                 end_date = "2024-02-06",
                                 time_interval = "60")
nd = Sys.time(); nd - tm  # 10.84063 mins

# =========================================================

# Get all rtide predictions
tm = Sys.time()
harms_rtide_one = rtide_tides_loop(harms_one,
                                   start_date = "2024-02-06",
                                   end_date = "2024-02-06")
nd = Sys.time(); nd - tm  # 6.264818 secs

# =========================================================

# Get all mtide predictions
tm = Sys.time()
harms_mtide_one = mtide_tides_loop(harms_one,
                                   start_date = "2024-02-06",
                                   end_date = "2024-02-06")
nd = Sys.time(); nd - tm  # 6.264818 secs

# =========================================================

# Add the full set of info back in
harms_noaa_one_st = merge(harms_noaa_one, harms_one,
                          by = "station_code", all.x = TRUE)

# Pull out any data from NOAA that failed to get point estimates
harms_noaa_one_st = as.data.table(harms_noaa_one_st)
harms_noaa_one_fail = harms_noaa_one_st[is.na(tide_level)]

# # Write results to temp table in harmonics DB
# pg_con = pg_con_local(dbname = "harmonics")
# tbl = Id(schema = "public", table = "noaa_harms_fail_temp")
# DBI::dbWriteTable(pg_con, tbl, harms_noaa_one_fail, row.names = FALSE, append = TRUE, copy = TRUE)
# DBI::dbDisconnect(pg_con)

# Results:
# 1778000, APIA: Now fixed in mtide...Is a sub in NOAA so won't process for point estimates
# 8517251, Worlds Fair Marina, Flushing Bay: Errors on website.
#          Says: Tide predictions are not available for this station. Remove
# 8517756, Kingsborough, Sheepshead Bay: Errors on website.
#          Says: Tide predictions are not available for this station. Remove

# Pull out data that did not fail
harms_noaa_one_st = harms_noaa_one_st[!is.na(tide_level)]

# Join rtide and noaa as comb_tide
harms_noaa_one_st$tide_time = as.POSIXct(harms_noaa_one_st$tide_time, tz = "UTC")
is.data.table(harms_noaa_one_st)
comb_tide_one = merge(harms_noaa_one_st, harms_rtide_one, by = c("station_code", "tide_time"), all.x = TRUE)

# Join mtide to comb_tide
is.data.table(harms_mtide_one)
harms_mtide_one = harms_mtide_one[, .(station_code, tide_time, mtide_level)]
comb_tide_one = merge(comb_tide_one, harms_mtide_one, by = c("station_code", "tide_time"), all.x = TRUE)

# Add in remaining data
comb_tide_one = merge(comb_tide_one, all_harms, by = "station_code", all.x = TRUE)
comb_tide_one = comb_tide_one[, .(station_name = station_name.x, station_code, time_meridian,
                                  established, removed, epoch_start, epoch_end, n_consts,
                                  tide_time, tide_level, rtide_level, mtide_level)]

# Identify differences ==========================

# Round tide hts   MTIDES LOOKS EXCELLENT !!!!
is.data.table(harms_mtide_one)
comb_tide_one$rtide_level = round(comb_tide_one$rtide_level, digits = 3)
comb_tide_one$mtide_level = round(comb_tide_one$mtide_level, digits = 3)
comb_tide_one$difr = abs(comb_tide_one$tide_level - comb_tide_one$rtide_level)
comb_tide_one$difm = abs(comb_tide_one$tide_level - comb_tide_one$mtide_level)

# Max difference for rtide: Result: 5.049m, huge = 16.56496ft
(max_diff_rtide_one = max(comb_tide_one$difr, na.rm = TRUE) / 0.3048)

# Max difference for mtide: Result: 0.001m, just rounding error.
(max_diff_mtide_one = max(comb_tide_one$difm, na.rm = TRUE) / 0.3048)

# # Write results to temp table in harmonics DB
# pg_con = pg_con_local(dbname = "harmonics")
# tbl = Id(schema = "public", table = "harms_comparison")
# DBI::dbWriteTable(pg_con, tbl, comb_tide_one, row.names = FALSE, append = TRUE, copy = TRUE)
# DBI::dbDisconnect(pg_con)

#==================================================================================
# Run the loop functions on second set of harm stations
#==================================================================================

# Pull out subset with names and codes
is.data.table(all_harms)
harms_two = all_harms[251:500, .(station_code, station_name)]

# =========================================================

# Get noaa predictions
tm = Sys.time()
harms_noaa_two = noaa_tides_loop(harms_two,
                                 start_date = "2024-02-06",
                                 end_date = "2024-02-06",
                                 time_interval = "60")
nd = Sys.time(); nd - tm  # 10.84063 mins

# =========================================================

# Get all rtide predictions
tm = Sys.time()
harms_rtide_two = rtide_tides_loop(harms_two,
                                   start_date = "2024-02-06",
                                   end_date = "2024-02-06")
nd = Sys.time(); nd - tm  # 6.264818 secs

# =========================================================

# Get all mtide predictions
tm = Sys.time()
harms_mtide_two = mtide_tides_loop(harms_two,
                                   start_date = "2024-02-06",
                                   end_date = "2024-02-06")
nd = Sys.time(); nd - tm  # 6.264818 secs

# =========================================================

# Add the full set of info back in
harms_noaa_two_st = merge(harms_noaa_two, harms_two,
                          by = "station_code", all.x = TRUE)

# Pull out any data from NOAA that failed to get point estimates
harms_noaa_two_st = as.data.table(harms_noaa_two_st)
harms_noaa_two_fail = harms_noaa_two_st[is.na(tide_level)]

# # Write results to temp table in harmonics DB
# pg_con = pg_con_local(dbname = "harmonics")
# tbl = Id(schema = "public", table = "noaa_harms_fail_temp")
# DBI::dbWriteTable(pg_con, tbl, harms_noaa_one_fail, row.names = FALSE, append = TRUE, copy = TRUE)
# DBI::dbDisconnect(pg_con)

# Results:
# 1778000, APIA: Now fixed in mtide...Is a sub in NOAA so won't process for point estimates
# 8517251, Worlds Fair Marina, Flushing Bay: Errors on website.
#          Says: Tide predictions are not available for this station. Remove
# 8517756, Kingsborough, Sheepshead Bay: Errors on website.
#          Says: Tide predictions are not available for this station. Remove

# Pull out data that did not fail
harms_noaa_two_st = harms_noaa_two_st[!is.na(tide_level)]

# Join rtide and noaa as comb_tide
harms_noaa_two_st$tide_time = as.POSIXct(harms_noaa_two_st$tide_time, tz = "UTC")
is.data.table(harms_noaa_two_st)
comb_tide_two = merge(harms_noaa_two_st, harms_rtide_two, by = c("station_code", "tide_time"), all.x = TRUE)

# Join mtide to comb_tide
is.data.table(harms_mtide_two)
harms_mtide_two = harms_mtide_two[, .(station_code, tide_time, mtide_level)]
comb_tide_two = merge(comb_tide_two, harms_mtide_two, by = c("station_code", "tide_time"), all.x = TRUE)

# Add in remaining data
comb_tide_two = merge(comb_tide_two, all_harms, by = "station_code", all.x = TRUE)
comb_tide_two = comb_tide_two[, .(station_name = station_name.x, station_code, time_meridian,
                                  established, removed, epoch_start, epoch_end, n_consts,
                                  tide_time, tide_level, rtide_level, mtide_level)]

# Identify differences ==========================

# Round tide hts   MTIDES LOOKS EXCELLENT !!!!
is.data.table(harms_mtide_one)
comb_tide_two$rtide_level = round(comb_tide_two$rtide_level, digits = 3)
comb_tide_two$mtide_level = round(comb_tide_two$mtide_level, digits = 3)
comb_tide_two$difr = abs(comb_tide_two$tide_level - comb_tide_two$rtide_level)
comb_tide_two$difm = abs(comb_tide_two$tide_level - comb_tide_two$mtide_level)

# Max difference for rtide: Result: 5.049m, huge = 10.32152ft
(max_diff_rtide_two = max(comb_tide_two$difr, na.rm = TRUE) / 0.3048)

# Max difference for mtide: Result: 0.00328084 ft, just rounding error.
(max_diff_mtide_two = max(comb_tide_two$difm, na.rm = TRUE) / 0.3048)

# # Write results to temp table in harmonics DB
# pg_con = pg_con_local(dbname = "harmonics")
# tbl = Id(schema = "public", table = "harms_comparison")
# DBI::dbWriteTable(pg_con, tbl, comb_tide_two, row.names = FALSE, append = TRUE, copy = TRUE)
# DBI::dbDisconnect(pg_con)

#==================================================================================
# Run the loop functions on third set of harm stations
#==================================================================================

# Pull out subset with names and codes
is.data.table(all_harms)
harms_three = all_harms[501:750, .(station_code, station_name)]

# =========================================================

# Get noaa predictions
tm = Sys.time()
harms_noaa_three = noaa_tides_loop(harms_three,
                                   start_date = "2024-02-06",
                                   end_date = "2024-02-06",
                                   time_interval = "60")
nd = Sys.time(); nd - tm  # 11.28623 mins

# =========================================================

# Get all rtide predictions
tm = Sys.time()
harms_rtide_three= rtide_tides_loop(harms_three,
                                    start_date = "2024-02-06",
                                    end_date = "2024-02-06")
nd = Sys.time(); nd - tm  # 4.314973 secs

# =========================================================

# Get all mtide predictions
tm = Sys.time()
harms_mtide_three = mtide_tides_loop(harms_three,
                                     start_date = "2024-02-06",
                                     end_date = "2024-02-06")
nd = Sys.time(); nd - tm  # 10.08324 secs

# =========================================================

# Add the full set of info back in
harms_noaa_three_st = merge(harms_noaa_three, harms_three,
                            by = "station_code", all.x = TRUE)

# Pull out any data from NOAA that failed to get point estimates
harms_noaa_three_st = as.data.table(harms_noaa_three_st)
harms_noaa_three_fail = harms_noaa_three_st[is.na(tide_level)]

# # Write results to temp table in harmonics DB
# pg_con = pg_con_local(dbname = "harmonics")
# tbl = Id(schema = "public", table = "noaa_harms_fail_temp")
# DBI::dbWriteTable(pg_con, tbl, harms_noaa_one_fail, row.names = FALSE, append = TRUE, copy = TRUE)
# DBI::dbDisconnect(pg_con)

# Results:
# 1778000, APIA: Now fixed in mtide...Is a sub in NOAA so won't process for point estimates
# 8517251, Worlds Fair Marina, Flushing Bay: Errors on website.
#          Says: Tide predictions are not available for this station. Remove
# 8517756, Kingsborough, Sheepshead Bay: Errors on website.
#          Says: Tide predictions are not available for this station. Remove

# Pull out data that did not fail
harms_noaa_three_st = harms_noaa_three_st[!is.na(tide_level)]

# Join rtide and noaa as comb_tide
harms_noaa_three_st$tide_time = as.POSIXct(harms_noaa_three_st$tide_time, tz = "UTC")
is.data.table(harms_noaa_three_st)
comb_tide_three = merge(harms_noaa_three_st, harms_rtide_three, by = c("station_code", "tide_time"), all.x = TRUE)

# Join mtide to comb_tide
is.data.table(harms_mtide_three)
harms_mtide_three = harms_mtide_three[, .(station_code, tide_time, mtide_level)]
comb_tide_three = merge(comb_tide_three, harms_mtide_three, by = c("station_code", "tide_time"), all.x = TRUE)

# Add in remaining data
comb_tide_three = merge(comb_tide_three, all_harms, by = "station_code", all.x = TRUE)
comb_tide_three = comb_tide_three[, .(station_name = station_name.x, station_code, time_meridian,
                                      established, removed, epoch_start, epoch_end, n_consts,
                                      tide_time, tide_level, rtide_level, mtide_level)]

# Identify differences ==========================

# Round tide hts   MTIDES LOOKS EXCELLENT !!!!
comb_tide_three$rtide_level = round(comb_tide_three$rtide_level, digits = 3)
comb_tide_three$mtide_level = round(comb_tide_three$mtide_level, digits = 3)
comb_tide_three$difr = abs(comb_tide_three$tide_level - comb_tide_three$rtide_level)
comb_tide_three$difm = abs(comb_tide_three$tide_level - comb_tide_three$mtide_level)

# Max difference for rtide: Result: huge = 6.138451ft
(max_diff_rtide_three = max(comb_tide_three$difr, na.rm = TRUE) / 0.3048)

# Max difference for mtide: Result: 0.00328084 ft, just rounding error.
(max_diff_mtide_three = max(comb_tide_three$difm, na.rm = TRUE) / 0.3048)

# # Write results to temp table in harmonics DB
# pg_con = pg_con_local(dbname = "harmonics")
# tbl = Id(schema = "public", table = "harms_comparison")
# DBI::dbWriteTable(pg_con, tbl, comb_tide_three, row.names = FALSE, append = TRUE, copy = TRUE)
# DBI::dbDisconnect(pg_con)

#==================================================================================
# Run the loop functions on fourth set of harm stations
#==================================================================================

# Pull out subset with names and codes
harms_four = all_harms[751:1139, .(station_code, station_name)]

# =========================================================

# Get noaa predictions
tm = Sys.time()
harms_noaa_four = noaa_tides_loop(harms_four,
                                  start_date = "2024-02-06",
                                  end_date = "2024-02-06",
                                  time_interval = "60")
nd = Sys.time(); nd - tm  # 18.69807 mins

# =========================================================

# Get all rtide predictions
tm = Sys.time()
harms_rtide_four = rtide_tides_loop(harms_four,
                                    start_date = "2024-02-06",
                                    end_date = "2024-02-06")
nd = Sys.time(); nd - tm  # 7.772297 secs

# =========================================================

# Get all mtide predictions
tm = Sys.time()
harms_mtide_four = mtide_tides_loop(harms_four,
                                    start_date = "2024-02-06",
                                    end_date = "2024-02-06")
nd = Sys.time(); nd - tm  # 15.98584 secs

# =========================================================

# Add the full set of info back in
harms_noaa_four_st = merge(harms_noaa_four, harms_four,
                           by = "station_code", all.x = TRUE)

# Pull out any data from NOAA that failed to get point estimates
harms_noaa_four_st = as.data.table(harms_noaa_four_st)
harms_noaa_four_fail = harms_noaa_four_st[is.na(tide_level)]

# # Write results to temp table in harmonics DB
# pg_con = pg_con_local(dbname = "harmonics")
# tbl = Id(schema = "public", table = "noaa_harms_fail_temp")
# DBI::dbWriteTable(pg_con, tbl, harms_noaa_one_fail, row.names = FALSE, append = TRUE, copy = TRUE)
# DBI::dbDisconnect(pg_con)

# Results:
# 1778000, APIA: Now fixed in mtide...Is a sub in NOAA so won't process for point estimates
# 8517251, Worlds Fair Marina, Flushing Bay: Errors on website.
#          Says: Tide predictions are not available for this station. Remove
# 8517756, Kingsborough, Sheepshead Bay: Errors on website.
#          Says: Tide predictions are not available for this station. Remove

# Pull out data that did not fail
harms_noaa_four_st = harms_noaa_four_st[!is.na(tide_level)]

# Join rtide and noaa as comb_tide
harms_noaa_four_st$tide_time = as.POSIXct(harms_noaa_four_st$tide_time, tz = "UTC")
comb_tide_four = merge(harms_noaa_four_st, harms_rtide_four, by = c("station_code", "tide_time"), all.x = TRUE)

# Join mtide to comb_tide
harms_mtide_four = harms_mtide_four[, .(station_code, tide_time, mtide_level)]
comb_tide_four = merge(comb_tide_four, harms_mtide_four, by = c("station_code", "tide_time"), all.x = TRUE)

# Add in remaining data
comb_tide_four = merge(comb_tide_four, all_harms, by = "station_code", all.x = TRUE)
comb_tide_four = comb_tide_four[, .(station_name = station_name.x, station_code, time_meridian,
                                    established, removed, epoch_start, epoch_end, n_consts,
                                    tide_time, tide_level, rtide_level, mtide_level)]

# Identify differences ==========================

# Round tide hts   MTIDES LOOKS EXCELLENT !!!!
comb_tide_four$rtide_level = round(comb_tide_four$rtide_level, digits = 3)
comb_tide_four$mtide_level = round(comb_tide_four$mtide_level, digits = 3)
comb_tide_four$difr = abs(comb_tide_four$tide_level - comb_tide_four$rtide_level)
comb_tide_four$difm = abs(comb_tide_four$tide_level - comb_tide_four$mtide_level)

# Max difference for rtide: Result: huge = 10.13451ft
(max_diff_rtide_four = max(comb_tide_four$difr, na.rm = TRUE) / 0.3048)

# Max difference for mtide: Result: 1.437008 ft. NEED TO INVESTIGATE !!!!!!!!!!!!!!!!!!!!!!!!!!
(max_diff_mtide_four = max(comb_tide_four$difm, na.rm = TRUE) / 0.3048)

# # Write results to temp table in harmonics DB
# pg_con = pg_con_local(dbname = "harmonics")
# tbl = Id(schema = "public", table = "harms_comparison")
# DBI::dbWriteTable(pg_con, tbl, comb_tide_four, row.names = FALSE, append = TRUE, copy = TRUE)
# DBI::dbDisconnect(pg_con)

#=======================================================================================
# Investigate differences further
#=======================================================================================

# Anchorage is the problem. May not be using all year constituents. Rtide is better !!!!

# Verified Anchorage has 120 constituents in station_constituent table
# But there are only 111 year constituents in year_constituent table
# Both Anchorage AK (9455920) and Chatham MA (8447435) have 120 constituents
# Need to re-upload year_constituents

# Pull out node_year data to see if I have all the node_year constituents in the DB. I do not
test_harms = rtide::harmonics
node_year = test_harms$NodeYear
node_year_df = as.data.table(node_year)
node_year_2024 = node_year_df[V2 == "2024"]
node_year_2024 = dcast(node_year_2024, V1 + V2 ~ V3, value.var = "value")
node_year_2024 = node_year_2024[, .(constituent_code = V1, node_year = V2,
                                    year_factor = NodeFactor, equilibrium_arg = EquilArg)]

# Update two constituents to new name then check to see all have match in constituent table
node_year_2024$constituent_code[node_year_2024$constituent_code %in% "LDA2"] = "LAM2"
node_year_2024$constituent_code[node_year_2024$constituent_code %in% "RHO1"] = "RHO"

# Update remaining constituents uncovered below
node_year_2024$constituent_code[node_year_2024$constituent_code %in% "SIG1"] = "SIGMA1"
node_year_2024$constituent_code[node_year_2024$constituent_code %in% "M2(KS)2"] = "M2KS2"
node_year_2024$constituent_code[node_year_2024$constituent_code %in% "2SN(MK)2"] = "2SNMK2"
node_year_2024$constituent_code[node_year_2024$constituent_code %in% "2KM(SN)2"] = "2KMSN2"
node_year_2024$constituent_code[node_year_2024$constituent_code %in% "2MNLS6"] = "2MLNS6"
node_year_2024$constituent_code[node_year_2024$constituent_code %in% "2(MS)8"] = "2MS8"
node_year_2024$constituent_code[node_year_2024$constituent_code %in% "2(MN)8"] = "2MN8"
node_year_2024$constituent_code[node_year_2024$constituent_code %in% "THE1"] = "THETA1"
node_year_2024$constituent_code[node_year_2024$constituent_code %in% "OQ2"] = "OO2"

# Get all constituents from DB
qry = glue::glue("select constituent_id, constituent_order, constituent_code ",
                 "from constituent")

pg_con = pg_con_local(dbname = "harmonics")
constituents = DBI::dbGetQuery(pg_con, qry)
DBI::dbDisconnect(pg_con)

# Check if all constituent_codes in constituents are in node_year_2024.
# They should not all be there, since there are 175 constituents in rtide and only 120 in mtide
all(constituents$constituent_code %in% node_year_2024$constituent_code)

# Find out which are missing
constituents$constituent_code[!constituents$constituent_code %in% node_year_2024$constituent_code]

# Join to see which are missing
consts = merge(constituents, node_year_2024, by = "constituent_code", all.x = TRUE)
consts = as.data.table(consts)
consts = consts[order(constituent_order)]

# Need to verify but:
#     HarmDB          RTide             Confirmed via TideHarmonics?
# 1.  SIGMA1          sig1              Yes
# 2.  M2KS2           M2(KS)2           x2K1M.2S2 or 2KM2S2
# 3.  2SNMK2          2SN(MK)2
# 4.  2KMSN2          2KM(SN)2
# 5.  2MLNS6          2MNLS6
# 6.  2MS8
# 7.  2MN8
# 8.  THETA1          THE1
# 9.  OO2

# Get names of harmonics from TideHarmonics package
harm_names = TideHarmonics::harmonics

# Inspect Node to see name for final missing constituent: 2MLNS6, or maybe 4MS6: Result: It was 2MNLS6
rtide_node = test_harms$Node
rtide_node = as.data.table(rtide_node)



# NEXT: RUN THROUGH SUBORDINATE STATIONS !!!!!!!!!!!!!!!!!!!!!!!!!!






























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
# Identify differences where time_meridian is not listed by NOAA as zero
#==================================================================================

# Round tide hts   MTIDES LOOKS EXCELLENT !!!!
comb_tide$rtide_level = round(comb_tide$rtide_level, digits = 3)
comb_tide$mtide_level = round(comb_tide$mtide_level, digits = 3)
comb_tide$difr = abs(comb_tide$tide_level - comb_tide$rtide_level)
comb_tide$difm = abs(comb_tide$tide_level - comb_tide$mtide_level)

# Max difference for rtide: Result: 0.321m, fairly substantial = 1.05315ft
max(comb_tide$difr, na.rm = TRUE)
0.321 / 0.3048

# Max difference for mtide: Result: 0.001m, just rounding error.
max(comb_tide$difm, na.rm = TRUE)

# # Check the one missing Charleston, Oregon....Need to re-import harmonics
# charelston = MarineTides::tide_level("Charleston, OR",
#                                      start_date = "2024-02-06",
#                                      end_date = "2024-02-06",
#                                      data_interval = "60-min",
#                                      timezone = "UTC",
#                                      verbose = TRUE)

# Check if any other station_names clash....None left after fixup in TideHarmonicsDB/R/harmonics_cleanup.R
dup_names = all_stations$station_name[duplicated(all_stations$station_name)]
dup_stations = subset(all_stations, station_name %in% dup_names)
dup_stations = dup_stations[order(dup_stations$station_name),]

# Test Atka: < 37 consts

# rtide
rtide_atka = rtide::tide_height("Atka",
                                from = as.Date("2024-02-06"),
                                to = as.Date("2024-02-06"),
                                minutes = 60L,
                                tz = "UTC")
# mtide
mtide_atka = MarineTides::tide_level("Atka",
                                     start_date = "2024-02-06",
                                     end_date = "2024-02-06",
                                     data_interval = "60-min",
                                     timezone = "UTC")
# noaa
noaa_atka = noaa_tides(station_code = "9461710",
                       start_date = "2024-02-06",
                       end_date = "2024-02-06",
                       time_interval = 60L)

# Combine and compare: mtide excellent, rtide a bit off.
comb_atka = cbind(noaa_atka, mtide_atka$tide_level, rtide_atka$TideHeight)
names(comb_atka) = c("tide_time", "station_code", "noaa_tide_level", "mtide_level", "rtide_level")

# Test Lake Worth Pier: < 37 consts

# rtide
rtide_worth = rtide::tide_height("Lake Worth Pier",
                                from = as.Date("2024-02-06"),
                                to = as.Date("2024-02-06"),
                                minutes = 60L,
                                tz = "UTC")
# mtide
mtide_worth = MarineTides::tide_level("Lake Worth Pier",
                                     start_date = "2024-02-06",
                                     end_date = "2024-02-06",
                                     data_interval = "60-min",
                                     timezone = "UTC")
# noaa
worth_code = mtide_worth$station_code[1]
noaa_worth = noaa_tides(station_code = worth_code,
                       start_date = "2024-02-06",
                       end_date = "2024-02-06",
                       time_interval = 60L)

# Combine and compare: mtide excellent, rtide a bit off.
comb_worth = cbind(noaa_worth, mtide_worth$tide_level, rtide_worth$TideHeight)
names(comb_worth) = c("tide_time", "station_code", "noaa_tide_level", "mtide_level", "rtide_level")

#==================================================================================
# Run the loop functions on all harm stations with less than 37 consts
#==================================================================================

# Pull out subset with names and codes
harms_miss_consts = harms_missing_consts[,c("station_code", "station_name")]

# Get noaa predictions
tm = Sys.time()
harms_noaa = noaa_tides_loop(harms_miss_consts,
                             start_date = "2024-02-06",
                             end_date = "2024-02-06",
                             time_interval = "60")
nd = Sys.time(); nd - tm  # 53.20814 secs

# Get all rtide predictions
tm = Sys.time()
harms_rtide = rtide_tides_loop(harms_miss_consts,
                               start_date = "2024-02-06",
                               end_date = "2024-02-06")
nd = Sys.time(); nd - tm  # 0.7237051 secs

# Get all mtide predictions
tm = Sys.time()
harms_mtide = mtide_tides_loop(harms_miss_consts,
                               start_date = "2024-02-06",
                               end_date = "2024-02-06")
nd = Sys.time(); nd - tm  # 0.7298121 sec

# Get the initial data back
harms_noaa = merge(harms_missing_consts, harms_noaa,
                   by = "station_code", all.x = TRUE)

# Join rtide and noaa as comb_tide
harms_noaa$tide_time = as.POSIXct(harms_noaa$tide_time, tz = "UTC")
comb_tide_mc = merge(harms_noaa, harms_rtide, by = c("station_code", "tide_time"), all.x = TRUE)

# Join mtide to comb_tide
harms_mtide = harms_mtide[, .(station_code, tide_time, mtide_level)]
comb_tide_mc = merge(comb_tide_mc, harms_mtide, by = c("station_code", "tide_time"), all.x = TRUE)
comb_tide_mc = comb_tide_mc[, c("station_name", "station_code", "time_meridian", "established", "removed",
                                "epoch_start", "epoch_end", "n_consts", "tide_time", "tide_level",
                                "rtide_level", "mtide_level")]

#==================================================================================
# Identify differences on harms with less than 37 consts
#==================================================================================

# Round tide hts   MTIDES LOOKS EXCELLENT !!!!
comb_tide_mc$rtide_level = round(comb_tide_mc$rtide_level, digits = 3)
comb_tide_mc$mtide_level = round(comb_tide_mc$mtide_level, digits = 3)
comb_tide_mc$difr = abs(comb_tide_mc$tide_level - comb_tide_mc$rtide_level)
comb_tide_mc$difm = abs(comb_tide_mc$tide_level - comb_tide_mc$mtide_level)

# Max difference for rtide: Result: 1.711m, Quite substantial = 5.613517ft !!!
max(comb_tide_mc$difr, na.rm = TRUE)
1.711 / 0.3048

# Max difference for mtide: Result: 0.001m, just rounding error.
max(comb_tide_mc$difm, na.rm = TRUE)

#==================================================================================
# Pull out foreign stations to test vs rtide and noaa
#==================================================================================

# Get foreign stations
foreign_st = subset(all_stations, !country_name == "United States")

# Get foreign harmonic stations
foreign_sth = subset(foreign_st, station_type_code == "H")

# Get foreign subordinate stations
foreign_sts = subset(foreign_st, station_type_code == "S")

# Check if all foreign_sts reference stations are in all_stations: Result, all present
all(foreign_sts$station_code %in% all_stations$station_code)

#==================================================================================
# Test foreign harmonic stations vs rtide and noaa
#==================================================================================

# Pull out subset with names and codes
harms_foreign = foreign_sth[,c("station_code", "station_name")]

# Get noaa predictions
tm = Sys.time()
harms_noaa = noaa_tides_loop(harms_foreign,
                             start_date = "2024-02-06",
                             end_date = "2024-02-06",
                             time_interval = "60")
nd = Sys.time(); nd - tm  # 26.24402 secs

# Get all rtide predictions
tm = Sys.time()
harms_rtide = rtide_tides_loop(harms_foreign,
                               start_date = "2024-02-06",
                               end_date = "2024-02-06")
nd = Sys.time(); nd - tm  # 0.1125519 secs

# Get all mtide predictions
tm = Sys.time()
harms_mtide = mtide_tides_loop(harms_foreign,
                               start_date = "2024-02-06",
                               end_date = "2024-02-06")
nd = Sys.time(); nd - tm  # 0.3823662 secs

# Get the initial data back
harms_noaa = merge(foreign_sth, harms_noaa,
                   by = "station_code", all.x = TRUE)

# Join rtide and noaa as comb_tide
harms_noaa$tide_time = as.POSIXct(harms_noaa$tide_time, tz = "UTC")
comb_tide_fh = merge(harms_noaa, harms_rtide, by = c("station_code", "tide_time"), all.x = TRUE)

# Join mtide to comb_tide
harms_mtide = harms_mtide[, .(station_code, tide_time, mtide_level)]
comb_tide_fh = merge(comb_tide_fh, harms_mtide, by = c("station_code", "tide_time"), all.x = TRUE)
comb_tide_fh = comb_tide_fh[, c("station_name", "station_code", "time_meridian", "established", "removed",
                                "epoch_start", "epoch_end", "n_consts", "tide_time", "tide_level",
                                "rtide_level", "mtide_level")]

#==================================================================================
# Identify differences on harms with less than 37 consts
#==================================================================================

# Round tide hts   MTIDES LOOKS EXCELLENT !!!!
comb_tide_fh$rtide_level = round(comb_tide_fh$rtide_level, digits = 3)
comb_tide_fh$mtide_level = round(comb_tide_fh$mtide_level, digits = 3)
comb_tide_fh$difr = abs(comb_tide_fh$tide_level - comb_tide_fh$rtide_level)
comb_tide_fh$difm = abs(comb_tide_fh$tide_level - comb_tide_fh$mtide_level)

# Max difference for rtide: Result: 0.018m, Noticable = 0.059ft
max(comb_tide_fh$difr, na.rm = TRUE)
0.018 / 0.3048

# Max difference for mtide: Result: 0.001m, just rounding error.
max(comb_tide_fh$difm, na.rm = TRUE)

#==================================================================================
# Test foreign subordinate stations vs rtide and noaa
#==================================================================================

# # Check a couple stations that did not run in mtides below
# MarineTides::identify_station("APIA (Observatory), Upolu Island", verbose = TRUE)
# MarineTides::tide_level(tide_station = "APIA (Observatory), Upolu Island",
#                         start_date = "2024-02-06",
#                         end_date = "2024-02-06",
#                         data_interval = "high-low")

# Pull out subset with names and codes
subs_foreign = foreign_sts[,c("station_code", "station_name")]

# Get noaa predictions
tm = Sys.time()
subs_noaa = noaa_tides_loop(subs_foreign,
                            start_date = "2024-02-06",
                            end_date = "2024-02-06",
                            time_interval = "hilo")
nd = Sys.time(); nd - tm  # 53.20814 secs

# Identify any missing stations: None: Got some errors that relooped till successful.
# All data were retrieved as intended. No more errors for Malakal, or APIA
length(unique(subs_foreign$station_code))
length(unique(subs_noaa$station_code))

# # Inspect...Malakal Harbor failed. No predictions, remove
# subs_noaa = subset(subs_noaa, !is.na(tide_level))

# Add id variables to allow comparison
subs_noaa_dt = as.data.table(subs_noaa)
subs_noaa_dt$id = rowidv(subs_noaa_dt, cols = c("station_code", "tide_type"))
subs_noaa_dt = subs_noaa_dt[, hl_tides := paste0(tide_type, "-", id)]
subs_noaa_dt = subs_noaa_dt[, .(station_code, hl_tides, noaa_tide_time = tide_time, noaa_tide_level = tide_level)]

# Get all mtide predictions
tm = Sys.time()
subs_mtide = mtide_tides_loop(subs_foreign,
                              start_date = "2024-02-06",
                              end_date = "2024-02-06",
                              data_interval = "high-low")
nd = Sys.time(); nd - tm  # 14.03711 secs

# Identify any missing stations: Result: No more missing now that APIA has harmonics
length(unique(subs_mtide$station_code))
# missing_subs = subs_foreign$station_code[!subs_foreign$station_code %in% subs_mtide$station_code]
# missing_foreign_sts = subset(foreign_sts, station_code %in% missing_subs)

# # Dump any data not within the start_date, end_date. Sometimes tides are predicted for following day
# is.data.table(subs_mtide)
# subs_mtide_trim = subs_mtide[inrange(tide_time, as.POSIXct("2024-02-06", tz = "UTC"),
#                                      as.POSIXct("2024-02-07", tz = "UTC"))]

# Get the initial data back
subs_noaa_dt = merge(foreign_sts, subs_noaa_dt,
                     by = "station_code", all.x = TRUE)

# Add id variables to allow comparison
subs_mtide_dt = as.data.table(subs_mtide)
subs_mtide_dt$id = rowidv(subs_mtide_dt, cols = c("station_code", "tide_type"))
subs_mtide_dt = subs_mtide_dt[, hl_tides := paste0(tide_type, "-", id)]
subs_mtide_dt = subs_mtide_dt[, .(station_code, hl_tides, mtide_tide_time = tide_time, mtide_tide_level = mtide_level)]

# Join mtide and noaa as comb_tide
subs_noaa_dt$noaa_tide_time = as.POSIXct(subs_noaa_dt$noaa_tide_time, tz = "UTC")
comb_tide_fs = merge(subs_noaa_dt, subs_mtide_dt, by = c("station_code", "hl_tides"), all.x = TRUE)
comb_tide_fs = comb_tide_fs[, c("station_name", "station_code", "ref_station_code", "time_meridian",
                                "tide_type", "established", "removed", "epoch_start", "epoch_end",
                                "n_consts", "hl_tides", "noaa_tide_time", "mtide_tide_time",
                                "noaa_tide_level", "mtide_tide_level")]

# Add ref station name, just for curiosity to see how far away
all_stations_dt = as.data.table(all_stations)
ref_names = all_stations_dt[, .(ref_station_code = station_code, ref_station_name = station_name)]
comb_tide_fs = merge(comb_tide_fs, ref_names, by = "ref_station_code", all.x = TRUE)

# Round tide hts
# is.data.table(comb_tide_fs)
comb_tide_fs$mtide_tide_level = round(comb_tide_fs$mtide_tide_level, digits = 3)

# Compute diffs: Result: Excellent. All now within 60 secs, and 0.001 meter.
comb_tide_fs = as.data.table(comb_tide_fs)
comb_tide_fs = comb_tide_fs[, ':=' (time_diff = abs(noaa_tide_time - mtide_tide_time),
                                    level_diff = abs(noaa_tide_level - mtide_tide_level))]
comb_tide_fs = comb_tide_fs[order(station_name, noaa_tide_time)]

#==================================================================================
# Identify differences on subs from foreign stations
#==================================================================================

# Max difference for mtide: Result: 0.001m, just rounding error.
max(comb_tide_fs$level_diff)
max(comb_tide_fs$time_diff)


#=================================================================================
# SQL Code
#=================================================================================

# # Some SQL tests
# SELECT ty.station_type_code, count( ty.station_type_code)
# FROM station as st
# LEFT JOIN station_type_lut as ty on ty.station_type_id = st.station_type_id
# group by ty.station_type_code






















