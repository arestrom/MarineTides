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
# Pull NOAA predictions that were previously run from database
#==================================================================================

# Pull data from harms_comparison
qry = glue::glue("select station_name, station_code, time_meridian, established, ",
                 "removed, epoch_start, epoch_end, n_consts, tide_time, tide_level, ",
                 "rtide_level ",
                 "from harms_comparison")

pg_con = pg_con_local(dbname = "harmonics")
harms_comp = dbGetQuery(pg_con, qry)
dbDisconnect(pg_con)

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

# # Will dump stations from harms_comparison that are in harms_four to reload with new data
# # now that all constituents have been added
# st_codes = unique(harms_four$station_code)
# st_cds = paste0(paste0("'", st_codes, "'"), collapse = ", ")
#
# # Dump from harms_comparison
# qry = glue::glue("delete from harms_comparison ",
#                  "where station_code in ({st_cds})")
#
# pg_con = pg_con_local(dbname = "harmonics")
# DBI::dbExecute(pg_con, qry)
# DBI::dbDisconnect(pg_con)

# # =========================================================
#
# # Get noaa predictions
# tm = Sys.time()
# harms_noaa_four = noaa_tides_loop(harms_four,
#                                   start_date = "2024-02-06",
#                                   end_date = "2024-02-06",
#                                   time_interval = "60")
# nd = Sys.time(); nd - tm  # 18.69807 mins
#
# # =========================================================
#
# # Get all rtide predictions
# tm = Sys.time()
# harms_rtide_four = rtide_tides_loop(harms_four,
#                                     start_date = "2024-02-06",
#                                     end_date = "2024-02-06")
# nd = Sys.time(); nd - tm  # 7.772297 secs

# =========================================================

# Get all mtide predictions
tm = Sys.time()
harms_mtide_four = mtide_tides_loop(harms_four,
                                    start_date = "2024-02-06",
                                    end_date = "2024-02-06")
nd = Sys.time(); nd - tm  # 15.98584 secs

# =========================================================

# # Add the full set of info back in
# harms_noaa_four_st = merge(harms_noaa_four, harms_four,
#                            by = "station_code", all.x = TRUE)
#
# # Pull out any data from NOAA that failed to get point estimates
# harms_noaa_four_st = as.data.table(harms_noaa_four_st)
# harms_noaa_four_fail = harms_noaa_four_st[is.na(tide_level)]

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

# # Pull out data that did not fail
# harms_noaa_four_st = harms_noaa_four_st[!is.na(tide_level)]
#
# # Join rtide and noaa as comb_tide
# harms_noaa_four_st$tide_time = as.POSIXct(harms_noaa_four_st$tide_time, tz = "UTC")
# comb_tide_four = merge(harms_mtide_four, harms_comp, by = c("station_code", "tide_time"), all.x = TRUE)

# Join mtide to comb_tide
harms_mtide_four = harms_mtide_four[, .(station_code, tide_time, mtide_level)]
comb_tide_four = merge(harms_mtide_four, harms_comp, by = c("station_code", "tide_time"), all.x = TRUE)

# Add in remaining data
# comb_tide_four = merge(comb_tide_four, all_harms, by = "station_code", all.x = TRUE)
comb_tide_four = comb_tide_four[, .(station_name, station_code, time_meridian,
                                    established, removed, epoch_start, epoch_end, n_consts,
                                    tide_time, tide_level, rtide_level, mtide_level)]

# Identify differences ==========================

# Round tide hts   MTIDES LOOKS MUCH BETTER WITH ALL CONSTS
comb_tide_four$rtide_level = round(comb_tide_four$rtide_level, digits = 3)
comb_tide_four$mtide_level = round(comb_tide_four$mtide_level, digits = 3)
comb_tide_four$difr = abs(comb_tide_four$tide_level - comb_tide_four$rtide_level)
comb_tide_four$difm = abs(comb_tide_four$tide_level - comb_tide_four$mtide_level)
is.data.table(comb_tide_four)
comb_tide_four = comb_tide_four[order(station_name, tide_time), ]

# Max difference for rtide: Result: huge = 10.13451ft
(max_diff_rtide_four = max(comb_tide_four$difr, na.rm = TRUE) / 0.3048)

# Max difference for mtide: Result: 0.328084 ft. Much better than before (1.437008 ft), but still a bit off.
(max_diff_mtide_four = max(comb_tide_four$difm, na.rm = TRUE) / 0.3048)

# # Write results to temp table in harmonics DB
# pg_con = pg_con_local(dbname = "harmonics")
# tbl = Id(schema = "public", table = "harms_comparison")
# DBI::dbWriteTable(pg_con, tbl, comb_tide_four, row.names = FALSE, append = TRUE, copy = TRUE)
# DBI::dbDisconnect(pg_con)

# #=======================================================================================
# # Investigate differences further.....Done. It was all in the missing year_constituents
# #=======================================================================================
#
# # Anchorage is the problem. May not be using all year constituents. Rtide is better !!!!
#
# # Verified Anchorage has 120 constituents in station_constituent table
# # But there are only 111 year constituents in year_constituent table
# # Both Anchorage AK (9455920) and Chatham MA (8447435) have 120 constituents
# # Need to re-upload year_constituents
#
# # Pull out node_year data to see if I have all the node_year constituents in the DB. I do not
# test_harms = rtide::harmonics
# node_year = test_harms$NodeYear
# node_year_df = as.data.table(node_year)
# node_year_2024 = node_year_df[V2 == "2024"]
# node_year_2024 = dcast(node_year_2024, V1 + V2 ~ V3, value.var = "value")
# node_year_2024 = node_year_2024[, .(constituent_code = V1, node_year = V2,
#                                     year_factor = NodeFactor, equilibrium_arg = EquilArg)]
#
# # Update two constituents to new name then check to see all have match in constituent table
# node_year_2024$constituent_code[node_year_2024$constituent_code %in% "LDA2"] = "LAM2"
# node_year_2024$constituent_code[node_year_2024$constituent_code %in% "RHO1"] = "RHO"
#
# # Update remaining constituents uncovered below
# node_year_2024$constituent_code[node_year_2024$constituent_code %in% "SIG1"] = "SIGMA1"
# node_year_2024$constituent_code[node_year_2024$constituent_code %in% "M2(KS)2"] = "M2KS2"
# node_year_2024$constituent_code[node_year_2024$constituent_code %in% "2SN(MK)2"] = "2SNMK2"
# node_year_2024$constituent_code[node_year_2024$constituent_code %in% "2KM(SN)2"] = "2KMSN2"
# node_year_2024$constituent_code[node_year_2024$constituent_code %in% "2MNLS6"] = "2MLNS6"
# node_year_2024$constituent_code[node_year_2024$constituent_code %in% "2(MS)8"] = "2MS8"
# node_year_2024$constituent_code[node_year_2024$constituent_code %in% "2(MN)8"] = "2MN8"
# node_year_2024$constituent_code[node_year_2024$constituent_code %in% "THE1"] = "THETA1"
# node_year_2024$constituent_code[node_year_2024$constituent_code %in% "OQ2"] = "OO2"
#
# # Get all constituents from DB
# qry = glue::glue("select constituent_id, constituent_order, constituent_code ",
#                  "from constituent")
#
# pg_con = pg_con_local(dbname = "harmonics")
# constituents = DBI::dbGetQuery(pg_con, qry)
# DBI::dbDisconnect(pg_con)
#
# # Check if all constituent_codes in constituents are in node_year_2024.
# # They should not all be there, since there are 175 constituents in rtide and only 120 in mtide
# all(constituents$constituent_code %in% node_year_2024$constituent_code)
#
# # Find out which are missing
# constituents$constituent_code[!constituents$constituent_code %in% node_year_2024$constituent_code]
#
# # Join to see which are missing
# consts = merge(constituents, node_year_2024, by = "constituent_code", all.x = TRUE)
# consts = as.data.table(consts)
# consts = consts[order(constituent_order)]
#
# # Need to verify but:
# #     HarmDB          RTide             Confirmed via TideHarmonics?
# # 1.  SIGMA1          sig1              Yes
# # 2.  M2KS2           M2(KS)2           x2K1M.2S2 or 2KM2S2
# # 3.  2SNMK2          2SN(MK)2
# # 4.  2KMSN2          2KM(SN)2
# # 5.  2MLNS6          2MNLS6
# # 6.  2MS8
# # 7.  2MN8
# # 8.  THETA1          THE1
# # 9.  OO2
#
# # Get names of harmonics from TideHarmonics package
# harm_names = TideHarmonics::harmonics
#
# # Inspect Node to see name for final missing constituent: 2MLNS6, or maybe 4MS6: Result: It was 2MNLS6
# rtide_node = test_harms$Node
# rtide_node = as.data.table(rtide_node)

#==================================================================================
# Test foreign subordinate stations vs rtide and noaa
#==================================================================================

# # Check a couple stations that did not run in mtides below
# MarineTides::identify_station("APIA (Observatory), Upolu Island", verbose = TRUE)
# MarineTides::tide_level(tide_station = "APIA (Observatory), Upolu Island",
#                         start_date = "2024-02-06",
#                         end_date = "2024-02-06",
#                         data_interval = "high-low")

#==================================================================================
# Run the loop functions on first set of subordinate stations
#==================================================================================

# Pull out subset with names and codes
is.data.table(all_subs)
subs_one = all_subs[1:500, .(station_code, station_name)]

# =========================================================

# Get noaa predictions
tm = Sys.time()
subs_noaa_one = noaa_tides_loop(subs_one,
                                start_date = "2024-02-06",
                                end_date = "2024-02-06",
                                time_interval = "hilo")
nd = Sys.time(); nd - tm  #  21.58158 mins

# =========================================================

# Get all mtide predictions
tm = Sys.time()
subs_mtide_one = mtide_tides_loop(subs_one,
                                  start_date = "2024-02-06",
                                  end_date = "2024-02-06",
                                  data_interval = "high-low")
nd = Sys.time(); nd - tm  #  1.621772 mins

# =========================================================

# Identify any missing stations in NOAA data: None: Got some errors that relooped till successful.
length(unique(subs_one$station_code))
length(unique(subs_noaa_one$station_code))

# # Inspect...Malakal Harbor failed. No predictions, remove
# subs_noaa = subset(subs_noaa, !is.na(tide_level))

# Add id variables to allow comparison
subs_noaa_one_dt = as.data.table(subs_noaa_one)
subs_noaa_one_dt$id = rowidv(subs_noaa_one_dt, cols = c("station_code", "tide_type"))
subs_noaa_one_dt = subs_noaa_one_dt[, hl_tides := paste0(tide_type, "-", id)]
subs_noaa_one_dt = subs_noaa_one_dt[, .(station_code, hl_tides, noaa_tide_time = tide_time, noaa_tide_level = tide_level)]

# Identify any missing stations: Result: No more missing now that APIA has harmonics
length(unique(subs_mtide_one$station_code))
# missing_subs = subs_foreign$station_code[!subs_foreign$station_code %in% subs_mtide$station_code]
# missing_foreign_sts = subset(foreign_sts, station_code %in% missing_subs)

# Add back full set of data
subs_noaa_one_dt = merge(subs_noaa_one_dt, all_subs,
                         by = "station_code", all.x = TRUE)

# Add id variables to allow comparison
subs_mtide_one_dt = as.data.table(subs_mtide_one)
subs_mtide_one_dt$id = rowidv(subs_mtide_one_dt, cols = c("station_code", "tide_type"))
subs_mtide_one_dt = subs_mtide_one_dt[, hl_tides := paste0(tide_type, "-", id)]
subs_mtide_one_dt = subs_mtide_one_dt[, .(station_code, hl_tides, mtide_tide_time = tide_time, mtide_tide_level = mtide_level)]

# Join mtide and noaa as comb_tide
subs_noaa_one_dt$noaa_tide_time = as.POSIXct(subs_noaa_one_dt$noaa_tide_time, tz = "UTC")
comb_subs_one = merge(subs_noaa_one_dt, subs_mtide_one_dt, by = c("station_code", "hl_tides"), all.x = TRUE)
is.data.table(comb_subs_one)
comb_subs_one = comb_subs_one[, .(station_name, station_code, ref_station_code, time_meridian,
                                  tide_type, established, removed, epoch_start, epoch_end,
                                  n_consts, hl_tides, noaa_tide_time, mtide_tide_time,
                                  noaa_tide_level, mtide_tide_level)]

# Add ref station name, just for curiosity to see how far away
all_stations_dt = as.data.table(all_stations)
ref_names = all_stations_dt[, .(ref_station_code = station_code, ref_station_name = station_name)]
comb_subs_one = merge(comb_subs_one, ref_names, by = "ref_station_code", all.x = TRUE)

# Round tide hts
# is.data.table(comb_subs_one)
comb_subs_one$mtide_tide_level = round(comb_subs_one$mtide_tide_level, digits = 3)

# Compute diffs: Result: Excellent. All now within 60 secs, and 0.001 meter.
comb_subs_one = comb_subs_one[, ':=' (time_diff = abs(noaa_tide_time - mtide_tide_time),
                                      level_diff = abs(noaa_tide_level - mtide_tide_level))]
comb_subs_one = comb_subs_one[order(station_name, noaa_tide_time)]

# Identify differences on subs ==================================

# Max difference for mtide: Result: 0.001m, just rounding error.
max(comb_subs_one$level_diff)
max(comb_subs_one$time_diff)

# # Write results to temp table in harmonics DB
# pg_con = pg_con_local(dbname = "harmonics")
# tbl = Id(schema = "public", table = "subs_comparison")
# DBI::dbWriteTable(pg_con, tbl, comb_subs_one, row.names = FALSE, append = TRUE, copy = TRUE)
# DBI::dbDisconnect(pg_con)

#==================================================================================
# Run the loop functions on second set of subordinate stations
#==================================================================================

# Pull out subset with names and codes
subs_two = all_subs[501:1000, .(station_code, station_name)]

# =========================================================

# Get noaa predictions
tm = Sys.time()
subs_noaa_two = noaa_tides_loop(subs_two,
                                start_date = "2024-02-06",
                                end_date = "2024-02-06",
                                time_interval = "hilo")
nd = Sys.time(); nd - tm  #  22.46601 mins

# =========================================================

# Get all mtide predictions
tm = Sys.time()
subs_mtide_two = mtide_tides_loop(subs_two,
                                  start_date = "2024-02-06",
                                  end_date = "2024-02-06",
                                  data_interval = "high-low")
nd = Sys.time(); nd - tm  #  1.597979 mins

# =========================================================

# Identify any missing stations in NOAA data: None: Got some errors that relooped till successful.
length(unique(subs_two$station_code))
length(unique(subs_noaa_two$station_code))

# # Inspect...Malakal Harbor failed. No predictions, remove
# subs_noaa = subset(subs_noaa, !is.na(tide_level))

# Add id variables to allow comparison
subs_noaa_two_dt = as.data.table(subs_noaa_two)
subs_noaa_two_dt$id = rowidv(subs_noaa_two_dt, cols = c("station_code", "tide_type"))
subs_noaa_two_dt = subs_noaa_two_dt[, hl_tides := paste0(tide_type, "-", id)]
subs_noaa_two_dt = subs_noaa_two_dt[, .(station_code, hl_tides, noaa_tide_time = tide_time, noaa_tide_level = tide_level)]

# Identify any missing stations: Result: No more missing now that APIA has harmonics
length(unique(subs_mtide_two$station_code))
# missing_subs = subs_foreign$station_code[!subs_foreign$station_code %in% subs_mtide$station_code]
# missing_foreign_sts = subset(foreign_sts, station_code %in% missing_subs)

# Add back full set of data
subs_noaa_two_dt = merge(subs_noaa_two_dt, all_subs,
                         by = "station_code", all.x = TRUE)

# Add id variables to allow comparison
subs_mtide_two_dt = as.data.table(subs_mtide_two)
subs_mtide_two_dt$id = rowidv(subs_mtide_two_dt, cols = c("station_code", "tide_type"))
subs_mtide_two_dt = subs_mtide_two_dt[, hl_tides := paste0(tide_type, "-", id)]
subs_mtide_two_dt = subs_mtide_two_dt[, .(station_code, hl_tides, mtide_tide_time = tide_time, mtide_tide_level = mtide_level)]

# No NOAA prediction for 8722338 Jensen Beach. Website says: Tide predictions are not available for this station. Omit
subs_noaa_two_dt = subs_noaa_two_dt[!is.na(noaa_tide_level)]

# Join mtide and noaa as comb_tide
subs_noaa_two_dt$noaa_tide_time = as.POSIXct(subs_noaa_two_dt$noaa_tide_time, tz = "UTC")
comb_subs_two = merge(subs_noaa_two_dt, subs_mtide_two_dt, by = c("station_code", "hl_tides"), all.x = TRUE)
is.data.table(comb_subs_two)
comb_subs_two = comb_subs_two[, .(station_name, station_code, ref_station_code, time_meridian,
                                  tide_type, established, removed, epoch_start, epoch_end,
                                  n_consts, hl_tides, noaa_tide_time, mtide_tide_time,
                                  noaa_tide_level, mtide_tide_level)]

# Add ref station name, just for curiosity to see how far away
# all_stations_dt = as.data.table(all_stations)
#ref_names = all_stations_dt[, .(ref_station_code = station_code, ref_station_name = station_name)]
comb_subs_two = merge(comb_subs_two, ref_names, by = "ref_station_code", all.x = TRUE)

# Round tide hts
# is.data.table(comb_subs_one)
comb_subs_two$mtide_tide_level = round(comb_subs_two$mtide_tide_level, digits = 3)

# Compute diffs: Result: Excellent. All now within 60 secs, and 0.001 meter.
comb_subs_two = comb_subs_two[, ':=' (time_diff = abs(noaa_tide_time - mtide_tide_time),
                                      level_diff = abs(noaa_tide_level - mtide_tide_level))]
comb_subs_two = comb_subs_two[order(station_name, noaa_tide_time)]

# Identify differences on subs ==================================

# Max difference for mtide: Result: 0.243m, Investigate!!!!.
max(comb_subs_two$level_diff)
max(comb_subs_two$time_diff)

# Issues:
# 1. Ankona 8722274
# 2. Sewall Point 8723178

# # Write results to temp table in harmonics DB
# pg_con = pg_con_local(dbname = "harmonics")
# tbl = Id(schema = "public", table = "subs_comparison")
# DBI::dbWriteTable(pg_con, tbl, comb_subs_two, row.names = FALSE, append = TRUE, copy = TRUE)
# DBI::dbDisconnect(pg_con)

#==================================================================================
# Run the loop functions on third set of subordinate stations
#==================================================================================

# Pull out subset with names and codes
subs_three = all_subs[1001:1500, .(station_code, station_name)]

# =========================================================

# Get noaa predictions
tm = Sys.time()
subs_noaa_three = noaa_tides_loop(subs_three,
                                  start_date = "2024-02-06",
                                  end_date = "2024-02-06",
                                  time_interval = "hilo")
nd = Sys.time(); nd - tm  #  20.36048 mins

# =========================================================

# Get all mtide predictions
tm = Sys.time()
subs_mtide_three = mtide_tides_loop(subs_three,
                                    start_date = "2024-02-06",
                                    end_date = "2024-02-06",
                                    data_interval = "high-low")
nd = Sys.time(); nd - tm  #  1.624548 mins

# =========================================================

# Identify any missing stations in NOAA data: None: Got some errors that relooped till successful.
length(unique(subs_three$station_code))
length(unique(subs_noaa_three$station_code))

# # Inspect...Malakal Harbor failed. No predictions, remove
# subs_noaa = subset(subs_noaa, !is.na(tide_level))

# Add id variables to allow comparison
subs_noaa_three_dt = as.data.table(subs_noaa_three)
subs_noaa_three_dt$id = rowidv(subs_noaa_three_dt, cols = c("station_code", "tide_type"))
subs_noaa_three_dt = subs_noaa_three_dt[, hl_tides := paste0(tide_type, "-", id)]
subs_noaa_three_dt = subs_noaa_three_dt[, .(station_code, hl_tides, noaa_tide_time = tide_time, noaa_tide_level = tide_level)]

# Identify any missing stations: Result: No more missing now that APIA has harmonics
length(unique(subs_mtide_three$station_code))
# missing_subs = subs_foreign$station_code[!subs_foreign$station_code %in% subs_mtide$station_code]
# missing_foreign_sts = subset(foreign_sts, station_code %in% missing_subs)

# Add back full set of data
subs_noaa_three_dt = merge(subs_noaa_three_dt, all_subs,
                           by = "station_code", all.x = TRUE)

# Add id variables to allow comparison
subs_mtide_three_dt = as.data.table(subs_mtide_three)
subs_mtide_three_dt$id = rowidv(subs_mtide_three_dt, cols = c("station_code", "tide_type"))
subs_mtide_three_dt = subs_mtide_three_dt[, hl_tides := paste0(tide_type, "-", id)]
subs_mtide_three_dt = subs_mtide_three_dt[, .(station_code, hl_tides, mtide_tide_time = tide_time, mtide_tide_level = mtide_level)]

# Join mtide and noaa as comb_tide
subs_noaa_three_dt$noaa_tide_time = as.POSIXct(subs_noaa_three_dt$noaa_tide_time, tz = "UTC")
comb_subs_three = merge(subs_noaa_three_dt, subs_mtide_three_dt, by = c("station_code", "hl_tides"), all.x = TRUE)
is.data.table(comb_subs_three)
comb_subs_three = comb_subs_three[, .(station_name, station_code, ref_station_code, time_meridian,
                                      tide_type, established, removed, epoch_start, epoch_end,
                                      n_consts, hl_tides, noaa_tide_time, mtide_tide_time,
                                      noaa_tide_level, mtide_tide_level)]

# Add ref station name, just for curiosity to see how far away
# all_stations_dt = as.data.table(all_stations)
#ref_names = all_stations_dt[, .(ref_station_code = station_code, ref_station_name = station_name)]
comb_subs_three = merge(comb_subs_three, ref_names, by = "ref_station_code", all.x = TRUE)

# Round tide hts
# is.data.table(comb_subs_one)
comb_subs_three$mtide_tide_level = round(comb_subs_three$mtide_tide_level, digits = 3)

# Compute diffs: Result: Excellent. All now within 60 secs, and 0.001 meter.
comb_subs_three = comb_subs_three[, ':=' (time_diff = abs(noaa_tide_time - mtide_tide_time),
                                          level_diff = abs(noaa_tide_level - mtide_tide_level))]
comb_subs_three = comb_subs_three[order(station_name, noaa_tide_time)]

# Identify differences on subs ==================================

# Max difference for mtide: Result: 0.001m, just rounding error.
max(comb_subs_three$level_diff, na.rm = TRUE)
max(comb_subs_three$time_diff, na.rm = TRUE)

# Issues with:
# 1. Cat Point 8728619
# 2. Little Hickory Island 8725283

# # Write results to temp table in harmonics DB
# pg_con = pg_con_local(dbname = "harmonics")
# tbl = Id(schema = "public", table = "subs_comparison")
# DBI::dbWriteTable(pg_con, tbl, comb_subs_three, row.names = FALSE, append = TRUE, copy = TRUE)
# DBI::dbDisconnect(pg_con)

#==================================================================================
# Run the loop functions on fourth set of subordinate stations
#==================================================================================

# Pull out subset with names and codes
subs_four = all_subs[1501:2194, .(station_code, station_name)]

# =========================================================

# Get noaa predictions
tm = Sys.time()
subs_noaa_four = noaa_tides_loop(subs_four,
                                 start_date = "2024-02-06",
                                 end_date = "2024-02-06",
                                 time_interval = "hilo")
nd = Sys.time(); nd - tm  #  28.24049 mins

# =========================================================

# Get all mtide predictions
tm = Sys.time()
subs_mtide_four = mtide_tides_loop(subs_four,
                                   start_date = "2024-02-06",
                                   end_date = "2024-02-06",
                                   data_interval = "high-low")
nd = Sys.time(); nd - tm  #  2.244147 mins

# =========================================================

# Identify any missing stations in NOAA data: None: Got some errors that relooped till successful.
length(unique(subs_four$station_code))
length(unique(subs_noaa_four$station_code))

# # Inspect...Malakal Harbor failed. No predictions, remove
# subs_noaa = subset(subs_noaa, !is.na(tide_level))

# Add id variables to allow comparison
subs_noaa_four_dt = as.data.table(subs_noaa_four)
subs_noaa_four_dt$id = rowidv(subs_noaa_four_dt, cols = c("station_code", "tide_type"))
subs_noaa_four_dt = subs_noaa_four_dt[, hl_tides := paste0(tide_type, "-", id)]
subs_noaa_four_dt = subs_noaa_four_dt[, .(station_code, hl_tides, noaa_tide_time = tide_time, noaa_tide_level = tide_level)]

# Identify any missing stations: Result: No more missing now that APIA has harmonics
length(unique(subs_mtide_four$station_code))
# missing_subs = subs_foreign$station_code[!subs_foreign$station_code %in% subs_mtide$station_code]
# missing_foreign_sts = subset(foreign_sts, station_code %in% missing_subs)

# Add back full set of data
subs_noaa_four_dt = merge(subs_noaa_four_dt, all_subs,
                          by = "station_code", all.x = TRUE)

# Add id variables to allow comparison
subs_mtide_four_dt = as.data.table(subs_mtide_four)
subs_mtide_four_dt$id = rowidv(subs_mtide_four_dt, cols = c("station_code", "tide_type"))
subs_mtide_four_dt = subs_mtide_four_dt[, hl_tides := paste0(tide_type, "-", id)]
subs_mtide_four_dt = subs_mtide_four_dt[, .(station_code, hl_tides, mtide_tide_time = tide_time, mtide_tide_level = mtide_level)]

# Join mtide and noaa as comb_tide
subs_noaa_four_dt$noaa_tide_time = as.POSIXct(subs_noaa_four_dt$noaa_tide_time, tz = "UTC")
comb_subs_four = merge(subs_noaa_four_dt, subs_mtide_four_dt, by = c("station_code", "hl_tides"), all.x = TRUE)
is.data.table(comb_subs_four)
comb_subs_four = comb_subs_four[, .(station_name, station_code, ref_station_code, time_meridian,
                                    tide_type, established, removed, epoch_start, epoch_end,
                                    n_consts, hl_tides, noaa_tide_time, mtide_tide_time,
                                    noaa_tide_level, mtide_tide_level)]

# Add ref station name, just for curiosity to see how far away
# all_stations_dt = as.data.table(all_stations)
#ref_names = all_stations_dt[, .(ref_station_code = station_code, ref_station_name = station_name)]
comb_subs_four = merge(comb_subs_four, ref_names, by = "ref_station_code", all.x = TRUE)

# Round tide hts
# is.data.table(comb_subs_one)
comb_subs_four$mtide_tide_level = round(comb_subs_four$mtide_tide_level, digits = 3)

# Compute diffs: Result: Excellent. All now within 60 secs, and 0.001 meter.
comb_subs_four = comb_subs_four[, ':=' (time_diff = abs(noaa_tide_time - mtide_tide_time),
                                        level_diff = abs(noaa_tide_level - mtide_tide_level))]
comb_subs_four = comb_subs_four[order(station_name, noaa_tide_time)]

# Identify differences on subs ==================================

# Max difference for mtide: Result: 0.001m, just rounding error.
max(comb_subs_four$level_diff)
max(comb_subs_four$time_diff)

# Write results to temp table in harmonics DB
pg_con = pg_con_local(dbname = "harmonics")
tbl = Id(schema = "public", table = "subs_comparison")
DBI::dbWriteTable(pg_con, tbl, comb_subs_four, row.names = FALSE, append = TRUE, copy = TRUE)
DBI::dbDisconnect(pg_con)


#=================================================================================
# SQL Code
#=================================================================================

# # Some SQL tests
# SELECT ty.station_type_code, count( ty.station_type_code)
# FROM station as st
# LEFT JOIN station_type_lut as ty on ty.station_type_id = st.station_type_id
# group by ty.station_type_code






















