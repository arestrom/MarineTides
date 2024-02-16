#====================================================================================
# Re-check all station data in Harmonics DB
#
#  Notes:
#  1. For results of running loops on all harmonic and subordinate stations in the
#     harmonics DB, see output in harms_comparison and subs_comparison tables in
#     harmonics DB.
#     For subs: select * from subs_comparison where level_diff > 0.001 order by level_diff desc
#                    Result: No levels as much as 0.002 off.
#               select * from subs_comparison where time_diff >= '00:01:00' order by time_diff desc
#                    Result: No time_diff > 1 minute off.
#    For harms: select * from harms_comparison where difm >= 0.003 order by difm desc
#                    Result: 80 cases, mostly all point estimates for Anchorage and Chatham.
#                            These have 120 constituents. Otherwise, no stations more than
#                            0.003 m off.
#
#  Questions:
#  1. Do subs all have ref codes and corrections? --Done: All have ref codes and full set of offsets.
#  2. Do harms all have at least 37 constituents, and have any been removed? --No, some have less,
#     Of stations with < 37 constituents, all but Atka and Lake Worth Pier have had stations removed.
#  5. Create a leaflet interface to identify stations?
#  6. Do a better job with identify_station() function  -- Done
#  7. For now, filter out stations with meridians other than 0?    -- No, when set to zero, output correct.
#  8. Redo harmonics data to eliminate unneeded harmonic stations. -- No will keep, but warn if removed!
#  9. Rewrite docs to give new counts.
# 10. Use leaflet tools to find stations in search box?
# 11. Use a markdown page an an interface?
# 12. Do foreign station predictions match NOAA web output?  -- Yes
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
# Get base data for all stations in DB
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
# Get data for stations previously checked, from DB, to identify problems:
#=========================================================================

# Pull up all harms_comparison data
qry = glue::glue("select * from harms_comparison")
pg_con = pg_con_local(dbname = "harmonics")
harm_comps = DBI::dbGetQuery(pg_con, qry)
DBI::dbDisconnect(pg_con)

# Pull up all subs_comparison data
qry = glue::glue("select * from subs_comparison")
pg_con = pg_con_local(dbname = "harmonics")
sub_comps = DBI::dbGetQuery(pg_con, qry)
DBI::dbDisconnect(pg_con)

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

# rtide function ==============================

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

# mtide function ==============================

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
# Identify stations where predictions were a bit off
#==================================================================================

# Pull out data where predictions were off
harm_comps = as.data.table(harm_comps)
harms_off = unique(harm_comps[difm >= 0.003, .(station_name, station_code)])

sub_comps = as.data.table(sub_comps)
subs_off = unique(sub_comps[level_diff >= 0.002, .(station_name, station_code)])

# Combine and add station_type info
all_stations = as.data.table(all_stations)
st_info = all_stations[, .(station_code, station_type_code)]
all_off = rbind(harms_off, subs_off)
all_off = merge(all_off, st_info, by = "station_code", all.x = TRUE)

# Pull out subs and harms
subs = all_off[station_type_code == "S"]
harms = all_off[station_type_code == "H"]

# For latest trial, just pull out Anchorge and Chatham, only two stations with > 37 consts
harms = harms[station_name %in% c("Chatham", "Anchorage")]

#==================================================================================
# Run the loop functions on harm stations
#==================================================================================

# Get noaa predictions
tm = Sys.time()
harms_noaa = noaa_tides_loop(harms,
                             start_date = "2024-02-06",
                             end_date = "2024-02-06",
                             time_interval = "60")
nd = Sys.time(); nd - tm  # 31.24569 secs

# =========================================================

# Get all rtide predictions
tm = Sys.time()
harms_rtide = rtide_tides_loop(harms,
                               start_date = "2024-02-06",
                               end_date = "2024-02-06")
nd = Sys.time(); nd - tm  # 6.264818 secs

# =========================================================

# Get all mtide predictions
tm = Sys.time()
harms_mtide = mtide_tides_loop(harms,
                               start_date = "2024-02-06",
                               end_date = "2024-02-06")
nd = Sys.time(); nd - tm  # 6.264818 secs

# =========================================================

# Add the full set of info back in
harms_noaa_st = merge(harms_noaa, all_stations,
                      by = "station_code", all.x = TRUE)

# Pull out any data from NOAA that failed to get point estimates
harms_noaa_st = as.data.table(harms_noaa_st)
harms_noaa_fail = harms_noaa_st[is.na(tide_level)]

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
harms_noaa_st = harms_noaa_st[!is.na(tide_level)]

# Join rtide and noaa as comb_tide
harms_noaa_st$tide_time = as.POSIXct(harms_noaa_st$tide_time, tz = "UTC")
is.data.table(harms_noaa_st)
comb_tide = merge(harms_noaa_st, harms_rtide, by = c("station_code", "tide_time"), all.x = TRUE)

# Join mtide to comb_tide
is.data.table(harms_mtide)
harms_mtide = harms_mtide[, .(station_code, tide_time, mtide_level)]
comb_tide = merge(comb_tide, harms_mtide, by = c("station_code", "tide_time"), all.x = TRUE)

# Pull out needed data
comb_tide = comb_tide[, .(station_name, station_code, time_meridian,
                          established, removed, epoch_start, epoch_end, n_consts,
                          tide_time, tide_level, rtide_level, mtide_level)]

# Identify differences ==========================

# Round tide hts...mtides looks good.
comb_tide$rtide_level = round(comb_tide$rtide_level, digits = 3)
comb_tide$mtide_level = round(comb_tide$mtide_level, digits = 3)
comb_tide$difr = abs(comb_tide$tide_level - comb_tide$rtide_level)
comb_tide$difm = abs(comb_tide$tide_level - comb_tide$mtide_level)

# Max difference for rtide: Result: 4.123 m, huge = 13.5269 ft
(max_diff_rtide = max(comb_tide$difr, na.rm = TRUE) / 0.3048)

# Max difference for mtide: Result: 0.053 m, 0.1738845 ft. Good enough (2 inches). Insufficient data to rename other consts.
(max_diff_mtide = max(comb_tide$difm, na.rm = TRUE) / 0.3048)

# Dump existing data for stations in comb_tide from harms_comparison, then add new
st_codes = unique(comb_tide$station_code)
st_codes = paste0(paste0("'", st_codes, "'"), collapse = ", ")

# qry = glue::glue("delete from harms_comparison ",
#                  "where station_code in ({st_codes})")
# pg_con = pg_con_local(dbname = "harmonics")
# DBI::dbExecute(pg_con, qry)
# DBI::dbDisconnect(pg_con)

# Write results to temp table in harmonics DB
pg_con = pg_con_local(dbname = "harmonics")
tbl = Id(schema = "public", table = "harms_comparison")
DBI::dbWriteTable(pg_con, tbl, comb_tide, row.names = FALSE, append = TRUE, copy = TRUE)
DBI::dbDisconnect(pg_con)

#==================================================================================
# Run the loop functions on first set of subordinate stations
#==================================================================================

# Count number of subs that needed correcting. Result: None, all issues were with harm stations!
nrow(subs)

# For safety, verify if any stations in st_codes were reported to subs before correcting.
# If so, then delete from subs_comparison
qry = glue::glue("select * from subs_comparison ",
                 "where station_code in ({st_codes})")
pg_con = pg_con_local(dbname = "harmonics")
subs_to_delete = DBI::dbGetQuery(pg_con, qry)
DBI::dbDisconnect(pg_con)

# There were several, so delete
qry = glue::glue("delete from subs_comparison ",
                 "where station_code in ({st_codes})")
pg_con = pg_con_local(dbname = "harmonics")
DBI::dbExecute(pg_con, qry)
DBI::dbDisconnect(pg_con)



















