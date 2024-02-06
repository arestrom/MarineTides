#====================================================================================
# Check data in Harmonics DB
#
# Notes:
#  1.
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

#====================================================================
# Check on specific stations around here. Why do I not get Tacoma?
#====================================================================

# Get all basic info
qry = glue("select s.station_code, s.station_name, st.station_type_code, s.time_meridian, ",
           "s.tide_type, s.datum_msl_meter, so.height_offset_factor_low_tide as low_factor ",
           "from station as s ",
           "left join station_type_lut as st on s.station_type_id = st.station_type_id ",
           "left join station_offsets as so on s.station_id = so.station_id ",
           "where s.station_code in ('9446484', '9445246','9447130','9446486')")
pg_con = pg_con_local(dbname = "harmonics")
local_st = dbGetQuery(pg_con, qry)
dbDisconnect(pg_con)

# Get harmonics info
qry = glue("select s.station_code, count (sc.constituent_id) as n_consts ",
           "from station_constituent as sc ",
           "left join station as s on sc.station_id = s.station_id ",
           "where s.station_code in ('9446484', '9445246','9447130','9446486') ",
           "group by s.station_code")
pg_con = pg_con_local(dbname = "harmonics")
const_count = dbGetQuery(pg_con, qry)
dbDisconnect(pg_con)

# Combine
local_st = merge(local_st, const_count, by = "station_code", all.x = TRUE)
local_st = as.data.table(local_st)
tacoma_st = local_st[station_name %ilike% "Tacoma"]

# See if I can pull tacoma from harms
mt_st = MarineTides::identify_station("Tacoma;", harms = harmonics)

# See if I can pull tacoma from harms
MarineTides::identify_station("Sea", harms = harmonics)

# Example of problem
station_list = data.table(stations = c("Old Tacoma", "Tacoma", "Tacoma Narrows Bridge", "Tacoma, Commencement Bay"))

# First try exact match, then do like
match_try = station_list[stations == "Tacoma"]
part_try = station_list[stations == "S Tacoma"]
station_list[stations %ilike% "Tacoma"]




#====================================================================
# Test various combinations of inputs for subordinate station
#====================================================================












#====================================================================
# Results: For one year at 6 minute resolution:
#          using data.table is ~ 34 times faster
#====================================================================

#====================================================================
# Test to see if output from rtide vs MarineTides is comparable
#====================================================================

# Combine tide height columns
tide_four = data.frame(tide_four)
tide_comparison = cbind(tide_four, rtide_four$TideHeight)
names(tide_comparison) = c("station_code", "station_name", "reference_station_code",
                           "tide_type", "tide_time", "mtide_ht", "rtide_ht")

# Compute difference
tide_comparison$mtide_ht = round(tide_comparison$mtide_ht, digits = 2)
tide_comparison$rtide_ht = round(tide_comparison$rtide_ht, digits = 2)

# Pull out cases where heights differ
tide_differences = subset(tide_comparison, !mtide_ht == rtide_ht)
tide_differences$diff = abs(tide_differences$mtide_ht - tide_differences$rtide_ht)

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
m_tides_match = subset(noaa_comparison, noaa_ht == mtide_ht)
rtides_match = subset(noaa_comparison, noaa_ht == rtide_ht)

# Percent of time mine vs rtide matches noaa output for one month at 6 min increments
(m_match = nrow(m_tides_match) / nrow(noaa_comparison))  # 79% of the time
(rtide_match = nrow(rtides_match) / nrow(noaa_comparison)) # 21% of the time

#============================================================================
# Result: MarineTides matches:  79% of the time
#         rtide matches: 21% of the time
# If rounded to one digit, both would likely always match NOAA
#============================================================================

# # Some SQL tests
# SELECT ty.station_type_code, count( ty.station_type_code)
# FROM station as st
# LEFT JOIN station_type_lut as ty on ty.station_type_id = st.station_type_id
# group by ty.station_type_code






















