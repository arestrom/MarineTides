#====================================================================================
# Output harmonics data in .rda format for MarineTides package
#
# Notes:
#  1. Can't include sqlite in package. Create .rda files instead.
#  2. https://stackoverflow.com/questions/22531477/using-lists-inside-data-table-columns
#  3. See: https://beckmw.wordpress.com/2017/04/12/predicting-tides-in-r/
#
# AS 2024-01-09
#====================================================================================

# Load libraries
library(glue)
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
# Pull out station data
#=============================================================================

# Function to get basic tide station data
get_stations = function() {
  qry = glue::glue("select s.station_code, s.station_name, ",
                   "r.country_name as country, ",
                   "r.region_name as region, ",
                   "s.time_meridian as station_meridian, ",
                   "s.datum_msl_meter as station_datum, ",
                   "ST_Y (s.geog::geometry) as lat, ",
                   "ST_X (s.geog::geometry) as lng, ",
                   "s.timezone, s.removed, ",
                   "st.station_type_code as station_type ",
                   "from station as s ",
                   "inner join station_type_lut as st ",
                   "on s.station_type_id = st.station_type_id ",
                   "inner join region_lut as r ",
                   "on s.region_id = r.region_id ",
                   "order by station_name")
  pg_con = pg_con_local(dbname = "harmonics")
  stations = DBI::dbGetQuery(pg_con, qry)
  DBI::dbDisconnect(pg_con)
  return(stations)
}

# Get data
stations = get_stations()

#=============================================================================
# Pull out speed for constituents
#=============================================================================

# Function to get station and year constituent data: amplitude, phase, node_year
get_constituent_speed = function() {
  qry = glue("select cn.constituent_order as order, cn.constituent_code as code, ",
             "cn.Constituent_speed as speed ",
             "from constituent as cn ",
             "order by cn.constituent_order")
  pg_con = pg_con_local(dbname = "harmonics")
  speed_data = dbGetQuery(pg_con, qry)
  dbDisconnect(pg_con)
  return(speed_data)
}

# Get data
constituent_speed = get_constituent_speed()

#=============================================================================
# Pull out station offsets data
#=============================================================================

# Function to get basic tide station data
get_station_offsets = function() {
  qry = glue::glue("select s.station_code, s.station_name, ",
                   "ss.station_code as reference_station_code, ",
                   "ha.adjusted_type_code as height_offset_type, ",
                   "so.height_offset_high_tide, ",
                   "so.height_offset_low_tide, ",
                   "so.time_offset_high_tide_minutes, ",
                   "so.time_offset_low_tide_minutes ",
                   "from station as s ",
                   "inner join station_offsets as so ",
                   "on s.station_id = so.station_id ",
                   "inner join station as ss ",
                   "on so.reference_station_id = ss.station_id ",
                   "inner join height_adjusted_type_lut as ha ",
                   "on so.height_adjusted_type_id = ha.height_adjusted_type_id ",
                   "order by s.station_name")
  pg_con = pg_con_local(dbname = "harmonics")
  station_offsets = DBI::dbGetQuery(pg_con, qry)
  DBI::dbDisconnect(pg_con)
  return(station_offsets)
}

# Get data
station_offsets = get_station_offsets()

#=============================================================================
# Pull out year constituents
#=============================================================================

# Function to get station and year constituent data: amplitude, phase, node_year
get_year_constituents = function() {
  qry = glue("select cn.constituent_order as order, cn.constituent_code as code, ",
             "yc.node_year, yc.year_factor, yc.equilibrium_arg as equil_arg ",
             "from constituent as cn ",
             "inner join year_constituent as yc on cn.constituent_id = yc.constituent_id ",
             "order by yc.node_year, cn.constituent_order")
  pg_con = pg_con_local(dbname = "harmonics")
  year_data = dbGetQuery(pg_con, qry)
  dbDisconnect(pg_con)
  return(year_data)
}

# Get data
year_constituents = get_year_constituents()

#=============================================================================
# Pull out station constituents
#=============================================================================

# Function to get station and year constituent data: amplitude, phase, node_year
get_station_constituents = function() {
  qry = glue("select s.station_code, st.station_type_code as station_type, ",
             "cn.constituent_order as order, cn.constituent_code as code, ",
             "sc.amplitude_meter as amplitude, sc.phase_gmt as phase ",
             "from station_constituent as sc ",
             "inner join station as s on sc.station_id = s.station_id ",
             "inner join station_type_lut as st on s.station_type_id = st.station_type_id ",
             "inner join constituent as cn on cn.constituent_id = sc.constituent_id ",
             "where st.station_type_code = 'H' ",
             "order by s.station_name, cn.constituent_order")
  pg_con = pg_con_local(dbname = "harmonics")
  station_constituents = dbGetQuery(pg_con, qry)
  dbDisconnect(pg_con)
  return(station_constituents)
}

# Get data
station_constituents = get_station_constituents()

#=============================================================================
# Combine individual datasets into one harmonics list
#=============================================================================

# Combine to list
harmonics = list(st_data = stations,
                 st_offsets = station_offsets,
                 st_constituents = station_constituents,
                 yr_constituents = year_constituents,
                 ct_speed = constituent_speed)

# Write to .rda
save(harmonics, file = "data/harmonics.rda")


