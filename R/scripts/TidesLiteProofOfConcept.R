#====================================================================================
# Generate tides from data in harmonics DB
#
# AS 2023-12-27
#====================================================================================

# Load libraries
library(data.table)
library(DBI)
library(glue)
library(RPostgres)
library(lubridate)

#===============================================================================
# Functions
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

# Function to calculate the number of hours since start of the year for a given datetime.
hours_from_newyear <- function(datetime) {
  year = data.table::year(datetime)
  newyear = ISOdate(year, 1, 1, 0, tz = "UTC")
  hours = difftime(datetime, newyear, units = 'hours')
  hours = as.numeric(hours)
  return(hours)
}

#============================================================================================
# Pull out variables for testing
#============================================================================================

# Input globals for main function, or from web interface
stations = "Seattle"
minutes = 60L                                # Set how many minutes between predictions
from = data.table::as.IDate("2023-12-08")    # Use local timezone for simplicity
to = data.table::as.IDate("2023-12-09")
tz = "America/Los_Angeles"

# Process dates
from = ISOdatetime(year = lubridate::year(from), 
                   month = lubridate::month(from),
                   day = lubridate::day(from), 
                   hour = 0, min = 0, sec = 0, 
                   tz = tz)
to = ISOdatetime(year = lubridate::year(to), 
                 month = lubridate::month(to),
                 day = lubridate::day(to), 
                 hour = 23, min = 59, sec = 59, 
                 tz = tz)

# Generate vector of minute increments for predictions...in local timezone, then UTC.
prediction_span_local = seq(from, to, by = paste(minutes, "min"))
prediction_span_utc = lubridate::with_tz(prediction_span_local, tzone = "UTC")

# Get station data: First query. Need to verify first we have correct station.
qry = glue("select station_id, station_code, station_name, ",
           "time_meridian, datum_msl_meter ",
           "from station ",
           "where station_name like '%Seattle%'")

# Get values from harmonics
pg_con = pg_con_local(dbname = "harmonics")
station_data = dbGetQuery(pg_con, qry)
dbDisconnect(pg_con)

# Pull out station parameters
station_id = station_data$station_id
station_code = station_data$station_code
station_datum = station_data$datum_msl_meter   # Meters: rtide: 2.0229, mine: 2.024, noaa: 2.024
station_meridian = station_data$time_meridian  # Feet: rtide: 6.6371, mine: 6.6404, noaa: 6.64

# Get year range for query
year_range = c(lubridate::year(from), lubridate::year(to))
year_range = as.integer(unique(year_range))
year_range = paste0(paste0(year_range), collapse = ", ")

# Create station_id parameter for query
station_qid = paste0(paste0("'", station_id, "'"), collapse = ", ")

# Get station/year constituent data: amplitude, phase, node_year data
qry = glue("select cn.constituent_order as order, cn.constituent_code as code, ",
           "sc.amplitude_meter, sc.phase_gmt, cn.constituent_speed, yc.node_year, ",
           "yc.year_factor, yc.equilibrium_arg ",
           "from station_constituent as sc ",
           "inner join constituent as cn on cn.constituent_id = sc.constituent_id ",
           "inner join year_constituent as yc on cn.constituent_id = yc.constituent_id ",
           "where sc.station_id in ({station_qid}) and yc.node_year in ({year_range}) ",
           "order by yc.node_year, cn.constituent_order")

# Get values from harmonics
pg_con = pg_con_local(dbname = "harmonics")
station_constituent_data = dbGetQuery(pg_con, qry)
dbDisconnect(pg_con)

# Pull out amplitude, phase and speed...unique for station_constituents
st_dat = station_constituent_data |> 
  tidytable::select(order, code, amplitude = amplitude_meter,
                    phase = phase_gmt, speed = constituent_speed) |> 
  tidytable::distinct()

# Pull out year_constituent data
yr_dat = station_constituent_data |> 
  tidytable::select(order, code, node_year, year_factor,
                    equil_arg = equilibrium_arg)

# Run a test loop....WORKS PERFECT !!!!!
all_heights = NULL
dt = prediction_span_utc
i = 1
for (i in 1:length(dt) ) {
  timei = dt[i]                                     # "2023-12-08 08:00:00 UTC"
  yeari = lubridate::year(timei)
  predict_houri = hours_from_newyear(timei)
  yr_dati = yr_dat |> 
    tidytable::filter(node_year == yeari)
  year_fact_i = yr_dati$year_factor
  year_equil_i = yr_dati$equil_arg
  height = station_datum +
    sum(year_fact_i * st_dat$amplitude *            # sum(yr_fact * amplitude): 13.3252
          cos((st_dat$speed * (predict_houri - station_meridian) +
                 year_equil_i - st_dat$phase) * pi/180))
  all_heights = rbind(all_heights, height)
  i = i + 1
}
















