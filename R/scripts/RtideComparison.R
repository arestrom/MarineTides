#====================================================================================
# Determine where differences are occurring for stations like Tacoma. RTide better.
#
# Notes:
#  1. See comparisons of data output by noaa, rtide, mtide in scripts/MtideComparison
#
# Result: Need to set time_meridian to zero for all harmonic stations.
#         Then output will be correct. Need to test on all improper harmonics.
#
# AS 2024-02-07
#====================================================================================

# Load needed libraries
library(rtide)        # Needed for harmonics data
library(magrittr)     # Will not be needed later
library(datacheckr)   # Will not be essential later

# Generate tides to test output below
rtide_tacoma = rtide::tide_height(
  stations = "Tacoma",
  minutes = 60L,
  from = as.Date("2023-12-08"),
  to = as.Date("2023-12-09"),
  tz = "UTC"
)

#=============================================================
# Required functions
#=============================================================

# Function to process and check harmonics data
check_tide_harmonics <- function(x) {
  if (!is.tide_harmonics(x)) stop("x is not class 'tide_harmonics'")
  if (!all(c("Station", "Node", "StationNode", "NodeYear") %in% names(x)))
    stop("x is missing components", call. = FALSE)
  check_data2(x$Station, values = list(
    Station = "",
    Units = c("feet", "ft", "m", "metre"),
    Longitude = 1,
    Latitude = 1,
    Hours = c(-12,12),
    TZ = "",
    Datum = 1),
    key = "Station")
  check_data2(x$Node, values = list(
    Node = "",
    Speed = 1),
    key = "Node")
  if (!is.array(x$StationNode)) stop("StationNode must be an array", call. = FALSE)
  if (!is.array(x$NodeYear)) stop("NodeYear must be an array", call. = FALSE)
  if (mode(x$StationNode) != "numeric")
    stop("StationNode must be a numeric array", call. = FALSE)
  if (mode(x$NodeYear) != "numeric")
    stop("NodeYear must be a numeric array", call. = FALSE)
  if (!identical(dimnames(x$StationNode), list(x$Station$Station, x$Node$Node, c("A", "Kappa"))))
    stop("StationNode has invalid dimnames", call. = FALSE)
  if (!identical(dimnames(x$NodeYear)[c(1,3)], list(x$Node$Node, c("NodeFactor", "EquilArg"))))
    stop("NodeYear has invalid dimnames", call. = FALSE)
  years <- dimnames(x$NodeYear)[2][[1]]
  years <- as.numeric(years)
  years <- diff(years)
  if (!all(years == 1)) stop("NodeYear has invalid dimnames", call. = FALSE)
  x
}

# Function to create tide datetimes
tide_datetimes <- function(minutes = 60L, from = as.Date("2015-01-01"), to = as.Date("2015-12-31"),
                           tz = "PST8PDT") {
  if (class(minutes) == "numeric"){
    check_scalar(minutes, c(1, 60))
    if (minutes %% 1 != 0)	# If modulo isn't 0, decimal value is present
      warning("Truncating minutes interval to whole number", call.=FALSE)
    minutes %<>% as.integer()
  }
  check_scalar(minutes, c(1L, 60L))
  datacheckr::check_date(from)
  datacheckr::check_date(to)
  datacheckr::check_string(tz)
  from <- ISOdatetime(year = lubridate::year(from), month = lubridate::month(from),
                      day = lubridate::day(from), hour = 0, min = 0, sec = 0, tz = tz)
  to <- ISOdatetime(year = lubridate::year(to), month = lubridate::month(to),
                    day = lubridate::day(to), hour = 23, min = 59, sec = 59, tz = tz)
  seq(from, to, by = paste(minutes, "min"))
}

# Function to pull out data for selected station
tide_stations <- function(stations = ".*", harmonics = rtide::harmonics) {
  datacheckr::check_vector(stations, value = c(""))
  check_tide_harmonics(harmonics)
  if (!is.tide_harmonics(harmonics))
    stop("harmonics must be an object of class 'tide_harmonics'", call. = FALSE)
  # Clean up station names
  stations %<>% stringr::str_replace_all("[(]", "[(]") %>% stringr::str_replace_all("[)]", "[)]")
  stations <- paste0("(", paste(stations, collapse = ")|("), ")")
  match <- stringr::str_detect(harmonics$Station$Station, stations)
  match %<>% which()
  if (!length(match)) stop("no matching stations", call. = FALSE)
  harmonics$Station$Station[sort(unique(match))]
}

# Function to create vector of hours of prediction request span
hours_year <- function(datetime) {
  check_vector(datetime, value = Sys.time())
  stopifnot(identical(lubridate::tz(datetime), "UTC"))
  year <- lubridate::year(datetime)
  startdatetime <- ISOdate(year, 1, 1, 0, tz = "UTC")
  hours <- difftime(datetime, startdatetime, units = 'hours')
  hours %<>% as.numeric()
  hours
}

# Convert feet to meters
ft2m = function(x) {
  x = x * 0.3048
  return(x)
}

#============================================================================================
# Pull out variables for testing
#============================================================================================

# Am pulling out globals from main function below
stations = "Tacoma"
minutes = 60L
from = as.Date("2024-02-06")
to = as.Date("2024-02-06")
tz = "UTC"
harmonics = rtide::harmonics
# datetime = d$DateTime

# # Basic data
stations %<>% tide_stations(harmonics)
datetimes <- tide_datetimes(minutes = minutes, from = from, to = to, tz = tz)   # Vector of datetimes in local
data <- tidyr::crossing(Station = stations, DateTime = datetimes)               # Tibble with station and datetimes
data %<>% datacheckr::check_data2(values = list(Station = "", DateTime = Sys.time()))
tz <- lubridate::tz(data$DateTime)
data %<>% dplyr::mutate_(DateTime = ~lubridate::with_tz(DateTime, tzone = "UTC"))  # Convert to UTC
#years <- range(lubridate::year(data$DateTime), na.rm = TRUE)
harmonics = subset(harmonics, data$Station[1])
# Verify I have correct data
harmonics[["Station"]]

# Data for loop
d = data
h = harmonics

# Parameters
station_datum = h$Station$Datum                   # Station mean level. Scalar: 6.6371.
# node_year_factor = h$NodeYear[,,"NodeFactor"]   # Captures effect of 18.6 year cycle on amplitudes. Numeric vector. 1:175.
station_amplitude = h$StationNode[,,"A"]          # Amplitude: Numeric vector 1:175
station_speed = h$Node$Speed                      # Speed: Numeric vector 1:175
# prediction_span_hours = hours_year(d$DateTime)  # Time in hours from start of the year for requested prediction[i]: Scalar: 8192
station_hours = h$Station$Hours                   # Scalar: typically 0.
# node_year_equil = h$NodeYear[,,"EquilArg"]      # Equilibrium phase angle: Numeric vector: 1:175.
station_phase = h$StationNode[,,"Kappa"]          # Phase of constituents. Numeric vector: 1:175.


all_heights = NULL
i = 1
for (i in 1:nrow(data) ) {
  d = data[i,]                                        # "2023-12-08 08:00:00 UTC"
  prediction_span_hours = hours_year(d$DateTime)      # 8192
  h$NodeYear = h$NodeYear[,as.character(lubridate::year(d$DateTime)),,drop = FALSE]
  node_year_factor = h$NodeYear[,,"NodeFactor"]
  node_year_equil = h$NodeYear[,,"EquilArg"]
  height = station_datum +
    sum(node_year_factor * station_amplitude *        # sum(yr_factor * amplitude): 13.32027
          cos((station_speed * (prediction_span_hours - station_hours) +
                 node_year_equil - station_phase) * pi/180))
  height = ft2m(height)
  all_heights = rbind(all_heights, height)
  i = i + 1
}

# Handy converter for comparisons
ft2m(3.736)








