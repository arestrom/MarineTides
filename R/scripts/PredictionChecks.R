#====================================================================================
# Verify output of tide prediction functions
#
# Notes:
#  1.
#
# AS 2024-02-03
#====================================================================================

# Load libraries
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
# Test various combinations of inputs for harmonic stations
#====================================================================

# Test one min harmonic
tm = Sys.time()
harm_point_1 = tide_level(
  tide_station = "Seattle",
  start_date = as.Date("2024-01-25"),
  end_date = as.Date("2024-01-26"),
  data_interval = "1-min",
  verbose = TRUE
)
nd = Sys.time(); nd - tm  # 0.12431 secs

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

#====================================================================
# Check all highs and lows for year vs tides table in ps_shellfish DB
# RESULT: Times differed by ~ one minute in 10% of cases over 2024.
#         NOAA was typically one min earlier when values disagreed.
#====================================================================

# Get all low tides for Seattle for year 2024
qry = glue("select loc.location_name as tide_station, td.low_tide_datetime as ps_tide_time, ",
           "td.tide_height_feet as ps_tide_level, st.tide_strata_code as tide_strata ",
           "from tide as td ",
           "inner join location as loc on td.tide_station_location_id = loc.location_id ",
           "inner join tide_strata_lut as st on td.tide_strata_id = st.tide_strata_id ",
           "where location_name = 'Seattle' ",
           "and date_part('year', low_tide_datetime) = 2024 ",
           "order by td.low_tide_datetime")
pg_con = pg_con_local(dbname = "ps_shellfish")
ps_year_24 = dbGetQuery(pg_con, qry)
dbDisconnect(pg_con)

# Convert to meters
ps_year_24 = data.table(ps_year_24)
ps_year_24 = ps_year_24[, ':=' (ps_tide_level = ps_tide_level / 3.28084)]
attr(ps_year_24$ps_tide_time, "tzone") = "America/Los_Angeles"
ps_year_24 = ps_year_24[2:nrow(ps_year_24),]

# Combine ps_year_24 with harm_year_high_low
comp_ps = merge(harm_year_high_low, ps_year_24, by.x = "tide_time",
                by.y = "ps_tide_time", all.x = TRUE)

# Pull out cases that agree vs disagree
comp_ps = data.table(comp_ps)
comp_agree = comp_ps[!is.na(ps_tide_level)]
comp_diff = comp_ps[is.na(ps_tide_level)]
nrow(comp_diff) / nrow(comp_agree)
# 0.1003891

#====================================================================
# Test various combinations of inputs for subordinate station
#====================================================================

# Test subordinate 1-min increment...get warning!
tm = Sys.time()
sub_point_1 = tide_level(
  tide_station = "Whitney Point",
  start_date = "2024-01-25",
  end_date = "2024-01-26",
  data_interval = "1-min",
  verbose = FALSE
)
nd = Sys.time(); nd - tm  # 0.1433229 secs

# Test subordinate 6-min warning, verbose true
tm = Sys.time()
sub_point_6 = tide_level(
  tide_station = "Whitney Point",
  start_date = "2024-01-25",
  end_date = "2024-01-26",
  data_interval = "6-min",
  verbose = TRUE
)
nd = Sys.time(); nd - tm  # 0.145246 secs

# Test subordinate
tm = Sys.time()
sub_high_low = tide_level(
  tide_station = "Whitney Point",
  start_date = "2024-01-25",
  end_date = "2024-01-26",
  data_interval = "high-low",
  verbose = FALSE
)
nd = Sys.time(); nd - tm  # 0.1502721 secs

# Test subordinate
tm = Sys.time()
sub_high = tide_level(
  tide_station = "Whitney Point",
  start_date = "2024-01-25",
  end_date = "2024-01-26",
  data_interval = "high-only",
  verbose = FALSE
)
nd = Sys.time(); nd - tm  # 0.153089 secs

# Test subordinate
tm = Sys.time()
sub_low = tide_level(
  tide_station = "Whitney Point",
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
nd = Sys.time(); nd - tm  # 1.287391 mins vs 2.282157 secs DT
                          # (1.287391 * 60) / 2.282157 secs = 34 times faster with DT

#====================================================================
# Testing time to compute...rtide
#====================================================================

# Test one month at one hour increments
tm = Sys.time()
rtide_one = rtide::tide_height(
  stations = "Seattle",
  minutes = 60L,
  from = as.Date("2023-12-08"),
  to = as.Date("2024-01-08"),
  tz = "America/Los_Angeles"
)
nd = Sys.time(); nd - tm  # 0.328156 secs

# Test one year at one hour increments
tm = Sys.time()
rtide_two = rtide::tide_height(
  stations = "Seattle",
  minutes = 60L,
  from = as.Date("2023-12-08"),
  to = as.Date("2024-12-08"),
  tz = "America/Los_Angeles"
)
nd = Sys.time(); nd - tm  # 3.64111 secs

# Test one month at six minute increments
tm = Sys.time()
rtide_three = rtide::tide_height(
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


