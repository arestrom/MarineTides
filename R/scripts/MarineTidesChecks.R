#====================================================================================
# Test MarineTides, then compare output for MarineTides vs rtide and NOAA
#
# Notes:
#  1. Looks good
#
# AS 2024-02-14
#====================================================================================

# Load libraries
library(MarineTides)

#====================================================================
# Test various combinations of inputs for harmonic stations
#====================================================================

# Test one min harmonic
tm = Sys.time()
harm_point_1 = tide_level(
  tide_station = "Seattle",
  start_date = "2024-01-25",
  end_date = "2024-01-26",
  data_interval = "1-min",
  verbose = TRUE
)
nd = Sys.time(); nd - tm  # 0.137969 secs

# Test harmonic
tm = Sys.time()
harm_point_6 = tide_level(
  tide_station = "Seattle",
  start_date = "2024-01-25",
  end_date = "2024-01-26",
  data_interval = "6-min",
  verbose = FALSE
)
nd = Sys.time(); nd - tm  # 0.05675316 secs

# Test harmonic
tm = Sys.time()
harm_point_15 = tide_level(
  tide_station = "Seattle",
  start_date = "2024-01-25",
  end_date = "2024-01-26",
  data_interval = "15-min",
  verbose = FALSE
)
nd = Sys.time(); nd - tm  # 0.07404494 secs

# Test harmonic
tm = Sys.time()
harm_point_30 = tide_level(
  tide_station = "Seattle",
  start_date = "2024-01-25",
  end_date = "2024-01-26",
  data_interval = "30-min",
  verbose = FALSE
)
nd = Sys.time(); nd - tm  # 0.04517698 secs

# Test harmonic
tm = Sys.time()
harm_point_60 = tide_level(
  tide_station = "Seattle",
  start_date = "2024-01-25",
  end_date = "2024-01-26",
  data_interval = "60-min",
  verbose = FALSE
)
nd = Sys.time(); nd - tm  # 0.0438211 secs

# Test harmonic
tm = Sys.time()
harm_high_low = tide_level(
  tide_station = "Seattle",
  start_date = "2024-01-25",
  end_date = "2024-01-26",
  data_interval = "high-low",
  verbose = TRUE
)
nd = Sys.time(); nd - tm  # 0.135776 secs

# Test harmonic
tm = Sys.time()
harm_high = tide_level(
  tide_station = "Seattle",
  start_date = "2024-01-25",
  end_date = "2024-01-26",
  data_interval = "high-only",
  verbose = FALSE
)
nd = Sys.time(); nd - tm  # 0.145772 secs

# Test harmonic
tm = Sys.time()
harm_low = tide_level(
  tide_station = "Seattle",
  start_date = "2024-01-25",
  end_date = "2024-01-26",
  data_interval = "low-only",
  verbose = FALSE
)
nd = Sys.time(); nd - tm  # 0.1522071 secs

# Test harmonic high_low for six months
tm = Sys.time()
harm_year_high_low = tide_level(
  tide_station = "Seattle",
  start_date = "2024-01-01",
  end_date = "2024-06-30",
  data_interval = "high-low",
  verbose = FALSE
)
nd = Sys.time(); nd - tm  # 9.362377 secs

# Test harmonic high-low for a full year at one minute resolution
tm = Sys.time()
harm_year_high_low = tide_level(
  tide_station = "Seattle",
  start_date = "2024-01-01",
  end_date = "2024-12-31",
  data_interval = "high-low",
  verbose = TRUE
)
nd = Sys.time(); nd - tm  # 19.66335 secs

# Test harmonic high-low for a full year at one minute resolution
tm = Sys.time()
harm_year_1min = tide_level(
  tide_station = "Seattle",
  start_date = "2024-01-01",
  end_date = "2024-12-31",
  data_interval = "1-min",
  verbose = TRUE
)
nd = Sys.time(); nd - tm  # 14.82077 secs

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
nd = Sys.time(); nd - tm  # 0.1141739 secs

# Test subordinate
tm = Sys.time()
sub_high_low = tide_level(
  tide_station = "Whitney Point",
  start_date = "2024-01-25",
  end_date = "2024-01-26",
  data_interval = "high-low",
  verbose = TRUE
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
nd = Sys.time(); nd - tm  # 0.253371 secs

# Test subordinate
tm = Sys.time()
sub_low = tide_level(
  tide_station = "Whitney Point",
  start_date = "2024-01-25",
  end_date = "2024-01-26",
  data_interval = "low-only",
  verbose = FALSE
)
nd = Sys.time(); nd - tm  # 0.2494249 secs


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
nd = Sys.time(); nd - tm  # 0.3623669 secs pre: 0.07381392 secs DT

# Test one year at one hour increments
tm = Sys.time()
tide_two = tide_level(
  tide_station = "Seattle",
  start_date = "2023-12-08",
  end_date = "2024-12-08",
  data_interval = "60-min"
)
nd = Sys.time(); nd - tm  # 2.842646 secs pre: 0.2937629 secs DT

# Test one month at six minute increments
tm = Sys.time()
tide_three = tide_level(
  tide_station = "Seattle",
  start_date = "2023-12-08",
  end_date = "2024-01-08",
  data_interval = "6-min"

)
nd = Sys.time(); nd - tm  # 2.774406 secs pre: 0.2608321 secs DT

# Test one year at six minute increments
tm = Sys.time()
tide_four = tide_level(
  tide_station = "Seattle",
  start_date = "2023-12-08",
  end_date = "2024-12-08",
  data_interval = "6-min"
)
nd = Sys.time(); nd - tm  # 1.287391 mins vs 2.410178 secs DT
                          # (1.287391 * 60) / 2.410178 secs = 32 times faster with DT

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
#          using data.table is ~ 32 times faster
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






















