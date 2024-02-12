#' @title
#' Predict tide levels for harmonic or subordinate stations
#'
#' @description
#' `tide_level()` generates tide predictions for a `tide_station` from `start_date` to
#' `end_date` at the time interval specified by `data_interval`. The `timezone`, by
#' default, is that of the specified `tide_station`, but can be set to other zones if
#' needed.
#'
#' @param tide_station A character field similar, or identical, to the tide station name
#' @param start_date A character field for the start date of the tide prediction time-span
#' @param end_date A character field for the end date of the tide prediction time-span
#' @param data_interval A character value for the time increment between predictions.
#'   Allowable options include `1-min`, `6-min`, `15-min`, `30-min`, `60-min`, `high-low`,
#'   `high-only`, `low-only`
#' @param timezone Typically the timezone of the `tide_station`, but can also be set manually.
#' @param verbose A boolean requesting additional information be printed to the R console.
#' @param harms Harmonics data
#'
#' @details
#' The `harmonics` dataset contains information for over three thousand harmonic and subordinate
#' tide stations worldwide. Most stations are located in the United States, but stations are also
#' included for locations adjacent to USA territories in the Pacific Ocean and the Carribbean.
#' The internal [MarineTides::identify_station()] function uses the [data.table::%ilike%] function
#' to identify stations in the `harmonics` dataset with similar names. Unless you know the exact
#' station name, it is usually better to enter just the first few letters of the station name.
#' This will return a message in the R console with a list of possible matches.
#'
#' The `start_date` and `end_dates` for predictions are parsed by the [anytime::anydate] function.
#' This allows dates to be entered in a number of different formats. Typical formats such as
#' `2024-03-29`, `3/29/2024`, `20240329`, `2024.03.29` will return the expected values, but
#' entries such as `29/3/2024` or `3/3/24` will fail to parse and result in an error. Toggeling
#' the `verbose` argument to `TRUE` will send a message to the R-console displaying the start and
#' end dates requested, or as least the dates the function thought you intended.
#'
#' The `data_interval` variable includes options similar to those available when requesting
#' tide data from the NOAA Tides and Currents website. For harmonic stations, the same set of
#' increments from 1 minute, to 60 minutes are provided. You can also request only highs, only
#' lows, or both. For subordinate stations, NOAA only allows the latter three options. Because
#' tide predictions for subordinate stations are only defined in terms of offsets from the daily
#' high and low values, it would be speculative at best to infer tide levels, or tide times,
#' for other intervals. The shape of tidal curves between highs and lows at subordinate stations
#' may differ considerably from those of the reference harmonic stations.
#'
#' When predictions are requested for subordinate stations, or when the `data_interval` is one
#' of `high-low`, `high-only`, `low-only`, the time increment between predictions are set
#' to one minute. This allows peaks and valleys in the tidal curve to be found for the daily
#' high and low times and levels.
#'
#' @returns
#' A dataframe of predicted values. Includes information on the tide station
#'
#' @format A data.frame:
#' \describe{
#'   \item{station_code}{The NOAA Station ID (chr).}
#'   \item{station_name}{The NOAA Station Name (chr).}
#'   \item{reference_station_code}{Station ID for the reference station (chr).}
#'   \item{tide_type}{Harmonic or Subordinate (chr).}
#'   \item{tide_time}{Datetime of the prediction (time).}
#'   \item{MLLW}{Tide level in meters (dbl).}
#' }
#'
#' @export
tide_level = function(tide_station = "Seattle",
                      start_date = Sys.Date(),
                      end_date = Sys.Date() + 1,
                      data_interval = "15-min",
                      timezone = NULL,
                      verbose = FALSE,
                      harms = MarineTides::harmonics) {
  station_code = identify_station(tide_station, verbose, harms)
  if ( is.na(station_code) ) {
    stop("Station name is ambiguous")
  } else {
    station_info = get_reference_station(station_code, verbose)
  }
  if ( !data_interval %in% c("1-min", "6-min", "15-min", "30-min", "60-min",
                             "high-low", "high-only", "low-only") ) {
    stop(glue::glue("Please select one of the allowable options for data_interval:\n ",
                    "1-min, 6-min, 15-min, 30-min, 60-min, high-low, high-only, low-only"))
  } else if ( data_interval %in% c("1-min", "6-min", "15-min", "30-min", "60-min") ) {
    pred_inc = as.integer(sub("-min", "", data_interval))
  } else if ( data_interval %in% c("high-low", "high-only", "low-only") ) {
    pred_inc = 1L
  } else {
    pred_inc = 1L
  }
  if ( is.null(timezone) ) {
    timezone = station_info[[3]]
  }
  start_date = anytime::anydate(start_date, tz = timezone)
  end_date = anytime::anydate(end_date, tz = timezone)
  if ( is.na(start_date) ) {
    stop("start_date is ambiguous")
  }
  if ( is.na(end_date) ) {
    stop("end_date is ambiguous")
  }
  if ( verbose == TRUE ) {
    cat(glue::glue("\nTides will be predicted from {start_date} to {end_date}\n\n"))
  }
  prediction_dts = get_prediction_range(start_date, end_date, pred_inc, timezone)
  if ( station_info[[2]] == "S" & data_interval %in% c("1-min", "6-min", "15-min", "30-min", "60-min") ) {
    data_interval = "high-low"
    warning("\nFor subordinate stations, only high and low tide values will be computed.\n")
  }
  # Harmonic tides
  tide_pred = harmonic_tides(station_code,
                             station_info,
                             start_date, end_date,
                             prediction_dts,
                             timezone,
                             verbose,
                             harms)
  # Highs and lows
  if ( station_info[[2]] == "S" | data_interval %in% c("high-low", "high-only", "low-only") ) {
    hl_tides = high_low_tides(tide_pred, data_interval, tide_station)
  }
  # Subordinate station tides
  if ( station_info[[2]] == "S" ) {
    tide_out = subordinate_tides(hl_tides, harms)
  } else if ( station_info[[2]] == "H" & data_interval %in% c("high-low", "high-only", "low-only") ) {
    tide_out = hl_tides
  } else if (station_info[[2]] == "H" & data_interval %in% c("1-min", "6-min", "15-min", "30-min", "60-min") ) {
    tide_pred = as.data.table(tide_pred)
    tide_pred = tide_pred[, list(station_code, station_name,
                                 reference_station_code,
                                 tide_type, tide_time, tide_level)]
    tide_out = tide_pred
  }
  tide_out = tide_out[inrange(tide_time, min(prediction_dts), max(prediction_dts))]
  return(tide_out)
}

#' @title
#' Predict tide levels for harmonic stations
#'
#' @description
#' Given a `station_code`, a vector of datetimes (`prediction_dts`), and some additional
#' information in the `station_info` list output by [`MarineTides::get_reference_station`],
#' `harmonic_tides()` generates tide predictions for one harmonic station. It is the core
#' function called by [`MarineTides::tide_level`]
#'
#' @param station_code A character field identical to the NOAA Station ID
#' @param station_info A list including codes for the reference or subordinate stations
#' @param start_date A character field for the start date of the tide prediction time-span
#' @param end_date A character field for the end date of the tide prediction time-span
#' @param prediction_dts A vector of datetimes for the tide prediction.
#' @param timezone Typically the timezone of the `tide_station`, but can also be set manually.
#' @param verbose A boolean requesting additional information be printed to the R console.
#' @param harms Harmonics data
#'
#' @returns
#' A dataframe of predicted values. Includes information on the tide station
#'
#' @export
harmonic_tides = function(station_code, station_info,
                          start_date, end_date,
                          prediction_dts, timezone,
                          verbose, harms) {
  stations_dt = as.data.table(harms$st_data)
  offsets_dt = as.data.table(harms$st_offsets)
  stconsts_dt = as.data.table(harms$st_constituents)
  yrconsts_dt = as.data.table(harms$yr_constituents)
  speeds_dt = as.data.table(harms$ct_speed)
  # Station data
  station_dt = stations_dt[station_code == station_info[[1]]]
  datum = station_dt[, station_datum]
  meridian = 0L
  # Station Constituents data: amplitude, speed, phase
  consts = stconsts_dt[station_code == station_info[[1]], list(order, code, amplitude, phase)]
  consts = speeds_dt[consts, list(order, code, amplitude, phase, speed), on = "order"]
  amplitude = consts[, amplitude]
  speed = consts[, speed]
  phase = consts[, phase]
  # Year constants
  year_range = get_year_range(start_date, end_date)
  yr_dat = yrconsts_dt[node_year %in% year_range & order %in% consts[,order]]
  yr_args = data.table(node_year = year_range,
                       year_factor = NA_real_,
                       equil_arg = NA_real_)
  for (i in seq_along(year_range) ) yr_args$year_factor[i] =
    list(yr_dat$year_factor[yr_dat$node_year == year_range[i]])
  for (i in seq_along(year_range) ) yr_args$equil_arg[i] =
    list(yr_dat$equil_arg[yr_dat$node_year == year_range[i]])
  td_hts = data.table(tide_time = prediction_dts, datum = datum, meridian = meridian)
  set(td_hts, j = "amplitude", value = list(amplitude))
  set(td_hts, j = "speed", value = list(speed))
  set(td_hts, j = "phase", value = list(phase))
  set(td_hts, j = "node_year", value = year(td_hts$tide_time))
  set(td_hts, j = "pred_hour", value = hours_from_newyear(td_hts$tide_time))
  td_hts = td_hts[yr_args, on = "node_year"]
  for ( i in seq_along(td_hts$tide_time) ) {
    set(td_hts, i, j = "tide_height",
        value = (td_hts$datum[i] +
                   sum(unlist(td_hts$year_factor[[i]]) * unlist(td_hts$amplitude[[i]]) *
                         cos((unlist(td_hts$speed[[i]]) * (td_hts$pred_hour[[i]] - td_hts$meridian[[i]]) +
                                unlist(td_hts$equil_arg[[i]]) - unlist(td_hts$phase[[i]])) * pi/180))))
  }
  tide_hts = data.frame(station_code = station_code,
                        station_name = station_dt$station_name,
                        reference_station_code = station_info[[1]],
                        station_type = station_info[[2]],
                        tide_type = "P",
                        tide_time = td_hts$tide_time,
                        tide_level = td_hts$tide_height)
  attr(tide_hts$tide_time, "tzone") = timezone
  return(tide_hts)
}

# From: https://github.com/stas-g/findPeaks/blob/master/find_peaks.R
find_peaks = function (x, m = 3){
  shape = diff(sign(diff(x, na.pad = FALSE)))
  pks = sapply(which(shape < 0), FUN = function(i){
    z = i - m + 1
    z = ifelse(z > 0, z, 1)
    w = i + m + 1
    w = ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks = unlist(pks)
  return(pks)
}

# High and low tide values.
high_low_tides = function(tide_pred, data_interval, tide_station) {
  tide_pred = as.data.table(tide_pred)
  high_peaks = find_peaks(tide_pred$tide_level, m = 5)
  low_peaks = find_peaks(-tide_pred$tide_level, m = 5)
  for ( i in seq_along(tide_pred$tide_time) ) {
    set(tide_pred, i, j = "tide_type",
        value = if ( i %in% high_peaks ) {"H"} else if (i %in% low_peaks) {"L"} else {"P"})
  }
  if ( data_interval == "high-only" ) {
    tide_hl = tide_pred[tide_type == "H"]
  } else if (data_interval == "low-only" ) {
    tide_hl = tide_pred[tide_type == "L"]
  } else if ( data_interval == "high-low" ) {
    tide_hl = tide_pred[tide_type %in% c("H", "L")]
  } else {
    tide_hl = tide_pred
  }
  return(tide_hl)
}

# Apply offsets for subordinate stations
subordinate_tides = function(hl_tides, harms) {
  offsets_dt = data.table(harms$st_offsets)
  offsets_dt = offsets_dt[station_code == hl_tides$station_code[1]]
  height_offset_type = offsets_dt[, height_offset_type]
  tide_hl = cbind(offsets_dt, hl_tides[, .(tide_type, tide_time, tide_level)])
  if ( height_offset_type == "R" ) {
    tide_hl[tide_type == "H", ':=' (offset_time = tide_time + (time_offset_high_tide_minutes * 60),
                                    offset_level = tide_level * height_offset_high_tide)]
    tide_hl[tide_type == "L", ':=' (offset_time = tide_time + (time_offset_low_tide_minutes * 60),
                                    offset_level = tide_level * height_offset_low_tide)]
  } else {
    tide_hl[tide_type == "H", ':=' (offset_time = tide_time + (time_offset_high_tide_minutes * 60),
                                    offset_level = tide_level + height_offset_high_tide)]
    tide_hl[tide_type == "L", ':=' (offset_time = tide_time + (time_offset_low_tide_minutes * 60),
                                    offset_level = tide_level + height_offset_low_tide)]
  }
  tide_hl = tide_hl[, list(station_code, station_name, reference_station_code, tide_type,
                           tide_time = offset_time, tide_level = offset_level)]
  return(tide_hl)
}
