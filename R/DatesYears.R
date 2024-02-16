#' @title
#' Generate a sequence of datetime values from \code{start_date} to \code{end_date}.
#'
#' @description
#' Generates a vector of datetime values over the requested time span for tide
#' predictions. The timezone of the resulting datetime values will be \code{UTC}.
#'
#' @param start_date A character field for start of prediction timespan
#' @param end_date A character field for end of prediction timespan
#' @param pred_inc An integer field to specify number of minutes between predictions
#' @param timezone Typically the timezone of the \code{tide_station}, but can also be set manually.
#'
#' @returns
#' \code{get_prediction_range()} returns a vector of datetimes in \code{UTC} timezone between \code{start_date}
#' and \code{end_date}. The spacing between datetime values is set by the \code{pred_inc} variable.
#'
#' @export
get_prediction_range = function(start_date, end_date, pred_inc, timezone) {
  start = ISOdatetime(
    year = data.table::year(start_date),
    month = data.table::month(start_date),
    day = data.table::mday(start_date),
    hour = 0, min = 0, sec = 0,
    tz = timezone)
  end = ISOdatetime(
    year = data.table::year(end_date),
    month = data.table::month(end_date),
    day = data.table::mday(end_date),
    hour = 23, min = 59, sec = 59,
    tz = timezone)
    pred_span = seq(start, end, by = paste(pred_inc, "min"))
  attr(pred_span, "tzone") = "UTC"
  return(pred_span)
}

# Calculate the number of hours since start of the year for a given datetime.
hours_from_newyear <- function(datetime) {
  year = data.table::year(datetime)
  newyear = ISOdate(year, 1, 1, 0, tz = "UTC")
  hours = difftime(datetime, newyear, units = 'hours')
  hours = as.numeric(hours)
  return(hours)
}

# Get two-element vector composed of year values for start and end dates
get_year_range = function(start_date, end_date) {
  year_rng = c(data.table::year(start_date),
               data.table::year(end_date)) |>
    unique() |>
    as.integer()
  return(year_rng)
}
