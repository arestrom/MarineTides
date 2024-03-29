#' @title
#' Retrieve a unique station code by entering a station name
#'
#' @description
#' Inspects \code{harmonics} data for stations with similar names to the \code{station}
#' variable. Users can enter either an exact name, or the first few letters. If an exact
#' match is not found, it will print a list of possible matches. If no partial matches are
#' found then a message will issued to retry using a different spelling.
#'
#' @param station A character field approximating, or identical, to the tide station name
#' @param verbose A boolean requesting additional information be printed to the R console
#' @param harms Harmonics data
#'
#' @details
#' The \code{harmonics} dataset contains information for over three thousand harmonic and subordinate
#' tide stations worldwide. Most stations are located in the United States, but stations are also
#' included for locations adjacent to USA territories in the Pacific Ocean and the Carribbean.
#' The \code{identify_station()} function uses the \code{data.table::ilike()} function to identify stations
#' in the \code{harmonics} dataset with similar names. Unless you know the exact station name, it is
#' usually better to enter just the first few letters of the station name. This will return a message
#' in the R console with a list of possible matches.
#'
#' @returns
#' If a matching name is found \code{identify_station()} will return a unique \code{station_code}.
#' This is the official station identifier assigned by NOAA CO-OPS. If more than one partial matches
#' are found \code{identify_station()} will issue a message with a list of matching names. If no
#' matches are found then a message will suggest trying a different spelling.
#'
#' @export
identify_station = function(station, verbose, harms = MarineTides::harmonics) {
  stations_dt = as.data.table(harms$st_data)
  # Pull out possible stations and ids
  first_try = stations_dt[station_name == station, list(station_name, station_code)]
  if ( nrow(first_try) == 1L ) {
    station_code = first_try$station_code
    alt_names = stations_dt[station_name %ilike% station, list(station_name, station_code)]
    if ( verbose == TRUE ) {
      alt_stations = alt_names$station_name[!alt_names$station_name == station]
      if ( length(alt_stations) > 1L ) {
        alt_stations = paste0(alt_stations, collapse = "; ")
        cat(glue::glue("Stations listed below have similar names:\n{alt_stations}", "\n"))
      }
    }
  } else if ( !nrow(first_try) == 1L ) {
    station_codes = stations_dt[station_name %ilike% station, list(station_name, station_code)]
    if ( nrow(station_codes) > 1L ) {
      # Message in case more than one station matches
      n_stations = station_codes$station_name
      n_stations = paste0(n_stations, collapse = "; ")
      cat(glue::glue("Stations listed below have similar names:\n{n_stations}", "\n",
                     "Please enter one specific station"), "\n\n")
      station_code = NA_character_
    } else if ( nrow(station_codes) == 1L ) {
      station_code = station_codes$station_code
    } else if ( nrow(station_codes) == 0L ) {
      cat(glue::glue("Please try again: \n'{station}' did not match any existing station names"), "\n\n")
      station_code = NA_character_
    }
  }
  return(station_code)
}

#' @title
#' Retrieve a subset of information for the selected tide station
#'
#' @description
#' Retrieves a list of tide station data including \code{timezone} and \code{station_type}
#' (harmonic or subordinate). If the \code{station_type} is \code{subordinate} it will retrieve
#' the \code{station_code} for the harmonic reference station, otherwise it will return the same
#' \code{station_code} that was entered as a variable. The \code{verbose} argument enables
#' printing additional information to the console window regarding status of tide station
#' as \code{harmonic} or \code{subordinate}, and how tide predictions are generated for
#' subordinate stations.
#'
#' @param station_code A character field
#' @param verbose A boolean requesting additional information be printed to the R console
#' @param harms Harmonics data
#'
#' @returns
#' A list of text values including \code{station_code, station_type, timezone}. If the tide
#' station is a subordinate station the \code{station_code} will be that of the harmonic reference
#' station. The remaining values, \code{timezone} and \code{station_type}, will always belong to
#' the tide station identified by the \code{station_code} variable, regardless of whether it is
#' a subordinate or harmonic station.

#' @export
get_reference_station = function(station_code, verbose, harms = MarineTides::harmonics) {
  station_cd = station_code
  stations_dt = as.data.table(harms$st_data)
  offsets_dt = as.data.table(harms$st_offsets)
  station_dt = stations_dt[station_code == station_cd]
  station_type = station_dt[, station_type]
  station_name = station_dt[, station_name]
  station_timezone = station_dt[, timezone]
  station_removed = station_dt[, removed]
  if ( station_type == "H" ) {
    ref_station_code = list(station_cd, station_type, station_timezone)
    if ( !is.na(station_removed) ) {
      station_removed = format(station_removed, format = '%m/%d/%Y')
      warning(cat(glue::glue("{station_name} tide station is no longer active.", "\n",
                     "It was removed on {station_removed}.", "\n",
                     "USE WITH CAUTION!"), "\n\n"))
    }
    if ( verbose == TRUE ) {
      cat(glue::glue("{station_name} is a reference station. Tide heights are", "\n ",
                     "calculated from {station_name} harmonic constituents."), "\n\n")
    }
  } else {
    ref_station_code = offsets_dt[station_code == station_cd, reference_station_code]
    ref_station_km = offsets_dt[station_code == station_cd, ref_km]
    ref_station_name = stations_dt[station_code == ref_station_code, station_name]
    ref_station_code = list(ref_station_code, station_type, station_timezone)
    if ( verbose == TRUE ) {
      cat(glue::glue("{station_name} is a subordinate station. The reference station, ", "\n ",
                     "{ref_station_name}, is located {ref_station_km} km. away. Tide levels are ", "\n ",
                     "calculated from {ref_station_name} harmonic constituents at one ", "\n ",
                     "minute increments. Values for high and low tide are extracted, ", "\n ",
                     "then time and height offset corrections are applied to obtain the ", "\n ",
                     "{station_name} predictions."), "\n\n")
    }
  }
  return(ref_station_code)
}
