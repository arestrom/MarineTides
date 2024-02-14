#' MarineTides
#'
#' Generates tide predictions for harmonic reference stations, and subordinate stations. Uses
#' harmonic data obtained from NOAA CO-OPS API
#'
#' @name MarineTides.package
#' @author Are Strom
#'
#' Maintainer: Are Strom <are.strom@@gmail.com>
#'
#' @keywords internal
"_PACKAGE"
#'
#' @import data.table
NULL

## quiets concerns of R CMD check re: the .'s and vars in data.table
if(getRversion() >= "2.15.1")  utils::globalVariables(c("timezone",
                                                        "removed",
                                                        "reference_station_code",
                                                        "ref_km",
                                                        "ref_station_km",
                                                        "station_name",
                                                        "station_datum",
                                                        "station_meridian",
                                                        "code",
                                                        "node_year",
                                                        "set",
                                                        "year",
                                                        "tide_type",
                                                        "%ilike%",
                                                        "station_code",
                                                        "tide_type",
                                                        "tide_time",
                                                        ":=",
                                                        "time_offset_high_tide_minutes",
                                                        "time_offset_low_tide_minutes",
                                                        "height_offset_high_tide",
                                                        "height_offset_low_tide",
                                                        "offset_level",
                                                        "offset_time",
                                                        "height_offset_type"))
