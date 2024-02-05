#' @title
#' Harmonics
#'
#' @description
#' A set of harmonic data for NOAA CO-OPS tide stations. Also includes year constituents.
#'
#' @details
#' All tide station data, including constituent data and subordinate station offsets, were
#' downloaded from the [NOAA CO-OPS Data API](https://api.tidesandcurrents.noaa.gov/api/prod/)
#' Year constituent data captures the effects of the 18.6 year orbital cycle on amplitudes
#' of tidal constituents. Values for years 1700 to 2100 were extracted from the [rtide::harmonics]
#' data originally processed by David Flater for [XTide](https://flaterco.com/xtide/) and repackaged
#' for use in the`rtide` package by Joe Thorley of Poisson Consulting Ltd.
"harmonics"
