
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MarineTides

<!-- badges: start -->

[![License:
GPL3](https://img.shields.io/badge/License-GPL3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
<!-- badges: end -->

## Introduction

The `MarineTides` package can be used to generate tide predictions for
3,333 tide stations worldwide. The majority of these stations are
located in the United States. Remaining stations are located in areas
adjacent to American territories in the Pacific Ocean and Carribbean.
This includes places such as the Marshall Islands, Fiji, Cuba, and the
Virgin Islands. Both harmonic and subordinate tide stations are
included. At current count, there are 1,143 stations with harmonic
constituents, and 2,190 subordinate stations.

The package name is intended to distinguish Marine Tides from
[`Earth Tides`](https://en.wikipedia.org/wiki/Earth_tide), or tides in
large freshwater bodies such as the
[`Great Lakes`](https://oceanservice.noaa.gov/facts/gltides.html). The
`MarineTides` package can only be used to predict tides for stations in
coastal locations, the open ocean, or marine influenced waters such as
estuaries.

All station data, including harmonic constituents, were downloaded from
the [NOAA CO-OPS API for Data
Retrieval](https://api.tidesandcurrents.noaa.gov/api/prod/#products).
Lunar node year corrections to tidal constituents for years 1700 to 2100
were extracted from the output file of David Flater’s [congen
program](https://flaterco.com/xtide/files.html#harmonicsfiles). The node
year data captures the effects of the 18.6 year lunar orbital cycle on
amplitudes and phase of lunar tidal constituents. In order to match NOAA
CO-OPS harmonic constituent naming conventions, some constituent names
needed to be updated. Issues related to constituent naming conventions,
and derivation of lunar nodal corrections are documented by David Flater
in the [`congen`](https://flaterco.com/files/xtide/congen_input.txt)
program input file.

For tide stations where [NOAA
CO-OPS](https://api.tidesandcurrents.noaa.gov/api/prod/#products)
provide harmonics data, predictions can be made at intervals from one
minute to one hour. This is modeled on the options available at [NOAA
Tides & Currents](https://tidesandcurrents.noaa.gov/) Alternately,
values can also be generated for just daily high and low tides.

For subordinate stations, predictions are restricted to daily highs and
lows. These subordinate station predictions are based on tide levels
calculated for an adjacent harmonic reference station. The reference
station predictions are generated at one-minute intervals. Then local
peaks and valleys in the harmonic tidal curves are extracted to obtain
the daily highs and lows. Finally, offset values provided by NOAA CO-OPS
are used to adjust the harmonic derived values and generate subordinate
station estimates.

## Motivation

The `MarineTides` package is motivated by need for large volumes of tide
predictions, often at one-minute increments. Primary uses are for
intertidal shellfisheries management, and for climate-related marine
research. The [`rtide`](https://github.com/poissonconsulting/rtide)
package has ably filled this need in the past, but speed can become an
issue when longer time-series at higher resolutions are needed. NOAA
sensibly limits the amount of data that can be downloaded in one batch,
and `rtide` can take several minutes to churn out a full years worth of
predictions at one-minute intervals. Another limitation of `rtide` is
that it does not allow for tide predictions at subordinate stations.

In addition, upcoming revisions of the [National Tidal Datum
Epoch](https://tidesandcurrents.noaa.gov/datum-updates/ntde/) and the
[North American Vertical Datum
(NAVD88)](https://oceanservice.noaa.gov/geodesy/three-datums.html),
suggests the need for a tide prediction package that can be easily
updated as new datums and harmonic data become available. Minor changes
to station data commonly occur on a quarterly schedule, and updates to
datums will likely occur more frequently as climate warms and sea levels
rise. The aim is to update `MarineTides` on a semi-regular schedule to
reflect changes to station data pushed by
[`NOAA CO-OPS`](https://tidesandcurrents.noaa.gov/products.html).

`MarineTides` has intentionally been designed with minimal dependencies
and makes use of functions in the
[`data.table`](https://rdatatable.gitlab.io/data.table/) package to
speed up calculations. [NOAA
CO-OPS](https://tidesandcurrents.noaa.gov/about_harmonic_constituents.html)
has an excellent write-up of the formula and methods used to predict
tides. The core functions in `MarineTides` were derived in part from Joe
Thorley’s `rtide` package. In turn, `rtide` was modeled on David
Flater’s [`XTide`](https://flaterco.com/xtide/).

## Installation

`MarineTides` is still in an early stage of development and some
functions may change as needs arise. The core API will hopefully remain
stable, but there are no guarantees. Expect surprises! Do not use for
navigation!

To install the latest version from
[GitHub](https://github.com/arestrom/MarineTides)

``` r
# install.packages("remotes")
remotes::install_github("arestrom/MarineTides")
```

## Contributions

Please report any
[issues](https://github.com/arestrom/MarineTides/issues).

[Pull requests](https://github.com/arestrom/MarineTides/pulls) are
always welcome.
