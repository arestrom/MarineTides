
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

## Usage

``` r
library(MarineTides)

# With no arguments, outputs a data.table of today's tide at Seattle, WA in 15 minute increments.
todays_tide = tide_level()
todays_tide[, .(station_code, station_name, tide_time, tide_level)]
#>      station_code station_name           tide_time   tide_level
#>            <char>       <char>              <POSc>      <units>
#>   1:      9447130      Seattle 2024-06-15 00:00:00 3.320440 [m]
#>   2:      9447130      Seattle 2024-06-15 00:15:00 3.336625 [m]
#>   3:      9447130      Seattle 2024-06-15 00:30:00 3.338127 [m]
#>   4:      9447130      Seattle 2024-06-15 00:45:00 3.325300 [m]
#>   5:      9447130      Seattle 2024-06-15 01:00:00 3.298657 [m]
#>  ---                                                           
#> 188:      9447130      Seattle 2024-06-16 22:45:00 2.594660 [m]
#> 189:      9447130      Seattle 2024-06-16 23:00:00 2.683913 [m]
#> 190:      9447130      Seattle 2024-06-16 23:15:00 2.772680 [m]
#> 191:      9447130      Seattle 2024-06-16 23:30:00 2.859218 [m]
#> 192:      9447130      Seattle 2024-06-16 23:45:00 2.941645 [m]

# To search for a station, try entering part of the name. Message will include list of possible matches.
# tide_level(tide_station = "Whitney")

# Then entering a few more letters will allow filtering to a unique station
subordinate_tide = tide_level("Whitney Point",
                   start_date = "2024-03-11",
                   end_date = "2024-03-12",
                   data_interval = "low-only",
                   tide_unit = "feet",
                   verbose = TRUE)
#> Whitney Point, Dabob Bay is a subordinate station. The reference station, 
#> Seattle, is located 42.23 km. away. Tide levels are 
#> calculated from Seattle harmonic constituents at one 
#> minute increments. Values for high and low tide are extracted, 
#> then time and height offset corrections are applied to obtain the 
#> Whitney Point, Dabob Bay predictions. 
#> 
#> Tides will be predicted from 2024-03-11 to 2024-03-12
subordinate_tide[, .(station_code, station_name, tide_time, tide_level)]
#>    station_code             station_name           tide_time      tide_level
#>          <char>                   <char>              <POSc>         <units>
#> 1:      9445246 Whitney Point, Dabob Bay 2024-03-11 00:01:00 -0.2454090 [ft]
#> 2:      9445246 Whitney Point, Dabob Bay 2024-03-11 12:41:00  2.0966149 [ft]
#> 3:      9445246 Whitney Point, Dabob Bay 2024-03-12 00:45:00  1.1837560 [ft]
#> 4:      9445246 Whitney Point, Dabob Bay 2024-03-12 13:25:00  0.7557052 [ft]
```

## Disclaimers

All data used in this package originated with the Center for Operational
Oceanic Products and Services (CO-OPS), National Ocean Service (NOS),
National Oceanic and Atmospheric Administration (NOAA), USA. All tide
station data were downloaded, processed, and modified by Are Strom.
Lunar node year constituent corrections were obtained from David
Flater’s [`XTide`](https://flaterco.com/xtide/) website. This modified
version of NOS data is not official government material. It is not
identical to the data as originally provided, and may introduce errors
that were not present in the source data.

##### [David Flater](https://flaterco.com/xtide/disclaimer.html) includes the following note regarding any use of data originating from his domain:

##### NOT FOR NAVIGATION

##### DO NOT RELY ON THIS DATA FILE FOR DECISIONS THAT CAN RESULT IN HARM TO ANYONE OR ANYTHING.

Highly trustworthy tide predictions cannot be achieved on a zero budget.
If you need guaranteed results, don’t use these data! Contact the tide
authority for your region (NOAA in the U.S.).

The user assumes the entire risk related to any use, misuse, or
redistribution of these data. David Flater is providing these data “as
is,” and WITHOUT ANY WARRANTY; without any warranty that their use will
NOT INFRINGE ANY RIGHTS, and without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

In no event will David Flater be liable to you or to any third party for
any direct, indirect, incidental, consequential, special or exemplary
damages or lost profit resulting from any use, misuse, or redistribution
of these data.

The same disclaimer applies to any data downloaded, processed, and
modified by Are Strom.

##### [NOAA CO-OPS](https://tidesandcurrents.noaa.gov/disclaimers.html) adds the additional disclaimers below (content downloaded 2024-02-18):

##### Raw Data Disclaimer

These raw data have not been subjected to the National Ocean Service’s
quality control or quality assurance procedures and do not meet the
criteria and standards of official National Ocean Service data. They are
released for limited public use as preliminary data to be used only with
appropriate caution.

##### Use of Data and Products

The information on government servers are in the public domain, unless
specifically annotated otherwise, and may be used freely by the public.
Before using information obtained from this server, special attention
should be given to the date and time of the data and products being
displayed. This information shall not be modified in content and then
presented as official government material.

The user assumes the entire risk related to its use of these data. NOS
is providing these data “as is,” and NOS disclaims any and all
warranties, whether express or implied, including (without limitation)
any implied warranties of merchantability or fitness for a particular
purpose. In no event will NOS be liable to you or to any third party for
any direct, indirect, incidental, consequential, special or exemplary
damages or lost profit resulting from any use or misuse of this data.

NOS requests that attribution be given whenever NOS material is
reproduced and re-disseminated. Pursuant to 17 U.S.C. 403, third parties
producing copyrighted (compilation) works consisting predominantly of
material created by Federal Government employees are encouraged to
provide notice with such work(s) identifying the U.S. Government
material incorporated and stating that such material is not subject to
copyright protection.

## Contributions

Please report any
[issues](https://github.com/arestrom/MarineTides/issues).

[Pull requests](https://github.com/arestrom/MarineTides/pulls) are
always welcome.
