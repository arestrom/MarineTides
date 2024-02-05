
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MarineTides

<!-- badges: start -->

[![License:
GPL3](https://img.shields.io/badge/License-GPL3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
<!-- badges: end -->

## Introduction

`MarineTides` is an `R` package that can be used to generate tide
predictions for over three thousand locations worldwide. The majority of
these stations are located in the United States, but others are located
in areas adjacent to American territories in the Pacific Ocean and
Carribbean. This includes places such as the Marshall Islands, Fiji,
Cuba, and the Virgin Islands.

For tide stations where [NOAA
CO-OPS](https://api.tidesandcurrents.noaa.gov/api/prod/#products)
provide harmonics data, predictions can be made at intervals from one
minute to one hour. This is modeled on the options available from [NOAA
Tides & Currents](https://tidesandcurrents.noaa.gov/) Alternately,
values for just the daily high and low tide values can be also be
generated.

For subordinate stations, predictions are restricted to the daily high
and low tide values. These subordinate predictions are based on tide
levels calculated for an adjacent harmonic reference station. The
reference station predictions are generated at one-minute intervals.
Then local peaks and valleys in the harmonic tidal curves are extracted
to obtain the daily highs and lows. Finally, offset values published by
NOAA CO-OPS are used to adjust the harmonic values and generate
subordinate station estimates.

## Installation

`MarineTides` is still in an early stage of development. It has not been
fully tested yet. You should expect surprises!

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

## Inspiration

Almost since the first release, Joe Thorley’s excellent [`rtide`
package](https://github.com/poissonconsulting/rtide) has been an
indispensible tool to generate tides for research and analysis. But
speed can become an issue when longer time-series and higher resolutions
are needed. NOAA sensibly limits the amount of data that can be
downloaded in one batch, and `rtide` can take several minutes to churn
out a full years worth of predictions. Another limitation of `rtide` is
that it does not allow tide predictions for subordinate stations.

In addition, upcoming revisions of the [National Tidal Datum
Epoch](https://tidesandcurrents.noaa.gov/datum-updates/ntde/) and the
[North American Vertical Datum
(NAVD88)](https://oceanservice.noaa.gov/geodesy/three-datums.html),
suggested the need for a new package that I could quickly adapt as new
datums and harmonic data became available. Updates to datums will likely
occur more frequently as climate warms and sea levels rise.

`MarineTides` has intentionally been designed with minimal dependencies
and makes use of functions in the `data.table` package to speed up
calculations. [NOAA
CO-OPS](https://tidesandcurrents.noaa.gov/about_harmonic_constituents.html)
has an excellent write-up of the formula and methods used to predict
tides. The core functions in `MarineTides` were derived in large part
from the `rtide` package. In turn, `rtide` was modeled on David Flater’s
[`XTide`](https://flaterco.com/xtide/).
