opensensorwebr
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# 1 Description

This package provides access to the OpenSensorWeb-API
(api.opensensorweb.de) to create data sets for crop modelling. \# Usage
\#\# Install Package

``` r
# This package is not yet on CRAN. The easiest way to get sapflowr is to install it via github:
library(devtools)
install_github("RikardGrass/opensensorwebr") # this will install the version on "main"
```

``` r
# At the moment the package is a private package, you will need credentials
https://rdrr.io/cran/remotes/man/install_github.html recomends: 
# "
# To install from a private repo, use auth_token with a token
# from https://github.com/settings/tokens. You only need the
# repo scope. Best practice is to save your PAT in env var called
# GITHUB_PAT.
# "
install_github("RikardGrass/opensensorwebr", auth_token = "abcdefasdf")

# pottentially you may also use the https link if you are logged into your github account at the same time (e.g. via www.github.de in our browser)
```

## 1.1 load the package

``` r
library(opensensorwebr)
```

## 1.2 Get a listing of available sensors

``` r
opensensorwebr::availablesensors("https://api.opensensorweb.de/v0/networks/AMMS_WETTERDATEN", my.device = "S034")
```

## 1.3 Get data for one sensor

``` r
opensensorwebr::hourly(url = "https://api.opensensorweb.de/v0/networks/AMMS_WETTERDATEN", my.device = "S021", my.sensor = "Niederschlag", aggregation = "MEAN", my.interval = 1000, my.startdate = "2015-09-18T00:00:00Z")
```

## 1.4 Get a dataset to calculate evaportranspiration according to Penman Monteight

  - Todo: insert functionality to transform wind speed measured in 2.5m
    into 2m

<!-- end list -->

``` r
coswig <- opensensorwebr::etmodeldata("https://api.opensensorweb.de/v0/networks/AMMS_WETTERDATEN",
                                      my.device = "S021",
                                      my.startdate = "2024-01-01T00:00:00Z",
                                      my.interval = 100,
                                      ID.GlobRad = "Globalstrahlg_200cm",
                                      ID.AirTemp = "Lufttemp_200cm",
                                      ID.RH = "Luftfeuchtigkeit_200cm",
                                      ID.Rain = "Niederschlag",
                                      ID.Wind = "Windgeschw_250cm",
                                      file = "temp/Wetter_Coswig_",
                                      write.RData = FALSE,
                                      write.csv = FALSE)
```

## 1.5 Getting help

  - contact: <rikard.grass@ufz.de>
