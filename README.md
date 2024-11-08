# csmweather

The purpose of this package is to provide an interface to weather data sources and utilities for preparing data suitable for crop modeling.

The package currently provides:

- an interface to NASA POWER agroclimatology datasets (see [https://power.larc.nasa.gov/](https://power.larc.nasa.gov/)) using the `nasapower` R package.

Future versions will include:

- an interface to the Global Historical Climate Network (GHCN) daily dataset
- a function for coercing existing data frames to a form suitable for use with the `write_wth()` function from the DSSAT R package ([https://cran.r-project.org/package=DSSAT](https://cran.r-project.org/package=DSSAT))
- additional functions to facilitate unit conversion
