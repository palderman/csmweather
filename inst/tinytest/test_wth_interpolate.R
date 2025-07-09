library(tinytest)

expect_identical(csmweather::wth_interpolate(c(1, 2, NA, 4, NA, 6), method = "linear"),
                 as.double(1:6))

expect_identical(csmweather::wth_interpolate(c(1, 2, NA, NA, NA, 36), method = "linear"),
                 as.double(c(1, 2, 10.5, 19, 27.5, 36)))

expect_identical(csmweather::wth_interpolate(c(1, 2, NA, NA, NA, 36), method = "spline"),
                 as.double(c(1, 2, 6, 13, 23, 36)))
