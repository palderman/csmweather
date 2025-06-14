library(tinytest)

expect_identical(csmweather::lin_interp(c(1, 2, NA, 4, NA, 6)), as.double(1:6))
