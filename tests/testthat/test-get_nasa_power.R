test_that("get_nasa_power()", {

    wth <- get_nasa_power(lat = -17.82, long = 30.92,
                          start = "1985-01-01", end = "1985-12-31")

    # Exclude problems pointer from test
    attr(wth, "problems") <- NULL

    expect_snapshot( deparse( wth ) )

    })
