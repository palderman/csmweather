library(tinytest)

# Example calculation from FAO Irrigation and Drainage Paper 56 (Allen et al, 1998)

expect_equal(round(csmweather::wth_2m_wind(3.2, 10), digits = 1),
             2.4)
