test_that("Jevons index formula is correct", {
  expect_equal(round(jevons(milk, start = "2019-12", end = "2020-01"), 4), 1.0202)
})
test_that("Fisher index formula is correct", {
  expect_equal(round(fisher(milk, start = "2019-12", end = "2020-01"), 4), 0.9741)
})
test_that("Chain Fisher index formula is correct", {
  expect_equal(round(chfisher(milk, start = "2019-12", end = "2020-02"), 4), 1.0077)
})
test_that("GEKS index formula is correct", {
  expect_equal(round(geks(milk, start = "2019-12", end = "2020-02", window = 3), 4), 1.0108)
})
