test_that("noises sum correctly", {
  m <- map_maker_random(5, 5, 10)
  n <- add_noise(m$dist)
  expect_equal(n$total_noise, n$titre_noise + n$antigen_noise + n$serum_noise)
  expect_equivalent(n$total_noise, n$noise_dist_table - n$dist_table)
})

test_that("add_noise_targeted: single titre by name", {
  m <- map_maker_random(5, 5, 10, seed = 1)
  n <- add_noise_targeted(m$dist, antigen = "AG2", serum = "SR3", seed = 42)

  # only [AG2, SR3] should be non-zero
  expect_equal(sum(n$noise != 0), 1)
  expect_false(n$noise["AG2", "SR3"] == 0)
  expect_equal(n$noise_dist_table, n$dist_table + n$noise)
})

test_that("add_noise_targeted: single titre by index", {
  m <- map_maker_random(5, 5, 10, seed = 1)
  n_name  <- add_noise_targeted(m$dist, antigen = "AG2", serum = "SR3", seed = 42)
  n_index <- add_noise_targeted(m$dist, antigen = 2L,    serum = 3L,    seed = 42)

  expect_equal(n_name$noise, n_index$noise)
})

test_that("add_noise_targeted: full antigen row by name", {
  m <- map_maker_random(5, 5, 10, seed = 1)
  n <- add_noise_targeted(m$dist, antigen = "AG1", seed = 42)

  expect_true(all(n$noise["AG1", ] != 0))
  expect_true(all(n$noise[rownames(n$noise) != "AG1", ] == 0))
  expect_equal(n$noise_dist_table, n$dist_table + n$noise)
})

test_that("add_noise_targeted: full serum column by index", {
  m <- map_maker_random(5, 5, 10, seed = 1)
  n <- add_noise_targeted(m$dist, serum = 2L, seed = 42)

  expect_true(all(n$noise[, 2] != 0))
  expect_true(all(n$noise[, -2] == 0))
  expect_equal(n$noise_dist_table, n$dist_table + n$noise)
})

test_that("add_noise_targeted: errors on missing antigen/serum", {
  m <- map_maker_random(5, 5, 10, seed = 1)
  expect_error(add_noise_targeted(m$dist))
  expect_error(add_noise_targeted(m$dist, antigen = "AG99"))
  expect_error(add_noise_targeted(m$dist, serum = 99L))
})

test_that("add_noise_targeted: seed reproduces results", {
  m <- map_maker_random(5, 5, 10, seed = 1)
  n1 <- add_noise_targeted(m$dist, antigen = "AG3", serum = "SR4", seed = 7)
  n2 <- add_noise_targeted(m$dist, antigen = "AG3", serum = "SR4", seed = 7)
  expect_equal(n1$noise, n2$noise)
})
