true_ag_2d <- matrix(c(0, 0, 5, 5), ncol = 2, byrow = TRUE)
true_sr_2d <- matrix(c(10, 10, 15, 15), ncol = 2, byrow = TRUE)
true_ag_3d <- matrix(c(0, 0, 0, 5, 5, 5), ncol = 3, byrow = TRUE)

test_that("dimensions of map are correct", {
  expect_equal(ncol(map_maker_coord(2, 2, true_ag_2d, range = 1)$coord), 2)
  expect_equal(ncol(map_maker_coord(2, 2, true_ag_3d, range = 1, dimensions = 3)$coord), 3)
})

test_that("number of antigens and sera is as specified", {
  result <- map_maker_coord(2, 2, true_ag_2d, range = 1)
  expect_equal(length(grep("AG", rownames(result$coord))), 2)
  expect_equal(length(grep("SR", rownames(result$coord))), 2)

  true_ag_5 <- matrix(rep(0, 10), ncol = 2)
  result5 <- map_maker_coord(5, 3, true_ag_5, range = 1)
  expect_equal(length(grep("AG", rownames(result5$coord))), 5)
  expect_equal(length(grep("SR", rownames(result5$coord))), 3)
})

test_that("coordinates are within range of true coordinates", {
  result <- map_maker_coord(2, 2, true_ag_2d, range = 1, seed = 1)
  expect_true(all(result$antigen_coord >= true_ag_2d))
  expect_true(all(result$antigen_coord <= true_ag_2d + 1))
})

test_that("antigen and sera positions are coincident when same true coords used", {
  result <- map_maker_coord(2, 2, true_ag_2d, range = 1, seed = 42)
  expect_equivalent(result$antigen_coord, result$sera_coord)
})

test_that("antigen and sera positions differ when different true coords used", {
  result <- map_maker_coord(2, 2, true_ag_2d, true_sr_coord = true_sr_2d, range = 1, seed = 42)
  expect_false(isTRUE(all.equal(result$antigen_coord, result$sera_coord)))
  expect_true(all(result$sera_coord >= true_sr_2d))
  expect_true(all(result$sera_coord <= true_sr_2d + 1))
})

test_that("slim_dist has correct dimensions", {
  result <- map_maker_coord(3, 2, matrix(rep(0, 6), ncol = 2), range = 1)
  expect_equal(dim(result$slim_dist), c(3, 2))

  result2 <- map_maker_coord(4, 2, matrix(rep(0, 8), ncol = 2), range = 1)
  expect_equal(dim(result2$slim_dist), c(4, 2))
})

test_that("same seed gives same result", {
  r1 <- map_maker_coord(2, 2, true_ag_2d, range = 1, seed = 99)
  r2 <- map_maker_coord(2, 2, true_ag_2d, range = 1, seed = 99)
  expect_equivalent(r1$coord, r2$coord)
})

test_that("different seeds give different results", {
  r1 <- map_maker_coord(2, 2, true_ag_2d, range = 1, seed = 1)
  r2 <- map_maker_coord(2, 2, true_ag_2d, range = 1, seed = 2)
  expect_false(isTRUE(all.equal(r1$coord, r2$coord)))
})

test_that("a warning is produced when row counts differ", {
  true_ag <- matrix(c(0, 0, 5, 5), ncol = 2, byrow = TRUE)  # 2 rows
  true_sr <- matrix(c(10, 10, 15, 15, 20, 20, 25, 25), ncol = 2, byrow = TRUE)  # 4 rows
  expect_warning(
    map_maker_coord(2, 4, true_ag, true_sr_coord = true_sr, range = 1),
    "recycling"
  )
})

test_that("no warning when row counts are equal", {
  expect_no_warning(map_maker_coord(2, 2, true_ag_2d, range = 1, seed = 1))
})

test_that("recycled coords are placed near the correct true positions", {
  # true_ag has 2 rows, true_sr has 4 rows — true_ag gets recycled
  true_ag <- matrix(c(0, 0, 5, 5), ncol = 2, byrow = TRUE)
  true_sr <- matrix(c(10, 10, 15, 15, 20, 20, 25, 25), ncol = 2, byrow = TRUE)
  result <- suppressWarnings(
    map_maker_coord(2, 4, true_ag, true_sr_coord = true_sr, range = 1, seed = 1)
  )
  expect_equal(dim(result$slim_dist), c(2, 4))
  expect_true(all(result$antigen_coord >= true_ag))
  expect_true(all(result$antigen_coord <= true_ag + 1))
  expect_true(all(result$sera_coord >= true_sr))
  expect_true(all(result$sera_coord <= true_sr + 1))
})
