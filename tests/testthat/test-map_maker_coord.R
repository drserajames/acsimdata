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

test_that("antigen and sera positions differ when different true coords used", {
  result <- map_maker_coord(2, 2, true_ag_2d, true_sr_coord = true_sr_2d, range = 1, seed = 42)
  expect_false(isTRUE(all.equal(result$antigen_coord, result$sera_coord)))
  expect_true(all(result$sera_coord >= true_sr_2d))
  expect_true(all(result$sera_coord <= true_sr_2d + 1))
})

test_that("coincident=TRUE: first n_sera antigens become sera (exact positions)", {
  result <- map_maker_coord(4, 2, true_ag_2d, range = 1, seed = 7, coincident = TRUE)
  expect_equivalent(result$sera_coord, result$antigen_coord[1:2, ])
})

test_that("coincident=TRUE: dist matrix is n_antigens x n_antigens", {
  result <- map_maker_coord(4, 2, true_ag_2d, range = 1, seed = 7, coincident = TRUE)
  expect_equal(dim(result$dist), c(4, 4))
})

test_that("coincident=TRUE: slim_dist is n_antigens x n_sera with zeros on homologous diagonal", {
  result <- map_maker_coord(4, 2, true_ag_2d, range = 1, seed = 7, coincident = TRUE)
  expect_equal(dim(result$slim_dist), c(4, 2))
  # homologous pairs (AG1-SR1 and AG2-SR2) have distance 0
  expect_equal(result$slim_dist[1, 1], 0)
  expect_equal(result$slim_dist[2, 2], 0)
})

test_that("coincident=integer: specified antigen indices become sera", {
  result <- map_maker_coord(4, 2, true_ag_2d, range = 1, seed = 7, coincident = c(2L, 4L))
  expect_equivalent(result$sera_coord, result$antigen_coord[c(2, 4), ])
  expect_equal(result$slim_dist[2, 1], 0)   # AG2 = SR1
  expect_equal(result$slim_dist[4, 2], 0)   # AG4 = SR2
})

test_that("coincident=integer: dist matrix is n_antigens x n_antigens", {
  result <- map_maker_coord(4, 2, true_ag_2d, range = 1, seed = 7, coincident = c(1L, 3L))
  expect_equal(dim(result$dist), c(4, 4))
})

test_that("coincident: error on wrong-length index vector", {
  expect_error(
    map_maker_coord(4, 2, true_ag_2d, range = 1, coincident = c(1L, 2L, 3L)),
    "n_sera"
  )
})

test_that("coincident: error on out-of-range indices", {
  expect_error(
    map_maker_coord(4, 2, true_ag_2d, range = 1, coincident = c(1L, 99L)),
    "n_antigens"
  )
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

test_that("true_ag_coord with fewer rows than n_antigens is recycled silently", {
  # 2-row centre matrix recycled to 4 antigens: pattern is (0,0),(5,5),(0,0),(5,5)
  true_ag <- matrix(c(0, 0, 5, 5), ncol = 2, byrow = TRUE)
  expect_no_warning(map_maker_coord(4, 2, true_ag, range = 1, seed = 1))
  result <- map_maker_coord(4, 2, true_ag, range = 1, seed = 1)
  # Odd-indexed antigens (1, 3) near (0,0); even-indexed (2, 4) near (5,5)
  expect_true(all(result$antigen_coord[c(1, 3), ] >= 0 & result$antigen_coord[c(1, 3), ] <= 1))
  expect_true(all(result$antigen_coord[c(2, 4), ] >= 5 & result$antigen_coord[c(2, 4), ] <= 6))
})

test_that("independent sera placed near their own cluster centres", {
  true_ag <- matrix(c(0, 0, 5, 5), ncol = 2, byrow = TRUE)
  true_sr <- matrix(c(10, 10, 15, 15, 20, 20, 25, 25), ncol = 2, byrow = TRUE)
  result <- map_maker_coord(2, 4, true_ag, true_sr_coord = true_sr, range = 1, seed = 1)
  expect_equal(dim(result$slim_dist), c(2, 4))
  expect_true(all(result$antigen_coord >= 0 & result$antigen_coord <= 6))
  expect_true(all(result$sera_coord >= true_sr))
  expect_true(all(result$sera_coord <= true_sr + 1))
})
