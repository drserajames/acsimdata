test_that("dimensions of map are correct", {
  expect_equal(ncol(map_maker_random(2, 2, 2, dimensions = 2)$coord), 2)
  expect_equal(ncol(map_maker_random(2, 2, 2, dimensions = 3)$coord), 3)
  expect_equal(ncol(map_maker_random(2, 2, 2, dimensions = 5)$coord), 5)
})

test_that("number of antigens and sera is as specified", {
  expect_equal(length(grep("AG", rownames(map_maker_random(2, 2, 2, dimensions = 2)$coord))), 2)
  expect_equal(length(grep("AG", rownames(map_maker_random(2, 2, 2, dimensions = 3)$coord))), 2)
  expect_equal(length(grep("AG", rownames(map_maker_random(7, 2, 2, dimensions = 3)$coord))), 7)

  expect_equal(length(grep("SR", rownames(map_maker_random(2, 2, 2, dimensions = 2)$coord))), 2)
  expect_equal(length(grep("SR", rownames(map_maker_random(2, 2, 2, dimensions = 3)$coord))), 2)
  expect_equal(length(grep("SR", rownames(map_maker_random(7, 7, 2, dimensions = 3)$coord))), 7)
})

test_that("coordinates do not exceed range", {
  expect_lte(max(map_maker_random(2, 2, 2, dimensions = 2)$coord), 2)
  expect_gte(min(map_maker_random(2, 2, 2, dimensions = 2)$coord), 0)

  expect_lte(max(map_maker_random(2, 2, 2, dimensions = 3)$coord), 2)
  expect_gte(min(map_maker_random(2, 2, 2, dimensions = 3)$coord), 0)

  expect_lte(max(map_maker_random(2, 2, 10, dimensions = 2)$coord), 10)
})

test_that("antigen and sera positions are coincident", {
  expect_equivalent(map_maker_random(2, 2, 2, dimensions = 2, seed = 77)$coord[1:2, ], map_maker_random(2, 2, 2, dimensions = 2, seed = 77)$coord[3:4, ])
  expect_equivalent(map_maker_random(2, 2, 2, dimensions = 3, seed = 2459)$coord[1:2, ], map_maker_random(2, 2, 2, dimensions = 3, seed = 2459)$coord[3:4, ])
  expect_equivalent(map_maker_random(7, 2, 2, dimensions = 2, seed = 78589)$coord[1:2, ], map_maker_random(7, 2, 2, dimensions = 2, seed = 78589)$coord[8:9, ])
})
