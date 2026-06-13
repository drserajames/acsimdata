test_that("the output is the same size as the input titre table", {
  m <- map_maker_random(5, 5, 10)
  ti <- dist_to_hi_titre(m$dist)
  ti_miss <- miss_titres_random(ti$lessthan_titre, 0.2)
  expect_equal(dim(ti_miss$full_titre), dim(ti_miss$rm_titre))
})

test_that("right number removed", {
  m <- map_maker_random(5, 5, 10)
  ti <- dist_to_hi_titre(m$dist)
  ti_miss <- miss_titres_random(ti$lessthan_titre, 0.2)

  expect_equal(round(ti_miss$params$proportion * length(ti_miss$full_titre)), length(ti_miss$rm_ind))
})

test_that("random: warns when output underconstrained for 2D", {
  m  <- map_maker_random(5, 5, 10, seed = 1)
  ti <- dist_to_hi_titre(m$dist)
  # remove almost everything (keep_homologous=FALSE so pool = all 25 cells)
  expect_warning(
    miss_titres_random(ti$lessthan_titre, 0.99, keep_homologous = FALSE),
    "underconstrained"
  )
})

test_that("random: min_dim caps removal to maintain 2D constraint", {
  m  <- map_maker_random(5, 5, 10, seed = 1)
  ti <- dist_to_hi_titre(m$dist)
  min_obs <- 2L * (nrow(ti$lessthan_titre) + ncol(ti$lessthan_titre)) - 3L
  result  <- suppressWarnings(
    miss_titres_random(ti$lessthan_titre, 0.99, keep_homologous = FALSE, min_dim = 2L)
  )
  expect_gte(sum(result$rm_titre != "*"), min_obs)
})

test_that("random: min_dim warns when cap is applied", {
  m  <- map_maker_random(5, 5, 10, seed = 1)
  ti <- dist_to_hi_titre(m$dist)
  expect_warning(
    miss_titres_random(ti$lessthan_titre, 0.99, keep_homologous = FALSE, min_dim = 2L),
    "Capping"
  )
})
