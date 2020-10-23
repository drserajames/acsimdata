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

  expect_equal(round(ti_miss$params$proportion*length(ti_miss$full_titre)), length(ti_miss$rm_ind))
})
