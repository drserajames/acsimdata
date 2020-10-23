test_that("output right size", {
  d <- matrix(100*runif(25),ncol=5, dimnames=list(paste0("AG", 1:5), paste0("SR", 1:5)))
  hi <- dist_to_hi_titre(dist=d)
  expect_equal(dim(d), dim(hi$raw_titre))
  expect_equal(dim(d), dim(hi$hi_titre))

  d <- matrix(10*runif(25),ncol=5, dimnames=list(paste0("AG", 1:5), paste0("SR", 1:5)))
  hi <- dist_to_hi_titre(dist=d)
  expect_equal(dim(d), dim(hi$raw_titre))
  expect_equal(dim(d), dim(hi$hi_titre))

  d <- matrix(runif(25),ncol=5, dimnames=list(paste0("AG", 1:5), paste0("SR", 1:5)))
  hi <- dist_to_hi_titre(dist=d)
  expect_equal(dim(d), dim(hi$raw_titre))
  expect_equal(dim(d), dim(hi$hi_titre))

  m <- map_maker_random(5,5,100)
  hi <- dist_to_hi_titre(m)
  expect_equal(dim(m$dist), dim(hi$raw_titre))
  expect_equal(dim(m$dist)/2, dim(hi$hi_titre))

  m <- map_maker_random(5,5,10)
  hi <- dist_to_hi_titre(m)
  expect_equal(dim(m$dist), dim(hi$raw_titre))
  expect_equal(dim(m$dist)/2, dim(hi$hi_titre))

  m <- map_maker_random(5,5,1)
  hi <- dist_to_hi_titre(m)
  expect_equal(dim(m$dist), dim(hi$raw_titre))
  expect_equal(dim(m$dist)/2, dim(hi$hi_titre))
})

test_that("titre divisible by divisor and base", {
  # function from help for is.integer
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

  d <- matrix(runif(25),ncol=5, dimnames=list(paste0("AG", 1:5), paste0("SR", 1:5)))
  hi <- dist_to_hi_titre(dist=d, base=2, divisor=10)
  expect_equivalent(is.wholenumber(hi$hi_titre/10), matrix(TRUE, ncol=5, nrow=5))
  expect_equivalent(is.wholenumber(log(hi$hi_titre/10,2)), matrix(TRUE, ncol=5, nrow=5))
  hi <- dist_to_hi_titre(dist=d, base=77, divisor=3)
  expect_equivalent(is.wholenumber(hi$hi_titre/3), matrix(TRUE, ncol=5, nrow=5))
  expect_equivalent(is.wholenumber(log(hi$hi_titre/3,77)), matrix(TRUE, ncol=5, nrow=5))

  d <- matrix(10*runif(25),ncol=5, dimnames=list(paste0("AG", 1:5), paste0("SR", 1:5)))
  hi <- dist_to_hi_titre(dist=d, base=2, divisor=10)
  expect_equivalent(is.wholenumber(hi$hi_titre/10), matrix(TRUE, ncol=5, nrow=5))
  expect_equivalent(is.wholenumber(log(hi$hi_titre/10,2)), matrix(TRUE, ncol=5, nrow=5))
  hi <- dist_to_hi_titre(dist=d, base=77, divisor=3)
  expect_equivalent(is.wholenumber(hi$hi_titre/3), matrix(TRUE, ncol=5, nrow=5))
  expect_equivalent(is.wholenumber(log(hi$hi_titre/3,77)), matrix(TRUE, ncol=5, nrow=5))

  d <- matrix(100*runif(25),ncol=5, dimnames=list(paste0("AG", 1:5), paste0("SR", 1:5)))
  hi <- dist_to_hi_titre(dist=d, base=2, divisor=10)
  expect_equivalent(is.wholenumber(hi$hi_titre/10), matrix(TRUE, ncol=5, nrow=5))
  expect_equivalent(is.wholenumber(log(hi$hi_titre/10,2)), matrix(TRUE, ncol=5, nrow=5))
  hi <- dist_to_hi_titre(dist=d, base=77, divisor=3)
  expect_equivalent(is.wholenumber(hi$hi_titre/3), matrix(TRUE, ncol=5, nrow=5))
  expect_equivalent(is.wholenumber(log(hi$hi_titre/3,77)), matrix(TRUE, ncol=5, nrow=5))

  m <- map_maker_random(5,5,100)
  hi <- dist_to_hi_titre(m, base=2, divisor=10)
  expect_equivalent(is.wholenumber(hi$hi_titre/10), matrix(TRUE, ncol=5, nrow=5))
  expect_equivalent(is.wholenumber(log(hi$hi_titre/10,2)), matrix(TRUE, ncol=5, nrow=5))
  hi <- dist_to_hi_titre(m, base=77, divisor=3)
  expect_equivalent(is.wholenumber(hi$hi_titre/3), matrix(TRUE, ncol=5, nrow=5))
  expect_equivalent(is.wholenumber(log(hi$hi_titre/3,77)), matrix(TRUE, ncol=5, nrow=5))

  m <- map_maker_random(5,5,10)
  hi <- dist_to_hi_titre(m, base=2, divisor=10)
  expect_equivalent(is.wholenumber(hi$hi_titre/10), matrix(TRUE, ncol=5, nrow=5))
  expect_equivalent(is.wholenumber(log(hi$hi_titre/10,2)), matrix(TRUE, ncol=5, nrow=5))
  hi <- dist_to_hi_titre(m, base=77, divisor=3)
  expect_equivalent(is.wholenumber(hi$hi_titre/3), matrix(TRUE, ncol=5, nrow=5))
  expect_equivalent(is.wholenumber(log(hi$hi_titre/3,77)), matrix(TRUE, ncol=5, nrow=5))

  m <- map_maker_random(5,5,1)
  hi <- dist_to_hi_titre(m, base=2, divisor=10)
  expect_equivalent(is.wholenumber(hi$hi_titre/10), matrix(TRUE, ncol=5, nrow=5))
  expect_equivalent(is.wholenumber(log(hi$hi_titre/10,2)), matrix(TRUE, ncol=5, nrow=5))
  hi <- dist_to_hi_titre(m, base=77, divisor=3)
  expect_equivalent(is.wholenumber(hi$hi_titre/3), matrix(TRUE, ncol=5, nrow=5))
  expect_equivalent(is.wholenumber(log(hi$hi_titre/3,77)), matrix(TRUE, ncol=5, nrow=5))

})
