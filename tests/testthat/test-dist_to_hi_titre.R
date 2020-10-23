test_that("output right size", {

  d <- NULL
  size <- c(1,10,100)
  for (i in 1:3){
    d[[i]] <- matrix(size[i]*runif(25),ncol=5, dimnames=list(paste0("AG", 1:5), paste0("SR", 1:5)))

    hi <- dist_to_hi_titre(dist=d[[i]])
    expect_equal(dim(d[[i]]), dim(hi$raw_titre))

  }


})

test_that("titre divisible by divisor and base wih default min_log_titre=0", {
  # function from help for is.integer
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

  d <- NULL
  size <- c(1,10,100)
  for (i in 1:3){
    d[[i]] <- matrix(size[i]*runif(25),ncol=5, dimnames=list(paste0("AG", 1:5), paste0("SR", 1:5)))

    for (bas in c(2,7,77)){
      for (div in c(1:3,10,133)){
        hi <- dist_to_hi_titre(dist=d[[i]], base=bas, divisor=div)
        expect_equivalent(is.wholenumber(bas*hi$lessthanhack_titre/div), matrix(TRUE, ncol=5, nrow=5))
        expect_equivalent(is.wholenumber(log(bas*hi$lessthanhack_titre/div,bas)), matrix(TRUE, ncol=5, nrow=5))
      }
    }



  }





})
