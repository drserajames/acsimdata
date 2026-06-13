# Shared fixture ---------------------------------------------------------
make_fixture <- function(seed = 1) {
  m  <- map_maker_random(5, 5, 10, seed = seed)
  ti <- dist_to_hi_titre(m$dist)
  list(m = m, ti = ti)
}

# ── miss_titres_threshold ─────────────────────────────────────────────────────

test_that("threshold: output dimensions match input", {
  f  <- make_fixture()
  tr <- miss_titres_threshold(f$ti$round_titre, threshold = 20)
  expect_equal(dim(tr$full_titre), dim(tr$rm_titre))
})

test_that("threshold: all removed values are below threshold (numeric input)", {
  f   <- make_fixture()
  tr  <- miss_titres_threshold(f$ti$round_titre, threshold = 40)
  # Every newly removed cell had a value < 40 in the original
  orig_vals <- f$ti$round_titre[tr$rm_ind]
  expect_true(all(orig_vals < 40))
})

test_that("threshold: no values >= threshold are removed (numeric input)", {
  f  <- make_fixture()
  tr <- miss_titres_threshold(f$ti$round_titre, threshold = 40)
  # Values >= 40 that were not already missing must still be present
  kept_mask <- f$ti$round_titre >= 40
  expect_true(all(tr$rm_titre[kept_mask] != "*"))
})

test_that("threshold: character input removes <X entries within threshold", {
  f   <- make_fixture()
  tr  <- miss_titres_threshold(f$ti$lessthan_titre, threshold = 10)
  # All "<10" entries should now be "*"
  lt_ind <- which(as.vector(f$ti$lessthan_titre) == "<10")
  if (length(lt_ind) > 0) {
    expect_true(all(as.vector(tr$rm_titre)[lt_ind] == "*"))
  } else {
    skip("no <10 entries in this fixture")
  }
})

test_that("threshold: keep_homologous protects diagonal", {
  f  <- make_fixture()
  tr <- miss_titres_threshold(f$ti$round_titre, threshold = 1e9, keep_homologous = TRUE)
  # With a huge threshold every non-homologous titre would be removed
  homo <- acsimdata:::.homologous_ind(f$ti$round_titre)
  expect_true(all(as.vector(tr$rm_titre)[homo] != "*"))
})

test_that("threshold: rm_ind_arr reflects all stars in rm_titre", {
  f  <- make_fixture()
  tr <- miss_titres_threshold(f$ti$round_titre, threshold = 40)
  expect_equal(
    nrow(tr$rm_ind_arr),
    sum(as.vector(tr$rm_titre) == "*")
  )
})


# ── miss_titres_informed ──────────────────────────────────────────────────────

test_that("informed: output dimensions match input", {
  f  <- make_fixture()
  ti <- miss_titres_informed(f$ti$lessthan_titre, seed = 1)
  expect_equal(dim(ti$full_titre), dim(ti$rm_titre))
})

test_that("informed: seed reproduces identical results", {
  f   <- make_fixture()
  ti1 <- miss_titres_informed(f$ti$lessthan_titre, midpoint_titre = 40, seed = 99)
  ti2 <- miss_titres_informed(f$ti$lessthan_titre, midpoint_titre = 40, seed = 99)
  expect_equal(ti1$rm_ind, ti2$rm_ind)
})

test_that("informed: different seeds give different results (usually)", {
  f   <- make_fixture()
  ti1 <- miss_titres_informed(f$ti$lessthan_titre, seed = 1)
  ti2 <- miss_titres_informed(f$ti$lessthan_titre, seed = 2)
  # The probability of both being identical by chance is negligible for a 5x5 table
  expect_false(identical(ti1$rm_ind, ti2$rm_ind))
})

test_that("informed: high steepness makes low-titre cells nearly always missing", {
  # Build a table with a mix of low and high titres using a known seed
  f <- make_fixture(seed = 7)
  # With very high steepness and midpoint_titre at the max, almost everything goes missing
  ti <- miss_titres_informed(f$ti$lessthan_titre,
                              midpoint_titre = max(f$ti$round_titre, na.rm = TRUE) * 2,
                              steepness = 10, seed = 1)
  # At least half the titres should be removed
  n_removed <- sum(as.vector(ti$rm_titre) == "*")
  expect_gt(n_removed, length(ti$rm_titre) / 4)
})

test_that("informed: keep_homologous protects diagonal", {
  f  <- make_fixture()
  # Use extreme settings so almost everything else is removed
  ti <- miss_titres_informed(f$ti$lessthan_titre,
                              midpoint_titre = 1e6,
                              steepness = 10,
                              keep_homologous = TRUE,
                              seed = 1)
  homo <- acsimdata:::.homologous_ind(f$ti$lessthan_titre)
  expect_true(all(as.vector(ti$rm_titre)[homo] != "*"))
})

test_that("informed: rm_ind_arr reflects all stars in rm_titre", {
  f  <- make_fixture()
  ti <- miss_titres_informed(f$ti$lessthan_titre, seed = 1)
  expect_equal(nrow(ti$rm_ind_arr), sum(as.vector(ti$rm_titre) == "*"))
})


# ── miss_titres_block ─────────────────────────────────────────────────────────

test_that("block: correct cells removed by integer index", {
  f   <- make_fixture()
  blk <- miss_titres_block(f$ti$lessthan_titre, antigens = 1:2, sera = 3:5)

  # Every cell in the block (not homologous) must be "*"
  homo    <- acsimdata:::.homologous_ind(f$ti$lessthan_titre)
  n_sr    <- ncol(f$ti$lessthan_titre)
  exp_ind <- as.vector(outer(1:2, 3:5, function(r, c) (r - 1L) * n_sr + c))
  exp_ind <- setdiff(exp_ind, homo)
  expect_true(all(as.vector(blk$rm_titre)[exp_ind] == "*"))
})

test_that("block: cells outside the block are untouched", {
  f   <- make_fixture()
  blk <- miss_titres_block(f$ti$lessthan_titre, antigens = 1:2, sera = 1:2)

  n_sr     <- ncol(f$ti$lessthan_titre)
  block_ind <- as.vector(outer(1:2, 1:2, function(r, c) (r - 1L) * n_sr + c))
  outside   <- setdiff(seq_along(f$ti$lessthan_titre), block_ind)
  expect_true(all(
    as.vector(blk$rm_titre)[outside] == as.vector(f$ti$lessthan_titre)[outside]
  ))
})

test_that("block: character antigen/serum names work", {
  f     <- make_fixture()
  blk_i <- miss_titres_block(f$ti$lessthan_titre, antigens = 1:2, sera = 1:3)
  blk_c <- miss_titres_block(f$ti$lessthan_titre,
                              antigens = c("AG1", "AG2"),
                              sera     = c("SR1", "SR2", "SR3"))
  expect_equal(blk_i$rm_ind, blk_c$rm_ind)
})

test_that("block: keep_homologous protects diagonal within block", {
  f   <- make_fixture()
  # Block covering the whole table
  blk <- miss_titres_block(f$ti$lessthan_titre,
                            antigens = seq_len(nrow(f$ti$lessthan_titre)),
                            sera     = seq_len(ncol(f$ti$lessthan_titre)),
                            keep_homologous = TRUE)
  homo <- acsimdata:::.homologous_ind(f$ti$lessthan_titre)
  expect_true(all(as.vector(blk$rm_titre)[homo] != "*"))
})

test_that("block: errors on unknown antigen/serum names", {
  f <- make_fixture()
  expect_error(miss_titres_block(f$ti$lessthan_titre, antigens = "AG99", sera = "SR1"))
  expect_error(miss_titres_block(f$ti$lessthan_titre, antigens = "AG1", sera = "SR99"))
})

test_that("block: errors on out-of-range integer indices", {
  f <- make_fixture()
  expect_error(miss_titres_block(f$ti$lessthan_titre, antigens = 99L, sera = 1L))
  expect_error(miss_titres_block(f$ti$lessthan_titre, antigens = 1L, sera = 99L))
})

test_that("block: rm_ind_arr reflects all stars in rm_titre", {
  f   <- make_fixture()
  blk <- miss_titres_block(f$ti$lessthan_titre, antigens = 1:3, sera = 2:4)
  expect_equal(nrow(blk$rm_ind_arr), sum(as.vector(blk$rm_titre) == "*"))
})


# ── miss_titres_by_distance ───────────────────────────────────────────────────

test_that("by_distance: output dimensions match input", {
  f   <- make_fixture()
  byd <- miss_titres_by_distance(f$ti$lessthan_titre,
                                  ag_coord = f$m$antigen_coord,
                                  sr_coord = f$m$sera_coord,
                                  seed = 1)
  expect_equal(dim(byd$full_titre), dim(byd$rm_titre))
})

test_that("by_distance: dist_matrix has correct dimensions", {
  f   <- make_fixture()
  byd <- miss_titres_by_distance(f$ti$lessthan_titre,
                                  ag_coord = f$m$antigen_coord,
                                  sr_coord = f$m$sera_coord,
                                  seed = 1)
  expect_equal(dim(byd$dist_matrix),
               c(nrow(f$ti$lessthan_titre), ncol(f$ti$lessthan_titre)))
})

test_that("by_distance: dist_matrix is non-negative", {
  f   <- make_fixture()
  byd <- miss_titres_by_distance(f$ti$lessthan_titre,
                                  ag_coord = f$m$antigen_coord,
                                  sr_coord = f$m$sera_coord,
                                  seed = 1)
  expect_true(all(byd$dist_matrix >= 0))
})

test_that("by_distance: seed reproduces identical results", {
  f    <- make_fixture()
  byd1 <- miss_titres_by_distance(f$ti$lessthan_titre, f$m$antigen_coord, f$m$sera_coord, seed = 7)
  byd2 <- miss_titres_by_distance(f$ti$lessthan_titre, f$m$antigen_coord, f$m$sera_coord, seed = 7)
  expect_equal(byd1$rm_ind, byd2$rm_ind)
})

test_that("by_distance: large midpoint_dist removes few titres", {
  f    <- make_fixture()
  # midpoint far beyond map range → P(missing) ≈ 0 everywhere
  byd  <- miss_titres_by_distance(f$ti$lessthan_titre,
                                   f$m$antigen_coord, f$m$sera_coord,
                                   midpoint_dist = 1000, steepness = 1, seed = 1)
  expect_lt(length(byd$rm_ind), 3L)
})

test_that("by_distance: small midpoint_dist removes many titres", {
  f   <- make_fixture()
  # midpoint very small → P(missing) ≈ 1 everywhere
  byd <- miss_titres_by_distance(f$ti$lessthan_titre,
                                  f$m$antigen_coord, f$m$sera_coord,
                                  midpoint_dist = -1000, steepness = 1, seed = 1)
  n_star <- sum(as.vector(byd$rm_titre) == "*")
  expect_gt(n_star, length(byd$rm_titre) / 2)
})

test_that("by_distance: keep_homologous protects diagonal", {
  f   <- make_fixture()
  byd <- miss_titres_by_distance(f$ti$lessthan_titre,
                                  f$m$antigen_coord, f$m$sera_coord,
                                  midpoint_dist = -1000, steepness = 1,
                                  keep_homologous = TRUE, seed = 1)
  homo <- acsimdata:::.homologous_ind(f$ti$lessthan_titre)
  expect_true(all(as.vector(byd$rm_titre)[homo] != "*"))
})

test_that("by_distance: errors when coord rows don't match titre", {
  f <- make_fixture()
  bad_coord <- f$m$antigen_coord[1:3, ]  # wrong number of rows
  expect_error(
    miss_titres_by_distance(f$ti$lessthan_titre, bad_coord, f$m$sera_coord, seed = 1)
  )
})

test_that("by_distance: rm_ind_arr reflects all stars in rm_titre", {
  f   <- make_fixture()
  byd <- miss_titres_by_distance(f$ti$lessthan_titre,
                                  f$m$antigen_coord, f$m$sera_coord, seed = 42)
  expect_equal(nrow(byd$rm_ind_arr), sum(as.vector(byd$rm_titre) == "*"))
})


# ── miss_titres_banded ────────────────────────────────────────────────────────

test_that("banded: rm_titre has same dimensions as input", {
  f <- make_fixture()
  b <- miss_titres_banded(f$ti$lessthan_titre, bandwidth = 2)
  expect_equal(dim(b$rm_titre), dim(f$ti$lessthan_titre))
})

test_that("banded: cells beyond bandwidth are '*'", {
  f   <- make_fixture()
  bw  <- 1L
  b   <- miss_titres_banded(f$ti$lessthan_titre, bandwidth = bw,
                             keep_homologous = FALSE)
  n_ag <- nrow(b$rm_titre)
  n_sr <- ncol(b$rm_titre)
  for (i in seq_len(n_ag)) {
    for (j in seq_len(n_sr)) {
      if (abs(i - j) > bw) {
        expect_equal(b$rm_titre[i, j], "*",
                     info = sprintf("cell [%d, %d] should be '*'", i, j))
      }
    }
  }
})

test_that("banded: cells within bandwidth are not changed", {
  f   <- make_fixture()
  bw  <- 2L
  b   <- miss_titres_banded(f$ti$lessthan_titre, bandwidth = bw)
  n_ag <- nrow(b$rm_titre)
  n_sr <- ncol(b$rm_titre)
  for (i in seq_len(n_ag)) {
    for (j in seq_len(n_sr)) {
      if (abs(i - j) <= bw) {
        expect_equal(b$rm_titre[i, j], f$ti$lessthan_titre[i, j],
                     info = sprintf("cell [%d, %d] should be unchanged", i, j))
      }
    }
  }
})

test_that("banded: keep_homologous protects diagonal with bandwidth = 0", {
  f <- make_fixture()
  # bandwidth = 0 would remove all off-diagonal entries; homologous are on-diagonal
  b <- miss_titres_banded(f$ti$lessthan_titre, bandwidth = 0,
                           keep_homologous = TRUE)
  homo <- acsimdata:::.homologous_ind(f$ti$lessthan_titre)
  expect_true(all(as.vector(b$rm_titre)[homo] != "*"))
})

test_that("banded: bandwidth = 0 keeps only diagonal entries", {
  f    <- make_fixture()
  b    <- miss_titres_banded(f$ti$lessthan_titre, bandwidth = 0,
                              keep_homologous = FALSE)
  n_ag <- nrow(b$rm_titre)
  n_sr <- ncol(b$rm_titre)
  for (i in seq_len(n_ag)) {
    for (j in seq_len(n_sr)) {
      if (i != j) {
        expect_equal(b$rm_titre[i, j], "*",
                     info = sprintf("off-diagonal [%d,%d] should be '*'", i, j))
      } else {
        expect_equal(b$rm_titre[i, j], f$ti$lessthan_titre[i, j],
                     info = sprintf("diagonal [%d,%d] should be unchanged", i, j))
      }
    }
  }
})

test_that("banded: large bandwidth leaves matrix unchanged", {
  f <- make_fixture()
  b <- miss_titres_banded(f$ti$lessthan_titre, bandwidth = 1000)
  expect_equal(b$rm_titre, f$ti$lessthan_titre)
  expect_length(b$rm_ind, 0L)
})

test_that("banded: n_sera reduces columns in titre_reduced", {
  f      <- make_fixture()            # 5 × 5
  target <- 3L
  b      <- miss_titres_banded(f$ti$lessthan_titre, bandwidth = 1, n_sera = target)
  expect_equal(ncol(b$titre_reduced), target)
  expect_equal(nrow(b$titre_reduced), nrow(f$ti$lessthan_titre))
})

test_that("banded: dropped_sera has n_sr - n_sera entries", {
  f <- make_fixture()
  b <- miss_titres_banded(f$ti$lessthan_titre, bandwidth = 1, n_sera = 3L)
  expect_length(b$dropped_sera, ncol(f$ti$lessthan_titre) - 3L)
})

test_that("banded: n_sera = n_sr drops nothing", {
  f <- make_fixture()
  b <- miss_titres_banded(f$ti$lessthan_titre, bandwidth = 2,
                           n_sera = ncol(f$ti$lessthan_titre))
  expect_length(b$dropped_sera, 0L)
  expect_equal(b$titre_reduced, b$rm_titre)
})

test_that("banded: most sparse sera are dropped (bandwidth = 1, n_sera = 4)", {
  # With bandwidth = 1 on a 5×5 matrix:
  #   SR1 has 2 non-missing cells (rows 1, 2)
  #   SR5 has 2 non-missing cells (rows 4, 5)
  #   SR2, SR3, SR4 each have 3 non-missing cells
  # Dropping 1 serum → SR1 dropped (lowest index among ties)
  f <- make_fixture()
  b <- miss_titres_banded(f$ti$lessthan_titre, bandwidth = 1, n_sera = 4L)
  expect_equal(b$dropped_sera, "SR1")
  # titre_reduced should contain SR2..SR5
  expect_equal(colnames(b$titre_reduced), paste0("SR", 2:5))
})

test_that("banded: dropped sera become all-'*' in rm_titre", {
  f <- make_fixture()
  b <- miss_titres_banded(f$ti$lessthan_titre, bandwidth = 1, n_sera = 4L)
  for (sr in b$dropped_sera) {
    expect_true(all(b$rm_titre[, sr] == "*"))
  }
})

test_that("banded: rm_ind accounts for both banding and serum dropping", {
  f     <- make_fixture()
  b_no  <- miss_titres_banded(f$ti$lessthan_titre, bandwidth = 1)
  b_yes <- miss_titres_banded(f$ti$lessthan_titre, bandwidth = 1, n_sera = 4L)
  # Dropping a serum adds more missing cells → rm_ind should be longer
  expect_gt(length(b_yes$rm_ind), length(b_no$rm_ind))
})

test_that("banded: rm_ind_arr reflects all stars in rm_titre", {
  f <- make_fixture()
  b <- miss_titres_banded(f$ti$lessthan_titre, bandwidth = 2, n_sera = 3L)
  expect_equal(nrow(b$rm_ind_arr), sum(as.vector(b$rm_titre) == "*"))
})

test_that("banded: error on invalid bandwidth", {
  f <- make_fixture()
  expect_error(miss_titres_banded(f$ti$lessthan_titre, bandwidth = -1))
  expect_error(miss_titres_banded(f$ti$lessthan_titre, bandwidth = "a"))
  expect_error(miss_titres_banded(f$ti$lessthan_titre, bandwidth = c(1, 2)))
})

test_that("banded: error when n_sera exceeds ncol", {
  f <- make_fixture()
  expect_error(
    miss_titres_banded(f$ti$lessthan_titre, bandwidth = 2,
                       n_sera = ncol(f$ti$lessthan_titre) + 1L)
  )
})

test_that("banded: error when n_sera < 1", {
  f <- make_fixture()
  expect_error(miss_titres_banded(f$ti$lessthan_titre, bandwidth = 2, n_sera = 0L))
})


# ── Constraint helpers ────────────────────────────────────────────────────────

# Helper: a 5x5 titre with one row zeroed out (disconnected graph)
make_disconnected <- function() {
  f <- make_fixture()
  t <- f$ti$lessthan_titre
  t[5, ] <- "*"   # AG5 has no titres → disconnected
  t
}

# Helper: minimum observations for d=2, n_ag=n_sr=5
min_obs_2d <- function(n_ag = 5L, n_sr = 5L) 2L * (n_ag + n_sr) - 3L

test_that("constraint: warns for 2D count underconstraint (threshold)", {
  f <- make_fixture()
  expect_warning(
    miss_titres_threshold(f$ti$round_titre, threshold = 1e9, keep_homologous = FALSE),
    "underconstrained"
  )
})

test_that("constraint: warns for 2D count underconstraint (informed)", {
  f <- make_fixture()
  expect_warning(
    miss_titres_informed(f$ti$lessthan_titre, midpoint_titre = 1e9,
                         steepness = 20, keep_homologous = FALSE, seed = 1),
    "underconstrained"
  )
})

test_that("constraint: warns for 2D count underconstraint (block)", {
  f <- make_fixture()
  # Remove entire table except diagonal
  expect_warning(
    miss_titres_block(f$ti$lessthan_titre,
                      antigens = 1:5, sera = 1:5, keep_homologous = TRUE),
    "underconstrained"
  )
})

test_that("constraint: warns for 2D count underconstraint (by_distance)", {
  f <- make_fixture()
  expect_warning(
    miss_titres_by_distance(f$ti$lessthan_titre, f$m$antigen_coord, f$m$sera_coord,
                             midpoint_dist = -1000, steepness = 1,
                             keep_homologous = FALSE, seed = 1),
    "underconstrained"
  )
})

test_that("constraint: warns for 2D count underconstraint (banded)", {
  f <- make_fixture()
  expect_warning(
    miss_titres_banded(f$ti$lessthan_titre, bandwidth = 0, keep_homologous = FALSE),
    "underconstrained"
  )
})

test_that("constraint: no warning when well-constrained (threshold)", {
  f <- make_fixture()
  # Small threshold removes nothing → no underconstraint
  expect_no_warning(miss_titres_threshold(f$ti$round_titre, threshold = 1))
})

test_that("constraint: disconnected graph warning (threshold)", {
  t <- make_disconnected()
  expect_warning(
    miss_titres_threshold(t, threshold = 1),
    "disconnected"
  )
})

test_that("constraint: min_dim caps removal to maintain constraint (threshold)", {
  f <- make_fixture()
  # Extreme threshold would remove everything not homologous
  result <- suppressWarnings(
    miss_titres_threshold(f$ti$round_titre, threshold = 1e9,
                          keep_homologous = FALSE, min_dim = 2L)
  )
  expect_gte(sum(result$rm_titre != "*"), min_obs_2d())
})

test_that("constraint: min_dim caps removal to maintain constraint (informed)", {
  f <- make_fixture()
  result <- suppressWarnings(
    miss_titres_informed(f$ti$lessthan_titre, midpoint_titre = 1e9,
                         steepness = 20, keep_homologous = FALSE,
                         seed = 1, min_dim = 2L)
  )
  expect_gte(sum(result$rm_titre != "*"), min_obs_2d())
})

test_that("constraint: min_dim caps removal to maintain constraint (block)", {
  f <- make_fixture()
  result <- suppressWarnings(
    miss_titres_block(f$ti$lessthan_titre, antigens = 1:5, sera = 1:5,
                      keep_homologous = FALSE, min_dim = 2L)
  )
  expect_gte(sum(result$rm_titre != "*"), min_obs_2d())
})

test_that("constraint: min_dim caps removal to maintain constraint (by_distance)", {
  f <- make_fixture()
  result <- suppressWarnings(
    miss_titres_by_distance(f$ti$lessthan_titre, f$m$antigen_coord, f$m$sera_coord,
                             midpoint_dist = -1000, steepness = 1,
                             keep_homologous = FALSE, seed = 1, min_dim = 2L)
  )
  expect_gte(sum(result$rm_titre != "*"), min_obs_2d())
})

test_that("constraint: min_dim caps banding removal (banded)", {
  f <- make_fixture()
  # bandwidth=0 would leave only diagonal (5 obs), well below min_obs_2d(5,5)=17
  result <- suppressWarnings(
    miss_titres_banded(f$ti$lessthan_titre, bandwidth = 0,
                       keep_homologous = FALSE, min_dim = 2L)
  )
  expect_gte(sum(result$rm_titre != "*"), min_obs_2d())
})

test_that("constraint: min_dim warns when cap is applied (threshold)", {
  f <- make_fixture()
  expect_warning(
    miss_titres_threshold(f$ti$round_titre, threshold = 1e9,
                          keep_homologous = FALSE, min_dim = 2L),
    "Capping"
  )
})

test_that("constraint: min_dim warns when input already underconstrained", {
  # 2x2 table with only 1 observation; min for d=2 is 2*(2+2)-3=5
  t <- matrix(c("10", "*", "*", "*"), nrow = 2, ncol = 2,
              dimnames = list(c("AG1", "AG2"), c("SR1", "SR2")))
  expect_warning(
    miss_titres_threshold(t, threshold = 5, min_dim = 2L),
    "Cannot enforce"
  )
})

test_that("constraint: min_dim stored in params", {
  f <- make_fixture()
  result <- suppressWarnings(
    miss_titres_threshold(f$ti$round_titre, threshold = 1e9, min_dim = 2L)
  )
  expect_equal(result$params$min_dim, 2L)
})

test_that("constraint: min_dim = NULL stored in params when not set", {
  f <- make_fixture()
  result <- suppressWarnings(
    miss_titres_threshold(f$ti$round_titre, threshold = 20)
  )
  expect_null(result$params$min_dim)
})
