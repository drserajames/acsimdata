# Test setup: 18 AG x 6 SR distance matrix with known structure
# sera_idx = c(1,2, 7,8, 13,14) — reference pairs 1-6 (2 per cluster of 6)
# test AGs: rows c(3,4,5,6, 9,10,11,12, 15,16,17,18)
# ref_window=4, ref_step=2, n_blocks=2:
#   block 1 active: 1:4; block 2 active: 3:6
# block_test_ag_rows (sequential):
#   block 1: c(3,4,5,6,9,10); block 2: c(11,12,15,16,17,18)
# ref_ag_active_sera:
#   k=1 (row 1): sera 1:4; k=2 (row 2): 1:4
#   k=3 (row 7): 1:6; k=4 (row 8): 1:6
#   k=5 (row 13): 3:6; k=6 (row 14): 3:6

set.seed(99)
slim <- matrix(
  abs(stats::rnorm(18 * 6)),
  nrow = 18, ncol = 6,
  dimnames = list(paste0("AG", 1:18), paste0("SR", 1:6))
)
si <- c(1L, 2L, 7L, 8L, 13L, 14L)

small <- sim_cluster_surv(slim, si, n_blocks = 2,
                           ref_window = 4, ref_step = 2, seed = 1)

# --- Structure ---

test_that("returns four named titre tables", {
  expect_equal(length(small$titre_tables), 4)
  expect_setequal(names(small$titre_tables),
                  c("hiA_agA", "hiA_agB", "hiB_agA", "hiB_agB"))
})

test_that("titre table dimensions match slim_dist", {
  expect_equal(dim(small$titre_tables$hiA_agA), c(18L, 6L))
})

test_that("all four tables have identical missing pattern", {
  miss <- lapply(small$titre_tables, function(tt) tt == "*")
  expect_identical(miss[[1]], miss[[2]])
  expect_identical(miss[[1]], miss[[3]])
  expect_identical(miss[[1]], miss[[4]])
})

# --- Missing pattern: test antigens ---

test_that("block-1 test AGs measured only in active cols 1:4", {
  tt <- small$titre_tables$hiA_agA
  b1_rows <- c(3, 4, 5, 6, 9, 10)
  expect_true(all(tt[b1_rows, 1:4] != "*"))
  expect_true(all(tt[b1_rows, 5:6] == "*"))
})

test_that("block-2 test AGs measured only in active cols 3:6", {
  tt <- small$titre_tables$hiA_agA
  b2_rows <- c(11, 12, 15, 16, 17, 18)
  expect_true(all(tt[b2_rows, 3:6] != "*"))
  expect_true(all(tt[b2_rows, 1:2] == "*"))
})

# --- Missing pattern: reference antigens ---

test_that("ref AG pair 1 (row 1) measured in cols 1:4 only", {
  tt <- small$titre_tables$hiA_agA
  expect_true(all(tt[1, 1:4] != "*"))
  expect_true(all(tt[1, 5:6] == "*"))
})

test_that("ref AG pair 5 (row 13) measured in cols 3:6 only", {
  tt <- small$titre_tables$hiA_agA
  expect_true(all(tt[13, 3:6] != "*"))
  expect_true(all(tt[13, 1:2] == "*"))
})

test_that("ref AG pair 3 (row 7) measured in all 6 cols", {
  tt <- small$titre_tables$hiA_agA
  expect_true(all(tt[7, ] != "*"))
})

# --- Reproducibility ---

test_that("same seed gives identical result", {
  r1 <- sim_cluster_surv(slim, si, n_blocks = 2,
                          ref_window = 4, ref_step = 2, seed = 42)
  r2 <- sim_cluster_surv(slim, si, n_blocks = 2,
                          ref_window = 4, ref_step = 2, seed = 42)
  expect_identical(r1$titre_tables, r2$titre_tables)
})

test_that("different seeds produce different titre values", {
  r1 <- sim_cluster_surv(slim, si, n_blocks = 2,
                          ref_window = 4, ref_step = 2, seed = 1)
  r2 <- sim_cluster_surv(slim, si, n_blocks = 2,
                          ref_window = 4, ref_step = 2, seed = 2)
  expect_false(identical(r1$titre_tables$hiA_agA, r2$titre_tables$hiA_agA))
})

# --- Input validation ---

test_that("error when sera_idx length != ncol(slim_dist)", {
  expect_error(
    sim_cluster_surv(slim, c(1L, 2L), n_blocks = 2, seed = 1),
    "ncol"
  )
})

test_that("error when sera_idx out of row range", {
  expect_error(
    sim_cluster_surv(slim, c(1L, 2L, 7L, 8L, 13L, 99L), n_blocks = 2, seed = 1),
    "nrow"
  )
})

test_that("error when test AGs not divisible by n_blocks", {
  expect_error(
    sim_cluster_surv(slim, si, n_blocks = 5, seed = 1),
    "divisible"
  )
})

# --- Block assignment ---

test_that("block_active has correct indices", {
  expect_equal(small$block_active[[1]], 1:4)
  expect_equal(small$block_active[[2]], 3:6)
})

test_that("block_test_ag_rows assigns test AGs sequentially", {
  expect_equal(sort(unlist(small$block_test_ag_rows)),
               sort(setdiff(1:18, si)))
  expect_equal(small$block_test_ag_rows[[1]], c(3L, 4L, 5L, 6L, 9L, 10L))
  expect_equal(small$block_test_ag_rows[[2]], c(11L, 12L, 15L, 16L, 17L, 18L))
})

test_that("custom block_assignments are used when supplied", {
  custom <- list(c(3L, 4L, 9L, 10L, 15L, 16L),
                 c(5L, 6L, 11L, 12L, 17L, 18L))
  rc <- sim_cluster_surv(slim, si, n_blocks = 2, block_assignments = custom,
                          ref_window = 4, ref_step = 2, seed = 1)
  expect_equal(rc$block_test_ag_rows, custom)
})

# --- Noise dimensions ---

test_that("noise components have correct dimensions", {
  expect_equal(dim(small$noise$hi_A), c(18L, 6L))
  expect_equal(dim(small$noise$hi_B), c(18L, 6L))
  expect_equal(length(small$noise$ag_A), 18L)
  expect_equal(length(small$noise$serum_noise), 6L)
})

# --- Integration: 3-cluster equilateral triangle map ---

test_that("works with 3-cluster map_maker_coord output (1884 x 48)", {
  skip_on_cran()
  centres  <- matrix(c(0, 0, 3, 0, 1.5, 3 * sqrt(3) / 2), ncol = 2, byrow = TRUE)
  true_ag  <- centres[rep(1:3, each = 628), ]
  sidx     <- c(1:16, 629:644, 1257:1272)
  m        <- map_maker_coord(1884L, 48L, true_ag, range = 0.25,
                              coincident = sidx, seed = 1)
  r        <- sim_cluster_surv(m$slim_dist, sidx, n_blocks = 18L, seed = 1)
  expect_equal(dim(r$titre_tables$hiA_agA), c(1884L, 48L))
})
