# Test setup: reference AGs in rows 1-2, test AGs in rows 3-14 (6 per cluster of 5)
# sera_idx defaults to 1:2 (first n_sr rows)
# n_blocks=2, n_ref_per_block=2, n_test_ag_per_block=6, ref_step=2
#   block 1 active: 1:2; block 2 active: ... min(2, 3+2-1)=2? No:
#   start(1)=1, end(1)=min(2,1+2-1)=min(2,2)=2 → 1:2
#   start(2)=3, end(2)=min(2,3+2-1)=min(2,4)=2 → seq(3,2) = integer(0)!
# Use ref_step=1 to get meaningful overlap: block1=1:2, block2=2:3 (with 3 sera)
# Use n_sr=3 for meaningful test:
#   sera_idx = 1:3, test_ag_rows = 4:15 (12 test AGs), n_blocks=2, n_test=6
#   block 1: pairs 1:2, block 2: pairs 2:3
#   ref_ag_active_sera: k1→1:2, k2→1:3, k3→2:3
#   block_test_ag_rows: block1=4:9, block2=10:15

set.seed(99)
slim <- matrix(
  abs(stats::rnorm(15 * 3)),
  nrow = 15, ncol = 3,
  dimnames = list(paste0("AG", 1:15), paste0("SR", 1:3))
)

small <- sim_cluster_surv(slim,
                           n_blocks            = 2L,
                           n_ref_per_block     = 2L,
                           n_test_ag_per_block = 6L,
                           ref_step            = 1L,
                           seed                = 1)

# --- Structure ---

test_that("returns four named titre tables", {
  expect_equal(length(small$titre_tables), 4)
  expect_setequal(names(small$titre_tables),
                  c("hiA_agA", "hiA_agB", "hiB_agA", "hiB_agB"))
})

test_that("titre table dimensions match slim_dist", {
  expect_equal(dim(small$titre_tables$hiA_agA), c(15L, 3L))
})

test_that("all four tables have identical missing pattern", {
  miss <- lapply(small$titre_tables, function(tt) tt == "*")
  expect_identical(miss[[1]], miss[[2]])
  expect_identical(miss[[1]], miss[[3]])
  expect_identical(miss[[1]], miss[[4]])
})

# --- Default sera_idx = 1:ncol(slim_dist) ---

test_that("sera_idx defaults to first n_sr rows", {
  expect_equal(small$params$sera_idx, 1:3)
})

# --- Missing pattern: test antigens ---
# block 1 (rows 4:9): cols 1:2 measured; col 3 missing
# block 2 (rows 10:15): cols 2:3 measured; col 1 missing

test_that("block-1 test AGs measured only in active cols 1:2", {
  tt <- small$titre_tables$hiA_agA
  expect_true(all(tt[4:9, 1:2] != "*"))
  expect_true(all(tt[4:9, 3]   == "*"))
})

test_that("block-2 test AGs measured only in active cols 2:3", {
  tt <- small$titre_tables$hiA_agA
  expect_true(all(tt[10:15, 2:3] != "*"))
  expect_true(all(tt[10:15, 1]   == "*"))
})

# --- Missing pattern: reference antigens ---
# pair 1 (row 1): active block 1 → sera 1:2 only
# pair 2 (row 2): active blocks 1 & 2 → sera 1:3
# pair 3 (row 3): active block 2 → sera 2:3 only

test_that("ref AG pair 1 (row 1) measured in cols 1:2 only", {
  tt <- small$titre_tables$hiA_agA
  expect_true(all(tt[1, 1:2] != "*"))
  expect_true(tt[1, 3] == "*")
})

test_that("ref AG pair 2 (row 2) measured in all cols (spans both blocks)", {
  tt <- small$titre_tables$hiA_agA
  expect_true(all(tt[2, ] != "*"))
})

test_that("ref AG pair 3 (row 3) measured in cols 2:3 only", {
  tt <- small$titre_tables$hiA_agA
  expect_true(all(tt[3, 2:3] != "*"))
  expect_true(tt[3, 1] == "*")
})

# --- Reproducibility ---

test_that("same seed gives identical result", {
  r1 <- sim_cluster_surv(slim, n_blocks = 2L, n_ref_per_block = 2L,
                          n_test_ag_per_block = 6L, ref_step = 1L, seed = 42)
  r2 <- sim_cluster_surv(slim, n_blocks = 2L, n_ref_per_block = 2L,
                          n_test_ag_per_block = 6L, ref_step = 1L, seed = 42)
  expect_identical(r1$titre_tables, r2$titre_tables)
})

test_that("different seeds produce different titre values", {
  r1 <- sim_cluster_surv(slim, n_blocks = 2L, n_ref_per_block = 2L,
                          n_test_ag_per_block = 6L, ref_step = 1L, seed = 1)
  r2 <- sim_cluster_surv(slim, n_blocks = 2L, n_ref_per_block = 2L,
                          n_test_ag_per_block = 6L, ref_step = 1L, seed = 2)
  expect_false(identical(r1$titre_tables$hiA_agA, r2$titre_tables$hiA_agA))
})

# --- Input validation ---

test_that("error when n_blocks * n_test_ag_per_block != n_test_ag rows", {
  expect_error(
    sim_cluster_surv(slim, n_blocks = 3L, n_ref_per_block = 2L,
                     n_test_ag_per_block = 6L, seed = 1),
    "n_blocks"
  )
})

test_that("error when sera_idx length != ncol(slim_dist)", {
  expect_error(
    sim_cluster_surv(slim, n_blocks = 2L, n_ref_per_block = 2L,
                     n_test_ag_per_block = 6L, sera_idx = 1:2, seed = 1),
    "ncol"
  )
})

test_that("error when sera_idx out of row range", {
  expect_error(
    sim_cluster_surv(slim, n_blocks = 2L, n_ref_per_block = 2L,
                     n_test_ag_per_block = 6L, sera_idx = c(1L, 2L, 99L), seed = 1),
    "nrow"
  )
})

# --- Block structure ---

test_that("block_active has correct indices with ref_step=1", {
  expect_equal(small$block_active[[1]], 1:2)
  expect_equal(small$block_active[[2]], 2:3)
})

test_that("block_test_ag_rows cover all test AGs without overlap", {
  all_assigned <- sort(unlist(small$block_test_ag_rows))
  expect_equal(all_assigned, 4:15)
})

# --- Noise dimensions ---

test_that("noise components have correct dimensions", {
  expect_equal(dim(small$noise$hi_A), c(15L, 3L))
  expect_equal(dim(small$noise$hi_B), c(15L, 3L))
  expect_equal(length(small$noise$ag_A), 15L)
  expect_equal(length(small$noise$serum_noise), 3L)
})

# --- Integration: example from @examples ---

test_that("example in documentation runs and gives 15 x 3 tables", {
  centres <- matrix(c(0, 0,  3, 0,  1.5, 3*sqrt(3)/2), ncol = 2, byrow = TRUE)
  true_ag  <- rbind(centres, centres[rep(1:3, each = 4), ])
  m <- map_maker_coord(15L, 3L, true_ag, range = 0.25, coincident = 1:3, seed = 1)
  r <- sim_cluster_surv(m$slim_dist, n_blocks = 2L, n_ref_per_block = 2L,
                         n_test_ag_per_block = 6L, seed = 1)
  expect_equal(dim(r$titre_tables$hiA_agA), c(15L, 3L))
})

test_that("works with 3-cluster map_maker_coord output (1884 x 48)", {
  skip_on_cran()
  centres <- matrix(c(0, 0, 3, 0, 1.5, 3*sqrt(3)/2), ncol = 2, byrow = TRUE)
  true_ag  <- rbind(centres[rep(1:3, each = 16), ],
                    centres[rep(1:3, each = 612), ])
  m <- map_maker_coord(1884L, 48L, true_ag, range = 0.25,
                       coincident = 1:48, seed = 1)
  r <- sim_cluster_surv(m$slim_dist, n_blocks = 18L,
                         n_ref_per_block = 12L, n_test_ag_per_block = 102L,
                         seed = 1)
  expect_equal(dim(r$titre_tables$hiA_agA), c(1884L, 48L))
})
