# Test setup: 3 clusters, reference AGs in rows 1-3, test AGs in rows 4-15.
# n_blocks=2, n_ref_per_block=2, n_test_ag_per_block=6, ref_step=1
#   block 1 active pairs: 1:2
#   block 2 active pairs: 2:3
#   ref_ag_active_sera: k1→1:2, k2→1:3, k3→2:3
#   block_test_ag_rows: block1=4:9, block2=10:15

centres <- matrix(c(0, 0,  3, 0,  1.5, 3*sqrt(3)/2), ncol = 2, byrow = TRUE)
true_ag  <- rbind(centres,                        # rows 1-3:  reference AGs
                  centres[rep(1:3, each = 4), ])  # rows 4-15: test AGs

small <- sim_cluster_surv(
  n_ref_pairs         = 3L,
  n_blocks            = 2L,
  n_test_ag_per_block = 6L,
  true_ag_coord       = true_ag,
  range               = 0.25,
  n_ref_per_block     = 2L,
  ref_step            = 1L,
  seed                = 1
)

# --- Structure ---

test_that("returns four named titre tables", {
  expect_equal(length(small$titre_tables), 4)
  expect_setequal(names(small$titre_tables),
                  c("hiA_agA", "hiA_agB", "hiB_agA", "hiB_agB"))
})

test_that("titre table dimensions are n_antigens x n_ref_pairs", {
  expect_equal(dim(small$titre_tables$hiA_agA), c(15L, 3L))
})

test_that("all four tables have identical missing pattern", {
  miss <- lapply(small$titre_tables, function(tt) tt == "*")
  expect_identical(miss[[1]], miss[[2]])
  expect_identical(miss[[1]], miss[[3]])
  expect_identical(miss[[1]], miss[[4]])
})

test_that("ag_coord and sr_coord returned with correct dimensions", {
  expect_equal(dim(small$ag_coord), c(15L, 2L))
  expect_equal(dim(small$sr_coord), c(3L, 2L))
})

test_that("slim_dist returned with correct dimensions", {
  expect_equal(dim(small$slim_dist), c(15L, 3L))
})

# --- coincident default: rows 1:n_ref_pairs are reference AGs ---

test_that("params$coincident defaults to seq_len(n_ref_pairs)", {
  expect_equal(small$params$coincident, 1:3)
})

test_that("homologous distances are zero (ref AGs = sera positions)", {
  # Default coincident = 1:3 means AG1=SR1, AG2=SR2, AG3=SR3
  expect_equal(small$slim_dist[1, 1], 0)
  expect_equal(small$slim_dist[2, 2], 0)
  expect_equal(small$slim_dist[3, 3], 0)
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
  r1 <- sim_cluster_surv(
    n_ref_pairs = 3L, n_blocks = 2L, n_test_ag_per_block = 6L,
    true_ag_coord = true_ag, range = 0.25, n_ref_per_block = 2L,
    ref_step = 1L, seed = 42
  )
  r2 <- sim_cluster_surv(
    n_ref_pairs = 3L, n_blocks = 2L, n_test_ag_per_block = 6L,
    true_ag_coord = true_ag, range = 0.25, n_ref_per_block = 2L,
    ref_step = 1L, seed = 42
  )
  expect_identical(r1$titre_tables, r2$titre_tables)
})

test_that("different seeds produce different titre values", {
  r1 <- sim_cluster_surv(
    n_ref_pairs = 3L, n_blocks = 2L, n_test_ag_per_block = 6L,
    true_ag_coord = true_ag, range = 0.25, n_ref_per_block = 2L,
    ref_step = 1L, seed = 1
  )
  r2 <- sim_cluster_surv(
    n_ref_pairs = 3L, n_blocks = 2L, n_test_ag_per_block = 6L,
    true_ag_coord = true_ag, range = 0.25, n_ref_per_block = 2L,
    ref_step = 1L, seed = 2
  )
  expect_false(identical(r1$titre_tables$hiA_agA, r2$titre_tables$hiA_agA))
})

# --- Input validation ---


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
  r <- sim_cluster_surv(
    n_ref_pairs         = 3L,
    n_blocks            = 2L,
    n_test_ag_per_block = 6L,
    true_ag_coord       = true_ag,
    range               = 0.25,
    n_ref_per_block     = 2L,
    seed                = 1
  )
  expect_equal(dim(r$titre_tables$hiA_agA), c(15L, 3L))
})

test_that("works with 3-cluster full-scale structure (1884 x 48)", {
  skip_on_cran()
  true_ag_large <- rbind(
    centres[rep(1:3, each = 16), ],   # rows 1-48:   reference AGs
    centres[rep(1:3, each = 612), ]   # rows 49-1884: test AGs
  )
  r <- sim_cluster_surv(
    n_ref_pairs         = 48L,
    n_blocks            = 18L,
    n_test_ag_per_block = 102L,
    true_ag_coord       = true_ag_large,
    range               = 0.25,
    n_ref_per_block     = 12L,
    seed                = 1
  )
  expect_equal(dim(r$titre_tables$hiA_agA), c(1884L, 48L))
})
