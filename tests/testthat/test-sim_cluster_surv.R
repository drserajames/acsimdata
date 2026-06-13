# Test setup: 3 clusters, 5 AGs per cluster (grouped layout).
# Cluster 1: rows 1-5 (row 1 = reference AG -> SR1)
# Cluster 2: rows 6-10 (row 6 = reference AG -> SR6)
# Cluster 3: rows 11-15 (row 11 = reference AG -> SR11)
#
# n_ref_per_block=2, n_blocks=2, ref_step=1 => n_ref_pairs = 2+(2-1)*1 = 3
# n_antigens = 3 + 2*6 = 15
# coincident = c(1, 6, 11): reference AGs at rows 1, 6, 11
#
# block 1 active serum indices: 1:2 -> SR1, SR6 (cluster 1 and 2 refs)
# block 2 active serum indices: 2:3 -> SR6, SR11 (cluster 2 and 3 refs)
#
# test_ag_rows = c(2,3,4,5,7,8,9,10,12,13,14,15)
# block_test_ag_rows: block1 = c(2,3,4,5,7,8), block2 = c(9,10,12,13,14,15)
#
# ref_ag_active_sera:
#   k=1 (SR1, row 1): block 1 active -> cols SR1, SR6
#   k=2 (SR6, row 6): blocks 1,2   -> cols SR1, SR6, SR11
#   k=3 (SR11, row 11): block 2    -> cols SR6, SR11

centres <- matrix(c(0, 0,  3, 0,  1.5, 3*sqrt(3)/2), ncol = 2, byrow = TRUE)
true_ag  <- centres[rep(1:3, each = 5), ]  # 15 rows: cluster 1 first, then 2, then 3

small <- sim_cluster_surv(
  n_blocks            = 2L,
  n_test_ag_per_block = 6L,
  true_ag_coord       = true_ag,
  range               = 0.25,
  n_ref_per_block     = 2L,
  coincident          = c(1L, 6L, 11L),
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

test_that("serum columns named SR1, SR2, SR3 in coordinate order", {
  expect_equal(colnames(small$titre_tables$hiA_agA), c("SR1", "SR2", "SR3"))
  expect_equal(colnames(small$slim_dist), c("SR1", "SR2", "SR3"))
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

test_that("sr_coord row names are SR1, SR2, SR3 in coordinate order", {
  expect_equal(rownames(small$sr_coord), c("SR1", "SR2", "SR3"))
})

test_that("slim_dist returned with correct dimensions", {
  expect_equal(dim(small$slim_dist), c(15L, 3L))
})

# --- Derived counts in params ---

test_that("params records derived n_ref_pairs and n_antigens", {
  expect_equal(small$params$n_ref_pairs, 3L)
  expect_equal(small$params$n_antigens, 15L)
})

test_that("params$coincident records the supplied value", {
  expect_equal(small$params$coincident, c(1L, 6L, 11L))
})

test_that("homologous distances are zero (coincident AGs = sera positions)", {
  expect_equal(small$slim_dist[1,  "SR1"], 0)
  expect_equal(small$slim_dist[6,  "SR2"], 0)
  expect_equal(small$slim_dist[11, "SR3"], 0)
})

# --- Missing pattern: test antigens ---
# block 1 test AGs (rows 2,3,4,5,7,8): measured in SR1, SR6 only
# block 2 test AGs (rows 9,10,12,13,14,15): measured in SR6, SR11 only

test_that("block-1 test AGs measured only in SR1 and SR2", {
  tt <- small$titre_tables$hiA_agA
  block1_rows <- c(2, 3, 4, 5, 7, 8)
  expect_true(all(tt[block1_rows, c("SR1", "SR2")] != "*"))
  expect_true(all(tt[block1_rows, "SR3"] == "*"))
})

test_that("block-2 test AGs measured only in SR2 and SR3", {
  tt <- small$titre_tables$hiA_agA
  block2_rows <- c(9, 10, 12, 13, 14, 15)
  expect_true(all(tt[block2_rows, c("SR2", "SR3")] != "*"))
  expect_true(all(tt[block2_rows, "SR1"] == "*"))
})

# --- Missing pattern: reference antigens ---
# SR1 (row 1): block 1 -> SR1, SR6
# SR6 (row 6): blocks 1+2 -> SR1, SR6, SR11
# SR11 (row 11): block 2 -> SR6, SR11

test_that("ref AG for SR1 (row 1) measured in SR1 and SR2 only", {
  tt <- small$titre_tables$hiA_agA
  expect_true(all(tt[1, c("SR1", "SR2")] != "*"))
  expect_true(tt[1, "SR3"] == "*")
})

test_that("ref AG for SR2 (row 6) measured in all columns (spans both blocks)", {
  tt <- small$titre_tables$hiA_agA
  expect_true(all(tt[6, ] != "*"))
})

test_that("ref AG for SR3 (row 11) measured in SR2 and SR3 only", {
  tt <- small$titre_tables$hiA_agA
  expect_true(all(tt[11, c("SR2", "SR3")] != "*"))
  expect_true(tt[11, "SR1"] == "*")
})

# --- Reproducibility ---

test_that("same seed gives identical result", {
  r1 <- sim_cluster_surv(
    n_blocks = 2L, n_test_ag_per_block = 6L, true_ag_coord = true_ag,
    range = 0.25, n_ref_per_block = 2L, coincident = c(1L, 6L, 11L), seed = 42
  )
  r2 <- sim_cluster_surv(
    n_blocks = 2L, n_test_ag_per_block = 6L, true_ag_coord = true_ag,
    range = 0.25, n_ref_per_block = 2L, coincident = c(1L, 6L, 11L), seed = 42
  )
  expect_identical(r1$titre_tables, r2$titre_tables)
})

test_that("different seeds produce different titre values", {
  r1 <- sim_cluster_surv(
    n_blocks = 2L, n_test_ag_per_block = 6L, true_ag_coord = true_ag,
    range = 0.25, n_ref_per_block = 2L, coincident = c(1L, 6L, 11L), seed = 1
  )
  r2 <- sim_cluster_surv(
    n_blocks = 2L, n_test_ag_per_block = 6L, true_ag_coord = true_ag,
    range = 0.25, n_ref_per_block = 2L, coincident = c(1L, 6L, 11L), seed = 2
  )
  expect_false(identical(r1$titre_tables$hiA_agA, r2$titre_tables$hiA_agA))
})

# --- Input validation ---

test_that("error when ref_step > n_ref_per_block", {
  expect_error(
    sim_cluster_surv(
      n_blocks = 2L, n_test_ag_per_block = 6L, true_ag_coord = true_ag,
      range = 0.25, n_ref_per_block = 2L, coincident = c(1L, 6L, 11L),
      ref_step = 3L, seed = 1
    ),
    "ref_step"
  )
})

# --- Block structure ---

test_that("block_active has correct serum indices", {
  expect_equal(small$block_active[[1]], 1:2)
  expect_equal(small$block_active[[2]], 2:3)
})

test_that("block_test_ag_rows cover all test AGs without overlap", {
  all_assigned <- sort(unlist(small$block_test_ag_rows))
  expect_equal(all_assigned, c(2, 3, 4, 5, 7, 8, 9, 10, 12, 13, 14, 15))
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
    n_blocks            = 2L,
    n_test_ag_per_block = 6L,
    true_ag_coord       = true_ag,
    range               = 0.25,
    n_ref_per_block     = 2L,
    coincident          = c(1L, 6L, 11L),
    seed                = 1
  )
  expect_equal(dim(r$titre_tables$hiA_agA), c(15L, 3L))
})

test_that("works with 3-cluster full-scale structure (1884 x 48)", {
  skip_on_cran()
  # Grouped layout: 628 AGs per cluster; reference AGs = first 16 of each group
  # n_ref_per_block=14, n_blocks=18, ref_step=2 => n_ref_pairs = 14+17*2 = 48
  # n_antigens = 48 + 18*102 = 1884
  true_ag_large     <- centres[rep(1:3, each = 628), ]
  coincident_large  <- c(1:16, 629:644, 1257:1272)  # 16 ref AGs per cluster
  r <- sim_cluster_surv(
    n_blocks            = 18L,
    n_test_ag_per_block = 102L,
    true_ag_coord       = true_ag_large,
    range               = 0.25,
    n_ref_per_block     = 14L,
    ref_step            = 2L,
    coincident          = coincident_large,
    seed                = 1
  )
  expect_equal(dim(r$titre_tables$hiA_agA), c(1884L, 48L))
})
