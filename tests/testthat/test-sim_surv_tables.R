# Shared fixture: 5 tables, 8 antigens/table, 6 sera/table, 3 ag overlap, 2 sr overlap
base_result <- sim_surv_tables(
  n_tables = 5, n_antigens_per_table = 8, n_sera_per_table = 6,
  n_ag_overlap = 3, n_sr_overlap = 2, ag_drift = 3, range = 1, seed = 42
)

# ---- total point counts -------------------------------------------------------

test_that("total antigen and serum counts are correct", {
  # total = n_per_table + (n_tables - 1) * n_new
  expect_equal(nrow(base_result$ag_coord), 8 + 4 * 5)   # 5 new per table from T2 on
  expect_equal(nrow(base_result$sr_coord), 6 + 4 * 4)   # 4 new per table from T2 on
})

# ---- merged table dimensions -------------------------------------------------

test_that("merged table has correct dimensions", {
  expect_equal(nrow(base_result$merged_titre_table), nrow(base_result$ag_coord))
  expect_equal(ncol(base_result$merged_titre_table), nrow(base_result$sr_coord))
})

# ---- per-table membership sizes ----------------------------------------------

test_that("each table contains the right number of antigens and sera", {
  for (t in seq_len(5)) {
    expect_equal(length(base_result$ag_table_membership[[t]]), 8)
    expect_equal(length(base_result$sr_table_membership[[t]]), 6)
  }
})

# ---- overlap between consecutive tables --------------------------------------

test_that("consecutive tables share the specified number of antigens", {
  for (t in seq_len(4)) {
    shared_ag <- intersect(
      base_result$ag_table_membership[[t]],
      base_result$ag_table_membership[[t + 1]]
    )
    expect_equal(length(shared_ag), 3)
  }
})

test_that("consecutive tables share the specified number of sera", {
  for (t in seq_len(4)) {
    shared_sr <- intersect(
      base_result$sr_table_membership[[t]],
      base_result$sr_table_membership[[t + 1]]
    )
    expect_equal(length(shared_sr), 2)
  }
})

test_that("non-consecutive tables share no antigens when overlap < n_new", {
  # With n_ag_overlap=3 and n_new_ag=5, antigens persist for at most 2 tables
  shared <- intersect(
    base_result$ag_table_membership[["table1"]],
    base_result$ag_table_membership[["table3"]]
  )
  expect_equal(length(shared), 0)
})

# ---- missing values in merged table ------------------------------------------

test_that("untested pairs are encoded as '*'", {
  # AG1..AG3 only appear in table 1 (not table 3); SR from table 3 are never
  # in the same table as AG1..AG3
  t1_only_ags <- setdiff(
    base_result$ag_table_membership[["table1"]],
    base_result$ag_table_membership[["table2"]]
  )
  t3_only_srs <- setdiff(
    base_result$sr_table_membership[["table3"]],
    base_result$sr_table_membership[["table2"]]
  )
  expect_true(all(base_result$merged_titre_table[t1_only_ags, t3_only_srs] == "*"))
})

test_that("tested pairs within a table are not '*'", {
  for (t in seq_len(5)) {
    ags <- base_result$ag_table_membership[[t]]
    srs <- base_result$sr_table_membership[[t]]
    expect_true(all(base_result$merged_titre_table[ags, srs] != "*"))
  }
})

# ---- coordinate dimensions ---------------------------------------------------

test_that("coordinate matrices have the right number of dimensions", {
  expect_equal(ncol(base_result$ag_coord), 2)
  expect_equal(ncol(base_result$sr_coord), 2)

  result3d <- sim_surv_tables(
    n_tables = 2, n_antigens_per_table = 4, n_sera_per_table = 4,
    n_ag_overlap = 1, n_sr_overlap = 1, dimensions = 3, seed = 1
  )
  expect_equal(ncol(result3d$ag_coord), 3)
})

# ---- antigenic drift ---------------------------------------------------------

test_that("later antigens have larger x-coordinates than earlier ones", {
  # With ag_drift >> range, each cluster is clearly separated along x
  result <- sim_surv_tables(
    n_tables = 4, n_antigens_per_table = 4, n_sera_per_table = 4,
    n_ag_overlap = 1, n_sr_overlap = 1, ag_drift = 20, range = 1, seed = 1
  )
  t1_ags <- setdiff(result$ag_table_membership[["table1"]], result$ag_table_membership[["table2"]])
  t4_ags <- setdiff(result$ag_table_membership[["table4"]], result$ag_table_membership[["table3"]])
  expect_true(min(result$ag_coord[t4_ags, 1]) > max(result$ag_coord[t1_ags, 1]))
})

# ---- reproducibility ---------------------------------------------------------

test_that("same seed gives identical results", {
  r1 <- sim_surv_tables(n_tables = 3, n_antigens_per_table = 5, n_sera_per_table = 4,
                        n_ag_overlap = 2, n_sr_overlap = 1, seed = 99)
  r2 <- sim_surv_tables(n_tables = 3, n_antigens_per_table = 5, n_sera_per_table = 4,
                        n_ag_overlap = 2, n_sr_overlap = 1, seed = 99)
  expect_identical(r1$merged_titre_table, r2$merged_titre_table)
  expect_equal(r1$ag_coord, r2$ag_coord)
})

# ---- input validation --------------------------------------------------------

test_that("error when n_ag_overlap >= n_antigens_per_table", {
  expect_error(
    sim_surv_tables(n_tables = 3, n_antigens_per_table = 4, n_sera_per_table = 4,
                    n_ag_overlap = 4, n_sr_overlap = 1),
    "n_ag_overlap must be less than n_antigens_per_table"
  )
})

test_that("error when n_sr_overlap >= n_sera_per_table", {
  expect_error(
    sim_surv_tables(n_tables = 3, n_antigens_per_table = 4, n_sera_per_table = 4,
                    n_ag_overlap = 1, n_sr_overlap = 4),
    "n_sr_overlap must be less than n_sera_per_table"
  )
})

# ---- single table (edge case) ------------------------------------------------

test_that("works correctly with a single table", {
  r <- sim_surv_tables(n_tables = 1, n_antigens_per_table = 5, n_sera_per_table = 4,
                       n_ag_overlap = 2, n_sr_overlap = 1, seed = 7)
  expect_equal(nrow(r$merged_titre_table), 5)
  expect_equal(ncol(r$merged_titre_table), 4)
  expect_true(all(r$merged_titre_table != "*"))
})
