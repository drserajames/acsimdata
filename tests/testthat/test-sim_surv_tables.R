# Shared fixture: 5 tables, 5 table-specific antigens + 3 reference antigens = 8 total per table
#                            4 table-specific sera    + 2 reference sera     = 6 total per table
base_result <- sim_surv_tables(
  n_tables = 5, n_ag_per_table = 5, n_sr_per_table = 4,
  n_ref_ag = 3, n_ref_sr = 2, ag_drift = 3, range = 1, seed = 42
)

# ---- total point counts -------------------------------------------------------

test_that("total antigen count is correct (simple case)", {
  # References: 3 fixed = 3 total
  # Table-specific: 5 per table * 5 tables = 25 total
  expect_equal(nrow(base_result$ag_coord), 3 + 5 * 5)
})

test_that("total serum count is correct (simple case)", {
  expect_equal(nrow(base_result$sr_coord), 2 + 4 * 5)
})

# ---- merged table dimensions -------------------------------------------------

test_that("merged table has correct dimensions", {
  expect_equal(nrow(base_result$merged_titre_table), nrow(base_result$ag_coord))
  expect_equal(ncol(base_result$merged_titre_table), nrow(base_result$sr_coord))
})

# ---- per-table membership sizes ----------------------------------------------

test_that("each table contains the right number of antigens and sera", {
  for (t in seq_len(5)) {
    expect_equal(length(base_result$ag_table_membership[[t]]), 8)  # 3 ref + 5 specific
    expect_equal(length(base_result$sr_table_membership[[t]]), 6)  # 2 ref + 4 specific
  }
})

# ---- reference antigen/serum behaviour (simple case) -------------------------

test_that("reference antigens appear in all tables (simple case)", {
  ref_ags <- base_result$ag_table_membership[["table1"]][1:3]  # first 3 are refs
  for (t in seq_len(5)) {
    expect_true(all(ref_ags %in% base_result$ag_table_membership[[t]]))
  }
})

test_that("reference sera appear in all tables (simple case)", {
  ref_srs <- base_result$sr_table_membership[["table1"]][1:2]
  for (t in seq_len(5)) {
    expect_true(all(ref_srs %in% base_result$sr_table_membership[[t]]))
  }
})

test_that("consecutive tables share exactly n_ref_ag antigens (simple case)", {
  for (t in seq_len(4)) {
    shared <- intersect(
      base_result$ag_table_membership[[t]],
      base_result$ag_table_membership[[t + 1]]
    )
    expect_equal(length(shared), 3)
  }
})

test_that("non-consecutive tables also share exactly n_ref_ag antigens", {
  shared <- intersect(
    base_result$ag_table_membership[["table1"]],
    base_result$ag_table_membership[["table5"]]
  )
  expect_equal(length(shared), 3)
})

test_that("table-specific antigens are unique to one table", {
  spec_t1 <- setdiff(base_result$ag_table_membership[["table1"]],
                     base_result$ag_table_membership[["table2"]])
  for (t in 2:5) {
    expect_equal(length(intersect(spec_t1, base_result$ag_table_membership[[t]])), 0)
  }
})

# ---- rotating reference set (complex / WHO-like case) -----------------------

test_that("rotating refs: consecutive tables share n_ref_ag_overlap antigens", {
  r <- sim_surv_tables(
    n_tables = 5, n_ag_per_table = 5, n_sr_per_table = 4,
    n_ref_ag = 4, n_ref_sr = 3, n_ref_ag_overlap = 2, n_ref_sr_overlap = 2,
    ag_drift = 3, range = 1, seed = 1
  )
  for (t in seq_len(4)) {
    shared <- intersect(r$ag_table_membership[[t]], r$ag_table_membership[[t + 1]])
    expect_equal(length(shared), 2)
  }
})

test_that("rotating refs: non-consecutive tables share fewer than n_ref_ag antigens", {
  r <- sim_surv_tables(
    n_tables = 5, n_ag_per_table = 5, n_sr_per_table = 4,
    n_ref_ag = 4, n_ref_sr = 3, n_ref_ag_overlap = 2, n_ref_sr_overlap = 2,
    ag_drift = 3, range = 1, seed = 1
  )
  shared_1_5 <- intersect(r$ag_table_membership[["table1"]], r$ag_table_membership[["table5"]])
  expect_true(length(shared_1_5) < 4)
})

# ---- missing values in merged table ------------------------------------------

test_that("untested pairs are encoded as '*'", {
  # Table-specific antigens from T1 should never appear with T5-only sera
  spec_t1 <- setdiff(base_result$ag_table_membership[["table1"]],
                     base_result$ag_table_membership[["table2"]])
  spec_t5 <- setdiff(base_result$sr_table_membership[["table5"]],
                     base_result$sr_table_membership[["table4"]])
  expect_true(all(base_result$merged_titre_table[spec_t1, spec_t5] == "*"))
})

test_that("tested pairs within a table are not '*'", {
  for (t in seq_len(5)) {
    ags <- base_result$ag_table_membership[[t]]
    srs <- base_result$sr_table_membership[[t]]
    expect_true(all(base_result$merged_titre_table[ags, srs] != "*"))
  }
})

test_that("reference antigens have non-missing values with all tables' sera", {
  ref_ags <- base_result$ag_table_membership[["table1"]][1:3]
  # Reference antigens are in all tables, so they should have values vs all sera
  for (t in seq_len(5)) {
    srs <- base_result$sr_table_membership[[t]]
    expect_true(all(base_result$merged_titre_table[ref_ags, srs] != "*"))
  }
})

# ---- coordinate dimensions ---------------------------------------------------

test_that("coordinate matrices have the right number of dimensions", {
  expect_equal(ncol(base_result$ag_coord), 2)
  expect_equal(ncol(base_result$sr_coord), 2)

  r3d <- sim_surv_tables(
    n_tables = 2, n_ag_per_table = 4, n_sr_per_table = 4,
    n_ref_ag = 2, n_ref_sr = 2, dimensions = 3, seed = 1
  )
  expect_equal(ncol(r3d$ag_coord), 3)
})

# ---- antigenic drift ---------------------------------------------------------

test_that("later table-specific antigens have larger x-coords than earlier ones", {
  r <- sim_surv_tables(
    n_tables = 4, n_ag_per_table = 4, n_sr_per_table = 4,
    n_ref_ag = 2, n_ref_sr = 2, ag_drift = 20, range = 1, seed = 1
  )
  spec_t1 <- setdiff(r$ag_table_membership[["table1"]], r$ag_table_membership[["table2"]])
  spec_t4 <- setdiff(r$ag_table_membership[["table4"]], r$ag_table_membership[["table3"]])
  expect_true(min(r$ag_coord[spec_t4, 1]) > max(r$ag_coord[spec_t1, 1]))
})

# ---- variable table sizes ----------------------------------------------------

test_that("each table has the specified number of antigens when sizes vary", {
  ag_sizes <- c(10L, 5L, 8L, 6L)
  r <- sim_surv_tables(n_tables = 4, n_ag_per_table = ag_sizes,
                       n_sr_per_table = 4, n_ref_ag = 3, n_ref_sr = 2, seed = 1)
  for (t in seq_len(4)) {
    expect_equal(length(r$ag_table_membership[[t]]), ag_sizes[t] + 3)
  }
})

test_that("total antigen count is correct with variable table-specific sizes", {
  ag_sizes <- c(10L, 5L, 8L, 6L)
  r <- sim_surv_tables(n_tables = 4, n_ag_per_table = ag_sizes,
                       n_sr_per_table = 4, n_ref_ag = 3, n_ref_sr = 2, seed = 1)
  expect_equal(nrow(r$ag_coord), 3 + sum(ag_sizes))
})

# ---- no references (edge case) -----------------------------------------------

test_that("works with no reference antigens or sera", {
  r <- sim_surv_tables(n_tables = 3, n_ag_per_table = 5, n_sr_per_table = 4,
                       n_ref_ag = 0, n_ref_sr = 0, seed = 1)
  # No shared antigens between any pair of tables
  for (t in seq_len(2)) {
    shared <- intersect(r$ag_table_membership[[t]], r$ag_table_membership[[t + 1]])
    expect_equal(length(shared), 0)
  }
  expect_equal(nrow(r$ag_coord), 3 * 5)
})

# ---- single table (edge case) ------------------------------------------------

test_that("works correctly with a single table", {
  r <- sim_surv_tables(n_tables = 1, n_ag_per_table = 5, n_sr_per_table = 4,
                       n_ref_ag = 2, n_ref_sr = 1, seed = 7)
  expect_equal(nrow(r$merged_titre_table), 7)
  expect_equal(ncol(r$merged_titre_table), 5)
  expect_true(all(r$merged_titre_table != "*"))
})

# ---- reproducibility ---------------------------------------------------------

test_that("same seed gives identical results", {
  r1 <- sim_surv_tables(n_tables = 3, n_ag_per_table = 5, n_sr_per_table = 4,
                        n_ref_ag = 2, n_ref_sr = 2, seed = 99)
  r2 <- sim_surv_tables(n_tables = 3, n_ag_per_table = 5, n_sr_per_table = 4,
                        n_ref_ag = 2, n_ref_sr = 2, seed = 99)
  expect_identical(r1$merged_titre_table, r2$merged_titre_table)
  expect_equal(r1$ag_coord, r2$ag_coord)
})

# ---- input validation --------------------------------------------------------

test_that("error when n_ref_ag_overlap exceeds n_ref_ag", {
  expect_error(
    sim_surv_tables(n_tables = 3, n_ag_per_table = 5, n_sr_per_table = 4,
                    n_ref_ag = 3, n_ref_sr = 2, n_ref_ag_overlap = 4),
    "n_ref_ag_overlap cannot exceed n_ref_ag"
  )
})

test_that("error when n_ref_sr_overlap exceeds n_ref_sr", {
  expect_error(
    sim_surv_tables(n_tables = 3, n_ag_per_table = 5, n_sr_per_table = 4,
                    n_ref_ag = 3, n_ref_sr = 2, n_ref_sr_overlap = 3),
    "n_ref_sr_overlap cannot exceed n_ref_sr"
  )
})


# ── probabilistic serum turnover ───────────────────────────────────────────────

test_that("prob mode: p_sr_drop=1, p_sr_gain=0 shrinks panel by 1 each transition", {
  r <- sim_surv_tables(
    n_tables = 5, n_ag_per_table = 4, n_sr_per_table = 3,
    n_ref_ag = 2, n_ref_sr = 4,
    p_sr_drop = 1, p_sr_gain = 0,
    ag_drift = 3, range = 1, seed = 1
  )
  # Table 1: 4 ref sera; each subsequent table loses one
  ref_counts <- sapply(r$sr_table_membership, function(srs) {
    sum(startsWith(srs, "SR") & as.integer(sub("SR", "", srs)) <= r$params$n_ref_sr[1])
  })
  # Can't easily count "reference" vs "table-specific" by name once pool shrinks,
  # so instead check the total membership sizes:
  # Table t should have (4 - (t-1)) ref sera + 3 table-specific sera, until ref hits 0
  expected_total <- pmax(4L - (seq_len(5) - 1L), 0L) + 3L
  expect_equal(unname(lengths(r$sr_table_membership)), expected_total)
})

test_that("prob mode: p_sr_drop=0, p_sr_gain=1 grows panel by 1 each transition", {
  r <- sim_surv_tables(
    n_tables = 5, n_ag_per_table = 4, n_sr_per_table = 3,
    n_ref_ag = 2, n_ref_sr = 2,
    p_sr_drop = 0, p_sr_gain = 1,
    ag_drift = 3, range = 1, seed = 1
  )
  # Table 1: 2 ref sera; each subsequent table gains one
  # Total membership = (2 + (t-1)) ref + 3 table-specific
  expected_total <- (2L + (seq_len(5) - 1L)) + 3L
  expect_equal(unname(lengths(r$sr_table_membership)), expected_total)
})

test_that("prob mode: p_sr_drop=0, p_sr_gain=0 keeps panel fixed", {
  r <- sim_surv_tables(
    n_tables = 5, n_ag_per_table = 4, n_sr_per_table = 3,
    n_ref_ag = 2, n_ref_sr = 3,
    p_sr_drop = 0, p_sr_gain = 0,
    ag_drift = 3, range = 1, seed = 1
  )
  # 3 ref + 3 specific = 6 in every table
  expect_true(all(lengths(r$sr_table_membership) == 6L))
})

test_that("prob mode: oldest reference serum is the one dropped (p_drop=1)", {
  r <- sim_surv_tables(
    n_tables = 3, n_ag_per_table = 3, n_sr_per_table = 2,
    n_ref_ag = 2, n_ref_sr = 3,
    p_sr_drop = 1, p_sr_gain = 0,
    ag_drift = 3, range = 1, seed = 1
  )
  # Table 1 ref: SR1, SR2, SR3
  # Table 2 ref: SR2, SR3  (SR1 dropped)
  # Table 3 ref: SR3       (SR2 dropped)
  t1_ref <- r$sr_table_membership[["table1"]][1:3]
  t2_ref <- r$sr_table_membership[["table2"]][1:2]
  t3_ref <- r$sr_table_membership[["table3"]][1:1]
  expect_false(t1_ref[1] %in% t2_ref)   # oldest from T1 gone in T2
  expect_false(t2_ref[1] %in% t3_ref)   # oldest from T2 gone in T3
  expect_true(t2_ref[2] %in% t3_ref)    # newer one retained
})

test_that("prob mode: newly gained sera appear with later introduction drift", {
  r <- sim_surv_tables(
    n_tables = 5, n_ag_per_table = 3, n_sr_per_table = 2,
    n_ref_ag = 2, n_ref_sr = 1,
    p_sr_drop = 0, p_sr_gain = 1,
    ag_drift = 10, range = 0.1, seed = 1
  )
  # SR1 introduced at table 1; SR2 at table 2; etc. (p_gain=1 means one added every table)
  # SR1 x-coord should be < SR2 x-coord < SR3 x-coord (drift=10 >> range=0.1)
  ref_sr_names <- paste0("SR", seq_len(5))  # 1 initial + 4 gained
  x_coords <- r$sr_coord[ref_sr_names, 1]
  expect_true(all(diff(x_coords) > 0))
})

test_that("prob mode: seed reproduces results", {
  make <- function(seed) sim_surv_tables(
    n_tables = 6, n_ag_per_table = 4, n_sr_per_table = 3,
    n_ref_ag = 2, n_ref_sr = 3,
    p_sr_drop = 0.5, p_sr_gain = 0.3,
    ag_drift = 3, range = 1, seed = seed
  )
  r1 <- make(77)
  r2 <- make(77)
  expect_identical(r1$merged_titre_table, r2$merged_titre_table)
  expect_equal(r1$sr_coord, r2$sr_coord)
})

test_that("prob mode: different seeds give (usually) different results", {
  make <- function(seed) sim_surv_tables(
    n_tables = 6, n_ag_per_table = 4, n_sr_per_table = 3,
    n_ref_ag = 2, n_ref_sr = 3,
    p_sr_drop = 0.5, p_sr_gain = 0.5,
    ag_drift = 3, range = 1, seed = seed
  )
  expect_false(identical(make(1)$sr_table_membership, make(2)$sr_table_membership))
})

test_that("prob mode: merged table is the right size", {
  r <- sim_surv_tables(
    n_tables = 5, n_ag_per_table = 4, n_sr_per_table = 3,
    n_ref_ag = 2, n_ref_sr = 3,
    p_sr_drop = 0.4, p_sr_gain = 0.4,
    ag_drift = 3, range = 1, seed = 5
  )
  expect_equal(nrow(r$merged_titre_table), nrow(r$ag_coord))
  expect_equal(ncol(r$merged_titre_table), nrow(r$sr_coord))
})

test_that("prob mode: tested pairs within each table are not '*'", {
  r <- sim_surv_tables(
    n_tables = 5, n_ag_per_table = 4, n_sr_per_table = 3,
    n_ref_ag = 2, n_ref_sr = 3,
    p_sr_drop = 0.5, p_sr_gain = 0.5,
    ag_drift = 3, range = 1, seed = 9
  )
  for (t in seq_len(5)) {
    ags <- r$ag_table_membership[[t]]
    srs <- r$sr_table_membership[[t]]
    expect_true(all(r$merged_titre_table[ags, srs] != "*"))
  }
})

test_that("prob mode: p_sr_gain only (no drop) works without specifying p_sr_drop", {
  expect_no_error(
    sim_surv_tables(
      n_tables = 4, n_ag_per_table = 3, n_sr_per_table = 2,
      n_ref_ag = 2, n_ref_sr = 2, p_sr_gain = 0.5,
      ag_drift = 3, range = 1, seed = 1
    )
  )
})

test_that("prob mode: p_sr_drop only (no gain) works without specifying p_sr_gain", {
  expect_no_error(
    sim_surv_tables(
      n_tables = 4, n_ag_per_table = 3, n_sr_per_table = 2,
      n_ref_ag = 2, n_ref_sr = 3, p_sr_drop = 0.5,
      ag_drift = 3, range = 1, seed = 1
    )
  )
})

test_that("prob mode: error on invalid probabilities", {
  expect_error(
    sim_surv_tables(n_tables = 3, n_ag_per_table = 3, n_sr_per_table = 2,
                    n_ref_ag = 2, n_ref_sr = 2, p_sr_drop = 1.5),
    "p_sr_drop must be in"
  )
  expect_error(
    sim_surv_tables(n_tables = 3, n_ag_per_table = 3, n_sr_per_table = 2,
                    n_ref_ag = 2, n_ref_sr = 2, p_sr_gain = -0.1),
    "p_sr_gain must be in"
  )
})

test_that("prob mode: table-specific sera remain unique to one table", {
  r <- sim_surv_tables(
    n_tables = 5, n_ag_per_table = 4, n_sr_per_table = 3,
    n_ref_ag = 2, n_ref_sr = 3,
    p_sr_drop = 0.5, p_sr_gain = 0.5,
    ag_drift = 3, range = 1, seed = 42
  )
  # Find table-specific sera (those not in any other table)
  for (t in seq_len(5)) {
    other_tables <- setdiff(seq_len(5), t)
    other_srs    <- unlist(r$sr_table_membership[other_tables])
    # Table-specific sera for table t: those that appear only in table t's membership
    # We identify them as the last n_sr_per_table entries
    n_sr_spec <- r$params$n_sr_per_table[t]
    this_srs  <- r$sr_table_membership[[t]]
    spec_srs  <- tail(this_srs, n_sr_spec)
    expect_equal(length(intersect(spec_srs, other_srs)), 0L)
  }
})


# ── half-life serum turnover ───────────────────────────────────────────────────

test_that("halflife: very large halflife keeps panel nearly fixed", {
  r <- sim_surv_tables(
    n_tables = 10, n_ag_per_table = 3, n_sr_per_table = 2,
    n_ref_ag = 2, n_ref_sr = 4,
    sr_halflife = 1e6, p_sr_gain = 0,   # p_drop ≈ 0, never gains
    ag_drift = 3, range = 1, seed = 1
  )
  # With halflife >> n_tables, all 4 ref sera should survive every table
  for (t in seq_len(10)) {
    ref_in_t <- length(r$sr_table_membership[[t]]) - 2L  # subtract 2 table-specific
    expect_equal(ref_in_t, 4L)
  }
})

test_that("halflife: very small halflife empties panel rapidly", {
  # halflife = 0.01 → p_drop_each = 1 - 2^(-100) ≈ 1: almost all dropped each transition
  r <- sim_surv_tables(
    n_tables = 5, n_ag_per_table = 3, n_sr_per_table = 2,
    n_ref_ag = 2, n_ref_sr = 6,
    sr_halflife = 0.01, p_sr_gain = 0,
    ag_drift = 3, range = 1, seed = 1
  )
  # By table 3, reference panel should have shrunk substantially
  n_ref_t3 <- length(r$sr_table_membership[["table3"]]) - 2L
  expect_lt(n_ref_t3, 6L)
})

test_that("halflife: any serum can be dropped (not just oldest)", {
  # With oldest-first model SR1 is always the first eligible to drop, so
  # SR3 can only be absent from table 2 if SR1 and SR2 were also dropped.
  # With half-life (p_drop_each = 0.5 per serum, halflife = 1), any serum can
  # go while others survive — so SR1-present-and-SR3-absent should occur.
  # Use n_sr_per_table = 3 (≥ 2) to avoid the single-column matrix edge case.
  sr1_survives_sr3_drops <- vapply(seq_len(60L), function(s) {
    r <- sim_surv_tables(
      n_tables = 2, n_ag_per_table = 3, n_sr_per_table = 3,
      n_ref_ag = 2, n_ref_sr = 3,
      sr_halflife = 1, p_sr_gain = 0,
      ag_drift = 3, range = 1, seed = s
    )
    n_spec <- 3L
    ref_t2 <- head(r$sr_table_membership[["table2"]],
                   length(r$sr_table_membership[["table2"]]) - n_spec)
    "SR1" %in% ref_t2 && !("SR3" %in% ref_t2)
  }, logical(1L))
  # With oldest-first, SR3 is only absent when SR1 and SR2 are both dropped too
  # (so SR1 could never be present when SR3 is absent).
  # With half-life, P(SR1 survives and SR3 drops) = 0.5 × 0.5 = 0.25 per run.
  expect_true(any(sr1_survives_sr3_drops))
})

test_that("halflife: seed reproduces results", {
  make <- function(seed) sim_surv_tables(
    n_tables = 8, n_ag_per_table = 3, n_sr_per_table = 2,
    n_ref_ag = 2, n_ref_sr = 5,
    sr_halflife = 4, p_sr_gain = 0.3,
    ag_drift = 3, range = 1, seed = seed
  )
  r1 <- make(55)
  r2 <- make(55)
  expect_identical(r1$merged_titre_table, r2$merged_titre_table)
  expect_equal(r1$sr_coord, r2$sr_coord)
})

test_that("halflife: merged table dimensions are correct", {
  r <- sim_surv_tables(
    n_tables = 6, n_ag_per_table = 3, n_sr_per_table = 2,
    n_ref_ag = 2, n_ref_sr = 4,
    sr_halflife = 3, p_sr_gain = 0.5,
    ag_drift = 3, range = 1, seed = 7
  )
  expect_equal(nrow(r$merged_titre_table), nrow(r$ag_coord))
  expect_equal(ncol(r$merged_titre_table), nrow(r$sr_coord))
})

test_that("halflife: tested pairs within each table are not '*'", {
  r <- sim_surv_tables(
    n_tables = 6, n_ag_per_table = 3, n_sr_per_table = 2,
    n_ref_ag = 2, n_ref_sr = 4,
    sr_halflife = 3, p_sr_gain = 0.4,
    ag_drift = 3, range = 1, seed = 11
  )
  for (t in seq_len(6)) {
    ags <- r$ag_table_membership[[t]]
    srs <- r$sr_table_membership[[t]]
    expect_true(all(r$merged_titre_table[ags, srs] != "*"))
  }
})

test_that("halflife: error when combined with p_sr_drop", {
  expect_error(
    sim_surv_tables(
      n_tables = 4, n_ag_per_table = 3, n_sr_per_table = 2,
      n_ref_ag = 2, n_ref_sr = 3,
      sr_halflife = 4, p_sr_drop = 0.2
    ),
    "Specify either sr_halflife or p_sr_drop"
  )
})

test_that("halflife: error on non-positive halflife", {
  expect_error(
    sim_surv_tables(
      n_tables = 4, n_ag_per_table = 3, n_sr_per_table = 2,
      n_ref_ag = 2, n_ref_sr = 3, sr_halflife = 0
    ),
    "single positive number"
  )
  expect_error(
    sim_surv_tables(
      n_tables = 4, n_ag_per_table = 3, n_sr_per_table = 2,
      n_ref_ag = 2, n_ref_sr = 3, sr_halflife = -2
    ),
    "single positive number"
  )
})

test_that("halflife: sr_halflife stored in params", {
  r <- sim_surv_tables(
    n_tables = 4, n_ag_per_table = 3, n_sr_per_table = 2,
    n_ref_ag = 2, n_ref_sr = 3,
    sr_halflife = 5, p_sr_gain = 0.3,
    ag_drift = 3, range = 1, seed = 1
  )
  expect_equal(r$params$sr_halflife, 5)
})
