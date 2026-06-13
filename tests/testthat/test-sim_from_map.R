# ── Shared mock data ──────────────────────────────────────────────────────────
# A 3-antigen × 4-serum setup with known coordinates and a realistic missingness
# pattern.  The coordinates are fixed so that distances — and therefore simulated
# titres — are deterministic even without a seed.

ag_mock <- matrix(
  c(0, 0,
    2, 0,
    4, 2),
  nrow = 3, ncol = 2, byrow = TRUE,
  dimnames = list(c("A/Vic/75", "A/Tex/77", "A/Ban/79"), c("c1", "c2"))
)

sr_mock <- matrix(
  c(0,  0,
    2,  0,
    4,  2,
    6,  4),
  nrow = 4, ncol = 2, byrow = TRUE,
  dimnames = list(c("Ferret/Vic", "Ferret/Tex", "Ferret/Ban", "Ferret/SI"), c("c1", "c2"))
)

# Observed merged table: "*" where not tested (4 missing cells out of 12)
tt_mock <- matrix(
  c("640", "160",  "40",  "*",
    "160", "640",  "*",   "40",
     "*",   "80", "640", "160"),
  nrow = 3, ncol = 4, byrow = TRUE,
  dimnames = list(rownames(ag_mock), rownames(sr_mock))
)

# Two-layer split of tt_mock (no dimnames, matching real Racmacs behaviour for
# multi-layer maps).  Together the layers cover every non-"*" cell in tt_mock.
#   Layer 1: AG1/SR1, AG1/SR2, AG2/SR1, AG2/SR2 — top-left block
#   Layer 2: AG1/SR3, AG2/SR4, AG3/SR2, AG3/SR3, AG3/SR4 — remaining cells
layer1_mock <- matrix(
  c("640", "160",  "*",  "*",
    "160", "640",  "*",  "*",
     "*",   "*",   "*",  "*"),
  nrow = 3, ncol = 4, byrow = TRUE   # no dimnames — mirrors Racmacs multi-layer output
)
layer2_mock <- matrix(
  c( "*",   "*",  "40",  "*",
     "*",   "*",   "*",  "40",
     "*",  "80", "640", "160"),
  nrow = 3, ncol = 4, byrow = TRUE
)

# Helper: run sim_from_map with mocked Racmacs extractors.
# Always mocks titerTableLayers and layerNames so layers = TRUE also works.
run_sim <- function(...) {
  skip_if_not_installed("Racmacs")
  mock_map <- structure(list(), class = "acmap")
  testthat::local_mocked_bindings(
    agCoords          = function(map, ...) ag_mock,
    srCoords          = function(map, ...) sr_mock,
    titerTable        = function(map, ...) tt_mock,
    titerTableLayers  = function(map, ...) list(layer1_mock, layer2_mock),
    layerNames        = function(map, ...) c("Lab A", "Lab B"),
    .package          = "Racmacs"
  )
  sim_from_map(mock_map, ...)
}

# ── Output structure ──────────────────────────────────────────────────────────

test_that("sim_from_map returns the expected list elements", {
  r <- run_sim(seed = 1)
  expect_type(r, "list")
  expected <- c("sim_titre", "full_sim_titre", "sim_titre_layers",
                "observed_titre", "dist", "noise", "ag_coord", "sr_coord", "params")
  expect_true(all(expected %in% names(r)))
})

test_that("sim_titre and dist have correct dimensions", {
  r <- run_sim(seed = 1)
  expect_equal(dim(r$sim_titre),      c(3L, 4L))
  expect_equal(dim(r$full_sim_titre), c(3L, 4L))
  expect_equal(dim(r$dist),           c(3L, 4L))
})

test_that("original antigen and serum names are preserved in outputs", {
  r <- run_sim(seed = 1)
  expect_equal(rownames(r$sim_titre), rownames(ag_mock))
  expect_equal(colnames(r$sim_titre), rownames(sr_mock))
  expect_equal(rownames(r$dist),      rownames(ag_mock))
  expect_equal(colnames(r$dist),      rownames(sr_mock))
})

# ── Missingness ───────────────────────────────────────────────────────────────

test_that("observed '*' cells are '*' in sim_titre", {
  r <- run_sim(seed = 1)
  obs_miss <- which(tt_mock == "*")
  expect_true(all(r$sim_titre[obs_miss] == "*"))
})

test_that("observed present cells are not '*' in sim_titre", {
  r <- run_sim(seed = 1)
  obs_present <- which(tt_mock != "*")
  expect_false(any(r$sim_titre[obs_present] == "*"))
})

test_that("full_sim_titre contains no '*' (missingness not applied)", {
  r <- run_sim(seed = 1)
  expect_false(any(r$full_sim_titre == "*"))
})

test_that("observed_titre is returned unchanged", {
  r <- run_sim(seed = 1)
  expect_identical(r$observed_titre, tt_mock)
})

# ── Distances ─────────────────────────────────────────────────────────────────

test_that("distances are non-negative", {
  r <- run_sim(seed = 1)
  expect_true(all(r$dist >= 0))
})

test_that("distances are symmetric with ag/sr coord ordering", {
  r <- run_sim(seed = 1)
  # AG1 coords == SR1 coords in our mock, so distance should be ~0
  expect_equal(r$dist["A/Vic/75", "Ferret/Vic"], 0, tolerance = 1e-10)
  expect_equal(r$dist["A/Tex/77", "Ferret/Tex"], 0, tolerance = 1e-10)
})

# ── Noise ─────────────────────────────────────────────────────────────────────

test_that("noise is NULL when noise_params is empty", {
  r <- run_sim(seed = 1)
  expect_null(r$noise)
})

test_that("noise is non-NULL and has expected structure when noise_params provided", {
  r <- run_sim(
    noise_params = list(
      titre_noise_param   = c(0, 0.5),
      antigen_noise_param = c(0, 0),
      serum_noise_param   = c(0, 0)
    ),
    seed = 1
  )
  expect_false(is.null(r$noise))
  expect_true(all(c("noise_dist_table", "titre_noise", "total_noise") %in% names(r$noise)))
})

test_that("noise respects seed forwarding from top-level seed", {
  run1 <- run_sim(noise_params = list(titre_noise_param = c(0, 1)), seed = 99)
  run2 <- run_sim(noise_params = list(titre_noise_param = c(0, 1)), seed = 99)
  expect_identical(run1$sim_titre, run2$sim_titre)
  expect_identical(run1$noise$titre_noise, run2$noise$titre_noise)
})

test_that("noise seed in noise_params overrides top-level seed", {
  # Two calls with different top-level seeds but same seed inside noise_params
  # should produce identical noise
  run_a <- run_sim(
    noise_params = list(titre_noise_param = c(0, 1), seed = 7),
    seed = 1
  )
  run_b <- run_sim(
    noise_params = list(titre_noise_param = c(0, 1), seed = 7),
    seed = 2
  )
  expect_identical(run_a$noise$titre_noise, run_b$noise$titre_noise)
})

test_that("missingness is still correct when noise is applied", {
  r <- run_sim(
    noise_params = list(titre_noise_param = c(0, 1)),
    seed = 42
  )
  obs_miss <- which(tt_mock == "*")
  expect_true(all(r$sim_titre[obs_miss] == "*"))
})

# ── Coordinates returned ──────────────────────────────────────────────────────

test_that("ag_coord and sr_coord match the mock data", {
  r <- run_sim(seed = 1)
  expect_identical(r$ag_coord, ag_mock)
  expect_identical(r$sr_coord, sr_mock)
})

# ── params stored ─────────────────────────────────────────────────────────────

test_that("seed is stored in params", {
  r <- run_sim(seed = 123)
  expect_equal(r$params$seed, 123)
})

test_that("titre conversion parameters are stored in params", {
  r <- run_sim(base = 2, divisor = 10, max_log_titre = 8, min_log_titre = 1, seed = 1)
  expect_equal(r$params$base,          2)
  expect_equal(r$params$divisor,       10)
  expect_equal(r$params$max_log_titre, 8)
  expect_equal(r$params$min_log_titre, 1)
})

# ── Coordinate noise ─────────────────────────────────────────────────────────

test_that("coord_noise_sd = 0 (default): returned ag_coord matches source", {
  r <- run_sim(seed = 1)
  expect_identical(r$ag_coord, ag_mock)
  expect_identical(r$sr_coord, sr_mock)
})

test_that("coord_noise_sd > 0: returned coordinates differ from source", {
  r <- run_sim(coord_noise_sd = 1, seed = 1)
  expect_false(identical(r$ag_coord, ag_mock))
  expect_false(identical(r$sr_coord, sr_mock))
})

test_that("coord_noise_sd > 0: distances differ from unperturbed case", {
  r_clean <- run_sim(coord_noise_sd = 0, seed = 1)
  r_noise <- run_sim(coord_noise_sd = 1, seed = 1)
  expect_false(identical(r_clean$dist, r_noise$dist))
})

test_that("coord_noise_sd > 0: reproducible with same seed", {
  r1 <- run_sim(coord_noise_sd = 0.5, seed = 42)
  r2 <- run_sim(coord_noise_sd = 0.5, seed = 42)
  expect_identical(r1$ag_coord, r2$ag_coord)
  expect_identical(r1$sr_coord, r2$sr_coord)
  expect_identical(r1$dist,     r2$dist)
})

test_that("coord_noise_sd > 0: different seeds give different coordinates", {
  r1 <- run_sim(coord_noise_sd = 0.5, seed = 1)
  r2 <- run_sim(coord_noise_sd = 0.5, seed = 2)
  expect_false(identical(r1$ag_coord, r2$ag_coord))
})

test_that("coord_noise_sd is stored in params", {
  r <- run_sim(coord_noise_sd = 0.5, seed = 1)
  expect_equal(r$params$coord_noise_sd, 0.5)
})

test_that("coord_noise_sd = 0 stored in params by default", {
  r <- run_sim(seed = 1)
  expect_equal(r$params$coord_noise_sd, 0)
})

test_that("coord_noise + noise_params: measurement noise seed independent of coord noise", {
  # Same top-level seed; coord perturbation uses set.seed(seed) then
  # add_noise resets independently — so measurement noise should be identical
  # regardless of whether coord_noise_sd is applied
  r_no_coord <- run_sim(coord_noise_sd = 0,   noise_params = list(titre_noise_param = c(0, 1)), seed = 7)
  r_coord    <- run_sim(coord_noise_sd = 0.5, noise_params = list(titre_noise_param = c(0, 1)), seed = 7)
  expect_identical(r_no_coord$noise$titre_noise, r_coord$noise$titre_noise)
})

test_that("coord_noise_sd > 0: observed missingness still applied correctly", {
  r <- run_sim(coord_noise_sd = 1, seed = 1)
  obs_miss <- which(tt_mock == "*")
  expect_true(all(r$sim_titre[obs_miss] == "*"))
  obs_present <- which(tt_mock != "*")
  expect_false(any(r$sim_titre[obs_present] == "*"))
})

test_that("coord_noise_sd > 0 with layers: layer missingness still correct", {
  r <- run_sim(coord_noise_sd = 0.5, layers = TRUE, seed = 3)
  expect_true(all(r$sim_titre_layers[[1]][layer1_mock == "*"] == "*"))
  expect_true(all(r$sim_titre_layers[[2]][layer2_mock == "*"] == "*"))
})

# ── params stored: layers flag ───────────────────────────────────────────────

test_that("layers = FALSE is stored in params by default", {
  r <- run_sim(seed = 1)
  expect_false(r$params$layers)
})

test_that("layers = TRUE is stored in params", {
  r <- run_sim(layers = TRUE, seed = 1)
  expect_true(r$params$layers)
})

# ── Layers: disabled (default) ───────────────────────────────────────────────

test_that("sim_titre_layers is NULL when layers = FALSE", {
  r <- run_sim(seed = 1)
  expect_null(r$sim_titre_layers)
})

# ── Layers: enabled ──────────────────────────────────────────────────────────

test_that("sim_titre_layers is a list of 2 matrices when layers = TRUE", {
  r <- run_sim(layers = TRUE, seed = 1)
  expect_type(r$sim_titre_layers, "list")
  expect_length(r$sim_titre_layers, 2L)
})

test_that("each layer matrix has the same dimensions as sim_titre", {
  r <- run_sim(layers = TRUE, seed = 1)
  for (lyr in r$sim_titre_layers) {
    expect_equal(dim(lyr), dim(r$sim_titre))
  }
})

test_that("each layer matrix carries original antigen and serum names", {
  r <- run_sim(layers = TRUE, seed = 1)
  for (lyr in r$sim_titre_layers) {
    expect_equal(rownames(lyr), rownames(ag_mock))
    expect_equal(colnames(lyr), rownames(sr_mock))
  }
})

test_that("layer names from the map are used when non-empty", {
  r <- run_sim(layers = TRUE, seed = 1)
  expect_equal(names(r$sim_titre_layers), c("Lab A", "Lab B"))
})

test_that("observed '*' cells in each layer are '*' in the simulated layer", {
  r <- run_sim(layers = TRUE, seed = 1)
  expect_true(all(r$sim_titre_layers[[1]][layer1_mock == "*"] == "*"))
  expect_true(all(r$sim_titre_layers[[2]][layer2_mock == "*"] == "*"))
})

test_that("observed non-'*' cells in each layer are not '*' in the simulated layer", {
  r <- run_sim(layers = TRUE, seed = 1)
  expect_false(any(r$sim_titre_layers[[1]][layer1_mock != "*"] == "*"))
  expect_false(any(r$sim_titre_layers[[2]][layer2_mock != "*"] == "*"))
})

test_that("per-layer missingness differs from merged missingness", {
  r <- run_sim(layers = TRUE, seed = 1)
  # layer 2 is missing AG1/SR1 (layer2_mock[1,1] == "*") but merged tt_mock[1,1] != "*"
  expect_equal(r$sim_titre_layers[[2]][1, 1], "*")
  expect_false(r$sim_titre[1, 1] == "*")
})

test_that("merged sim_titre is unchanged whether or not layers = TRUE", {
  r_no_layers <- run_sim(layers = FALSE, seed = 1)
  r_layers    <- run_sim(layers = TRUE,  seed = 1)
  expect_identical(r_no_layers$sim_titre, r_layers$sim_titre)
})

test_that("layer simulation works correctly with noise", {
  r <- run_sim(
    layers       = TRUE,
    noise_params = list(titre_noise_param = c(0, 0.5)),
    seed         = 7
  )
  expect_length(r$sim_titre_layers, 2L)
  # Layer missingness still respected despite noise
  expect_true(all(r$sim_titre_layers[[1]][layer1_mock == "*"] == "*"))
  expect_true(all(r$sim_titre_layers[[2]][layer2_mock == "*"] == "*"))
})

# ── Error handling ────────────────────────────────────────────────────────────

test_that("error when agCoords returns NULL (no optimisation run)", {
  skip_if_not_installed("Racmacs")
  mock_map <- structure(list(), class = "acmap")
  testthat::local_mocked_bindings(
    agCoords         = function(map, ...) NULL,
    srCoords         = function(map, ...) sr_mock,
    titerTable       = function(map, ...) tt_mock,
    titerTableLayers = function(map, ...) list(layer1_mock, layer2_mock),
    layerNames       = function(map, ...) character(0),
    .package         = "Racmacs"
  )
  expect_error(sim_from_map(mock_map, seed = 1), "optimisation run")
})

test_that("error when titer table dimensions disagree with coordinates", {
  skip_if_not_installed("Racmacs")
  mock_map <- structure(list(), class = "acmap")
  bad_tt <- matrix("*", nrow = 2, ncol = 3)
  testthat::local_mocked_bindings(
    agCoords         = function(map, ...) ag_mock,
    srCoords         = function(map, ...) sr_mock,
    titerTable       = function(map, ...) bad_tt,
    titerTableLayers = function(map, ...) list(layer1_mock, layer2_mock),
    layerNames       = function(map, ...) character(0),
    .package         = "Racmacs"
  )
  expect_error(sim_from_map(mock_map, seed = 1), "dimensions")
})

test_that("error when a layer matrix has wrong dimensions", {
  skip_if_not_installed("Racmacs")
  mock_map <- structure(list(), class = "acmap")
  bad_layer <- matrix("*", nrow = 2, ncol = 3)   # wrong size
  testthat::local_mocked_bindings(
    agCoords         = function(map, ...) ag_mock,
    srCoords         = function(map, ...) sr_mock,
    titerTable       = function(map, ...) tt_mock,
    titerTableLayers = function(map, ...) list(bad_layer, layer2_mock),
    layerNames       = function(map, ...) character(0),
    .package         = "Racmacs"
  )
  expect_error(sim_from_map(mock_map, layers = TRUE, seed = 1), "dimensions")
})
