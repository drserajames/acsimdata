#' Simulate HI titre data from an existing Racmacs map
#'
#' Uses the antigen and serum coordinates of an optimised Racmacs antigenic map
#' as the "true" positions from which to simulate new HI titre data.  Observed
#' missingness from the real titer table is applied to the simulated data so
#' that the sparsity pattern matches the original.  Noise can optionally be
#' injected into the distances before titre conversion.
#'
#' @details
#' The pipeline is:
#' \enumerate{
#'   \item Extract antigen coordinates, serum coordinates, the merged titer
#'         table, and (optionally) per-layer titer tables from \code{map}.
#'   \item Compute pairwise Euclidean distances between all antigens and sera.
#'   \item If \code{noise_params} is a non-empty list, call
#'         \code{\link{add_noise}} with those parameters plus the distance
#'         matrix.
#'   \item Convert distances to HI titres via \code{\link{dist_to_hi_titre}}.
#'   \item Stamp \code{"*"} wherever the merged titer table contains \code{"*"}
#'         to produce \code{sim_titre}.
#'   \item If \code{layers = TRUE}, stamp \code{"*"} independently for each
#'         layer's \code{"*"} pattern to produce \code{sim_titre_layers}.
#' }
#'
#' \strong{Layer behaviour:} \code{sim_titre} and \code{sim_titre_layers} are
#' derived from the same underlying simulated titre values but with different
#' missingness masks applied.  The merged missingness (\code{sim_titre}) and
#' per-layer missingness (\code{sim_titre_layers}) are applied independently —
#' the Racmacs layer-merging algorithm is not replicated.  To build a new
#' multi-layer Racmacs map from the simulated layers use
#' \code{Racmacs::mergeMaps} on individual \code{acmap} objects constructed
#' from each element of \code{sim_titre_layers}.
#'
#' @param map A Racmacs \code{acmap} object with at least one optimisation run
#'   (so that \code{Racmacs::agCoords} and \code{Racmacs::srCoords} return
#'   non-\code{NULL} values).
#' @param layers Logical.  If \code{TRUE}, \code{Racmacs::titerTableLayers} is
#'   called and the observed missingness pattern of each layer is applied
#'   independently to the simulated titres, returning a list of per-layer
#'   matrices in \code{sim_titre_layers}.  Default \code{FALSE}.
#' @param noise_params A named list of arguments forwarded to
#'   \code{\link{add_noise}}, excluding \code{dists} (supplied automatically).
#'   Set to an empty list (the default) to skip noise entirely.  If
#'   \code{seed} is absent from the list, the top-level \code{seed} argument
#'   is used.  Example:
#'   \code{noise_params = list(titre_noise_param = c(0, 0.5))}.
#' @param base Titre base (default \code{2}, standard for HI assays).
#' @param divisor Titre divisor (default \code{10}).
#' @param max_log_titre Highest log titre.  Can be a scalar (applied to all
#'   sera) or a per-serum numeric vector of length equal to the number of sera
#'   in \code{map}.  To match the scale of the original map pass
#'   \code{max_log_titre = Racmacs::srColbases(map)}.  Default \code{9}.
#' @param min_log_titre Lowest log titre; measurements below this become
#'   threshold titres (e.g. \code{"<10"}).  Default \code{0}.
#' @param seed Integer random seed for reproducibility.  Used as the seed for
#'   \code{\link{add_noise}} (unless overridden in \code{noise_params}) and
#'   stored in \code{params}.  If omitted a random seed is drawn and stored.
#'
#' @return A list with elements:
#' \describe{
#'   \item{sim_titre}{Character matrix (antigens \eqn{\times} sera) of
#'     simulated titres with observed missingness applied.  Row and column
#'     names are the original antigen and serum names from \code{map}.  This
#'     is the main output for downstream Racmacs analysis.}
#'   \item{full_sim_titre}{Simulated titres before observed missingness is
#'     applied (same format as \code{sim_titre}).  Cells are never \code{"*"};
#'     values below the assay floor are encoded as \code{"<X"}.}
#'   \item{observed_titre}{The titer table extracted from \code{map}, for
#'     reference.}
#'   \item{dist}{Numeric matrix of pairwise Euclidean distances between
#'     antigens (rows) and sera (columns), with original antigen/serum names.}
#'   \item{sim_titre_layers}{If \code{layers = TRUE}, a named list of character
#'     matrices (one per layer) each with the same dimensions and antigen/serum
#'     names as \code{sim_titre}, with per-layer observed missingness applied.
#'     \code{NULL} when \code{layers = FALSE}.}
#'   \item{noise}{Output of \code{\link{add_noise}} if noise was applied,
#'     otherwise \code{NULL}.}
#'   \item{ag_coord}{Antigen coordinate matrix extracted from \code{map}.}
#'   \item{sr_coord}{Serum coordinate matrix extracted from \code{map}.}
#'   \item{params}{List of input parameters, with \code{seed} always present.}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' map <- Racmacs::read.acmap("my_map.ace")
#'
#' # Basic simulation — observed missingness, no noise
#' result <- sim_from_map(map, seed = 1)
#'
#' # With per-titre noise (sd = 0.5 antigenic units)
#' result_noisy <- sim_from_map(
#'   map,
#'   noise_params = list(titre_noise_param = c(0, 0.5)),
#'   seed = 1
#' )
#'
#' # Match the scale of the original map using the observed column bases
#' result_colbases <- sim_from_map(
#'   map,
#'   max_log_titre = Racmacs::srColbases(map),
#'   seed = 1
#' )
#'
#' # Use the simulated titre table to build a new Racmacs map
#' new_map <- Racmacs::acmap(titer_table = result$sim_titre)
#'
#' # Preserve layer structure — one simulated table per observed layer
#' result_layers <- sim_from_map(map, layers = TRUE, seed = 1)
#' layer_maps <- lapply(result_layers$sim_titre_layers, function(tt) {
#'   Racmacs::acmap(titer_table = tt)
#' })
#' }
sim_from_map <- function(map,
                          layers        = FALSE,
                          noise_params  = list(),
                          base          = 2,
                          divisor       = 10,
                          max_log_titre = 9,
                          min_log_titre = 0,
                          seed) {

  # ── 1. Extract from acmap ────────────────────────────────────────────────────
  ag_coord       <- Racmacs::agCoords(map)
  sr_coord       <- Racmacs::srCoords(map)
  observed_titre <- Racmacs::titerTable(map)

  if (is.null(ag_coord) || is.null(sr_coord)) {
    stop(
      "'map' must have at least one optimisation run ",
      "(Racmacs::agCoords / srCoords returned NULL)"
    )
  }

  n_ag     <- nrow(ag_coord)
  n_sr     <- nrow(sr_coord)
  ag_names <- rownames(ag_coord)
  sr_names <- rownames(sr_coord)

  # Validate titer table dimensions
  if (!is.null(observed_titre)) {
    if (nrow(observed_titre) != n_ag || ncol(observed_titre) != n_sr) {
      stop(
        "Titer table dimensions (", nrow(observed_titre), " x ", ncol(observed_titre),
        ") do not match coordinate dimensions (", n_ag, " x ", n_sr, ")"
      )
    }
  }

  # ── 2. Pairwise distances (slim n_ag x n_sr matrix) ─────────────────────────
  # Rename internally to "AG..."/"SR..." so add_noise / dist_to_hi_titre can
  # identify antigens and sera by name as they expect.
  ag_int <- ag_coord
  sr_int <- sr_coord
  rownames(ag_int) <- paste0("AG", seq_len(n_ag))
  rownames(sr_int) <- paste0("SR", seq_len(n_sr))

  slim_dist <- matrix(
    0, nrow = n_ag, ncol = n_sr,
    dimnames = list(
      paste0("AG", seq_len(n_ag)),
      paste0("SR", seq_len(n_sr))
    )
  )
  for (i in seq_len(n_ag)) {
    diffs          <- sweep(sr_int, 2L, ag_int[i, ], `-`)
    slim_dist[i, ] <- sqrt(rowSums(diffs^2))
  }

  # ── 3. Seed ──────────────────────────────────────────────────────────────────
  if (missing(seed)) seed <- sample(1:1e6, 1)

  # ── 4. Optional noise ────────────────────────────────────────────────────────
  noise_result <- NULL
  if (length(noise_params) > 0) {
    np       <- noise_params
    np$dists <- slim_dist
    if (!"seed" %in% names(np)) np$seed <- seed
    noise_result   <- do.call(add_noise, np)
    dist_for_titre <- noise_result$noise_dist_table
  } else {
    dist_for_titre <- slim_dist
  }

  # ── 5. Convert distances to titres ──────────────────────────────────────────
  titre_result <- dist_to_hi_titre(
    dist_for_titre,
    base          = base,
    divisor       = divisor,
    max_log_titre = max_log_titre,
    min_log_titre = min_log_titre
  )
  sim_titre <- titre_result$lessthan_titre   # character matrix, AG.../SR... names

  # ── 6. Apply observed missingness ────────────────────────────────────────────
  sim_titre_miss <- sim_titre
  if (!is.null(observed_titre)) {
    sim_titre_miss[observed_titre == "*"] <- "*"
  }

  # ── 7. Restore original antigen / serum names ────────────────────────────────
  rownames(sim_titre_miss) <- ag_names
  colnames(sim_titre_miss) <- sr_names
  rownames(sim_titre)      <- ag_names
  colnames(sim_titre)      <- sr_names
  rownames(slim_dist)      <- ag_names
  colnames(slim_dist)      <- sr_names

  # ── 8. Per-layer missingness (optional) ──────────────────────────────────────
  # sim_titre here is the pre-missingness full simulation with real names.
  # Each layer matrix from Racmacs may lack dimnames; matching is positional,
  # which is safe because the layer matrices have the same n_ag x n_sr layout
  # as the merged titer table — both derived from the same acmap.
  sim_titre_layers <- NULL
  if (layers) {
    titre_layers <- Racmacs::titerTableLayers(map)
    sim_titre_layers <- lapply(titre_layers, function(lyr) {
      if (nrow(lyr) != n_ag || ncol(lyr) != n_sr) {
        stop(
          "A titer layer has dimensions (", nrow(lyr), " x ", ncol(lyr),
          ") that do not match the coordinate dimensions (", n_ag, " x ", n_sr, ")"
        )
      }
      out        <- sim_titre   # full simulation, real names, no missingness
      out[lyr == "*"] <- "*"
      out
    })
    layer_nms <- Racmacs::layerNames(map)
    if (length(layer_nms) == length(sim_titre_layers) &&
        any(nzchar(layer_nms))) {
      names(sim_titre_layers) <- layer_nms
    }
  }

  list(
    sim_titre        = sim_titre_miss,
    full_sim_titre   = sim_titre,
    sim_titre_layers = sim_titre_layers,
    observed_titre   = observed_titre,
    dist             = slim_dist,
    noise            = noise_result,
    ag_coord         = ag_coord,
    sr_coord         = sr_coord,
    params           = list(
      layers        = layers,
      noise_params  = noise_params,
      base          = base,
      divisor       = divisor,
      max_log_titre = max_log_titre,
      min_log_titre = min_log_titre,
      seed          = seed
    )
  )
}
