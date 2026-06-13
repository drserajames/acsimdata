#' Simulate rolling-panel surveillance HI tables with factorial noise
#'
#' Generates coordinates via \code{\link{map_maker_coord}}, then produces
#' four HI titre tables using a rolling reference panel structure and a
#' 2 x 2 factorial noise design.
#'
#' \strong{Derived counts.}  The number of reference pairs (= sera) and the
#' total number of antigens are computed from the user-supplied parameters:
#' \itemize{
#'   \item \code{n_ref_pairs = n_ref_per_block + (n_blocks - 1) * ref_step}
#'   \item \code{n_antigens  = n_ref_pairs + n_blocks * n_test_ag_per_block}
#' }
#'
#' \strong{Coordinate layout.}  By default the first \code{n_ref_pairs}
#' antigens are used as the coincident reference sera.  Pass
#' \code{true_ag_coord} with reference antigen rows first and test antigen
#' rows after so that this default works without further configuration.
#' Supply a custom integer vector to \code{coincident} to choose different
#' antigen indices as reference sera.
#'
#' \strong{Rolling reference window.}  Block \code{b} has active reference
#' pair indices
#' \code{(1 + (b-1)*ref_step) : (n_ref_per_block + (b-1)*ref_step)}.
#'
#' \strong{Test antigens.}  Antigen rows not in \code{coincident} are test
#' antigens, assigned to blocks sequentially in row order.
#'
#' \strong{Factorial noise.}  A single shared serum noise draw (uniform) plus
#' two HI noise realisations (hi_A, hi_B) crossed with two antigen noise
#' realisations (ag_A, ag_B), each pre-scaled by \code{noise_scale}, yields
#' four noisy distance tables converted to HI titres.  Noise is added to
#' distances (equivalent to the log-titre scale), consistent with
#' \code{\link{add_noise}}.
#'
#' @param n_blocks Number of surveillance blocks.
#' @param n_test_ag_per_block Number of test antigens per block.
#' @param true_ag_coord Matrix of cluster centre coordinates.  Must have either
#'   one row per antigen (\code{n_ref_pairs + n_blocks * n_test_ag_per_block}
#'   rows) or fewer rows that \code{\link{map_maker_coord}} will recycle to
#'   the total antigen count.
#' @param range Within-cluster scatter range passed to
#'   \code{\link{map_maker_coord}}.
#' @param n_ref_per_block Number of active reference pairs per block (rolling
#'   window width).
#' @param ref_step Number of reference pairs added / dropped per block.
#'   Default 1.  Must satisfy \code{ref_step <= n_ref_per_block}; larger
#'   values would leave gaps in the reference panel where some pairs are never
#'   active in any block.  Together with \code{n_ref_per_block} and
#'   \code{n_blocks} this determines the total number of reference pairs:
#'   \code{n_ref_per_block + (n_blocks - 1) * ref_step}.
#' @param coincident Controls which antigen positions are reused as reference
#'   sera.  Defaults to \code{seq_len(n_ref_pairs)} (first \code{n_ref_pairs}
#'   antigens).  Pass an integer vector of the same length to choose specific
#'   indices.  Forwarded to \code{\link{map_maker_coord}}.
#' @param dimensions Number of map dimensions. Default 2.
#' @param rdistribution Distribution function for within-cluster scatter.
#'   Default \code{stats::runif}.
#' @param hi_noise_sd Standard deviation of per-titre (HI) noise before
#'   scaling. Default 1.
#' @param ag_noise_sd Standard deviation of per-antigen noise before scaling.
#'   Default 1.
#' @param noise_scale Scaling factor applied to both HI and antigen noise
#'   components. Default 0.5.
#' @param serum_noise_min Lower bound of the shared uniform serum noise.
#'   Default 0.
#' @param serum_noise_max Upper bound of the shared uniform serum noise.
#'   Default 1.
#' @param base Base for titre conversion. Default 2.
#' @param divisor Divisor for titre conversion. Default 10.
#' @param max_log_titre Maximum log titre (serum colbase). Default 9.
#' @param min_log_titre Minimum log titre floor. Default 0.
#' @param seed Integer random seed.  Used for both coordinate generation
#'   (forwarded to \code{\link{map_maker_coord}}) and noise draws (offset by
#'   1 internally).
#'
#' @return A list with:
#' \describe{
#'   \item{titre_tables}{Named list of four character matrices (hiA_agA,
#'     hiA_agB, hiB_agA, hiB_agB), each n_antigens x n_ref_pairs, with
#'     \code{"*"} for unmeasured cells.}
#'   \item{full_titre_tables}{Same four tables without the missing-data mask.}
#'   \item{true_titre_table}{Character matrix from true (noiseless) distances,
#'     no missing-data mask.}
#'   \item{ag_coord}{n_antigens x dimensions antigen coordinate matrix.}
#'   \item{sr_coord}{n_ref_pairs x dimensions serum coordinate matrix.}
#'   \item{slim_dist}{n_antigens x n_ref_pairs true distance matrix.}
#'   \item{block_active}{List of length n_blocks: active reference pair indices
#'     for each block.}
#'   \item{block_test_ag_rows}{List of length n_blocks: row indices of test
#'     antigens for each block.}
#'   \item{noise}{List: hi_A and hi_B (n_antigens x n_ref_pairs matrices),
#'     ag_A and ag_B (length-n_antigens vectors, already scaled),
#'     serum_noise (length-n_ref_pairs vector).}
#'   \item{params}{List of all resolved input parameters, including derived
#'     \code{n_ref_pairs} and \code{n_antigens}.}
#' }
#' @export
#'
#' @examples
#' # Three clusters in an equilateral triangle (side 3, scatter range 0.25).
#' # Grouped layout: 5 AGs per cluster (1 reference + 4 test), all cluster 1
#' # first, then cluster 2, then cluster 3.
#' # n_ref_per_block = 2, n_blocks = 2, ref_step = 1 (default)
#' # => n_ref_pairs = 2 + (2-1)*1 = 3; n_antigens = 3 + 2*6 = 15
#'
#' centres <- matrix(c(0, 0,  3, 0,  1.5, 3*sqrt(3)/2), ncol = 2, byrow = TRUE)
#' true_ag  <- centres[rep(1:3, each = 5), ]  # rows 1-5: cluster 1, 6-10: cluster 2, 11-15: cluster 3
#'
#' result <- sim_cluster_surv(
#'   n_blocks            = 2L,
#'   n_test_ag_per_block = 6L,
#'   true_ag_coord       = true_ag,
#'   range               = 0.25,
#'   n_ref_per_block     = 2L,
#'   coincident          = c(1L, 6L, 11L),  # first AG of each cluster = reference
#'   seed                = 1
#' )
#' dim(result$titre_tables$hiA_agA)    # 15 x 3
#' result$titre_tables$hiA_agA         # columns SR1, SR6, SR11 (matching homologous antigen); "*" marks unmeasured cells
sim_cluster_surv <- function(
  n_blocks,
  n_test_ag_per_block,
  true_ag_coord,
  range,
  n_ref_per_block,
  ref_step        = 1L,
  coincident      = NULL,
  dimensions      = 2L,
  rdistribution   = stats::runif,
  hi_noise_sd     = 1,
  ag_noise_sd     = 1,
  noise_scale     = 0.5,
  serum_noise_min = 0,
  serum_noise_max = 1,
  base            = 2,
  divisor         = 10,
  max_log_titre   = 9,
  min_log_titre   = 0,
  seed
) {
  if (missing(seed)) seed <- sample(1:1e6, 1)

  if (ref_step > n_ref_per_block) {
    stop(sprintf(
      "ref_step (%d) must be <= n_ref_per_block (%d); larger values create reference pairs that are never active in any block",
      ref_step, n_ref_per_block
    ))
  }

  n_ref_pairs <- as.integer(n_ref_per_block) + (as.integer(n_blocks) - 1L) * as.integer(ref_step)
  n_antigens  <- n_ref_pairs + as.integer(n_blocks) * as.integer(n_test_ag_per_block)

  if (is.null(coincident)) coincident <- seq_len(n_ref_pairs)

  # Sort coincident so serum columns follow antigen coordinate order (SR1 = lowest AG index)
  if (!isTRUE(coincident)) coincident <- sort(as.integer(coincident))

  # --- Coordinate generation ---
  m <- map_maker_coord(
    n_antigens    = n_antigens,
    n_sera        = n_ref_pairs,
    true_ag_coord = true_ag_coord,
    range         = range,
    dimensions    = dimensions,
    rdistribution = rdistribution,
    coincident    = coincident,
    seed          = seed
  )

  slim_dist <- m$slim_dist  # n_antigens x n_ref_pairs

  # Derive sera_idx from coincident (maps serum k → antigen row index)
  sera_idx <- if (isTRUE(coincident)) seq_len(n_ref_pairs) else coincident

  # Name serum columns to match their homologous antigen (e.g. SR1, SR6, SR11)
  serum_names <- paste0("SR", sera_idx)
  colnames(slim_dist)    <- serum_names
  rownames(m$sera_coord) <- serum_names

  n_ag <- n_antigens
  n_sr <- n_ref_pairs

  test_ag_rows <- sort(setdiff(seq_len(n_ag), sera_idx))

  # --- Rolling reference window ---
  block_active <- lapply(seq_len(n_blocks), function(b) {
    start <- 1L + (b - 1L) * as.integer(ref_step)
    end   <- start + as.integer(n_ref_per_block) - 1L
    seq.int(start, end)
  })

  # --- Test antigen block assignment (sequential) ---
  n_test_ag_per_block <- as.integer(n_test_ag_per_block)
  block_test_ag_rows <- lapply(seq_len(n_blocks), function(b) {
    idx <- seq.int((b - 1L) * n_test_ag_per_block + 1L,
                   b * n_test_ag_per_block)
    test_ag_rows[idx]
  })

  # --- Reference AG serum coverage ---
  ref_ag_active_sera <- lapply(seq_len(n_sr), function(k) {
    active_blocks <- which(vapply(block_active, function(ba) k %in% ba, logical(1)))
    sort(unique(unlist(block_active[active_blocks])))
  })

  # --- Noise (seed offset by 1 to decouple from coordinate seed) ---
  set.seed(seed + 1L)

  sr_noise_vals <- stats::runif(n_sr, serum_noise_min, serum_noise_max)
  sr_noise_mat  <- matrix(sr_noise_vals, nrow = n_ag, ncol = n_sr, byrow = TRUE)

  hi_A <- matrix(stats::rnorm(n_ag * n_sr, 0, hi_noise_sd * noise_scale),
                 nrow = n_ag, ncol = n_sr)
  hi_B <- matrix(stats::rnorm(n_ag * n_sr, 0, hi_noise_sd * noise_scale),
                 nrow = n_ag, ncol = n_sr)

  ag_A_vals <- stats::rnorm(n_ag, 0, ag_noise_sd * noise_scale)
  ag_B_vals <- stats::rnorm(n_ag, 0, ag_noise_sd * noise_scale)
  ag_A      <- matrix(ag_A_vals, nrow = n_ag, ncol = n_sr)
  ag_B      <- matrix(ag_B_vals, nrow = n_ag, ncol = n_sr)

  make_noisy_dist <- function(hi, ag) slim_dist + sr_noise_mat + hi + ag
  noisy_dists <- list(
    hiA_agA = make_noisy_dist(hi_A, ag_A),
    hiA_agB = make_noisy_dist(hi_A, ag_B),
    hiB_agA = make_noisy_dist(hi_B, ag_A),
    hiB_agB = make_noisy_dist(hi_B, ag_B)
  )

  # --- Convert to titres ---
  to_titre <- function(d) {
    dist_to_hi_titre(d, base = base, divisor = divisor,
                     max_log_titre = max_log_titre,
                     min_log_titre = min_log_titre)$lessthan_titre
  }
  full_titre_tables <- lapply(noisy_dists, to_titre)
  true_titre_table  <- to_titre(slim_dist)

  # --- Apply missing pattern ---
  apply_missing <- function(tt) {
    rm_tt   <- tt
    rm_tt[] <- "*"

    for (k in seq_len(n_sr)) {
      ag_row      <- sera_idx[k]
      active_cols <- ref_ag_active_sera[[k]]
      rm_tt[ag_row, active_cols] <- tt[ag_row, active_cols]
    }

    for (b in seq_len(n_blocks)) {
      ag_rows     <- block_test_ag_rows[[b]]
      active_cols <- block_active[[b]]
      rm_tt[ag_rows, active_cols] <- tt[ag_rows, active_cols]
    }

    rm_tt
  }

  titre_tables <- lapply(full_titre_tables, apply_missing)

  list(
    titre_tables       = titre_tables,
    full_titre_tables  = full_titre_tables,
    true_titre_table   = true_titre_table,
    ag_coord           = m$antigen_coord,
    sr_coord           = m$sera_coord,
    slim_dist          = slim_dist,
    block_active       = block_active,
    block_test_ag_rows = block_test_ag_rows,
    noise              = list(
      hi_A        = hi_A,
      hi_B        = hi_B,
      ag_A        = ag_A_vals,
      ag_B        = ag_B_vals,
      serum_noise = sr_noise_vals
    ),
    params = list(
      n_ref_pairs         = n_ref_pairs,
      n_antigens          = n_antigens,
      n_blocks            = n_blocks,
      n_ref_per_block     = n_ref_per_block,
      n_test_ag_per_block = n_test_ag_per_block,
      ref_step            = ref_step,
      coincident          = coincident,
      dimensions          = dimensions,
      hi_noise_sd         = hi_noise_sd,
      ag_noise_sd         = ag_noise_sd,
      noise_scale         = noise_scale,
      serum_noise_min     = serum_noise_min,
      serum_noise_max     = serum_noise_max,
      base                = base,
      divisor             = divisor,
      max_log_titre       = max_log_titre,
      min_log_titre       = min_log_titre,
      seed                = seed
    )
  )
}
