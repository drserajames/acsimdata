#' Simulate rolling-panel surveillance HI tables with factorial noise
#'
#' Generates coordinates via \code{\link{map_maker_coord}}, then produces
#' four HI titre tables using a rolling reference panel structure and a
#' 2 x 2 factorial noise design.
#'
#' \strong{Coordinate layout.}  By default the first \code{n_ref_pairs}
#' antigens are used as the coincident reference sera
#' (\code{coincident = seq_len(n_ref_pairs)}).  Pass \code{true_ag_coord}
#' with reference antigen rows first and test antigen rows after so that this
#' default works without further configuration.  Supply a custom integer vector
#' to \code{coincident} to choose different antigen indices as reference sera.
#'
#' \strong{Rolling reference window.}  Block \code{b} has active reference
#' pair indices
#' \code{(1 + (b-1)*ref_step) : min(n_ref_pairs, 1 + (b-1)*ref_step + n_ref_per_block - 1)}.
#' Reference pairs near the end of the sequence may fall in fewer blocks if
#' \code{n_ref_pairs < n_ref_per_block + (n_blocks - 1) * ref_step}.
#'
#' \strong{Test antigens.}  Rows of \code{slim_dist} not in \code{sera_idx}
#' (derived from \code{coincident}) are test antigens, assigned to blocks
#' sequentially in row order.
#'
#' \strong{Factorial noise.}  A single shared serum noise draw (uniform) plus
#' two HI noise realisations (hi_A, hi_B) crossed with two antigen noise
#' realisations (ag_A, ag_B), each pre-scaled by \code{noise_scale}, yields
#' four noisy distance tables converted to HI titres.  Noise is added to
#' distances (equivalent to the log-titre scale), consistent with
#' \code{\link{add_noise}}.
#'
#' @param n_antigens Total number of antigen points.
#' @param n_ref_pairs Number of reference antigen-serum pairs (= number of
#'   sera).
#' @param true_ag_coord Matrix of cluster centre coordinates, one row per
#'   antigen (or one row per cluster, recycled to \code{n_antigens} rows by
#'   \code{\link{map_maker_coord}}).
#' @param range Within-cluster scatter range passed to
#'   \code{\link{map_maker_coord}}.
#' @param n_blocks Number of surveillance blocks.
#' @param n_ref_per_block Number of active reference pairs per block (rolling
#'   window width).
#' @param n_test_ag_per_block Number of test antigens per block.
#' @param ref_step Number of reference pairs added / dropped per block.
#'   Default 2.
#' @param coincident Controls which antigen positions are reused as reference
#'   sera.  Defaults to \code{seq_len(n_ref_pairs)} (first \code{n_ref_pairs}
#'   antigens).  Pass an integer vector of length \code{n_ref_pairs} to choose
#'   specific indices.  Forwarded to \code{\link{map_maker_coord}}.
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
#'   \item{params}{List of all resolved input parameters.}
#' }
#' @export
#'
#' @examples
#' # Three clusters in an equilateral triangle (side 3, scatter range 0.25).
#' # Reference antigen rows first (one per cluster), test antigens after.
#'
#' centres <- matrix(c(0, 0,  3, 0,  1.5, 3*sqrt(3)/2), ncol = 2, byrow = TRUE)
#' true_ag  <- rbind(centres,                        # rows 1-3:  reference AGs
#'                   centres[rep(1:3, each = 4), ])  # rows 4-15: test AGs
#'
#' result <- sim_cluster_surv(
#'   n_antigens          = 15L,
#'   n_ref_pairs         = 3L,
#'   true_ag_coord       = true_ag,
#'   range               = 0.25,
#'   n_blocks            = 2L,
#'   n_ref_per_block     = 2L,
#'   n_test_ag_per_block = 6L,
#'   seed                = 1
#' )
#' dim(result$titre_tables$hiA_agA)  # 15 x 3
#' result$titre_tables$hiA_agA       # "*" marks unmeasured cells
sim_cluster_surv <- function(
  n_antigens,
  n_ref_pairs,
  true_ag_coord,
  range,
  n_blocks,
  n_ref_per_block,
  n_test_ag_per_block,
  ref_step        = 2L,
  coincident      = seq_len(n_ref_pairs),
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
  sera_idx <- if (isTRUE(coincident)) seq_len(n_ref_pairs) else as.integer(coincident)

  n_ag <- n_antigens
  n_sr <- n_ref_pairs

  # --- Validate block structure ---
  test_ag_rows <- sort(setdiff(seq_len(n_ag), sera_idx))
  n_test_ag    <- length(test_ag_rows)
  expected_test <- as.integer(n_blocks) * as.integer(n_test_ag_per_block)
  if (n_test_ag != expected_test) {
    stop(sprintf(
      "n_blocks (%d) * n_test_ag_per_block (%d) = %d but there are %d test antigen rows (n_antigens - n_ref_pairs = %d - %d)",
      n_blocks, n_test_ag_per_block, expected_test, n_test_ag, n_ag, n_sr
    ))
  }

  # --- Rolling reference window ---
  block_active <- lapply(seq_len(n_blocks), function(b) {
    start <- 1L + (b - 1L) * as.integer(ref_step)
    end   <- min(n_sr, start + as.integer(n_ref_per_block) - 1L)
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
      n_antigens          = n_antigens,
      n_ref_pairs         = n_ref_pairs,
      range               = range,
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
