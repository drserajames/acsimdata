#' Simulate rolling-panel surveillance HI tables with factorial noise
#'
#' Given a pre-computed antigen-serum distance matrix, generates four HI titre
#' tables using a rolling reference panel structure and a 2 x 2 factorial
#' noise design.
#'
#' \strong{Inputs.}  The caller is responsible for generating \code{slim_dist}
#' (e.g. via \code{\link{map_maker_coord}} or \code{\link{map_maker_random}})
#' and choosing \code{sera_idx}.  This separates coordinate generation from
#' surveillance structure, so any distance matrix can be used.
#'
#' \strong{Reference panel.}  \code{sera_idx[k]} gives the row index of the
#' antigen that is homologous to serum column \code{k}.  These reference
#' antigens are measured against all reference sera that share at least one
#' active window with them.
#'
#' \strong{Test antigens.}  Rows of \code{slim_dist} not in \code{sera_idx}
#' are test antigens.  They are assigned to blocks sequentially in row order
#' (block 1 gets the first \code{n_test_ag_per_block} test AG rows, block 2
#' gets the next batch, and so on).  Pass a custom \code{block_assignments}
#' list to override this.
#'
#' \strong{Rolling window.}  Block \code{b} has active reference pair indices
#' \code{(1 + (b-1)*ref_step) : min(n_sr, 1 + (b-1)*ref_step + ref_window - 1)}.
#' Reference pairs near the end of the sequence may appear in fewer blocks.
#'
#' \strong{Factorial noise.}  A single shared serum noise draw (uniform) plus
#' two HI noise realisations (hi_A, hi_B) crossed with two antigen noise
#' realisations (ag_A, ag_B), each pre-scaled by \code{noise_scale}, yields
#' four noisy distance tables that are converted to HI titres.
#'
#' @param slim_dist Numeric matrix of true antigen-serum distances
#'   (n_ag x n_sr) with rownames matching \code{"AG..."} and colnames matching
#'   \code{"SR..."} (as produced by \code{map_maker_coord} or
#'   \code{map_maker_random}).
#' @param sera_idx Integer vector of length \code{ncol(slim_dist)}: row index
#'   of the antigen homologous to each serum column.  \code{sera_idx[k]} is
#'   the reference antigen for serum \code{k}.
#' @param n_blocks Number of surveillance blocks.
#' @param n_test_ag_per_block Number of test antigens per block.  Defaults to
#'   \code{floor(n_test_ag / n_blocks)} where \code{n_test_ag} is the number
#'   of rows in \code{slim_dist} not in \code{sera_idx}.  An error is raised
#'   if the default does not divide evenly.
#' @param block_assignments Optional list of length \code{n_blocks}: each
#'   element is an integer vector of test-AG row indices (rows of
#'   \code{slim_dist}) for that block.  Overrides the sequential default.
#' @param ref_window Number of active reference pairs per block. Default 12.
#' @param ref_step Number of reference pairs added / dropped per block.
#'   Default 2.
#' @param hi_noise_sd Standard deviation of per-titre (HI) noise before
#'   scaling. Default 1.
#' @param ag_noise_sd Standard deviation of per-antigen noise before scaling.
#'   Default 1.
#' @param noise_scale Scaling factor applied to both noise components.
#'   Default 0.5.
#' @param serum_noise_min Lower bound of the shared uniform serum noise.
#'   Default 0.
#' @param serum_noise_max Upper bound of the shared uniform serum noise.
#'   Default 1.
#' @param base Base for titre conversion. Default 2.
#' @param divisor Divisor for titre conversion. Default 10.
#' @param max_log_titre Maximum log titre (serum colbase). Default 9.
#' @param min_log_titre Minimum log titre floor. Default 0.
#' @param seed Integer random seed for reproducibility.
#'
#' @return A list with:
#' \describe{
#'   \item{titre_tables}{Named list of four character matrices (hiA_agA,
#'     hiA_agB, hiB_agA, hiB_agB), each n_ag x n_sr, with \code{"*"} for
#'     unmeasured cells.}
#'   \item{full_titre_tables}{Same four tables without the missing-data mask.}
#'   \item{true_titre_table}{Character matrix from true (noiseless) distances,
#'     no missing-data mask.}
#'   \item{slim_dist}{The input distance matrix (unchanged).}
#'   \item{block_active}{List of length n_blocks: active reference pair indices
#'     (1-based within 1:n_sr) for each block.}
#'   \item{block_test_ag_rows}{List of length n_blocks: row indices of the test
#'     antigens for each block.}
#'   \item{noise}{List: hi_A and hi_B (n_ag x n_sr matrices), ag_A and ag_B
#'     (length-n_ag vectors, already scaled), serum_noise (length-n_sr
#'     vector).}
#'   \item{params}{List of all resolved input parameters.}
#' }
#' @export
#'
#' @examples
#' # 3-cluster equilateral triangle (side 3, within-cluster range 0.25)
#' centres    <- matrix(c(0, 0, 3, 0, 1.5, 3*sqrt(3)/2), ncol = 2, byrow = TRUE)
#' n_ag_per_cluster <- 628L
#' true_ag    <- centres[rep(1:3, each = n_ag_per_cluster), ]
#' sera_idx   <- c(1:16, 629:644, 1257:1272)  # 16 reference pairs per cluster
#' m <- map_maker_coord(1884L, 48L, true_ag, range = 0.25,
#'                      coincident = sera_idx, seed = 1)
#' result <- sim_cluster_surv(m$slim_dist, sera_idx, n_blocks = 18L)
#' dim(result$titre_tables$hiA_agA)  # 1884 x 48
sim_cluster_surv <- function(
  slim_dist,
  sera_idx,
  n_blocks,
  n_test_ag_per_block = NULL,
  block_assignments   = NULL,
  ref_window          = 12L,
  ref_step            = 2L,
  hi_noise_sd         = 1,
  ag_noise_sd         = 1,
  noise_scale         = 0.5,
  serum_noise_min     = 0,
  serum_noise_max     = 1,
  base                = 2,
  divisor             = 10,
  max_log_titre       = 9,
  min_log_titre       = 0,
  seed
) {
  n_ag <- nrow(slim_dist)
  n_sr <- ncol(slim_dist)

  # --- Validation ---
  sera_idx <- as.integer(sera_idx)
  if (length(sera_idx) != n_sr) {
    stop(sprintf(
      "sera_idx must have length ncol(slim_dist) = %d (got %d)",
      n_sr, length(sera_idx)
    ))
  }
  if (any(sera_idx < 1L | sera_idx > n_ag)) {
    stop("all sera_idx values must be in 1:nrow(slim_dist)")
  }

  if (missing(seed)) seed <- sample(1:1e6, 1)

  # --- Block assignments for test antigens ---
  test_ag_rows <- sort(setdiff(seq_len(n_ag), sera_idx))
  n_test_ag    <- length(test_ag_rows)

  if (!is.null(block_assignments)) {
    if (length(block_assignments) != n_blocks) {
      stop("block_assignments must be a list of length n_blocks")
    }
    block_test_ag_rows <- block_assignments
  } else {
    if (is.null(n_test_ag_per_block)) {
      if (n_test_ag %% n_blocks != 0L) {
        stop(sprintf(
          "n_test_ag (%d) is not divisible by n_blocks (%d); supply n_test_ag_per_block or block_assignments explicitly",
          n_test_ag, n_blocks
        ))
      }
      n_test_ag_per_block <- n_test_ag %/% n_blocks
    }
    block_test_ag_rows <- lapply(seq_len(n_blocks), function(b) {
      idx <- seq.int((b - 1L) * n_test_ag_per_block + 1L,
                     b * n_test_ag_per_block)
      test_ag_rows[idx]
    })
  }

  # --- Rolling reference window ---
  block_active <- lapply(seq_len(n_blocks), function(b) {
    start <- 1L + (b - 1L) * as.integer(ref_step)
    end   <- min(n_sr, start + as.integer(ref_window) - 1L)
    seq.int(start, end)
  })

  # --- Reference AG serum coverage ---
  ref_ag_active_sera <- lapply(seq_len(n_sr), function(k) {
    active_blocks <- which(vapply(block_active, function(ba) k %in% ba, logical(1)))
    sort(unique(unlist(block_active[active_blocks])))
  })

  # --- Noise ---
  set.seed(seed)

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

  # Factorial noisy distance tables (inherit dimnames from slim_dist)
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
      sera_idx            = sera_idx,
      n_blocks            = n_blocks,
      n_test_ag_per_block = n_test_ag_per_block,
      ref_window          = ref_window,
      ref_step            = ref_step,
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
