# ── Internal helpers ──────────────────────────────────────────────────────────

# Build sliding-window membership starting from start_idx.
# Table 1: all n_per_table[1] points are new.
# Table t > 1: keep the last n_overlap from table t-1, add n_per_table[t] - n_overlap new ones.
# With n_overlap == n_per_table (simple case): 0 new points after table 1 → fixed set.
# With n_overlap == 0: entirely fresh points every table → table-specific.
.build_membership <- function(n_per_table, n_overlap, n_tables, start_idx = 1L) {
  membership  <- vector("list", n_tables)
  intro_table <- integer(0)
  next_idx    <- as.integer(start_idx)
  for (t in seq_len(n_tables)) {
    n_new   <- if (t == 1L) n_per_table[t] else n_per_table[t] - n_overlap
    new_idx <- if (n_new > 0L) seq_len(n_new) + next_idx - 1L else integer(0)
    next_idx    <- next_idx + max(0L, n_new)
    intro_table <- c(intro_table, rep(t, max(0L, n_new)))
    membership[[t]] <- if (t == 1L) new_idx else c(tail(membership[[t - 1L]], n_overlap), new_idx)
  }
  list(membership = membership, intro_table = intro_table, total = next_idx - 1L)
}

# Build probabilistic reference-serum membership.
#
# Two dropping mechanisms (mutually exclusive; halflife takes precedence):
#
#   half-life mode  (halflife non-NULL):
#     At each transition every serum in the panel is independently dropped
#     with probability  p_drop_each = 1 - 2^(-1 / halflife).
#     This is a memoryless geometric-survival model — any serum can survive
#     longer than its half-life by chance, producing a realistic long tail.
#
#   oldest-first mode  (halflife NULL, p_drop > 0):
#     The single oldest (first-in) serum is dropped with probability p_drop.
#
# In both modes a new serum is independently added with probability p_gain.
# Called after set.seed(), so all draws are reproducible.
.build_ref_sr_probabilistic <- function(n_init, p_drop = 0, p_gain = 0,
                                        halflife = NULL, n_tables,
                                        start_idx = 1L) {
  use_halflife <- !is.null(halflife)
  p_drop_each  <- if (use_halflife) 1 - 2^(-1 / halflife) else NULL

  membership   <- vector("list", n_tables)
  intro_table  <- rep(1L, n_init)
  next_idx     <- as.integer(start_idx) + n_init
  current_pool <- seq(as.integer(start_idx), length.out = n_init)
  membership[[1L]] <- current_pool

  for (t in seq_len(n_tables - 1L) + 1L) {
    if (use_halflife) {
      # Each serum independently dropped
      if (length(current_pool) > 0L) {
        keep         <- stats::runif(length(current_pool)) >= p_drop_each
        current_pool <- current_pool[keep]
      }
    } else {
      # Drop the oldest (first-in) serum with probability p_drop
      if (length(current_pool) > 0L && stats::runif(1L) < p_drop) {
        current_pool <- current_pool[-1L]
      }
    }
    # Gain a new serum with probability p_gain
    if (stats::runif(1L) < p_gain) {
      current_pool <- c(current_pool, next_idx)
      intro_table  <- c(intro_table, t)
      next_idx     <- next_idx + 1L
    }
    membership[[t]] <- current_pool
  }

  list(
    membership  = membership,
    intro_table = intro_table,
    total       = next_idx - 1L
  )
}


#' Simulate multi-period surveillance HI data
#'
#' Generates a series of HI titre tables mimicking the structure of
#' influenza surveillance data. Each table contains two groups of antigens and
#' sera:
#' \itemize{
#'   \item \strong{Reference} antigens/sera — a shared set that spans multiple
#'     tables, analogous to the reference strains used across consecutive
#'     surveillance periods.
#'   \item \strong{Table-specific} antigens/sera — unique to a single table
#'     and never repeated, analogous to the current circulating strains
#'     characterised in each period.
#' }
#'
#' \strong{Reference serum turnover — deterministic mode} (default): set
#' \code{n_ref_sr_overlap} (or omit it for a fully fixed panel).  At each
#' transition exactly \code{n_ref_sr - n_ref_sr_overlap} sera are replaced.
#'
#' \strong{Reference serum turnover — probabilistic mode}: provide at least one
#' of \code{p_sr_drop}, \code{p_sr_gain}, or \code{sr_halflife}.  When either
#' probability parameter is non-\code{NULL}, \code{n_ref_sr_overlap} is ignored.
#' Two dropping sub-modes are available (mutually exclusive):
#' \itemize{
#'   \item \strong{Oldest-first} (\code{p_sr_drop}): at each transition the
#'     single oldest reference serum in the panel is dropped with probability
#'     \code{p_sr_drop}.
#'   \item \strong{Half-life} (\code{sr_halflife}): every serum in the panel is
#'     independently and identically retired at each transition with probability
#'     \eqn{1 - 2^{-1/\text{sr\_halflife}}}.  This memoryless geometric-survival
#'     model lets older sera persist by chance, generating the long-tailed
#'     persistence distributions seen in real surveillance data.  A serum with
#'     half-life \eqn{h} has a 50\% chance of surviving at least \eqn{h}
#'     transitions. Cannot be combined with \code{p_sr_drop}.
#' }
#' In both sub-modes a new serum is independently added at each transition with
#' probability \code{p_sr_gain}.
#'
#' \strong{Reference antigen turnover}: controlled deterministically via
#' \code{n_ref_ag_overlap} only (same sliding-window logic as the deterministic
#' serum mode).
#'
#' Spatial positions drift along the first dimension by \code{ag_drift} units
#' per table period based on when each antigen or serum is introduced,
#' reflecting antigenic evolution over time.
#'
#' @param n_tables Number of sequential tables (time periods).
#' @param n_ag_per_table Number of \emph{table-specific} (non-reference)
#'   antigens per table. Either a single integer (same for all tables) or a
#'   vector of length \code{n_tables}.
#' @param n_sr_per_table Number of \emph{table-specific} sera per table.
#'   Either a single integer or a vector of length \code{n_tables}.
#' @param n_ref_ag Number of reference antigens per table. Either a single
#'   integer or a vector of length \code{n_tables}. Default 0 (no references).
#' @param n_ref_sr Starting number of reference sera. In deterministic mode
#'   this is held fixed per table; in probabilistic mode it is the panel size
#'   at table 1 only. Default 0.
#' @param n_ref_ag_overlap Number of reference antigens carried over between
#'   consecutive tables. Must not exceed any element of \code{n_ref_ag}.
#'   Defaults to \code{n_ref_ag[1]}, giving a fixed reference antigen set.
#'   Set lower for a rotating antigen reference set.
#' @param n_ref_sr_overlap Number of reference sera retained between consecutive
#'   tables (deterministic mode). Ignored when \code{p_sr_drop} or
#'   \code{p_sr_gain} is non-\code{NULL}. Defaults to \code{n_ref_sr[1]}
#'   (fixed panel).
#' @param p_sr_drop Probability (in \eqn{[0, 1]}) that the single oldest
#'   reference serum is dropped at each table transition (oldest-first mode).
#'   Cannot be used together with \code{sr_halflife}. Default \code{NULL}.
#' @param p_sr_gain Probability (in \eqn{[0, 1]}) that a new reference serum
#'   is added at each table transition (probabilistic mode). Works with both
#'   \code{p_sr_drop} and \code{sr_halflife}. Default \code{NULL}.
#' @param sr_halflife Expected number of table transitions a reference serum
#'   survives before being retired (half-life mode). Each serum in the panel is
#'   independently retired at each transition with probability
#'   \eqn{1 - 2^{-1/\text{sr\_halflife}}}, so a serum has a 50\% chance of
#'   lasting at least \code{sr_halflife} transitions. Must be a single positive
#'   number. Cannot be combined with \code{p_sr_drop}. Default \code{NULL}.
#' @param ag_drift Antigenic distance drifted per table period, applied along
#'   the first map dimension.
#' @param range Maximum within-cluster spatial scatter of individual points
#'   around their cluster centre (upper bound passed to \code{rdistribution}).
#' @param dimensions Number of map dimensions.
#' @param base Base for titre conversion (default 2, standard for HI assays).
#' @param divisor Divisor for titre conversion (default 10).
#' @param max_log_titre Highest log titre possible, applied uniformly to all
#'   sera.
#' @param min_log_titre Lowest detectable log titre; measurements below this
#'   become threshold titres (e.g. \code{"<10"}).
#' @param rdistribution Random distribution function for within-cluster scatter.
#'   Default is \code{stats::runif}.
#' @param seed Random seed for reproducibility.
#'
#' @return A list with elements:
#' \describe{
#'   \item{merged_titre_table}{Full merged HI table across all time periods.
#'     Untested combinations are \code{"*"}. Where a pair appears in multiple
#'     tables (reference overlap), the most recent value is used.}
#'   \item{ag_coord}{Matrix of true antigen coordinates
#'     (total antigens x dimensions).}
#'   \item{sr_coord}{Matrix of true serum coordinates
#'     (total sera x dimensions).}
#'   \item{ag_table_membership}{Named list: antigen names present in each
#'     table. Reference antigens appear first, then table-specific.}
#'   \item{sr_table_membership}{Named list: serum names present in each table.
#'     Reference sera appear first, then table-specific.}
#'   \item{tables}{Named list of per-table results, each being the output of
#'     \code{\link{dist_to_hi_titre}}.}
#'   \item{params}{List of input parameters, with size vectors as expanded.}
#' }
#' @export
#'
#' @examples
#' # Simple case: 3 fixed reference antigens, 5 table-specific per table
#' sim_surv_tables(
#'   n_tables = 5, n_ag_per_table = 5, n_sr_per_table = 4,
#'   n_ref_ag = 3, n_ref_sr = 2, ag_drift = 3, range = 1, seed = 1
#' )
#'
#' # Rotating reference set (2 of 4 refs replaced each table)
#' sim_surv_tables(
#'   n_tables = 5, n_ag_per_table = 5, n_sr_per_table = 4,
#'   n_ref_ag = 4, n_ref_sr = 3, n_ref_ag_overlap = 2, n_ref_sr_overlap = 2,
#'   ag_drift = 3, range = 1, seed = 1
#' )
#'
#' # Oldest-first probabilistic turnover: 40% chance of dropping oldest per transition
#' sim_surv_tables(
#'   n_tables = 10, n_ag_per_table = 5, n_sr_per_table = 4,
#'   n_ref_ag = 3, n_ref_sr = 4, p_sr_drop = 0.4, p_sr_gain = 0.4,
#'   ag_drift = 3, range = 1, seed = 1
#' )
#'
#' # Half-life turnover: each serum has a 50% chance of surviving 6 transitions
#' sim_surv_tables(
#'   n_tables = 20, n_ag_per_table = 5, n_sr_per_table = 4,
#'   n_ref_ag = 3, n_ref_sr = 8, sr_halflife = 6, p_sr_gain = 0.4,
#'   ag_drift = 3, range = 1, seed = 1
#' )
sim_surv_tables <- function(n_tables,
                            n_ag_per_table,
                            n_sr_per_table,
                            n_ref_ag         = 0L,
                            n_ref_sr         = 0L,
                            n_ref_ag_overlap = NULL,
                            n_ref_sr_overlap = NULL,
                            p_sr_drop        = NULL,
                            p_sr_gain        = NULL,
                            sr_halflife      = NULL,
                            ag_drift = 3, range = 1, dimensions = 2,
                            base = 2, divisor = 10,
                            max_log_titre = 9, min_log_titre = 0,
                            rdistribution = stats::runif,
                            seed) {

  use_prob_sr <- !is.null(p_sr_drop) || !is.null(p_sr_gain) || !is.null(sr_halflife)

  # ── Validate / default overlap parameters ────────────────────────────────────
  if (is.null(n_ref_ag_overlap)) n_ref_ag_overlap <- n_ref_ag[1L]

  if (!use_prob_sr) {
    if (is.null(n_ref_sr_overlap)) n_ref_sr_overlap <- n_ref_sr[1L]
  } else {
    if (!is.null(sr_halflife) && !is.null(p_sr_drop)) {
      stop("Specify either sr_halflife or p_sr_drop, not both")
    }
    if (!is.null(sr_halflife) && (length(sr_halflife) != 1L || sr_halflife <= 0)) {
      stop("sr_halflife must be a single positive number")
    }
    p_sr_drop <- if (is.null(p_sr_drop)) 0 else p_sr_drop
    p_sr_gain <- if (is.null(p_sr_gain)) 0 else p_sr_gain
    if (p_sr_drop < 0 || p_sr_drop > 1) stop("p_sr_drop must be in [0, 1]")
    if (p_sr_gain < 0 || p_sr_gain > 1) stop("p_sr_gain must be in [0, 1]")
  }

  # Recycle scalar size params to length n_tables
  n_ag_per_table <- rep_len(n_ag_per_table, n_tables)
  n_sr_per_table <- rep_len(n_sr_per_table, n_tables)
  n_ref_ag       <- rep_len(n_ref_ag,       n_tables)
  n_ref_sr       <- rep_len(n_ref_sr,       n_tables)

  if (any(n_ref_ag_overlap > n_ref_ag)) {
    stop("n_ref_ag_overlap cannot exceed n_ref_ag")
  }
  if (!use_prob_sr && any(n_ref_sr_overlap > n_ref_sr)) {
    stop("n_ref_sr_overlap cannot exceed n_ref_sr")
  }

  if (missing(seed)) seed <- sample(1:1e6, 1)
  set.seed(seed)

  # Reference membership (sliding window; fixed when n_overlap == n_ref)
  ref_ag_built  <- .build_membership(n_ref_ag, n_ref_ag_overlap, n_tables, start_idx = 1L)

  if (use_prob_sr) {
    ref_sr_built <- .build_ref_sr_probabilistic(
      n_init    = n_ref_sr[1L],
      p_drop    = p_sr_drop,
      p_gain    = p_sr_gain,
      halflife  = sr_halflife,
      n_tables  = n_tables,
      start_idx = 1L
    )
  } else {
    ref_sr_built <- .build_membership(n_ref_sr, n_ref_sr_overlap, n_tables, start_idx = 1L)
  }

  # Table-specific membership (no overlap: each point appears in exactly one table)
  spec_ag_built <- .build_membership(n_ag_per_table, 0L, n_tables,
                                     start_idx = ref_ag_built$total + 1L)
  spec_sr_built <- .build_membership(n_sr_per_table, 0L, n_tables,
                                     start_idx = ref_sr_built$total + 1L)

  total_ag <- spec_ag_built$total
  total_sr <- spec_sr_built$total

  # Combined membership per table: references first, then table-specific
  ag_in_table <- lapply(seq_len(n_tables), function(t) {
    paste0("AG", c(ref_ag_built$membership[[t]], spec_ag_built$membership[[t]]))
  })
  sr_in_table <- lapply(seq_len(n_tables), function(t) {
    paste0("SR", c(ref_sr_built$membership[[t]], spec_sr_built$membership[[t]]))
  })
  names(ag_in_table) <- names(sr_in_table) <- paste0("table", seq_len(n_tables))

  # Introduction table per point drives the spatial drift
  ag_intro_table <- c(ref_ag_built$intro_table, spec_ag_built$intro_table)
  sr_intro_table <- c(ref_sr_built$intro_table, spec_sr_built$intro_table)

  # Coordinates: scatter within [0, range] around a centre that drifts along x
  ag_coord <- matrix(rdistribution(total_ag * dimensions, 0, range),
                     nrow = total_ag, ncol = dimensions)
  ag_coord[, 1] <- ag_coord[, 1] + (ag_intro_table - 1L) * ag_drift
  rownames(ag_coord) <- paste0("AG", seq_len(total_ag))

  sr_coord <- matrix(rdistribution(total_sr * dimensions, 0, range),
                     nrow = total_sr, ncol = dimensions)
  sr_coord[, 1] <- sr_coord[, 1] + (sr_intro_table - 1L) * ag_drift
  rownames(sr_coord) <- paste0("SR", seq_len(total_sr))

  # Full pairwise distance matrix
  all_coord <- rbind(ag_coord, sr_coord)
  full_dist <- as.matrix(stats::dist(all_coord))

  # Per-table titre results
  tables <- lapply(seq_len(n_tables), function(t) {
    sub_dist <- full_dist[ag_in_table[[t]], sr_in_table[[t]], drop = FALSE]
    dist_to_hi_titre(sub_dist, base = base, divisor = divisor,
                     max_log_titre = max_log_titre, min_log_titre = min_log_titre)
  })
  names(tables) <- paste0("table", seq_len(n_tables))

  # Merged titre table: "*" where untested, most-recent value where overlapping
  all_ag_names <- paste0("AG", seq_len(total_ag))
  all_sr_names <- paste0("SR", seq_len(total_sr))
  merged <- matrix("*", nrow = total_ag, ncol = total_sr,
                   dimnames = list(all_ag_names, all_sr_names))
  for (t in seq_len(n_tables)) {
    merged[ag_in_table[[t]], sr_in_table[[t]]] <- tables[[t]]$lessthan_titre
  }

  list(
    merged_titre_table  = merged,
    ag_coord            = ag_coord,
    sr_coord            = sr_coord,
    ag_table_membership = ag_in_table,
    sr_table_membership = sr_in_table,
    tables              = tables,
    params = list(
      n_tables         = n_tables,
      n_ag_per_table   = n_ag_per_table,
      n_sr_per_table   = n_sr_per_table,
      n_ref_ag         = n_ref_ag,
      n_ref_sr         = n_ref_sr,
      n_ref_ag_overlap = n_ref_ag_overlap,
      n_ref_sr_overlap = if (use_prob_sr) NULL else n_ref_sr_overlap,
      p_sr_drop        = if (use_prob_sr) p_sr_drop else NULL,
      p_sr_gain        = if (use_prob_sr) p_sr_gain else NULL,
      sr_halflife      = sr_halflife,
      ag_drift         = ag_drift,
      range            = range,
      dimensions       = dimensions,
      base             = base,
      divisor          = divisor,
      max_log_titre    = max_log_titre,
      min_log_titre    = min_log_titre,
      rdistribution    = rdistribution,
      seed             = seed
    )
  )
}
