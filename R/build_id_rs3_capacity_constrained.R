#' Incremental Union-Find with a temporal capacity constraint
#'
#' replaces igraph::graph_from_edgelist() + components()
#' for Stage 3 of the "advanced_3" panel. Merges edges one at a time -
#' rs2_edges first (already validated by Stage 2), followed by fuzzy edges
#' sorted from the closest matches (low match_score) to the most uncertain -
#' and REJECTS any merge that would create a temporal collision (two rows
#' from the same cluster in the same Ano/Trimestre). Unlike
#' igraph::components(), which takes the transitive closure of all edges
#' without ever checking the consistency of the resulting clusters, only
#' the specific offending edge is rejected here - the valid links in the
#' rest of the cluster are preserved.
#'
#' Diagnosis of the problem and empirical validation of this fix:
#' see notes_fuzzy_matching_igraph_pnadc.pdf,
#' journal_enquete_matching_pnadc_advanced3.pdf, and the panel-by-panel tests
#' from August 16-17, 2026 (Option A vs. Option B).
#'
#' @param candidates data.frame with at least row_id, Ano, Trimestre - rows
#'   eligible for fuzzy matching (q_count_rs2 < 5 or missing id_rs2)
#' @param valid_matches data.frame with row_id.A, row_id.B, match_score - edges
#'   produced by fuzzy matching, after applying the uniqueness tie-breaker
#' @param rs2_edges data.frame with row_id.A, row_id.B - internal edges within
#'   already known id_rs2 trajectories
#'
#' @return data.frame with row_id, cluster_root (internal group identifier,
#'   to be offset by max(id_rs2) to avoid any collision with id_rs2)
#'
#' @keywords internal
build_id_rs3_capacity_constrained <- function(candidates, valid_matches, rs2_edges) {

  all_row_ids <- candidates$row_id
  period_key  <- paste(candidates$Ano, candidates$Trimestre, sep = "-")
  n <- length(all_row_ids)

  parent   <- seq_len(n)
  occupied <- as.list(period_key)  # Each row initially occupies its own period

  find_root <- function(x) {
    while (parent[x] != x) {
      parent[x] <<- parent[parent[x]]  # path halving
      x <- parent[x]
    }
    x
  }

  try_merge <- function(a, b) {
    if (is.na(a) || is.na(b)) return(invisible(NULL))
    ra <- find_root(a); rb <- find_root(b)
    if (ra == rb) return(invisible(NULL))
    if (length(intersect(occupied[[ra]], occupied[[rb]])) == 0) {
      parent[rb] <<- ra
      occupied[[ra]] <<- union(occupied[[ra]], occupied[[rb]])
    }
    # Else: collision detected, we discard this edge (nothing to be done)
    invisible(NULL)
  }

  # 1. rs2_edges first : already reliable, never in conflict between them
  if (nrow(rs2_edges) > 0) {
    pa <- match(rs2_edges$row_id.A, all_row_ids)
    pb <- match(rs2_edges$row_id.B, all_row_ids)
    for (k in seq_along(pa)) try_merge(pa[k], pb[k])
  }

  # 2. Fuzzy edges next, from the closest to the most questionable
  if (nrow(valid_matches) > 0) {
    valid_matches <- valid_matches[order(valid_matches$match_score), ]
    pa <- match(valid_matches$row_id.A, all_row_ids)
    pb <- match(valid_matches$row_id.B, all_row_ids)
    for (k in seq_along(pa)) try_merge(pa[k], pb[k])
  }

  roots <- vapply(seq_len(n), find_root, integer(1))

  # IMPORTANT: keep only candidates that participated in AT LEAST ONE
  # edge (fuzzy or rs2). Without this filter, an isolated candidate (no edges,
  # never merged with anyone) would still receive a cluster_root -
  # its own singleton root - and therefore a new, purely artificial "singleton"
  # id_rs3 covering only one quarter. This matches the behavior of
  # igraph::graph_from_edgelist() that we are replacing: a node absent from
  # every edge is never created as a graph vertex, and therefore never appears
  # in igraph::components(). We faithfully reproduce that rule here.
  connected_ids <- unique(c(rs2_edges$row_id.A, rs2_edges$row_id.B,
                            valid_matches$row_id.A, valid_matches$row_id.B))

  cluster_map_raw <- data.frame(row_id = all_row_ids, cluster_root = roots)
  cluster_map_raw[cluster_map_raw$row_id %in% connected_ids, ]
}
