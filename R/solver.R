#' Résout un puzzle Slitherlink
#'
#' @param grid Objet SlitherlinkGrid avec les contraintes
#' @param max_iterations Nombre maximum d'itérations
#' @return Une grille avec la solution, ou NULL
#'
#' @export
solve_puzzle <- function(grid, max_iterations = 10000000) {
  cat("Début de la résolution du puzzle...\n")

  # Étape 1 : Propagation des contraintes
  solution_grid <- grid$clone(deep = TRUE)
  propagate_constraints(solution_grid)

  # Obtenir toutes les arêtes possibles
  possible_edges <- get_all_possible_edges(solution_grid)
  cat("Nombre d'arêtes possibles :", length(possible_edges), "\n")

  # Étape 2 : Backtracking
  cat("Recherche de solution (backtracking)...\n\n")

  # Environnement pour suivre le statut
  counter_env <- new.env(parent = emptyenv())
  counter_env$count <- 0
  counter_env$limit_reached <- FALSE

  # Lancement de l'exploration
  result <- backtrack_solve(solution_grid, possible_edges, 1, max_iterations, counter_env)

  # Transmission des métadonnées discrètement pour Shiny
  options(slitherlink_timeout = counter_env$limit_reached)
  options(slitherlink_iters = counter_env$count)

  if (!is.null(result)) {
    cat("\n✅ Solution trouvée en", counter_env$count, "itérations !\n")
    return(result)
  } else {
    cat("\n❌ Fin de la recherche (Itérations:", counter_env$count, ")\n")
    return(NULL)
  }
}
