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

#' Algorithme de backtracking
backtrack_solve <- function(grid, possible_edges, edge_index, max_iter, counter_env) {
  # Si une autre branche a déjà atteint la limite, on stoppe tout immédiatement
  if (counter_env$limit_reached) return(NULL)

  # Incrémenter le compteur
  counter_env$count <- counter_env$count + 1

  # Afficher progression tous les 1000 itérations
  if (counter_env$count %% 10000 == 0) {
    cat("  Itération", counter_env$count, "...\n")
  }

  # Vérifier la limite d'itérations
  if (counter_env$count > max_iter) {
    counter_env$limit_reached <- TRUE
    return(NULL)
  }

  # Cas de base : toutes les arêtes ont été décidées
  if (edge_index > length(possible_edges)) {
    if (validate_solution(grid)) {
      return(grid)
    } else {
      return(NULL)
    }
  }
