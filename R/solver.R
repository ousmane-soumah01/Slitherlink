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

  # Obtenir l'arête courante
  edge <- possible_edges[[edge_index]]
  from <- edge$from
  to <- edge$to

  # BRANCHE 1 : AVEC l'arête
  tryCatch({
    grid$add_edge(from, to)

    # Vérifier si c'est prometteur (élagage)
    if (is_promising(grid)) {
      result <- backtrack_solve(grid, possible_edges, edge_index + 1, max_iter, counter_env)
      if (!is.null(result)) return(result)
    }

    # Backtrack : retirer l'arête
    grid$remove_edge(from, to)
  }, error = function(e) {
    # Si erreur, retirer l'arête quand même
    grid$remove_edge(from, to)
  })

  # Vérification vitale : si la branche 1 a provoqué un timeout, on n'explore pas la branche 2 !
  if (counter_env$limit_reached) return(NULL)

  # BRANCHE 2 : SANS l'arête (continuer directement)
  backtrack_solve(grid, possible_edges, edge_index + 1, max_iter, counter_env)
}


#' Propagation de contraintes
#'
#' @description
#' Applique des règles simples pour pré-remplir des arêtes évidentes.
#' Pour l'instant, version minimale sans propagation.
#'
#' @param grid SlitherlinkGrid object
propagate_constraints <- function(grid) {
  # Version simple : pas de propagation avancée
  # Améliorations possibles :
  # - Contrainte 0 → aucune arête autour
  # - Contrainte 3 dans un coin → 2 arêtes forcées
  # - Contrainte 2 avec 2 arêtes adjacentes déjà placées → forcer les 2 autres
  invisible(NULL)
}


#' Génère toutes les arêtes possibles (L'ULTIME OPTIMISATION)
#'
#' @description
#' Combine les DEUX meilleures stratégies :
#' 1. Exclure les arêtes des cases '0' (réduit l'espace de recherche).
#' 2. Ordre géographique (permet de déclencher l'élagage très tôt).
#'
#' @param grid SlitherlinkGrid object
#' @return Liste d'arêtes
get_all_possible_edges <- function(grid) {
  edges <- list()

  # --- MINIS-FONCTIONS POUR VÉRIFIER SI UNE ARÊTE TOUCHE UN '0' ---

  # Vérifie si une arête HORIZONTALE touche un zéro
  touches_zero_h <- function(r, c) {
    # r (0 à H), c (0 à W-1)
    if (r > 0 && !is.na(grid$constraints[r, c + 1]) && grid$constraints[r, c + 1] == 0) return(TRUE)
    if (r < grid$height && !is.na(grid$constraints[r + 1, c + 1]) && grid$constraints[r + 1, c + 1] == 0) return(TRUE)
    return(FALSE)
  }

  # Vérifie si une arête VERTICALE touche un zéro
  touches_zero_v <- function(r, c) {
    # r (0 à H-1), c (0 à W)
    if (c > 0 && !is.na(grid$constraints[r + 1, c]) && grid$constraints[r + 1, c] == 0) return(TRUE)
    if (c < grid$width && !is.na(grid$constraints[r + 1, c + 1]) && grid$constraints[r + 1, c + 1] == 0) return(TRUE)
    return(FALSE)
  }

  # --- PARCOURS GÉOGRAPHIQUE (Case par Case) ---
  for (row in 1:grid$height) {
    for (col in 1:grid$width) {

      # 1. Arête HAUT de la case
      if (!touches_zero_h(row - 1, col - 1)) {
        edges <- c(edges, list(list(from = c(row - 1, col - 1), to = c(row - 1, col))))
      }

      # 2. Arête GAUCHE de la case
      if (!touches_zero_v(row - 1, col - 1)) {
        edges <- c(edges, list(list(from = c(row - 1, col - 1), to = c(row, col - 1))))
      }

      # 3. Arête DROITE (Seulement si on est sur le bord droit de la grille)
      if (col == grid$width) {
        if (!touches_zero_v(row - 1, col)) {
          edges <- c(edges, list(list(from = c(row - 1, col), to = c(row, col))))
        }
      }

      # 4. Arête BAS (Seulement si on est sur le bord bas de la grille)
      if (row == grid$height) {
        if (!touches_zero_h(row, col - 1)) {
          edges <- c(edges, list(list(from = c(row, col - 1), to = c(row, col))))
        }
      }

    }
  }

  return(edges)
}

