#' Valide une solution complète
#'
#' @description
#' Vérifie qu'une solution respecte toutes les règles du Slitherlink :
#' 1. Les arêtes forment une seule boucle fermée
#' 2. La boucle ne se croise pas et ne se touche pas
#' 3. Toutes les contraintes (chiffres) sont respectées
#'
#' @param grid Objet SlitherlinkGrid
#' @return TRUE si la solution est valide, FALSE sinon
#'
#' @examples
#' grid <- create_grid(3, 3)
#' grid$add_constraint(1, 1, 2)
#' # ... ajouter des arêtes ...
#' is_valid <- validate_solution(grid)
#'
#' @export
validate_solution <- function(grid) {
  # 1. Vérifier qu'il y a des arêtes
  if (length(grid$edges) == 0) {
    return(FALSE)
  }

  # 2. Vérifier que c'est une boucle fermée unique
  if (!is_valid_loop(grid)) {
    return(FALSE)
  }

  # 3. Vérifier les contraintes
  if (!check_constraints(grid)) {
    return(FALSE)
  }

  return(TRUE)
}



#' Vérifie que les arêtes forment une boucle fermée unique
#'
#' @description
#' Une boucle valide doit :
#' - Être connexe (tous les points sont reliés)
#' - Chaque point utilisé a exactement 2 arêtes (degré 2)
#' - Former un cycle (pas d'arbre)
#'
#' @param grid Objet SlitherlinkGrid
#' @return TRUE si c'est une boucle valide, FALSE sinon
#'
#' @export
is_valid_loop <- function(grid) {
  if (length(grid$edges) == 0) {
    return(FALSE)
  }

  # Construire une liste d'adjacence
  adjacency <- list()

  for (edge in grid$edges) {
    from <- paste(edge$from, collapse = ",")
    to <- paste(edge$to, collapse = ",")

    # Ajouter les connexions
    if (is.null(adjacency[[from]])) {
      adjacency[[from]] <- c()
    }
    if (is.null(adjacency[[to]])) {
      adjacency[[to]] <- c()
    }

    adjacency[[from]] <- c(adjacency[[from]], to)
    adjacency[[to]] <- c(adjacency[[to]], from)
  }

  # VÉRIFICATION 1 : Chaque point a exactement 2 connexions (degré 2)
  for (point in names(adjacency)) {
    if (length(adjacency[[point]]) != 2) {
      return(FALSE)  # ← TON OBSERVATION EST ICI !
    }
  }

  # VÉRIFICATION 2 : Tous les points forment un seul cycle
  if (length(adjacency) == 0) {
    return(FALSE)
  }

  start_point <- names(adjacency)[1]
  visited <- c()
  current <- start_point
  previous <- NULL

  # Parcourir la boucle
  repeat {
    visited <- c(visited, current)

    # Trouver le prochain point (pas celui d'où on vient)
    neighbors <- adjacency[[current]]
    next_point <- NULL

    for (neighbor in neighbors) {
      if (is.null(previous) || neighbor != previous) {
        next_point <- neighbor
        break
      }
    }

    if (is.null(next_point)) {
      return(FALSE)
    }

    # Si on revient au début
    if (next_point == start_point) {
      # Vérifier qu'on a visité tous les points
      return(length(visited) == length(adjacency))
    }

    # Sécurité : éviter les boucles infinies
    if (length(visited) > length(adjacency)) {
      return(FALSE)
    }

    previous <- current
    current <- next_point
  }
}





#' Vérifie que toutes les contraintes de la case sont respectées
#'
#' @description
#' Pour chaque case avec un chiffre, vérifie que le nombre d'arêtes
#' tracées autour de cette case correspond exactement au chiffre.
#'
#' @param grid Objet SlitherlinkGrid
#' @return TRUE si toutes les contraintes sont respectées, FALSE sinon
#'
#' @export
check_constraints <- function(grid) {
  for (row in 1:grid$height) {
    for (col in 1:grid$width) {
      constraint <- grid$constraints[row, col]

      # Si pas de contrainte, passer à la case suivante
      if (is.na(constraint)) {
        next
      }

      # Compter les arêtes autour de la case
      count <- grid$count_edges_around_cell(row, col)

      # Vérifier la contrainte
      if (count != constraint) {
        return(FALSE)
      }
    }
  }

  return(TRUE)
}





#' Vérifie qu'aucun point n'a trop de connexions
#'
#' @description
#' Dans une solution valide, chaque point doit avoir 0 ou 2 arêtes.
#' Cette fonction vérifie qu'aucun point n'en a plus de 2.
#'
#' @param grid Objet SlitherlinkGrid
#' @return TRUE si aucun point n'a plus de 2 connexions, FALSE sinon
#'
#' @export
check_vertex_degrees <- function(grid) {
  # Compter le degré de chaque point
  degrees <- list()

  for (edge in grid$edges) {
    from <- paste(edge$from, collapse = ",")
    to <- paste(edge$to, collapse = ",")

    if (is.null(degrees[[from]])) {
      degrees[[from]] <- 0
    }
    if (is.null(degrees[[to]])) {
      degrees[[to]] <- 0
    }

    degrees[[from]] <- degrees[[from]] + 1
    degrees[[to]] <- degrees[[to]] + 1
  }

  # Vérifier qu'aucun point n'a plus de 2 arêtes
  for (point in names(degrees)) {
    if (degrees[[point]] > 2) {
      return(FALSE)
    }
  }

  return(TRUE)
}
