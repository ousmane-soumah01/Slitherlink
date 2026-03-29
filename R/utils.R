# utils.R

#' Affiche une grille dans la console (mode texte)
#'
#' @param grid Objet SlitherlinkGrid
#' @export
plot_grid <- function(grid) {
  cat("\n")
  cat("═══════════════════════════════════\n")
  cat("    GRILLE SLITHERLINK ", grid$width, "×", grid$height, "\n")
  cat("═══════════════════════════════════\n\n")

  # Parcours matriciel (Top-Down) pour la restitution spatiale des contraintes
  for (row in 1:grid$height) {
    # Génération du maillage horizontal (intersections et segments)
    cat("  ")
    for (col in 1:grid$width) {
      cat("+───")
    }
    cat("+\n")

    # Injection des valeurs de contraintes dans l'espace inter-nœuds
    cat("  ")
    for (col in 1:grid$width) {
      constraint <- grid$constraints[row, col]
      if (is.na(constraint)) {
        cat("│   ")
      } else {
        cat("│ ", constraint, " ", sep = "")
      }
    }
    cat("│\n")
  }

  # Clôture du graphe (bordure inférieure)
  cat("  ")
  for (col in 1:grid$width) {
    cat("+───")
  }
  cat("+\n\n")

  cat("Arêtes tracées :", length(grid$edges), "\n")
  cat("═══════════════════════════════════\n\n")
}

#' Affiche une solution dans la console
#'
#' @param grid Objet SlitherlinkGrid avec solution
#' @export
plot_solution <- function(grid) {
  cat("\n")
  cat("═══════════════════════════════════\n")
  cat("       SOLUTION SLITHERLINK\n")
  cat("═══════════════════════════════════\n\n")

  plot_grid(grid)

  # Dump structuré de la liste d'adjacence pour audit de la topologie
  cat("Liste des arêtes tracées :\n")
  cat("───────────────────────────────────\n")
  for (i in seq_along(grid$edges)) {
    edge <- grid$edges[[i]]
    cat(sprintf("  %2d. (%d,%d) ─→ (%d,%d)\n",
                i,
                edge$from[1], edge$from[2],
                edge$to[1], edge$to[2]))
  }
  cat("───────────────────────────────────\n")

  # Évaluation finale de l'état du graphe
  is_valid <- validate_solution(grid)
  if (is_valid) {
    cat("\n✅ SOLUTION VALIDE !\n\n")
  } else {
    cat("\n❌ Solution invalide (violation de contrainte ou topologie ouverte)\n\n")
  }
}
