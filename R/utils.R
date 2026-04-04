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


#' Convertit une grille en format JSON
#'
#' @param grid Objet SlitherlinkGrid
#' @return Chaîne JSON
#' @export
grid_to_json <- function(grid) {
  # Extraction et nettoyage des contraintes (ignorance des NA pour alléger le payload)
  constraints_list <- list()
  for (row in 1:grid$height) {
    for (col in 1:grid$width) {
      value <- grid$constraints[row, col]
      if (!is.na(value)) {
        constraints_list[[length(constraints_list) + 1]] <- list(
          row = row, col = col, value = value
        )
      }
    }
  }

  # Mapping de la liste d'arêtes en structure dictionnaire
  edges_list <- list()
  for (edge in grid$edges) {
    edges_list[[length(edges_list) + 1]] <- list(
      from = edge$from, to = edge$to
    )
  }

  # Assemblage du DTO (Data Transfer Object)
  data <- list(
    width = grid$width,
    height = grid$height,
    constraints = constraints_list,
    edges = edges_list
  )

  jsonlite::toJSON(data, pretty = TRUE, auto_unbox = TRUE)
}


#' Convertit du JSON en grille
#'
#' @param json_str Chaîne JSON
#' @return Objet SlitherlinkGrid
#' @export
json_to_grid <- function(json_str) {
  # Désérialisation et instanciation dynamique
  data <- jsonlite::fromJSON(json_str)
  grid <- create_grid(data$width, data$height)

  # Reconstruction de l'état des contraintes
  if (!is.null(data$constraints) && nrow(data$constraints) > 0) {
    for (i in 1:nrow(data$constraints)) {
      constraint <- data$constraints[i, ]
      grid$add_constraint(constraint$row, constraint$col, constraint$value)
    }
  }

  # Reconstruction de la topologie
  if (!is.null(data$edges) && nrow(data$edges) > 0) {
    for (i in 1:nrow(data$edges)) {
      edge <- data$edges[i, ]
      grid$add_edge(as.numeric(edge$from), as.numeric(edge$to))
    }
  }

  return(grid)
}


#' Calcule des statistiques sur une grille
#'
#' @param grid Objet SlitherlinkGrid
#' @return Liste de statistiques
#' @export
grid_statistics <- function(grid) {
  # Métriques absolues de contraintes
  num_constraints <- sum(!is.na(grid$constraints))
  count_0 <- sum(grid$constraints == 0, na.rm = TRUE)
  count_1 <- sum(grid$constraints == 1, na.rm = TRUE)
  count_2 <- sum(grid$constraints == 2, na.rm = TRUE)
  count_3 <- sum(grid$constraints == 3, na.rm = TRUE)

  # Calcul géométrique du nombre maximum théorique d'arêtes dans une grille matricielle
  num_edges <- length(grid$edges)
  max_possible_edges <- grid$width * (grid$height + 1) + grid$height * (grid$width + 1)

  # Calcul du ratio d'information (utilisé par les heuristiques d'élagage)
  density <- num_constraints / (grid$width * grid$height)

  list(
    width = grid$width,
    height = grid$height,
    total_cells = grid$width * grid$height,
    num_constraints = num_constraints,
    constraint_density = density,
    constraints_by_value = list("0" = count_0, "1" = count_1, "2" = count_2, "3" = count_3),
    num_edges = num_edges,
    max_possible_edges = max_possible_edges,
    edge_usage = if (max_possible_edges > 0) num_edges / max_possible_edges else 0
  )
}


#' Affiche les statistiques d'une grille
#'
#' @param grid Objet SlitherlinkGrid
#' @export
print_statistics <- function(grid) {
  stats <- grid_statistics(grid)

  cat("\n")
  cat("═══════════════════════════════════\n")
  cat("     MÉTRIQUES HEURISTIQUES\n")
  cat("═══════════════════════════════════\n\n")
  cat("Dimensions         :", stats$width, "×", stats$height, "\n")
  cat("Capacité mémoire   :", stats$total_cells, "cellules\n")
  cat("Densité contraintes:", stats$num_constraints,
      sprintf("(%.1f%% d'information a priori)", stats$constraint_density * 100), "\n")
  cat("  - Zéros (Clear)  :", stats$constraints_by_value$`0`, "\n")
  cat("  - Un    (Low)    :", stats$constraints_by_value$`1`, "\n")
  cat("  - Deux  (Pass)   :", stats$constraints_by_value$`2`, "\n")
  cat("  - Trois (High)   :", stats$constraints_by_value$`3`, "\n")
  cat("Saturation du graphe:", stats$num_edges, "/", stats$max_possible_edges,
      sprintf("(%.1f%%)", stats$edge_usage * 100), "\n")
  cat("═══════════════════════════════════\n\n")
}


#' Dessine une grille avec ggplot2
#'
#' @param grid Objet SlitherlinkGrid
#' @return Un objet ggplot
#' @export
draw_grid_ggplot <- function(grid) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Dépendance manquante : ggplot2 est requis pour le rendu spatial.")
  }

  library(ggplot2)

  # Étape 1 : Cartographie de l'espace vectoriel (matrice des intersections)
  points_data <- expand.grid(x = 0:grid$width, y = 0:grid$height)

  # Étape 2 : Projection des scalaires (contraintes) aux barycentres des cellules
  constraints_data <- data.frame()
  for (row in 1:grid$height) {
    for (col in 1:grid$width) {
      value <- grid$constraints[row, col]
      if (!is.na(value)) {
        constraints_data <- rbind(constraints_data, data.frame(
          x = col - 0.5, y = grid$height - row + 0.5, label = as.character(value)
        ))
      }
    }
  }

  # Étape 3 : Traduction de la liste d'adjacence en segments vectoriels
  edges_data <- data.frame()
  if (length(grid$edges) > 0) {
    for (edge in grid$edges) {
      edges_data <- rbind(edges_data, data.frame(
        x = edge$from[2], y = grid$height - edge$from[1],
        xend = edge$to[2], yend = grid$height - edge$to[1]
      ))
    }
  }

  # Assemblage graphique selon la grammaire des graphiques (Layers)
  p <- ggplot() +
    geom_point(data = points_data, aes(x = x, y = y), size = 3, color = "gray30") +
    geom_hline(yintercept = 0:grid$height, color = "gray80", linewidth = 0.3) +
    geom_vline(xintercept = 0:grid$width, color = "gray80", linewidth = 0.3) +

    # Rendu conditionnel des couches de contraintes et topologies
    {if (nrow(constraints_data) > 0)
      geom_text(data = constraints_data, aes(x = x, y = y, label = label),
                size = 8, color = "blue", fontface = "bold")
    } +
    {if (nrow(edges_data) > 0)
      geom_segment(data = edges_data, aes(x = x, y = y, xend = xend, yend = yend),
                   color = "red", linewidth = 2, lineend = "round")
    } +

    # Contrainte de ratio pour assurer des carrés parfaits
    coord_fixed(ratio = 1) +
    scale_x_continuous(breaks = 0:grid$width, expand = c(0.1, 0.1)) +
    scale_y_continuous(breaks = 0:grid$height, expand = c(0.1, 0.1)) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
    ) +
    labs(title = paste("Graphe Slitherlink - Structure", grid$width, "×", grid$height))

  return(p)
}
