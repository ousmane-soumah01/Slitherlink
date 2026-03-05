# test-utils.R
library(testthat)

# -------------------------------------------------------------------------
# Couverture des utilitaires transverses (I/O, Métriques, Rendu)
# -------------------------------------------------------------------------

test_that("Sérialisation I/O : l'aller-retour JSON préserve l'intégrité de la topologie", {
  # Création d'un état complexe pour valider la profondeur de la sérialisation
  grid <- create_grid(3, 3)
  grid$add_constraint(1, 1, 3)
  grid$add_constraint(3, 3, 0)
  grid$add_edge(c(0, 0), c(0, 1))

  # Phase 1 : Export du DTO (Data Transfer Object)
  json_payload <- grid_to_json(grid)

  # Phase 2 : Réhydratation de l'objet depuis le payload JSON
  restored_grid <- json_to_grid(json_payload)

  # Assertions sur les invariants spatiaux et topologiques
  expect_equal(restored_grid$width, grid$width, info = "Perte de la dimension spatiale (width) lors de la désérialisation.")
  expect_equal(restored_grid$height, grid$height, info = "Perte de la dimension spatiale (height).")

  # Vérification de la matrice des contraintes
  expect_equal(restored_grid$constraints[1, 1], 3, info = "Corruption des données matricielles (contraintes).")

  # Vérification de la liste d'adjacence
  expect_equal(length(restored_grid$edges), 1, info = "Perte d'informations dans la topologie des arêtes.")
})


test_that("Métriques heuristiques : les statistiques de graphe sont exactes", {
  grid <- create_grid(4, 4)

  # Injection de la "charge utile" (contraintes)
  grid$add_constraint(1, 1, 0)
  grid$add_constraint(2, 2, 3)
  grid$add_constraint(3, 3, 3)

  # Tracé d'un chemin partiel
  grid$add_edge(c(0,0), c(0,1))
  grid$add_edge(c(0,1), c(1,1))

  # Génération du rapport d'analyse
  stats <- grid_statistics(grid)

  # Vérification de l'intégrité de l'aire matricielle
  expect_equal(stats$total_cells, 16, info = "Le calcul de l'aire totale est erroné.")
  expect_equal(stats$num_constraints, 3, info = "Dénombrement des contraintes actives défaillant.")

  # Vérification de l'agrégation fréquentielle
  expect_equal(stats$constraints_by_value$`3`, 2, info = "Erreur de distribution fréquentielle sur les valeurs de contraintes.")

  # Vérification de la capacité théorique du graphe
  expect_equal(stats$num_edges, 2, info = "Comptage des segments actifs défaillant.")
  expect_true(stats$max_possible_edges == 40, info = "La capacité théorique maximale du graphe (arêtes possibles) est fausse.")
})


test_that("Rendu CLI : le buffer de sortie standard (stdout) est correctement formaté", {
  grid <- create_grid(2, 2)
  grid$add_constraint(1, 1, 2)
  grid$add_edge(c(0,0), c(0,1))

  # Utilisation de expect_output pour intercepter le flux console (I/O stream)
  # Cela garantit l'absence de crash sans polluer les logs de la pipeline CI/CD
  expect_output(plot_grid(grid), "GRILLE SLITHERLINK", info = "L'en-tête du buffer CLI est manquante ou corrompue.")
  expect_output(plot_solution(grid), "SOLUTION SLITHERLINK", info = "Le dump ASCII de la solution a échoué.")
  expect_output(print_statistics(grid), "MÉTRIQUES HEURISTIQUES", info = "Le rendu console des statistiques a échoué.")
})

test_that("Rendu Spatial : l'usine ggplot2 génère un espace vectoriel structuré", {
  # Contournement pour éviter l'échec en environnement minimal (ex: CI sans ggplot2)
  skip_if_not_installed("ggplot2")

  grid <- create_grid(2, 2)
  grid$add_constraint(1, 1, 1)
  grid$add_edge(c(0,0), c(0,1))

  # Instanciation de l'objet graphique
  p <- draw_grid_ggplot(grid)

  # Vérification stricte du typage S3
  expect_s3_class(p, "ggplot", info = "Le moteur de rendu ne retourne pas une instance ggplot valide.")

  # Vérification de l'assemblage vectoriel (les couches de la grammaire graphique)
  # On attend au moins les points de maillage, les grilles hline/vline, etc.
  expect_true(length(p$layers) >= 3, info = "Couches graphiques (layers) manquantes dans l'assemblage de la matrice.")
})
