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
