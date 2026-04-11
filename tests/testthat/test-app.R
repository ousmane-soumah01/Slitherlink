# test-app.R
library(testthat)
library(shiny)

# -------------------------------------------------------------------------
# Tests d'Intégration Front-End (Shiny Server Logic)
# -------------------------------------------------------------------------

# Résolution dynamique du chemin absolu de l'application (pour CI/CD et local)
app_path <- file.path("..", "..", "inst", "shiny", "app.R")
if (!file.exists(app_path)) {
  app_path <- system.file("shiny", "app.R", package = "Slitherlink")
}

test_that("Hydratation du Graphe Réactif : L'état initial charge la grille Facile", {
  skip_if_not(file.exists(app_path), "Application Shiny introuvable pour les tests d'intégration.")

  # Chargement du contexte de l'application dans un environnement isolé
  app_env <- new.env()
  suppressWarnings(source(app_path, local = app_env))

  # Simulation du cycle de vie du serveur (sans render UI)
  testServer(app_env$server, {
    # Vérification de l'instanciation de l'objet dans la variable réactive
    grid <- game_grid()
    expect_false(is.null(grid), info = "Le State Manager n'a pas instancié la grille par défaut.")
    expect_equal(grid$width, 2, info = "La grille par défaut n'est pas la grille Facile (2x2).")
    expect_equal(grid$height, 2, info = "La grille par défaut n'est pas la grille Facile (2x2).")

    # Vérification du typage des notifications
    expect_equal(message_text(), "")
    expect_equal(message_type(), "info")
  })
})
