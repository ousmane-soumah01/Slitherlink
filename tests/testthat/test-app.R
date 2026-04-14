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

  app_env <- new.env()
  suppressWarnings(source(app_path, local = app_env))

  testServer(app_env$server, {
    # Forcer l'exécution de l'observe() d'initialisation (lazy par défaut dans testServer)
    session$flushReact()

    grid <- game_grid()
    expect_false(is.null(grid), info = "Le State Manager n'a pas instancié la grille par défaut.")
    expect_equal(grid$width, 2, info = "La grille par défaut n'est pas la grille Facile (2x2).")
    expect_equal(grid$height, 2, info = "La grille par défaut n'est pas la grille Facile (2x2).")

    expect_equal(message_text(), "")
    expect_equal(message_type(), "info")
  })
})

test_that("Event Dispatcher : Les sélecteurs de puzzles mettent à jour la mémoire", {
  skip_if_not(file.exists(app_path))
  app_env <- new.env()
  suppressWarnings(source(app_path, local = app_env))

  testServer(app_env$server, {
    session$flushReact()

    session$setInputs(puzzle_select = "hard", new_game = 1)

    grid <- game_grid()
    expect_equal(grid$width, 5, info = "Le changement de puzzle n'a pas modifié la dimension (attendu: 5x5).")
    expect_match(message_text(), "Nouvelle partie démarrée", info = "Le logger UI n'a pas capturé le succès de l'action.")
    expect_equal(message_type(), "success")
    expect_true(is.null(solver_status()), info = "Le statut du solveur n'a pas été réinitialisé après le chargement.")
  })
})

test_that("Éditeur Dynamique : Ajout, suppression et purge des contraintes spatiales", {
  skip_if_not(file.exists(app_path))
  app_env <- new.env()
  suppressWarnings(source(app_path, local = app_env))

  testServer(app_env$server, {
    session$flushReact()

    # 1. Instanciation d'une matrice custom (4x4)
    session$setInputs(custom_w = 4, custom_h = 4, create_custom = 1)
    expect_equal(game_grid()$width, 4)
    expect_equal(game_grid()$height, 4)

    # 2. Injection d'une contrainte (Write)
    session$setInputs(c_row = 2, c_col = 3, c_val = 3, add_custom_c = 1)
    expect_equal(game_grid()$constraints[2, 3], 3, info = "Échec de l'injection matricielle (ajout contrainte).")

    # 3. Suppression ciblée (Delete)
    session$setInputs(remove_custom_c = 1)
    expect_true(is.na(game_grid()$constraints[2, 3]), info = "La libération mémoire de la cellule a échoué (suppression).")

    # 4. Protection Out of Bounds (Erreur utilisateur)
    session$setInputs(c_row = 10, c_col = 10, add_custom_c = 2)
    expect_equal(message_type(), "error", info = "Le validateur UI n'a pas bloqué des coordonnées hors-limites.")

    # 5. Purge globale (Reset)
    session$setInputs(c_row = 1, c_col = 1, c_val = 2, add_custom_c = 3)
    session$setInputs(clear_all_custom_c = 1)
    expect_true(is.na(game_grid()$constraints[1, 1]), info = "Le bouton de purge globale n'a pas écrasé la matrice.")
  })
})

test_that("Bridge Back-End : Le bouton de résolution déclenche l'arbre de recherche", {
  skip_if_not(file.exists(app_path))
  app_env <- new.env()
  suppressWarnings(source(app_path, local = app_env))

  testServer(app_env$server, {
    session$flushReact()

    session$setInputs(puzzle_select = "easy", new_game = 1)
    session$setInputs(solve = 1)

    expect_equal(solver_status(), "solved", info = "Le solveur a échoué sur la grille d'échauffement.")
    expect_equal(message_type(), "success")

    grid <- game_grid()
    expect_true(length(grid$edges) > 0, info = "La liste d'adjacence est restée vide après la résolution.")

    session$setInputs(check_solution = 1)
    expect_match(message_text(), "Félicitations", info = "Le moteur de validation UI a rejeté une solution mathématiquement exacte.")
  })
})
