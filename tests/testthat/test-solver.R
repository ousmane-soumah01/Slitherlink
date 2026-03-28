# test-solver.R
library(testthat)

# -------------------------------------------------------------------------
# Validation du moteur de résolution (Solver)
# -------------------------------------------------------------------------

test_that("Le solveur converge vers une solution valide pour une configuration basique", {
  # Initialisation d'une grille minimale (2x2) pour valider la logique d'adjacence
  grid <- create_grid(2, 2)

  # Implémentation d'une contrainte en diagonale (cas d'école Slitherlink)
  grid$add_constraint(1, 1, 2)
  grid$add_constraint(2, 2, 2)

  # Exécution du backtracking avec limite d'itérations pour éviter les boucles infinies
  solution <- solve_puzzle(grid, max_iterations = 500)

  # Vérification de l'intégrité : la solution doit exister et respecter la topologie
  expect_false(is.null(solution), info = "Le solveur a échoué à trouver une solution évidente.")
  expect_true(validate_solution(solution), info = "La boucle générée est topologiquement invalide.")
})
