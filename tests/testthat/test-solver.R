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


test_that("Le solveur interrompt la recherche et retourne NULL face à une topologie impossible", {
  grid <- create_grid(2, 2)

  # Création d'un paradoxe géométrique : 4 cases adjacentes demandant 3 segments.
  # Topologiquement, une boucle fermée ne peut pas satisfaire cette configuration.
  grid$add_constraint(1, 1, 3)
  grid$add_constraint(1, 2, 3)
  grid$add_constraint(2, 1, 3)
  grid$add_constraint(2, 2, 3)

  # Le solveur doit épuiser l'arbre de recherche rapidement sur une 2x2
  solution <- solve_puzzle(grid, max_iterations = 100)

  # On s'assure d'une sortie gracieuse (NULL) plutôt qu'un crash ou une boucle infinie
  expect_true(is.null(solution), info = "Le solveur a retourné une solution pour une grille mathématiquement insoluble.")
})
