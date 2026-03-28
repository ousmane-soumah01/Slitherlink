# test-validation.R
library(testthat)

# -------------------------------------------------------------------------
# Validation des règles du jeu et théorie des graphes (Slitherlink Ruleset)
# -------------------------------------------------------------------------

test_that("Validation topologique : détection d'un cycle fermé valide", {
  grid <- create_grid(2, 2)

  # Construction manuelle d'un graphe cyclique périphérique (carré externe 2x2)
  # On simule le parcours complet de la liste d'adjacence pour vérifier
  # la capacité du moteur à confirmer l'unicité et la fermeture de la boucle.
  grid$add_edge(c(0,0), c(0,1))
  grid$add_edge(c(0,1), c(0,2))
  grid$add_edge(c(0,2), c(1,2))
  grid$add_edge(c(1,2), c(2,2))
  grid$add_edge(c(2,2), c(2,1))
  grid$add_edge(c(2,1), c(2,0))
  grid$add_edge(c(2,0), c(1,0))
  grid$add_edge(c(1,0), c(0,0))

  # L'algorithme de parcours en profondeur (DFS) doit confirmer le cycle
  expect_true(is_valid_loop(grid), info = "Le détecteur de cycle n'a pas reconnu une boucle parfaitement fermée.")
})
