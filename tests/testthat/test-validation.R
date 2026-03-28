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


test_that("Invariants du graphe : stricte application de la règle des degrés", {
  grid <- create_grid(2, 2)

  # Initialisation d'un segment en équerre (le point 0,0 connecte deux arêtes)
  grid$add_edge(c(0, 0), c(0, 1))
  grid$add_edge(c(0, 0), c(1, 0))

  # Règle fondamentale : un sommet actif dans Slitherlink doit avoir exactement un degré de 2.
  expect_true(check_vertex_degrees(grid), info = "Faux positif sur une intersection légale (degré 2).")

  # Introduction d'une bifurcation intentionnelle (degré 3 au sommet 0,1)
  grid$add_edge(c(0, 1), c(0, 2))
  grid$add_edge(c(0, 1), c(1, 1))

  # Le validateur doit rejeter immédiatement cet état topologique (pas de croisements permis)
  expect_false(check_vertex_degrees(grid), info = "Le validateur a toléré une bifurcation illégale (sommet de degré 3).")
})
