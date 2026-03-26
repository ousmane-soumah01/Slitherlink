test_that("Création de grille fonctionne", {
  grid <- create_grid(5, 5)

  expect_equal(grid$width, 5)
  expect_equal(grid$height, 5)
  expect_equal(nrow(grid$constraints), 5)
  expect_equal(ncol(grid$constraints), 5)
  expect_equal(length(grid$edges), 0)
})

test_that("Ajout de contraintes fonctionne", {
  grid <- create_grid(3, 3)
  grid$add_constraint(1, 1, 2)
  grid$add_constraint(2, 2, 3)

  expect_equal(grid$constraints[1, 1], 2)
  expect_equal(grid$constraints[2, 2], 3)
  expect_true(is.na(grid$constraints[1, 2]))
})

test_that("Ajout d'arêtes fonctionne", {
  grid <- create_grid(2, 2)
  grid$add_edge(c(0, 0), c(0, 1))
  grid$add_edge(c(0, 1), c(1, 1))

  expect_equal(length(grid$edges), 2)
  expect_true(grid$has_edge(c(0, 0), c(0, 1)))
  expect_false(grid$has_edge(c(0, 0), c(1, 0)))
})

test_that("Comptage d'arêtes autour d'une case fonctionne", {
  grid <- create_grid(2, 2)
  grid$add_edge(c(0, 0), c(0, 1))  # Haut de case (1,1)
  grid$add_edge(c(0, 0), c(1, 0))  # Gauche de case (1,1)

  count <- grid$count_edges_around_cell(1, 1)
  expect_equal(count, 2)
})
