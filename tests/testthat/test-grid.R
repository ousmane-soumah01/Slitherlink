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
