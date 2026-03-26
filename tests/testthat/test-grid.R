test_that("Création de grille fonctionne", {
  grid <- create_grid(5, 5)

  expect_equal(grid$width, 5)
  expect_equal(grid$height, 5)
  expect_equal(nrow(grid$constraints), 5)
  expect_equal(ncol(grid$constraints), 5)
  expect_equal(length(grid$edges), 0)
})
