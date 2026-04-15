test_that("Création de puzzles d'exemple fonctionne", {
  easy <- create_example_easy()
  expect_equal(easy$width, 2)    # Corrigé : le facile est maintenant 2x2
  expect_equal(easy$height, 2)

  medium <- create_example_medium()
  expect_equal(medium$width, 3)  # Corrigé : le moyen est maintenant 3x3
  expect_equal(medium$height, 3)

  hard <- create_example_hard()
  expect_equal(hard$width, 5)    # Reste 5x5
  expect_equal(hard$height, 5)
})

test_that("Génération aléatoire fonctionne", {
  grid <- generate_random_puzzle(5, 5, 0.3)

  expect_equal(grid$width, 5)
  expect_equal(grid$height, 5)

  # Vérifier qu'il y a des contraintes
  num_constraints <- sum(!is.na(grid$constraints))
  expect_true(num_constraints > 0)
})

test_that("Sauvegarde et chargement de puzzle fonctionne", {
  grid <- create_grid(3, 3)
  grid$add_constraint(1, 1, 2)
  grid$add_constraint(2, 2, 3)

  # Sauvegarder
  temp_file <- tempfile(fileext = ".json")
  save_puzzle(grid, temp_file)

  expect_true(file.exists(temp_file))

  # Charger
  loaded <- load_puzzle(temp_file)

  expect_equal(loaded$width, 3)
  expect_equal(loaded$height, 3)
  expect_equal(loaded$constraints[1, 1], 2)
  expect_equal(loaded$constraints[2, 2], 3)

  # Nettoyer
  unlink(temp_file)
})
