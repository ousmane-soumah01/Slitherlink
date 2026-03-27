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
