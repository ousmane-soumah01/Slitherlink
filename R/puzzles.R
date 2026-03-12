#' Charge un puzzle depuis un fichier JSON
#'
#' @export
load_puzzle <- function(filename) {
  if (!grepl("\\.json$", filename)) {
    filename_pkg <- system.file("puzzles", paste0(filename, ".json"), package = "Slitherlink")

    if (filename_pkg == "") {
      filename <- file.path("inst", "puzzles", paste0(filename, ".json"))
    } else {
      filename <- filename_pkg
    }
  }

  if (!file.exists(filename)) {
    stop(paste("Fichier puzzle introuvable:", filename))
  }

  puzzle_data <- jsonlite::fromJSON(filename)
  grid <- create_grid(puzzle_data$width, puzzle_data$height)

  if (!is.null(puzzle_data$constraints)) {
    for (i in 1:nrow(puzzle_data$constraints)) {
      constraint <- puzzle_data$constraints[i, ]
      grid$add_constraint(constraint$row, constraint$col, constraint$value)
    }
  }

  return(grid)
}

#' Sauvegarde un puzzle
#'
#' @export
save_puzzle <- function(grid, filename) {
  constraints_list <- list()

  for (row in 1:grid$height) {
    for (col in 1:grid$width) {
      value <- grid$constraints[row, col]
      if (!is.na(value)) {
        constraints_list[[length(constraints_list) + 1]] <- list(
          row = row,
          col = col,
          value = value
        )
      }
    }
  }

  puzzle_data <- list(
    width = grid$width,
    height = grid$height,
    constraints = constraints_list
  )

  jsonlite::write_json(puzzle_data, filename, pretty = TRUE, auto_unbox = TRUE)
  cat("Puzzle sauvegardé dans", filename, "\n")
}
