#' Classe SlitherlinkGrid
#'
#' @description
#' Représente une grille de jeu Slitherlink.
#' Une grille est composée de points et d'arêtes potentielles.
#' Les contraintes (chiffres) sont stockées dans les cases.
#'
#' @export
SlitherlinkGrid <- R6::R6Class(
  "SlitherlinkGrid",

  public = list(
    #' @field width Largeur de la grille (nombre de colonnes de cases)
    width = NULL,

    #' @field height Hauteur de la grille (nombre de lignes de cases)
    height = NULL,

    #' @field constraints Matrice des contraintes (chiffres dans les cases)
    constraints = NULL,

    #' @field edges Liste des arêtes tracées (solution actuelle)
    edges = NULL,

    #' @description
    #' Initialise une nouvelle grille Slitherlink
    #'
    #' @param width Largeur (colonnes de cases)
    #' @param height Hauteur (lignes de cases)
    #' @return Un nouvel objet SlitherlinkGrid
    initialize = function(width = 5, height = 5) {
      self$width <- width
      self$height <- height
      # Matrice de contraintes (NA = pas de contrainte)
      self$constraints <- matrix(NA, nrow = height, ncol = width)
      # Liste vide d'arêtes
      self$edges <- list()
    }
  )
)
