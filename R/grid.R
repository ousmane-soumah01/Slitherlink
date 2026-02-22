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
    },


    #' @description
    #' Ajoute une contrainte (chiffre) dans une case
    #'
    #' @param row Ligne de la case (1 à height)
    #' @param col Colonne de la case (1 à width)
    #' @param value Valeur de la contrainte (0, 1, 2, ou 3)
    add_constraint = function(row, col, value) {
      # Vérifier que la position est dans la grille
      if (row < 1 || row > self$height || col < 1 || col > self$width) {
        stop("Position hors limites de la grille")
      }
      # Vérifier que la valeur est valide
      if (!value %in% c(0, 1, 2, 3)) {
        stop("La valeur doit être 0, 1, 2, ou 3")
      }
      # Ajouter la contrainte dans la matrice
      self$constraints[row, col] <- value
    },



    #' @description
    #' Ajoute une arête (ligne) à la solution
    #'
    #' @param from Point de départ (vecteur c(row, col))
    #' @param to Point d'arrivée (vecteur c(row, col))
    add_edge = function(from, to) {
      # Vérifier que les points sont adjacents
      if (!self$are_adjacent(from, to)) {
        stop("Les points doivent être adjacents horizontalement ou verticalement")
      }

      # Ajouter l'arête si elle n'existe pas déjà
      if (!self$has_edge(from, to)) {
        edge <- list(from = from, to = to)
        self$edges <- c(self$edges, list(edge))
      }
    },



    #' @description
    #' Retire une arête de la solution
    #'
    #' @param from Point de départ
    #' @param to Point d'arrivée
    remove_edge = function(from, to) {
      # Filtrer pour retirer l'arête
      self$edges <- Filter(function(e) {
        !(all(e$from == from) && all(e$to == to)) &&
          !(all(e$from == to) && all(e$to == from))
      }, self$edges)
    },

  )
)
