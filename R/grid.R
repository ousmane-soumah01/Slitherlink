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


    #' @description
    #' Vérifie si deux points sont adjacents
    #'
    #' @param p1 Premier point c(row, col)
    #' @param p2 Deuxième point c(row, col)
    #' @return TRUE si adjacents, FALSE sinon
    are_adjacent = function(p1, p2) {
      # Adjacence horizontale ou verticale uniquement
      (abs(p1[1] - p2[1]) == 1 && p1[2] == p2[2]) ||
        (abs(p1[2] - p2[2]) == 1 && p1[1] == p2[1])
    },




    #' @description
    #' Vérifie si une arête existe déjà
    #'
    #' @param from Point de départ
    #' @param to Point d'arrivée
    #' @return TRUE si l'arête existe, FALSE sinon
    has_edge = function(from, to) {
      any(sapply(self$edges, function(e) {
        (all(e$from == from) && all(e$to == to)) ||
          (all(e$from == to) && all(e$to == from))
      }))
    },


    #' @description
    #' Compte le nombre d'arêtes autour d'une case
    #'
    #' @param row Ligne de la case
    #' @param col Colonne de la case
    #' @return Nombre d'arêtes tracées autour de la case
    count_edges_around_cell = function(row, col) {
      # Les 4 coins de la case
      top_left <- c(row - 1, col - 1)
      top_right <- c(row - 1, col)
      bottom_left <- c(row, col - 1)
      bottom_right <- c(row, col)

      count <- 0
      # Côté haut
      if (self$has_edge(top_left, top_right)) count <- count + 1
      # Côté bas
      if (self$has_edge(bottom_left, bottom_right)) count <- count + 1
      # Côté gauche
      if (self$has_edge(top_left, bottom_left)) count <- count + 1
      # Côté droit
      if (self$has_edge(top_right, bottom_right)) count <- count + 1

      return(count)
    },

  )
)
