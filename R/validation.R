#' Valide une solution complète
#'
#' @description
#' Vérifie qu'une solution respecte toutes les règles du Slitherlink :
#' 1. Les arêtes forment une seule boucle fermée
#' 2. La boucle ne se croise pas et ne se touche pas
#' 3. Toutes les contraintes (chiffres) sont respectées
#'
#' @param grid Objet SlitherlinkGrid
#' @return TRUE si la solution est valide, FALSE sinon
#'
#' @examples
#' grid <- create_grid(3, 3)
#' grid$add_constraint(1, 1, 2)
#' # ... ajouter des arêtes ...
#' is_valid <- validate_solution(grid)
#'
#' @export
validate_solution <- function(grid) {
  # 1. Vérifier qu'il y a des arêtes
  if (length(grid$edges) == 0) {
    return(FALSE)
  }

  # 2. Vérifier que c'est une boucle fermée unique
  if (!is_valid_loop(grid)) {
    return(FALSE)
  }

  # 3. Vérifier les contraintes
  if (!check_constraints(grid)) {
    return(FALSE)
  }

  return(TRUE)
}
