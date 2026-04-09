# inst/shiny/app.R
library(shiny)
library(jsonlite)
library(ggplot2)

# Résolution des dépendances (Fallback pour environnement de développement local)
if (requireNamespace("Slitherlink", quietly = TRUE)) {
  library(Slitherlink)
} else {
  source("../../R/grid.R"); source("../../R/validation.R")
  source("../../R/solver.R"); source("../../R/puzzles.R"); source("../../R/utils.R")
}

# ==========================================
# INTERFACE UTILISATEUR (Scaffolding UI)
# ==========================================
ui <- fluidPage(
  titlePanel("🧩 Slitherlink - Puzzle Logique"),
  p("Tracez une seule boucle fermée en respectant les chiffres dans les cases."),
  hr(),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      h3("🎮 Contrôles"),
      tabsetPanel(
        tabPanel("Prédéfinis",
                 br(),
                 selectInput("puzzle_select", "Choisir un puzzle :",
                             choices = c("Facile (2×2)" = "easy", "Moyen (3×3)" = "medium", "Difficile (5×5)" = "hard"), selected = "easy"),
                 actionButton("new_game", "🎲 Charger la Partie", class = "btn-primary btn-block", style = "margin-bottom: 10px;")
        ),
        tabPanel("Personnalisé",
                 br(),
                 p(strong("1. Créer une grille vide :")),
                 fluidRow(
                   column(6, numericInput("custom_w", "Largeur", value = 3, min = 1, max = 15)),
                   column(6, numericInput("custom_h", "Hauteur", value = 3, min = 1, max = 15))
                 ),
                 actionButton("create_custom", "Créer la grille", class = "btn-info btn-block"),
                 hr(),
                 p(strong("2. Gérer les contraintes :")),
                 fluidRow(
                   column(4, numericInput("c_row", "Ligne", value = 1, min = 1)),
                   column(4, numericInput("c_col", "Colonne", value = 1, min = 1)),
                   column(4, numericInput("c_val", "Valeur", value = 0, min = 0, max = 3))
                 ),
                 fluidRow(
                   column(6, actionButton("add_custom_c", "➕ Ajouter", class = "btn-secondary btn-block")),
                   column(6, actionButton("remove_custom_c", "➖ Retirer", class = "btn-warning btn-block"))
                 ),
                 br(),
                 actionButton("clear_all_custom_c", "🧹 Effacer TOUTES les contraintes", class = "btn-danger btn-block")
        )
      ),
      hr(),
      h4("⚡ Actions"),
      actionButton("clear_solution", "🗑️ Effacer les lignes tracées", class = "btn-warning btn-block", style = "margin-bottom: 10px;"),
      actionButton("check_solution", "✅ Vérifier la solution", class = "btn-success btn-block", style = "margin-bottom: 10px;"),
      actionButton("solve", "🤖 Résoudre Automatiquement", class = "btn-danger btn-block", style = "margin-bottom: 10px;"),
      hr(),
      h4("📊 Statistiques"),
      verbatimTextOutput("stats")
    ),

    mainPanel(
      width = 8,
      uiOutput("message"),
      h3("🎯 Grille"),
      plotOutput("grid_plot", height = "550px"),
      hr(),
      h4("Affichage texte (Console)"),
      verbatimTextOutput("grid_display")
    )
  )
)

# ==========================================
# LOGIQUE SERVEUR (State Management & Reactive Graph)
# ==========================================

server <- function(input, output, session) {

  # Initialisation du Graphe Réactif (State variables)
  game_grid <- reactiveVal(NULL)
  solver_status <- reactiveVal(NULL)
  message_text <- reactiveVal("")
  message_type <- reactiveVal("info")

  # Hydratation de l'état initial (Fallback sur grille simple)
  observe({
    if (is.null(game_grid())) {
      game_grid(create_example_easy())
    }
  })

  # --- Écouteurs d'événements : Cycle de vie d'une partie ---

  observeEvent(input$new_game, {
    grid <- switch(input$puzzle_select,
                   "easy" = create_example_easy(),
                   "medium" = create_example_medium(),
                   "hard" = create_example_hard())
    game_grid(grid)
    solver_status(NULL)
    message_text("✅ Nouvelle instance chargée en mémoire !")
    message_type("success")
  })

  observeEvent(input$clear_solution, {
    grid <- game_grid()
    if (!is.null(grid)) {
      # Deep clone obligatoire pour forcer la réévaluation du graphe réactif
      new_grid <- grid$clone(deep = TRUE)
      new_grid$edges <- list()
      game_grid(new_grid)
      message_text("🗑️ Purge de la liste d'adjacence effectuée.")
      message_type("info")
    }
  })

  observeEvent(input$check_solution, {
    grid <- game_grid()
    if (is.null(grid)) return()

    # Appel au moteur Back-End de validation
    if (validate_solution(grid)) {
      message_text("🎉 Topologie parfaite ! La solution satisfait tous les invariants.")
      message_type("success")
    } else {
      details <- ""
      if (length(grid$edges) == 0) details <- "Graphe vide : aucune arête détectée."
      else if (!is_valid_loop(grid)) details <- "Violation topologique : cycle ouvert ou degré > 2."
      else if (!check_constraints(grid)) details <- "Violation numérique : désaccord avec la matrice de contraintes."

      status <- solver_status()
      if (!is.null(status) && status == "impossible") {
        details <- paste(details, " || ❌ REJET : Le solveur a déjà prouvé l'insolvabilité de cette matrice.")
      }

      message_text(paste("❌ Validation échouée.", details))
      message_type("error")
    }
  })

  # --- Logique de l'Éditeur Dynamique de Grille ---

  observe({
    # Sécurisation des inputs (bridage dynamique sur les dimensions de la matrice)
    grid <- game_grid()
    if (!is.null(grid)) {
      updateNumericInput(session, "c_row", max = grid$height)
      updateNumericInput(session, "c_col", max = grid$width)
    }
  })

  observeEvent(input$create_custom, {
    grid <- create_grid(input$custom_w, input$custom_h)
    game_grid(grid)
    solver_status(NULL)
    message_text(paste("✅ Matrice vierge", input$custom_w, "×", input$custom_h, "instanciée."))
    message_type("info")
  })

  observeEvent(input$add_custom_c, {
    grid <- game_grid()
    if (is.null(grid)) return()
    if (input$c_row < 1 || input$c_row > grid$height || input$c_col < 1 || input$c_col > grid$width) {
      message_text(paste("❌ Out of Bounds : Les limites de l'espace sont", grid$height, "×", grid$width))
      message_type("error")
      return()
    }
    new_grid <- grid$clone(deep = TRUE)
    new_grid$add_constraint(input$c_row, input$c_col, input$c_val)
    game_grid(new_grid)
    solver_status(NULL)
    message_text(paste("✅ Mutation : Contrainte", input$c_val, "injectée en (", input$c_row, ",", input$c_col, ")."))
    message_type("success")
  })

  observeEvent(input$remove_custom_c, {
    grid <- game_grid()
    if (is.null(grid)) return()
    new_grid <- grid$clone(deep = TRUE)
    new_grid$constraints[input$c_row, input$c_col] <- NA  # Libération mémoire de la cellule
    game_grid(new_grid)
    solver_status(NULL)
    message_text(paste("🗑️ Mutation : Effacement de la coordonnée (", input$c_row, ",", input$c_col, ")."))
    message_type("info")
  })

  observeEvent(input$clear_all_custom_c, {
    grid <- game_grid()
    if (is.null(grid)) return()
    new_grid <- grid$clone(deep = TRUE)
    # Réinitialisation matricielle optimisée
    new_grid$constraints <- matrix(NA, nrow = grid$height, ncol = grid$width)
    new_grid$edges <- list()
    game_grid(new_grid)
    solver_status(NULL)
    message_text("🧹 Reset global : Matrice des contraintes effacée.")
    message_type("info")
  })

  # --- Interface Asynchrone : Bridge avec le Solveur Back-End ---
  observeEvent(input$solve, {
    grid <- game_grid()
    if (is.null(grid)) return()

    message_text("🤖 Computation en cours (Timeout de sécurité : 15 min)... Blocage du thread UI.")
    message_type("info")
    Sys.sleep(0.1) # Forcer le flush du Graphe Réactif avant blocage

    # Isolation de l'exécution dans un TryCatch pour éviter le crash de l'app Shiny
    solution <- tryCatch({
      solve_puzzle(grid, max_iterations = 10000000)
    }, error = function(e) NULL)

    is_timeout <- getOption("slitherlink_timeout", FALSE)
    iters <- getOption("slitherlink_iters", 0)

    if (!is.null(solution) && validate_solution(solution)) {
      game_grid(solution)
      solver_status("solved")
      message_text(paste("✅ Succès de l'algorithme : Solution trouvée en", iters, "itérations d'arbre !"))
      message_type("success")
    } else {
      if (is_timeout) {
        solver_status("timeout")
        message_text("⚠️ Timeout d'élagage : Capacité de calcul dépassée avant épuisement de l'arbre.")
        message_type("warning")
      } else {
        solver_status("impossible")
        message_text("❌ Contradiction mathématique prouvée : l'arbre de recherche a été épuisé sans solution.")
        message_type("error")
      }
    }
  })

  # Rendu de la grille (Vectoriel)
  output$grid_plot <- renderPlot({
    if (!is.null(game_grid())) draw_grid_ggplot(game_grid())
  })

  # Dump ASCII pour l'audit et le debug
  output$grid_display <- renderText({
    grid <- game_grid()
    if (is.null(grid)) return("Initialisation de l'espace mémoire...")

    result <- "\n"
    for (row in 1:grid$height) {
      result <- paste0(result, "  "); for (col in 1:grid$width) result <- paste0(result, "+───")
      result <- paste0(result, "+\n  ")
      for (col in 1:grid$width) {
        constraint <- grid$constraints[row, col]
        result <- paste0(result, if(is.na(constraint)) "│   " else paste0("│ ", constraint, " "))
      }
      result <- paste0(result, "│\n")
    }
    result <- paste0(result, "  "); for (col in 1:grid$width) result <- paste0(result, "+───")
    result <- paste0(result, "+\n\nArêtes tracées : ", length(grid$edges), "\n")
    return(result)
  })

  # Agrégation des métriques en temps réel
  output$stats <- renderText({
    grid <- game_grid()
    if (is.null(grid)) return("Aucun objet instancié")
    stats <- grid_statistics(grid)
    paste0("Dimensions : ", stats$width, "×", stats$height, "\nContraintes actives : ", stats$num_constraints, "\nSegments : ", stats$num_edges)
  })

  # Rendu dynamique des notifications utilisateur
  output$message <- renderUI({
    if (message_text() == "") return(NULL)
    bg_color <- switch(message_type(), "success"="#d4edda", "error"="#f8d7da", "info"="#d1ecf1", "warning"="#fff3cd")
    text_color <- switch(message_type(), "success"="#155724", "error"="#721c24", "info"="#0c5460", "warning"="#856404")
    div(style = paste0("padding: 15px; margin: 10px 0; border-radius: 5px; background-color: ", bg_color, "; color: ", text_color, "; font-weight: bold; border: 1px solid ", text_color, "40;"), message_text())
  })
}

shinyApp(ui = ui, server = server)

