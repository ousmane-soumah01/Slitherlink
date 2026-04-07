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

