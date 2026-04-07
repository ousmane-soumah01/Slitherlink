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
  # (À implémenter : Gestion de l'état réactif)
}

shinyApp(ui = ui, server = server)
