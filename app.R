library(shiny)
library(DT)
library(httr)
library(jsonlite)

readRenviron(".Renviron")

Sys.getenv("FT_API_ENDPOINT")
Sys.getenv("AUTH_API_ENDPOINT")
Sys.getenv("CLIENT_SECRET")
Sys.getenv("CLIENT_ID")

source("scripts/fonction_getAccessToken.R")
source("scripts/fonction_fetchAppelations.R")
source("scripts/fonction_getFichesMetier.R")

# Fonction pour transformer les données retournées par `fetch_appellations`
transform_appellations <- function(result) {
  if (is.null(result) || length(result) == 0) {
    return(data.frame())
  }
  
  # Extraire les données pertinentes
  do.call(rbind, lapply(result[[1]]$metiersRome, function(x) {
    data.frame(
      CodeAppellation = x$codeAppellation,
      CodeRome = x$codeRome,
      LibelleAppellation = x$libelleAppellation,
      LibelleRome = x$libelleRome,
      ScorePrediction = x$scorePrediction,
      stringsAsFactors = FALSE
    )
  }))
}

# Fonction pour transformer les données retournées par `get_fiches_metier`
transform_fiches_metier <- function(fiches) {
  if (is.null(fiches) || length(fiches) == 0) {
    return(data.frame())
  }
  
  # Extraire les données pertinentes
  do.call(rbind, lapply(fiches, function(x) {
    data.frame(
      Code = x$code,
      Libelle = x$metier$libelle,
      stringsAsFactors = FALSE
    )
  }))
}

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Application Shiny : Métiers du ROME"),
  
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        id = "menu",
        tabPanel("Le ROME", value = "rome"),
        tabPanel("Prédiction d'appelations", value = "predictions")
      )
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.menu == 'rome'",
        h3("Les Métiers du ROME"),
        DTOutput("table_rome")
      ),
      
      conditionalPanel(
        condition = "input.menu == 'predictions'",
        h3("Prédiction d'appelations"),
        textInput("libelle", "Cherchez un métier :", ""),
        actionButton("search", "Rechercher"),
        DTOutput("table_predictions")
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  # Récupération et affichage des métiers du ROME
  output$table_rome <- renderDT({
    # Appeler `get_fiches_metier` pour obtenir les données
    fiches <- get_fiches_metier()  # Remplacez par votre fonction réelle
    transformed_fiches <- transform_fiches_metier(fiches)
    datatable(transformed_fiches, options = list(pageLength = 10))
  })
  
  # Récupération et affichage des prédictions d'appelations
  predictions <- eventReactive(input$search, {
    libelle <- input$libelle
    if (libelle == "") {
      return(NULL)
    }
    
    result <- fetch_appellations(libelle)  # Remplacez par votre fonction réelle
    transform_appellations(result)
  })
  
  output$table_predictions <- renderDT({
    pred_data <- predictions()
    if (is.null(pred_data)) {
      return(datatable(data.frame()))
    }
    datatable(pred_data, options = list(pageLength = 10))
  })
}

# Lancer l'application Shiny
shinyApp(ui, server)
