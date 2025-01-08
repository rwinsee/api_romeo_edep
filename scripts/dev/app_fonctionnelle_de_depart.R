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

# Fonction pour transformer les données retournées par fetch_appellations
transform_appellations <- function(result) {
  if (is.null(result) || length(result) == 0) {
    return(data.frame())
  }
  
  # Extraire les données pertinentes
  do.call(rbind, lapply(result[[1]]$metiersRome, function(x) {
    data.frame(
      CodeRome = x$codeRome,
      Intitule = x$libelleRome,
      LibelleAppellation = x$libelleAppellation,
      
      Score = x$scorePrediction,
      stringsAsFactors = FALSE
    )
  }))
}

# Fonction pour transformer les données retournées par get_fiches_metier
transform_fiches_metier <- function(fiches) {
  if (is.null(fiches) || length(fiches) == 0) {
    return(data.frame())
  }
  
  # Extraire les données pertinentes
  do.call(rbind, lapply(fiches, function(x) {
    data.frame(
      Code = x$code,
      Libellé = x$metier$libelle,
      stringsAsFactors = FALSE
    )
  }))
}

# Interface utilisateur
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
        DTOutput("table_predictions")
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  # Récupération et affichage des métiers du ROME
  output$table_rome <- renderDT({
    # Appeler get_fiches_metier pour obtenir les données
    fiches <- get_fiches_metier()  # Remplacez par votre fonction réelle
    transformed_fiches <- transform_fiches_metier(fiches)
    datatable(
      transformed_fiches,
      rownames = FALSE, # Suppression des numéros de ligne
      options = list(pageLength = 10),  
      colnames = c("Code", "Libellé")  # Renommage des colonnes
    )
  })
  
  # Recherche automatique dès la saisie
  predictions <- reactive({
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
    # Convertir la colonne Score en pourcentage
    pred_data$Score <- round(pred_data$Score * 100, 1)  # Multiplier par 100 et arrondir à 1 décimale
    pred_data$Score <- paste0(pred_data$Score, "%")  # Ajouter le symbole %
    
    datatable(
      pred_data,
      rownames = FALSE,
      options = list(pageLength = 10),  # Suppression des numéros de ligne
      colnames = c("Code Rome", "Appellation", "Intitulé", "Score")  # Renommage des colonnes
    )
  })
}

# Lancer l'application Shiny
shinyApp(ui, server)