library(DT)
library(httr)
library(jsonlite)
library(bslib)  # Pour des thèmes modernes Bootstrap


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
ui <- navbarPage(
  title = "Application Client ROMEO",
  theme = bs_theme(bootswatch = "flatly"),  # Thème moderne
  
  # Page d'accueil
  tabPanel(
    "Accueil",
    fluidPage(
      titlePanel("Bienvenue sur l'application ROMEO"),
      fluidRow(
        column(
          8,
          h3("Présentation"),
          p("Cette application vous permet d'exploiter l'intelligence artificielle pour rapprocher un texte libre à une appellation ou à un savoir / savoir-faire issu du Répertoire Opérationnel des Métiers et des Emplois (ROME)."),
          p("Utilisez les fonctionnalités disponibles dans les onglets pour rechercher des métiers, explorer les appellations associées, et découvrir les savoir-faire liés."),
          
          h3("Fonctionnalités principales"),
          tags$ul(
            tags$li("Recherche avancée basée sur un texte libre."),
            tags$li("Rapprochement automatique des termes à une appellation ou un savoir-faire."),
            tags$li("Exploration du référentiel ROME avec des données à jour."),
            tags$li("Accès direct aux informations et métadonnées fournies par l'API de France Travail.")
          ),
          
          h3("À propos de l'API ROMEO"),
          p("L'API ROMEO est un service proposé par France Travail permettant de rapprocher des informations textuelles à des métiers, appellations ou savoir-faire du Répertoire Opérationnel des Métiers et des Emplois."),
          p(
            "Pour plus d'informations sur l'API ROMEO, consultez la ",
            a("documentation officielle", href = "https://francetravail.io/produits-partages/catalogue/romeo-2/documentation#/", target = "_blank"),
            "."
          ),
          
          h3("Comment commencer ?"),
          tags$ol(
            tags$li("Utilisez l'onglet 'Recherche' pour saisir un texte libre, comme un intitulé de poste ou une description de compétence."),
            tags$li("Obtenez une liste d'appellations ou de savoir-faire associés, avec un score de correspondance."),
            tags$li("Explorez les résultats et consultez les métadonnées.")
          ),
          # Ajouter une note sur les accents
          h3("Note importante"),
          p(
            strong("À noter : "), 
            "Il est recommandé de prêter attention aux accents dans la saisie des mots-clés, car leur absence peut influencer les résultats de l'intelligence artificielle."
          )
        )
      )
    )
  ),
  
  # Page des métiers ROME
  tabPanel(
    "Référentiel ROME",
    fluidPage(
      titlePanel("Les Métiers du ROME"),
      sidebarLayout(
        sidebarPanel(
          h4("Filtrer les métiers"),
          textInput("search_rome", "Rechercher dans les métiers :", placeholder = "Exemple : électricien")
        ),
        mainPanel(
          h4("Liste des métiers"),
          DTOutput("table_rome")
        )
      )
    )
  ),
  
  # Page de recherche sur les métiers ROME
  tabPanel(
    "Recherche",
    fluidPage(
      titlePanel("Prédictions d'appelations"),
      sidebarLayout(
        sidebarPanel(
          h4("Interroger l'API ROME"),
          textInput("libelle", "Entrez un mot-clé :", placeholder = "Exemple : électricien"),
          # Ajouter une note importante ici
          h4("Note importante"),
          p(
            strong("À noter : "), 
            "L'importance des accents dans la saisie ne doit pas être négligée. Par exemple, une recherche avec ", 
            strong("affréteur"), 
            " peut donner des résultats différents de ", 
            strong("affreteur"), 
            ". Pensez à vérifier votre saisie pour obtenir les meilleurs résultats."
          )
        ),
        mainPanel(
          uiOutput("search_results"),  # Affichage conditionnel des résultats
          uiOutput("note_lecture")    # Note de lecture
          
        )
      )
    )
  ),
  
  
  # Page de références
  # Page de références
  tabPanel(
    "Références et API",
    fluidPage(
      titlePanel("Références et documentation"),
      fluidRow(
        column(
          8,
          h4("Documentation et crédits"),
          p("Cette application utilise l'API ROME de France Travail pour récupérer des données sur les métiers et appellations."),
          p("Pour plus d'information sur les API France Travail : ",
            a("Documentation API France Travail", href = "https://francetravail.io/produits-partages/documentation", target = "_blank")
          ),
          p("Pour plus d'informations sur l'API ROME : ",
            a("Documentation API ROME", href = "https://api.gouv.fr/documentation/rome", target = "_blank")
          ),
          p("Pour en savoir plus sur France Travail, visitez le site officiel : ",
            a("France Travail", href = "https://www.francetravail.fr", target = "_blank")
          ),
          hr(),  # Séparateur
          h4("Informations techniques"),
          tags$ul(
            tags$li(
              HTML(
                "Développeur / Maintenicien : <strong>Romuald Weidmann</strong> ",
                "(<a href='mailto:romuald.weidmann@example.com' target='_blank'>romuald.weidmann@insee.fr</a>)"
              )
            ),
            tags$li(
              HTML("Technologie utilisée : <strong>R Shiny</strong>")
            ),
            tags$li(
              HTML("Code source : "),
              a("GitHub Repository", href = "https://github.com/rwinsee/api_romeo_edep", target = "_blank")
            ),
            tags$li(
              HTML("Déployé via : "),
              a("Plateforme SSPCloud de l'INSEE ", href = "https://datalab.sspcloud.fr/", target = "_blank")
            )
          )
        )
      )
    )
  )
  
)

# Serveur
server <- function(input, output, session) {
  # Récupération et transformation des données des métiers ROME
  rome_data <- reactive({
    fiches <- get_fiches_metier()
    transform_fiches_metier(fiches)
  })
  
  # Filtrer les métiers en fonction de la recherche
  filtered_data <- reactive({
    req(rome_data())  # Assure que les données sont disponibles
    if (input$search_rome == "" || is.null(input$search_rome)) {
      return(rome_data())  # Pas de filtre si la barre de recherche est vide
    } else {
      rome_data() %>%
        dplyr::filter(
          grepl(input$search_rome, Libellé, ignore.case = TRUE) |  # Recherche dans les libellés
            grepl(input$search_rome, Code, ignore.case = TRUE)  # Recherche dans les codes
        )
    }
  })
  
  # Afficher le tableau des métiers
  output$table_rome <- renderDT({
    datatable(
      filtered_data(),
      rownames = FALSE,
      options = list(pageLength = 10),  # Options pour la pagination
      colnames = c("Code", "Libellé")
    )
  })
  
  # Fonction pour récupérer les données en fonction de la saisie
  predictions <- reactive({
    if (input$libelle == "" || is.null(input$libelle)) {
      return(NULL)  # Retourne NULL si aucun critère n'est saisi
    }
    result <- fetch_appellations(input$libelle)
    transform_appellations(result)
  })
  
  # Affichage conditionnel
  output$search_results <- renderUI({
    if (is.null(predictions())) {
      # Aucun texte saisi, affichez un message
      div(
        style = "margin-top: 20px;",
        h4("Veuillez saisir un mot-clé pour lancer la recherche.")
      )
    } else {
      # Texte saisi, affichez les résultats et le titre
      tagList(
        h4("Résultats de la recherche"),
        DTOutput("table_predictions")
      )
    }
  })
  
  # Affichage des prédictions dans le tableau
  output$table_predictions <- renderDT({
    pred_data <- predictions()
    if (is.null(pred_data)) {
      return(NULL)  # Retourne rien si aucune donnée n'est trouvée
    }
    # Convertir le score en pourcentage
    pred_data$Score <- round(pred_data$Score * 100, 1)
    pred_data$Score <- paste0(pred_data$Score, "%")
    
    datatable(
      pred_data,
      rownames = FALSE,
      options = list(pageLength = 10),
      colnames = c("Code Rome", "Appellation", "Intitulé", "Score")
    )
  })
  
  # Note de lecture basée sur le score le plus élevé
  output$note_lecture <- renderUI({
    pred_data <- predictions()
    if (is.null(pred_data) || nrow(pred_data) == 0) {
      return(NULL)  # Pas de note si aucun résultat
    }
    
    # Obtenez le résultat avec le score le plus élevé
    best_result <- pred_data[which.max(as.numeric(pred_data$Score)), ]
    
    # Texte explicatif
    div(
      style = "margin-top: 20px; padding: 10px; border: 1px solid #ddd; border-radius: 5px; background-color: #f9f9f9;",
      h4("Note de lecture"),
      p(
        HTML(
          paste0(
            "La meilleure correspondance pour votre recherche <strong>'", input$libelle, 
            "'</strong> est l'appellation : <strong>", best_result$LibelleAppellation, 
            "</strong> avec l'intitulé : <strong>", best_result$Intitule,
            "</strong> (Code ROME : <strong>", best_result$CodeRome, "</strong>)."
          )
        )
      ),
      p(
        HTML(
          paste0(
            "Ce résultat a un score de <strong>", round(as.numeric(best_result$Score) * 100, 1), 
            "%</strong>, ce qui indique un haut degré de correspondance entre votre saisie et cette appellation."
          )
        )
      )
    )
  })
  
  
  
}

# Lancer l'application Shiny
shinyApp(ui, server) 