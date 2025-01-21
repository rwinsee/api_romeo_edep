library(DT)
library(httr)
library(jsonlite)
library(bslib)  # Pour des thèmes modernes Bootstrap
library(shinyjs)
library(dplyr)

date_deploiement <- "21-01-2025"

#readRenviron(".Renviron")

#Sys.getenv("FT_API_ENDPOINT")
#Sys.getenv("AUTH_API_ENDPOINT")
#Sys.getenv("CLIENT_SECRET")
#Sys.getenv("CLIENT_ID")

source("scripts/fonction_getAccessToken.R")
#source("scripts/fonction_fetchAppelationsContexte.R")
#source("scripts/fetch_predictions_with_context.R")
source("scripts/fetch_predictions_with_context_normalisationNAF.R")
#source("scripts/fonction_loadNAF.R")
source("scripts/fonction_loadNAF_normalisee.R")
source("scripts/fonction_getFichesMetier.R")

# Fonction pour transformer les données retournées par fetch_appellations
transform_appellations <- function(result) {
  if (is.null(result) || length(result) == 0) {
    return(data.frame())
  }
  
  # Extraire les données pertinentes
  do.call(rbind, lapply(result[[1]]$metiersRome, function(x) {
    data.frame(
      CodeRome = x$codeRome, # Code ROME de la fiche parente (3premiers digit domaine pro et les deux derniers fiches)  
      Intitule = x$libelleRome, # Libellé de la fiche ROME parente
      LibelleAppellation = x$libelleAppellation, # Libellé de l'appellation métier du ROME prédite
      CodeAppellation = x$codeAppellation, # Code de l'appellation métier ROME prédite
      Score = x$scorePrediction, # Score de confiance de l'IA suite à sa prédiction (plus on est proche de 1 plus l'IA est confiante)
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
  useShinyjs(),
  # Ajouter les styles CSS
  tags$style(HTML("
  /* Harmoniser les styles des champs de texte et de sélection */
  .form-control, .selectize-input {
    font-family: Arial, sans-serif; /* Harmonisation de la police */
    font-size: 1em; /* Taille uniforme */
    color: #555; /* Gris moyen pour le texte */
    background-color: #fff; /* Fond blanc */
    border: 1px solid #ccc; /* Bordure grise claire */
    border-radius: 4px; /* Coins arrondis */
    padding: 8px; /* Espacement intérieur */
    box-shadow: none; /* Supprime les ombres */
    height: auto; /* S'assure que la hauteur est uniforme */
  }

  /* Placeholder en gris clair */
  ::placeholder {
    color: #aaa; /* Placeholder en gris clair */
    font-family: Arial, sans-serif;
    font-size: 1em;
  }

  /* Ajuster les styles pour le champ selectize */
  .selectize-control {
    font-family: Arial, sans-serif; /* Harmonisation de la police */
    font-size: 1em; /* Taille uniforme */
    color: #555; /* Texte en gris moyen */
    background-color: #fff; /* Fond blanc */
    border: 1px solid #ccc; /* Bordure grise claire */
    border-radius: 4px; /* Coins arrondis */
  }

  /* Options dans le menu déroulant selectize */
  .selectize-dropdown-content {
    font-family: Arial, sans-serif; /* Harmonisation de la police */
    font-size: 1em; /* Taille uniforme */
    color: #555; /* Texte en gris moyen */
    background-color: #fff; /* Fond blanc */
  }

  /* Placeholder pour le champ selectize */
  .selectize-input > input {
    color: #aaa; /* Placeholder en gris clair */
    font-family: Arial, sans-serif;
    font-size: 1em;
  }

  /* Focus pour tous les champs */
  .form-control:focus, .selectize-input:focus, .selectize-control.single .selectize-input {
    border-color: #aaa; /* Gris clair pour le bord lors du focus */
    outline: none; /* Supprime la bordure extérieure */
    box-shadow: none; /* Supprime les ombres */
  }

  /* Survol des options */
  .selectize-dropdown-content div:hover {
    background-color: #f7f7f7; /* Fond gris clair au survol */
    border-radius: 4px; /* Coins arrondis pour les options */
  }

  /* Styles pour les croix de réinitialisation */
  #clear_contexte_icon, #clear_libelle_icon {
    position: absolute;
    top: 50%;
    right: 8px;
    transform: translateY(-50%);
    cursor: pointer;
    color: #aaa;
    font-size: 1em;
    z-index: 10;
  }

  #clear_contexte_icon:hover, #clear_libelle_icon:hover {
    color: #333; /* Change la couleur au survol */
  }
"))
  ,
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
          textInput("search_rome", "Rechercher dans les métiers :", placeholder = "Exemple : électricien"),
          textInput("contexte_test", "Entrez un contexte :", placeholder = "Exemple : horticulture") # Champ de saisie pour le contexte
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
          h4("Interroger l'API ROMEO"),
          #textInput("libelle", "Entrez un mot-clé :", placeholder = "Exemple : électricien"),
          div(
            style = "position: relative; display: inline-block; width: 100%;",
            textInput("libelle", "Entrez un mot-clé :", value = "", placeholder = "Exemple : électricien"),
            tags$span(
              id = "clear_libelle_icon",
              style = "
      position: absolute;
      top: 55%;
      right: 8x;
      transform: translateY(-50%);
      cursor: pointer;
      color: #aaa;
      font-size: 1em;
      z-index: 10;",
              icon("times")
            )
          ),
          # Champ pour le code APET avec autocomplétion
          selectizeInput(
            "code_apet", 
            "Entrez un Code APET :", 
            choices = c("", naf_data$Code),  # Ajouter une valeur vide par défaut
            options = list(
              placeholder = "Exemple : 1071C",
              create = FALSE,
              maxOptions = 15
            )
          ),
          #textInput("contexte", "Entrez un contexte :", value = "", placeholder = "Exemple : horticulture"),
          div(
            style = "position: relative; display: inline-block; width: 100%;",
            textInput("contexte", "Entrez un contexte :", value = "", placeholder = "Exemple : horticulture"),
            tags$span(
              id = "clear_contexte_icon",
              style = "
      position: absolute;
      top: 55%;
      right: 8px;
      transform: translateY(-50%);
      cursor: pointer;
      color: #aaa;
      font-size: 1em;
      z-index: 10;",
              icon("times")  # L'icône FontAwesome pour la croix
            )
          ),
          verbatimTextOutput("debug_context"),
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
            a("Documentation API ROME", href = "https://francetravail.io/produits-partages/catalogue/romeo-2/documentation#/api-reference/", target = "_blank")
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
            ),
            tags$li(
              HTML("Version actuelle : <strong>0.0.3</strong>")
            ),
            tags$li(
              HTML("Historique des versions : "),
              a("Releases GitHub", href = "https://github.com/rwinsee/api_romeo_edep/releases", target = "_blank")
            ),
            tags$li(
              HTML(paste0("Date de mise à jour : <strong>", date_deploiement, "</strong>"))
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
  
  contexte_reactif <- reactiveVal("")  # Valeur réactive pour le contexte
  
  # Observer les changements dans le champ contexte avec debounce
  observe({
    debounce_input <- reactive(input$contexte) %>% debounce(1000)
    
    observeEvent(debounce_input(), {
      new_value <- debounce_input()
      current_value <- contexte_reactif()
      
      # Mettre à jour seulement si la valeur a changé
      if (!identical(new_value, current_value)) {
        contexte_reactif(new_value)
        message("Contexte mis à jour après debounce : ", new_value)
      }
    })
  })
  
  # Observer pour gérer la sélection APET
  observeEvent(input$code_apet, {
    selected_code <- input$code_apet
    
    # Vérifier si le code APET est valide
    if (selected_code %in% naf_data$Code) {
      libelle_naf <- naf_data$Libellé_bis[naf_data$Code == selected_code]
      
      # Mettre à jour le contexte avec le libellé normalisé
      contexte_reactif(as.character(libelle_naf))
      updateTextInput(session, "contexte", value = libelle_naf)
      message("Contexte mis à jour avec libellé normalisé APET : ", libelle_naf)
    } else {
      # Si le code APET est invalide, réinitialiser le contexte
      contexte_reactif("Contexte non défini ou introuvable")
      updateTextInput(session, "contexte", value = "Contexte non défini ou introuvable")
      message("Aucun contexte APET valide, contexte réinitialisé.")
    }
  })
  
  # Gérer la réinitialisation via la croix
  observe({
    runjs("
    $('#clear_contexte_icon').on('click', function() {
      $('#contexte').val('');  // Vide le champ contexte
      Shiny.setInputValue('contexte', '');  // Met à jour dans Shiny
      Shiny.setInputValue('clear_contexte', Math.random());  // Déclenche un événement
    });

    $('#clear_libelle_icon').on('click', function() {
      $('#libelle').val('');  // Vide le champ mot-clé
      Shiny.setInputValue('libelle', '');  // Met à jour dans Shiny
      Shiny.setInputValue('clear_mot_cle', Math.random());  // Déclenche un événement
    });
  ")
  })
  
  observeEvent(input$clear_contexte, {
    contexte_reactif("")  # Réinitialiser la valeur réactive
    message("Contexte réactif réinitialisé après clic sur la croix.")
  })
  
  observeEvent(input$clear_mot_cle, {
    updateTextInput(session, "libelle", value = "")  # Réinitialiser le champ mot clé
    message("Mot clé réinitialisé après clic sur la croix.")
  })
  
  
  # Synchroniser le champ contexte avec la valeur réactive
  observe({
    updateTextInput(session, "contexte", value = contexte_reactif())
  })
  
  
  
  # Observer pour mettre à jour la valeur réactive lorsque le champ APET est sélectionné
  observeEvent(input$code_apet, {
    selected_code <- input$code_apet
    
    if (selected_code %in% naf_data$Code) {
      libelle_naf <- naf_data$Libellé_bis[naf_data$Code == selected_code]
      
      if (is.null(input$contexte) || input$contexte == "") {
        contexte_reactif(as.character(libelle_naf))  # Contexte normalisé si le contexte libre est vide
        updateTextInput(session, "contexte", value = libelle_naf)
        message("Contexte normalisé mis à jour avec : ", libelle_naf)
      }
    } else {
      contexte_reactif("Contexte non défini ou introuvable")
      updateTextInput(session, "contexte", value = "Contexte non défini ou introuvable")
      message("Aucun contexte APET valide, contexte réinitialisé.")
    }
  })
  
  
  # Observer pour capturer les modifications manuelles du champ texte
  observeEvent(input$contexte_test, {
    contexte_reactif(input$contexte_test)  # Mettre à jour la valeur réactive avec la saisie manuelle
    message("Valeur réactive mise à jour depuis le champ texte : ", input$contexte_test)
  })
  
  # Synchroniser automatiquement la valeur réactive avec le champ contexte à tout moment
  observe({
    updateTextInput(session, "contexte", value = contexte_reactif())
  })
  
  observeEvent(input$clear_contexte, {
    updateTextInput(session, "contexte", value = "")  # Réinitialiser le champ contexte
    contexte_reactif("")  # Réinitialiser la valeur réactive associée
    message("Le champ contexte a été réinitialisé par l'utilisateur.")
  })
  
  # Fonctionnalité pour envoyer le contexte et le mot-clé à l'API
  predictions <- reactive({
    if (input$libelle == "" || is.null(input$libelle)) {
      return(NULL)  # Aucun critère saisi
    }
    
    # Utiliser le contexte réactif final (debounced)
    contexte_final <- as.character(contexte_reactif())
    result <- fetch_predictions_with_context(
      intitule = input$libelle,
      contexte = contexte_final,
      naf_data = naf_data
    )
    transform_appellations(result)
  })
  
  
  
  # Affichage conditionnel
  output$search_results <- renderUI({
    if (is.null(predictions())) {
      # Aucun texte saisi, affichez un message
      div(
        style = "margin-top: 20px;",
        h4("Guide de recherche : comment utiliser les champs ?"),
        p("Pour lancer une recherche, vous pouvez utiliser les combinaisons suivantes :"),
        tags$ul(
          tags$li("Mot-clé uniquement : Exemple : 'vendeur'"),
          tags$li("Mot-clé et Code APET : Exemple : Mot-clé 'vendeur', Code APET '1071C', le contexte s'actualisera selon le libellé normalisé de l'APET choisie"),
          tags$li("Mot-clé et Contexte : Exemple : Mot-clé 'vendeur', Contexte 'boulangerie'"),
          tags$li("Mot-clé, Code APET, et Contexte : Exemple : Mot-clé 'vendeur', Code APET '1071C', Contexte 'commerce de détail'. Toutefois, le mot-clef et le contexte prendront le dessus.")
        ),
        h4("Exemples de recherche :"),
        tags$ul(
          tags$li("Mot-clé : 'vendeur', Contexte : 'boulangerie'"),
          tags$li("Mot-clé : 'professeur', Contexte : 'enseignement supérieur'"),
          tags$li("Mot-clé : 'boucher', Code APET : '1011Z'"),
          tags$li("Mot-clé : 'plombier', Code APET : '4322A', Contexte : 'travaux sanitaires'")
        ),
        p("N'hésitez pas à ajuster vos recherches en fonction des informations que vous avez pour obtenir des résultats plus précis.")
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
    
    # Sélectionner uniquement les colonnes "Appellation" et "Score"
    pred_data <- pred_data[, c("CodeAppellation", "LibelleAppellation", "Score")]
    
    datatable(
      pred_data,
      rownames = FALSE,
      options = list(pageLength = 10),
      colnames = c(#"Code Rome", "Intitulé", 
        "Code OGR", "Appellation", "Score")
    )
  })
  
  # Note de lecture basée sur le score le plus élevé
  output$note_lecture <- renderUI({
    pred_data <- predictions()
    
    # Debug: inspecter les données de pred_data
    if (!is.null(pred_data)) {
      message("Structure de pred_data :")
      print(str(pred_data))
    }
    
    if (is.null(pred_data) || nrow(pred_data) == 0) {
      return(NULL)  # Pas de note si aucun résultat
    }
    
    # Obtenez le résultat avec le score le plus élevé
    best_result <- pred_data[which.max(as.numeric(pred_data$Score)), ]
    
    # Debug: inspecter les données de best_result
    message("Structure de best_result :")
    print(str(best_result))
    
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
            "</strong> (Code ROME : <strong>", best_result$CodeAppellation, 
            "</strong>, code fiche métier ROME : <strong>", best_result$CodeRome, "</strong>)."
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