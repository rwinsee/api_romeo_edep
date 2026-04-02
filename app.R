source("scripts/fonction_packaging.R")

date_deploiement <- "02-04-2026"

readRenviron(".Renviron")
Sys.getenv("FT_API_ENDPOINT")
Sys.getenv("AUTH_API_ENDPOINT")
Sys.getenv("CLIENT_SECRET")
Sys.getenv("CLIENT_ID")

source("scripts/fonction_getAccessToken.R")
source("scripts/fetch_predictions_with_context_normalisationNAF.R")
source("scripts/fonction_loadNAF_normalisee.R")
source("scripts/fonction_getFichesMetier.R")
source("scripts/fonction_load_domaines_rome.R")
source("scripts/fonction_load_appellations_rome.R")

domaines_rome <- load_domaines_rome("data/cr_gd_dp_v4_utf8.csv")
appellations_rome <- load_appellations_rome("data/unix_referentiel_appellation_v460_utf8.csv")

transform_appellations <- function(result, domaines_rome = NULL) {
  if (is.null(result) || length(result) == 0) {
    return(data.frame())
  }
  
  df <- do.call(rbind, lapply(result[[1]]$metiersRome, function(x) {
    data.frame(
      CodeRome = trimws(x$codeRome),
      Intitule = x$libelleRome,
      LibelleAppellation = x$libelleAppellation,
      CodeAppellation = x$codeAppellation,
      Score = x$scorePrediction,
      stringsAsFactors = FALSE
    )
  }))
  
  if (!is.null(domaines_rome) && nrow(df) > 0) {
    df <- dplyr::left_join(df, domaines_rome, by = "CodeRome")
  }
  
  df
}

transform_fiches_metier <- function(fiches, domaines_rome = NULL, appellations_rome = NULL) {
  if (is.null(fiches) || length(fiches) == 0) {
    return(data.frame())
  }
  
  df <- do.call(rbind, lapply(fiches, function(x) {
    data.frame(
      CodeRome = trimws(x$code),
      MetierGenerique = x$metier$libelle,
      stringsAsFactors = FALSE
    )
  }))
  
  if (!is.null(domaines_rome) && nrow(df) > 0) {
    df <- dplyr::left_join(
      df,
      domaines_rome,
      by = "CodeRome"
    )
  }
  
  if (!is.null(appellations_rome) && nrow(df) > 0) {
    df <- dplyr::left_join(
      df,
      appellations_rome,
      by = "CodeRome"
    )
  }
  
  df
}

ui <- navbarPage(
  # Titre de l'application
  title = "Application Client ROMEO",
  
  # Thème Bootstrap pour un style moderne
  theme = bs_theme(bootswatch = "flatly"),
  
  # Utilisation de shinyjs pour des fonctionnalités interactives
  useShinyjs(),
  
  # Ajout de styles CSS personnalisés
  tags$style(HTML("
  /* Harmonisation des styles pour les champs de texte et de sélection */
  .form-control, .selectize-input {
    font-family: Arial, sans-serif; /* Police uniforme pour tous les champs */
    font-size: 1em; /* Taille de texte standardisée */
    color: #555; /* Couleur du texte en gris moyen */
    background-color: #fff; /* Fond blanc pour tous les champs */
    border: 1px solid #ccc; /* Bordure grise claire pour un style épuré */
    border-radius: 4px; /* Coins légèrement arrondis pour une meilleure apparence */
    padding: 8px; /* Espacement intérieur uniforme */
    box-shadow: none; /* Suppression des ombres pour un design plat */
    height: auto; /* Maintien d'une hauteur uniforme */
  }

  /* Placeholder (texte indicatif dans les champs) en gris clair */
  ::placeholder, 
  .form-control::placeholder,
  .selectize-input::placeholder {
    color: #aaa !important; /* Gris clair uniforme */
    font-family: Arial, sans-serif !important; /* Police uniforme */
    font-size: 1em !important; /* Taille cohérente */
  }

  /* Appliquer la même couleur aux champs non sélectionnés */
  .form-control, .selectize-input {
    color: #555 !important; /* Gris plus foncé pour le texte saisi */
  }


  /* Styles spécifiques pour les menus déroulants (selectize) */
  .selectize-control {
    font-family: Arial, sans-serif; /* Police harmonisée avec les champs texte */
    font-size: 1em; /* Taille uniforme */
    color: #555; /* Texte en gris moyen pour plus de lisibilité */
    background-color: #fff; /* Fond blanc pour cohérence visuelle */
    border: 1px solid #ccc; /* Bordure grise claire similaire aux champs texte */
    border-radius: 4px; /* Coins arrondis pour un effet cohérent */
  }

  /* Options dans les menus déroulants (selectize) */
  .selectize-dropdown-content {
    font-family: Arial, sans-serif; /* Police harmonisée avec le reste de l'interface */
    font-size: 1em; /* Taille standard pour les options */
    color: #555; /* Couleur en gris moyen pour les options */
    background-color: #fff; /* Fond blanc pour une bonne lisibilité */
  }

  /* Placeholder dans les champs selectize */
  .selectize-input > input {
    color: #aaa; /* Couleur grise claire pour le texte indicatif */
    font-family: Arial, sans-serif; /* Police alignée avec le reste */
    font-size: 1em; /* Taille uniforme avec les autres champs */
  }

  /* Styles lors du focus (interaction avec les champs) */
  .form-control:focus, .selectize-input:focus, .selectize-control.single .selectize-input {
    border-color: #aaa; /* Bordure gris clair lors du focus */
    outline: none; /* Suppression des bordures extérieures par défaut */
    box-shadow: none; /* Suppression des ombres pour garder un style épuré */
  }

  /* Survol des options dans les menus déroulants */
  .selectize-dropdown-content div:hover {
    background-color: #f7f7f7; /* Fond gris clair pour indiquer le survol */
    border-radius: 4px; /* Coins arrondis pour une meilleure apparence */
  }

  /* Styles pour les icônes de réinitialisation (croix) */
  #clear_contexte_icon, #clear_libelle_icon {
    position: absolute; /* Positionnement absolu pour placer les croix dans les champs */
    top: 50%; /* Centrage vertical par rapport au champ */
    right: 8px; /* Distance à droite par rapport au bord du champ */
    transform: translateY(-50%); /* Ajustement pour centrer parfaitement verticalement */
    cursor: pointer; /* Curseur de type 'main' pour indiquer une action cliquable */
    color: #aaa; /* Couleur grise claire pour discrétion */
    font-size: 1em; /* Taille standard pour l'icône */
    z-index: 10; /* Priorité d'affichage pour éviter les chevauchements */
  }

  /* Changement de couleur des icônes de réinitialisation au survol */
  #clear_contexte_icon:hover, #clear_libelle_icon:hover {
    color: #333; /* Couleur plus foncée pour indiquer une interaction */
  }
")
  ),
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
    # Titre de l'onglet affiché dans la barre de navigation
    "Référentiel ROME",
    
    # Contenu de la page organisé avec un layout fluide
    fluidPage(
      # Titre principal de la page
      titlePanel("Les Métiers du ROME"),
      
      # Organisation en deux colonnes avec une barre latérale et un panneau principal
      sidebarLayout(
        # Barre latérale pour les filtres et les champs de recherche
        sidebarPanel(
          width = 3,
          # Titre de la section des filtres
          h4("Filtrer les métiers"),
          
          # Champ de saisie pour rechercher des métiers par mots-clés
          textInput(
            inputId = "search_rome", 
            label = "Rechercher dans les métiers :", 
            placeholder = "Exemple : vendeur"
          ),
          
          # Champ de saisie supplémentaire pour spécifier un contexte
          textInput(
            inputId = "contexte_test", 
            label = "Entrez un contexte :", 
            placeholder = "Exemple : horticulture"
          ),
          selectInput(
            inputId = "filtre_domaine_pro",
            label = "Filtrer par domaine professionnel :",
            choices = c("Tous" = ""),
            selected = ""
          ),
          
          selectInput(
            inputId = "filtre_grand_domaine",
            label = "Filtrer par grand domaine :",
            choices = c("Tous" = ""),
            selected = ""
          ),
          
          actionButton(
            inputId = "reset_filtres_rome",
            label = "Réinitialiser les filtres",
            icon = icon("eraser")
          )
        ),
        
        # Panneau principal pour afficher les résultats
        mainPanel(
          width = 9,
          # Titre pour la liste des métiers
          h4("Liste des métiers"),
          
          # Tableau interactif pour afficher les résultats des métiers
          DTOutput("table_rome")
        )
      )
    )
  ),
  
  
  # Page de recherche sur les métiers ROME
  tabPanel(
    # Titre de l'onglet affiché dans la barre de navigation
    "Prédictions d'appellation",
    
    # Contenu principal avec une mise en page fluide
    fluidPage(
      # Titre principal de la page
      titlePanel("Prédictions d'appelations"),
      
      # Disposition avec une barre latérale et un panneau principal
      sidebarLayout(
        # Barre latérale pour les champs de recherche et les filtres
        sidebarPanel(
          width = 3,
          # Titre de la section pour interroger l'API
          h4("Interroger l'API ROMEO"),
          
          # Champ pour saisir un mot-clé avec une croix pour effacer
          div(
            style = "position: relative; display: inline-block; width: 100%;", # Style pour positionner la croix
            textInput(
              inputId = "libelle", 
              label = "Entrez un mot-clé :", 
              value = "", 
              placeholder = "Exemple : vendeur" # Exemple affiché pour guider l'utilisateur
            ),
            tags$span( # Croix pour effacer le champ
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
          
          # Champ pour saisir un Code APET avec autocomplétion
          selectizeInput(
            inputId = "code_apet", 
            label = "Entrez un Code APET :", 
            choices = c("", naf_data$Code), # Options possibles basées sur les codes NAF
            options = list(
              placeholder = "Exemple : 1071C", # Exemple pour guider l'utilisateur
              create = FALSE,                 # Désactive la création d'options personnalisées
              maxOptions = 15                 # Limite le nombre d'options affichées
            )
          ),
          
          # Champ pour saisir un contexte avec une croix pour effacer
          div(
            style = "position: relative; display: inline-block; width: 100%;", # Style pour positionner la croix
            textInput(
              inputId = "contexte", 
              label = "Entrez un contexte :", 
              value = "", 
              placeholder = "Exemple : horticulture" # Exemple affiché pour guider l'utilisateur
            ),
            # Croix pour réinitialiser le champ "contexte"
            tags$span(
              id = "clear_contexte_icon", # Identifiant unique pour la croix
              style = "
    position: absolute;          /* Position absolue par rapport à son conteneur */
    top: 55%;                    /* Position verticale au centre (ajustée pour alignement visuel) */
    right: 8px;                  /* Espacement depuis le bord droit */
    transform: translateY(-50%); /* Centre verticalement en tenant compte de la hauteur de l'élément */
    cursor: pointer;             /* Changement de curseur pour indiquer l'interaction */
    color: #aaa;                 /* Couleur gris clair par défaut */
    font-size: 1em;              /* Taille de la croix */
    z-index: 10;                 /* Assure que la croix reste au-dessus des autres éléments */
  ",
              icon("times") # L'icône de croix provenant de FontAwesome
            )
            
          ),
          
          # Zone de texte pour afficher les informations de débogage sur le contexte
          verbatimTextOutput("debug_context"),
          
          # Note importante pour sensibiliser les utilisateurs sur l'importance des accents
          div(
            style = "margin-top: 10px; font-size: 0.9em; color: #555;", # Style général de la note
            h6(
              "Note importante", 
              style = "font-weight: bold; color: #333; margin-bottom: 5px;" # Style du titre
            ),
            p(
              strong("À noter : "), 
              "L'importance des accents dans la saisie ne doit pas être négligée. Par exemple, une recherche avec ", 
              strong("affréteur"), 
              " peut donner des résultats différents de ", 
              strong("affreteur"), 
              ". Pensez à vérifier votre saisie pour obtenir les meilleurs résultats."
            )
          )
        ),
        
        # Panneau principal pour afficher les résultats et les notes de lecture
        mainPanel(
          width = 9,
          # Affichage conditionnel des résultats de la recherche
          uiOutput("search_results"),
          
          # Note de lecture affichée après les résultats
          uiOutput("note_lecture")
        )
      )
    )
  ),
  
  
  # Page de références
  tabPanel(
    "A propos",
    fluidPage(
      titlePanel("Références et documentation"),
      fluidRow(
        column(
          8,
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
              HTML(paste0("Version actuelle : <strong>0.0.4</strong> (", date_deploiement, ")"))
            ),
            tags$li(
              HTML("Historique des versions : "),
              a("Releases GitHub", href = "https://github.com/rwinsee/api_romeo_edep/releases", target = "_blank")
            )
          ),
          hr(),  # Séparateur
          
          h4("Qu’est-ce que le ROME de France Travail ?"),
          p(HTML("Le <strong>Répertoire Opérationnel des Métiers et des Emplois (ROME)</strong> est un référentiel créé par <strong>France Travail</strong> (anciennement Pôle emploi). Il sert à :")),
          tags$ul(
            tags$li("Classer les métiers et les emplois en France en regroupant les activités professionnelles similaires sous un même code ROME."),
            tags$li("Faciliter les correspondances entre les offres et les demandes d’emploi :"),
            tags$ul(
              tags$li("Chaque métier ou emploi est associé à un code unique (exemple : M1102 pour “Responsable des achats”)."),
              tags$li("Décrire les compétences, activités et savoir-faire nécessaires pour chaque emploi ou métier.")
            )
          ),
          p("Le ROME est une ressource clé pour les recruteurs, les demandeurs d’emploi, les conseillers en insertion, et les professionnels de la formation. Il aide à uniformiser les définitions des métiers et facilite la compréhension du marché du travail."),
          hr(),
          
          h4("Qu’est-ce que l’API ROMEO ?"),
          p(HTML("L’API <strong>ROMEO (Référentiel Opérationnel des Métiers et des Emplois Opérationnel)</strong> est une interface informatique développée par <strong>France Travail</strong>. Elle permet aux applications externes d’interagir avec les données du ROME.")),
          tags$ul(
            tags$li(HTML("<strong>Rapprochement de texte libre :</strong> Elle utilise des algorithmes d’intelligence artificielle pour associer un texte libre (par exemple, une description de poste ou un intitulé de métier) à des appellations ou métiers du ROME.")),
            tags$li(HTML("<strong>Recherche et consultation :</strong> Les utilisateurs peuvent interroger l’API pour explorer les métiers et appellations correspondant à une recherche.")),
            tags$li(HTML("<strong>Scores de correspondance :</strong> L’API retourne des résultats avec un score de pertinence qui indique à quel point le texte saisi correspond à une appellation ou un métier.")),
            tags$li(HTML("<strong>Interopérabilité :</strong> L’API est utilisée par des plateformes d’emploi, des outils RH, ou des systèmes d’aide à l’orientation pour automatiser le traitement des données métiers."))
          ),
          hr(),
          
          h4("Différences entre le ROME et l’API ROMEO"),
          tableOutput("comparison_table"),  # Une table HTML pour afficher les différences
          p("Pour plus d'informations sur l'utilisation de l'API ROMEO dans votre service, consultez la documentation ci-dessous."),
          hr(),
          
          h4("Documentation et crédits"),
          p("Cette application utilise l'API ROMEO de France Travail pour récupérer des données sur les métiers et appellations."),
          p("Pour plus d'information sur les API France Travail : ",
            a("Documentation API France Travail", href = "https://francetravail.io/produits-partages/documentation", target = "_blank")
          ),
          p("Pour plus d'informations sur l'API ROMEO : ",
            a("Documentation API ROMEO", href = "https://francetravail.io/produits-partages/catalogue/romeo-2/documentation#/api-reference/", target = "_blank")
          ),
          p("Pour en savoir plus sur France Travail, visitez le site officiel : ",
            a("France Travail", href = "https://www.francetravail.fr", target = "_blank")
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
    
    transform_fiches_metier(
      fiches,
      domaines_rome = domaines_rome,
      appellations_rome = appellations_rome
    )
  })
  
  # Filtrer les métiers en fonction de la recherche
  filtered_data <- reactive({
    req(rome_data())
    
    df <- rome_data()
    
    if (!is.null(input$search_rome) && input$search_rome != "") {
      df <- df %>%
        dplyr::filter(
          grepl(input$search_rome, MetierGenerique, ignore.case = TRUE) |
            grepl(input$search_rome, LibelleAppellationLong, ignore.case = TRUE) |
            grepl(input$search_rome, CodeRome, ignore.case = TRUE) |
            grepl(input$search_rome, CodeOGR, ignore.case = TRUE) |
            grepl(input$search_rome, LibelleDomaineProfessionnel, ignore.case = TRUE) |
            grepl(input$search_rome, LibelleGrandDomaine, ignore.case = TRUE)
        )
    }
    
    if (!is.null(input$filtre_domaine_pro) && input$filtre_domaine_pro != "") {
      df <- df %>%
        dplyr::filter(LibelleDomaineProfessionnel == input$filtre_domaine_pro)
    }
    
    if (!is.null(input$filtre_grand_domaine) && input$filtre_grand_domaine != "") {
      df <- df %>%
        dplyr::filter(LibelleGrandDomaine == input$filtre_grand_domaine)
    }
    
    df
  })
  
  output$table_rome <- renderDT({
    df <- filtered_data()
    
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    
    df$AppellationLien <- paste0(
      df$LibelleAppellationLong,
      " <br><a href='https://candidat.francetravail.fr/metierscope/fiche-metier/",
      df$CodeRome,
      "' target='_blank' style='font-size: 0.8em;'>Voir la fiche métier</a>"
    )
    
    colonnes <- c(
      "CodeOGR",
      "AppellationLien",
      "MetierGenerique",
      "LibelleDomaineProfessionnel",
      "LibelleGrandDomaine"
    )
    
    labels <- c(
      CodeOGR = "Code OGR",
      AppellationLien = "Appellation",
      MetierGenerique = "Métier générique",
      LibelleDomaineProfessionnel = "Domaine professionnel",
      LibelleGrandDomaine = "Grand domaine"
    )
    
    colonnes <- intersect(colonnes, names(df))
    
    datatable(
      df[, colonnes, drop = FALSE],
      rownames = FALSE,
      escape = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        search = list(search = "")
      ),
      colnames = unname(labels[colonnes])
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
    
    if (selected_code %in% naf_data$Code) {
      # On récupère le libellé APET correspondant
      libelle_naf <- naf_data$Libellé_bis[naf_data$Code == selected_code]
      
      contexte_reactif(as.character(libelle_naf))
      updateTextInput(session, "contexte", value = libelle_naf, placeholder = "Exemple : boulangerie")
      
      message("✅ Contexte mis à jour avec libellé normalisé APET : ", libelle_naf)
      
    } else {
      # APET supprimé → on vide le champ "contexte" et on affiche seulement le placeholder
      contexte_reactif("")  
      updateTextInput(session, "contexte", value = "", placeholder = "Exemple : Boulangerie")
      
      message("🚫 APET supprimé, réinitialisation du contexte sans texte actif.")
    }
  })
  
  observe({
    req(rome_data())
    
    df_ref <- rome_data()
    
    if (!is.null(input$filtre_grand_domaine) && input$filtre_grand_domaine != "") {
      df_dp <- df_ref %>%
        dplyr::filter(LibelleGrandDomaine == input$filtre_grand_domaine)
    } else {
      df_dp <- df_ref
    }
    
    if (!is.null(input$filtre_domaine_pro) && input$filtre_domaine_pro != "") {
      df_gd <- df_ref %>%
        dplyr::filter(LibelleDomaineProfessionnel == input$filtre_domaine_pro)
    } else {
      df_gd <- df_ref
    }
    
    choix_dp <- c(
      "Tous" = "",
      sort(unique(na.omit(df_dp$LibelleDomaineProfessionnel)))
    )
    
    choix_gd <- c(
      "Tous" = "",
      sort(unique(na.omit(df_gd$LibelleGrandDomaine)))
    )
    
    selected_dp <- if (is.null(input$filtre_domaine_pro)) "" else input$filtre_domaine_pro
    selected_gd <- if (is.null(input$filtre_grand_domaine)) "" else input$filtre_grand_domaine
    
    if (!(selected_dp %in% unname(choix_dp))) {
      selected_dp <- ""
    }
    
    if (!(selected_gd %in% unname(choix_gd))) {
      selected_gd <- ""
    }
    
    updateSelectInput(
      session,
      "filtre_domaine_pro",
      choices = choix_dp,
      selected = selected_dp
    )
    
    updateSelectInput(
      session,
      "filtre_grand_domaine",
      choices = choix_gd,
      selected = selected_gd
    )
  })
  
  
  observeEvent(input$reset_filtres_rome, {
    updateTextInput(session, "search_rome", value = "")
    updateTextInput(session, "contexte_test", value = "")
    updateSelectInput(session, "filtre_domaine_pro", selected = "")
    updateSelectInput(session, "filtre_grand_domaine", selected = "")
    
    df_reset <- rome_data()
    
    colonnes <- c(
      "Code",
      "CodeOGR",
      "Libellé",
      "LibelleDomaineProfessionnel",
      "LibelleGrandDomaine"
    )
    
    colonnes <- intersect(colonnes, names(df_reset))
    
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
      contexte_reactif("")
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
    # transform_appellations(result)
    transform_appellations(result, domaines_rome = domaines_rome)
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
          tags$li("Mot-clé uniquement : Exemple : ", em("'vendeur'")),
          tags$li(
            "Mot-clé et Code APET : Exemple : Mot-clé ", em("'vendeur'"), 
            ", Code APET ", em("'1071C'"), 
            ", le contexte s'actualisera selon le libellé normalisé de l'APET choisie"
          ),
          tags$li(
            "Mot-clé et Contexte : Exemple : Mot-clé ", em("'vendeur'"), 
            ", Contexte ", em("'boulangerie'")
          ),
          tags$li(
            "Mot-clé, Code APET, et Contexte : Exemple : Mot-clé ", em("'vendeur'"), 
            ", Code APET ", em("'1071C'"), 
            ", Contexte ", em("'commerce de détail'"), 
            ". Toutefois, le mot-clé et le contexte prendront le dessus."
          )
        ),
        h4("Exemples de recherche :"),
        tags$ul(
          tags$li("Mot-clé : ", em("'vendeur'"), ", Contexte : ", em("'boulangerie'")),
          tags$li("Mot-clé : ", em("'professeur'"), ", Contexte : ", em("'enseignement supérieur'")),
          tags$li("Mot-clé : ", em("'boucher'"), ", Code APET : ", em("'1011Z'")),
          tags$li("Mot-clé : ", em("'plombier'"), ", Code APET : ", em("'4322A'"), ", Contexte : ", em("'travaux sanitaires'"))
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
    
    if (is.null(pred_data) || nrow(pred_data) == 0) {
      return(NULL)
    }
    
    pred_data$ScoreNum <- pred_data$Score
    pred_data$Score <- paste0(round(pred_data$Score * 100, 1), "%")
    
    pred_data$LibelleAppellation <- paste0(
      pred_data$LibelleAppellation,
      " <br><a href='https://candidat.francetravail.fr/metierscope/fiche-metier/",
      pred_data$CodeRome,
      "' target='_blank' style='font-size: 0.8em;'>Voir la fiche métier</a>"
    )
    
    # Colonnes à afficher
    colonnes_affichees <- c(
      "CodeAppellation",
      "LibelleAppellation",
      "CodeRome",
      "Intitule",
      "LibelleDomaineProfessionnel",
      "LibelleGrandDomaine",
      "Score"
    )
    
    # Sécuriser si certaines colonnes sont absentes
    colonnes_affichees <- intersect(colonnes_affichees, names(pred_data))
    pred_data <- pred_data[, colonnes_affichees, drop = FALSE]
    
    datatable(
      pred_data,
      rownames = FALSE,
      escape = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        search = list(search = "")
      ),
      colnames = c(
        "Code OGR",
        "Appellation",
        "Code ROME",
        "Métier générique",
        "Domaine professionnel",
        "Grand domaine",
        "Score"
      )[seq_along(colonnes_affichees)]
    )
  })
  
  # Note de lecture basée sur le score le plus élevé
  output$note_lecture <- renderUI({
    pred_data <- predictions()
    
    if (is.null(pred_data) || nrow(pred_data) == 0) {
      return(NULL)
    }
    
    best_result <- pred_data[which.max(pred_data$Score), ]
    
    div(
      style = "margin-top: 20px; padding: 10px; border: 1px solid #ddd; border-radius: 5px; background-color: #f9f9f9;",
      h4("Note de lecture"),
      p(
        HTML(
          paste0(
            "La meilleure correspondance pour votre recherche <strong>'", input$libelle,
            "'</strong> est l'appellation : <strong>", best_result$LibelleAppellation,
            "</strong>."
          )
        )
      ),
      p(
        HTML(
          paste0(
            "Code OGR : <strong>", best_result$CodeAppellation,
            "</strong> | Code ROME parent : <a href='https://candidat.francetravail.fr/metierscope/fiche-metier/",
            best_result$CodeRome,
            "' target='_blank'><strong>", best_result$CodeRome, "</strong></a>",
            " | Métier générique : <strong>", best_result$Intitule, "</strong>."
          )
        )
      ),
      p(
        HTML(
          paste0(
            "Grand domaine : <strong>", ifelse(is.na(best_result$LibelleGrandDomaine), "Non renseigné", best_result$LibelleGrandDomaine),
            "</strong> | Domaine professionnel : <strong>",
            ifelse(is.na(best_result$LibelleDomaineProfessionnel), "Non renseigné", best_result$LibelleDomaineProfessionnel),
            "</strong>."
          )
        )
      ),
      p(
        HTML(
          paste0(
            "Ce résultat a un score de <strong>", round(best_result$Score * 100, 1),
            "%</strong>."
          )
        )
      )
    )
  })

  
}

# Lancer l'application Shiny
shinyApp(ui, server)
