fetch_predictions_with_context <- function(intitule, identifiant = "123456", contexte = "", naf_data) {
  
  print("🟢 DEBUG : Début de fetch_predictions_with_context()")
  print(paste("Intitulé reçu :", intitule))
  print(paste("Contexte reçu :", contexte))
  
  ft_api_endpoint <- Sys.getenv("FT_API_ENDPOINT")
  access_token <- get_access_token()
  
  # Vérifiez si le contexte libre est saisi
  if (!is.null(contexte) && trimws(contexte) != "") {
    print(paste("✅ Contexte utilisateur utilisé :", contexte))
    message("Contexte libre saisi par l'utilisateur : ", contexte)
    contexte_final <- enc2utf8(as.character(contexte))  # Utilisation directe du contexte libre
  } else {
    matched_context <- get_context_from_code_naf(contexte, naf_data)
    if (!is.null(matched_context) && matched_context != "") {
      message("🔄 Contexte normalisé APET utilisé :", matched_context)
      contexte_final <- enc2utf8(as.character(matched_context))
    } else {
      contexte_final <- ""  # S'assurer que le contexte reste vide et non une phrase fixe
      message("🚫 Aucun contexte valide détecté, contexte ignoré.")
    }
  }
  

  
  # Préparer le corps de la requête
  body <- list(
    appellations = list(
      list(
        intitule = intitule,
        identifiant = identifiant,
        contexte = contexte_final
      )
    ),
    options = list(
      nomAppelant = "ApplicationClient",
      nbResultats = 10,
      seuilScorePrediction = 0.1
    )
  )
  
  # Afficher le corps de la requête pour le débogage
  message("Corps de la requête : ", toJSON(body, auto_unbox = TRUE, pretty = TRUE))
  
  print("📤 Corps de la requête envoyé à l'API :")
  print(toJSON(body, auto_unbox = TRUE, pretty = TRUE))
  
  # Envoyer la requête POST
  response <- tryCatch({
    POST(
      url = paste0(ft_api_endpoint, "/romeo/v2/predictionMetiers"),
      body = toJSON(body, auto_unbox = TRUE),
      encode = "raw",
      add_headers(
        Authorization = paste("Bearer", access_token),
        `Content-Type` = "application/json; charset=utf-8",
        Accept = "application/json; charset=utf-8, application/json"
      )
    )
  }, error = function(e) {
    stop("Erreur lors de l'envoi de la requête à l'API : ", e$message)
  })
  
  # Vérifier et traiter la réponse
  if (http_status(response)$category == "Success") {
    return(content(response, "parsed"))
  } else {
    error_message <- tryCatch(
      content(response, "text"),
      error = function(e) "Impossible de lire le contenu de la réponse d'erreur"
    )
    stop(paste("Erreur lors de l'appel API (PredictionMetiers) :", error_message))
  }
}
