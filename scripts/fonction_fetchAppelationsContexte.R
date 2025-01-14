fetch_appellations <- function(libelle, contexte = "") {
  ft_api_endpoint <- Sys.getenv("FT_API_ENDPOINT")
  access_token <- get_access_token() # Appeler la fonction pour récupérer le jeton d'accès
  
  # Préparer le corps de la requête avec le contexte
  body <- list(
    appellations = list(
      list(
        intitule = libelle,  # Mot-clé saisi
        identifiant = "1",   # Identifiant unique
        contexte = contexte  # Contexte saisi
      )
    ),
    options = list(
      nomAppelant = "jfeudeline", # Nom de l'appelant
      nbResultats = 10,          # Nombre de résultats souhaités
      seuilScorePrediction = 0.1 # Seuil minimal de confiance
    )
  )
  
  # Envoyer la requête POST
  response <- POST(
    url = paste0(ft_api_endpoint, "/romeo/v2/predictionMetiers"),
    body = toJSON(body, auto_unbox = TRUE),
    encode = "raw",
    add_headers(
      Authorization = paste("Bearer", access_token),   # Jeton d'accès
      `Content-Type` = "application/json; charset=utf-8",
      Accept = "application/json; charset=utf-8, application/json"
    )
  )
  
  # Vérifier et traiter la réponse
  if (http_status(response)$category == "Success") {
    content(response, "parsed") # Retourner la réponse parsée
  } else {
    stop(paste("Erreur lors de l'appel API :", content(response, "text")))
  }
}
