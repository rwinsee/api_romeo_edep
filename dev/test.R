fetch_appellations <- function(libelle) {
  ft_api_endpoint <- Sys.getenv("FT_API_ENDPOINT")
  access_token <- get_access_token() # Appeler la fonction pour récupérer le jeton d'accès
  
  # Préparer le corps de la requête
  body <- list(
    appellations = list(list(intitule = libelle, identifiant = "1", contexte = "")),
    options = list(
      nomAppelant = "jfeudeline",
      nbResultats = 10,
      seuilScorePrediction = 0.1
    )
  )
  
  # Envoyer la requête POST
  response <- POST(
    url = paste0(ft_api_endpoint, "/romeo/v2/predictionMetiers"),
    body = toJSON(body, auto_unbox = TRUE),
    encode = "raw",
    add_headers(
      Authorization = paste("Bearer", access_token),
      `Content-Type` = "application/json; charset=utf-8",
      Accept = "application/json; charset=utf-8, application/json"
    )
  )
  
  # Vérifier et traiter la réponse
  if (http_status(response)$category == "Success") {
    content(response, "parsed")
  } else {
    stop(paste("Erreur lors de l'appel API :", content(response, "text")))
  }
}
