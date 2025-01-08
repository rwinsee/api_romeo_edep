get_fiches_metier <- function() {
  ft_api_endpoint <- Sys.getenv("FT_API_ENDPOINT")
  access_token <- get_access_token() # Appeler la fonction pour récupérer le jeton d'accès
  
  # Envoyer la requête GET
  response <- GET(
    url = paste0(ft_api_endpoint, "/rome-fiches-metiers/v1/fiches-rome/fiche-metier"),
    add_headers(
      Authorization = paste("Bearer", access_token),
      `Content-Type` = "application/json; charset=utf-8",
      Accept = "application/json"
    )
  )
  
  # Vérifier et traiter la réponse
  if (http_status(response)$category == "Success") {
    content(response, "parsed")
  } else {
    stop(paste("Erreur lors de l'appel API :", content(response, "text")))
  }
}
