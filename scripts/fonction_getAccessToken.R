
get_access_token <- function() {
  auth_api_endpoint <- Sys.getenv("AUTH_API_ENDPOINT")
  client_id <- Sys.getenv("CLIENT_ID")
  client_secret <- Sys.getenv("CLIENT_SECRET")
  
  # Vérification des variables d'environnement
  if (is.na(auth_api_endpoint) || is.na(client_id) || is.na(client_secret)) {
    stop("Les variables d'environnement AUTH_API_ENDPOINT, CLIENT_ID ou CLIENT_SECRET ne sont pas définies.")
  }
  
  # Préparer le corps de la requête
  body <- list(
    client_id = client_id,
    client_secret = client_secret,
    scope = "api_romeov2 api_rome-fiches-metiersv1 nomenclatureRome",
    grant_type = "client_credentials"
  )
  
  # Envoyer la requête POST
  response <- POST(
    url = paste0(auth_api_endpoint, "?realm=partenaire"),
    body = body,
    encode = "form",
    add_headers(`Content-Type` = "application/x-www-form-urlencoded")
  )
  
  # Vérifier et traiter la réponse
  if (http_status(response)$category == "Success") {
    content(response, "parsed")$access_token
  } else {
    stop(paste("Erreur d'authentification :", content(response, "text")))
  }
}
