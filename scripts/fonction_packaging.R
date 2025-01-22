# Liste des packages nécessaires
required_packages <- c("DT", "httr", "jsonlite", "bslib", "shinyjs", "dplyr")

# Fonction pour vérifier, installer si nécessaire, puis charger les packages
install_and_load <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)  # Installer le package s'il n'est pas disponible
      library(pkg, character.only = TRUE)  # Charger le package
      message(paste("Le package", pkg, "a été installé et chargé."))
    } else {
      library(pkg, character.only = TRUE)  # Charger le package s'il est déjà installé
      message(paste("Le package", pkg, "est déjà installé et a été chargé."))
    }
  }
}

# Appeler la fonction pour vos packages
install_and_load(required_packages)
