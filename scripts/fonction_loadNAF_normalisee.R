get_context_from_code_naf <- function(code_naf, naf_data) {
  if (grepl("^\\d{4}[A-Z]$", code_naf)) {
    match <- naf_data %>% dplyr::filter(Code == code_naf)
    message("Match trouvé : ", match$Libellé_bis[1])  # Debug : afficher le libellé trouvé
    if (nrow(match) > 0) {
      return(as.character(match$Libellé_bis[1]))
    }
  }
  message("Aucun match pour le code : ", code_naf)  # Debug : afficher le code NAF non trouvé
  return(as.character(code_naf))
}


load_nomenclature_naf <- function() {
  naf_url <- "https://www.insee.fr/fr/statistiques/fichier/2120875/naf2008_liste_n5.xls"
  temp_file <- tempfile(fileext = ".xls")
  
  # Télécharger et lire le fichier
  download.file(naf_url, temp_file, mode = "wb")
  
  # Lire le fichier en spécifiant les colonnes et nettoyer les encodages
  naf_data <- readxl::read_excel(temp_file, sheet = 1, skip = 1, col_names = TRUE) %>%
    dplyr::rename(Code = 1, Libellé = 2) %>%
    dplyr::filter(!is.na(Code)) %>%
    dplyr::mutate(
      Code = gsub("\\.", "", Code),
      Libellé = iconv(Libellé, from = "UTF-8", to = "UTF-8", sub = ""),  # Conserve le libellé brut
      Libellé_bis = sapply(Libellé, nettoyer_libelle)  # Ajoute le libellé nettoyé
    )
  
  return(naf_data)
}

# Charger les données
naf_data <- load_nomenclature_naf()

# Tester avec un code NAF
example_code <- "1071C"
context <- get_context_from_code_naf(example_code, naf_data)

# Afficher le résultat
print(context)  # Résultat attendu : "boulangerie patisserie"



# Charger la nomenclature au démarrage
naf_data <- load_nomenclature_naf()

# Fonction pour nettoyer les libellés
nettoyer_libelle <- function(libelle) {
  # Supprimer les apostrophes et les remplacer par un espace si nécessaire
  libelle <- gsub("'", " ", libelle)
  
  # Remplacer les tirets par un espace
  libelle <- gsub("-", " ", libelle)
  
  # Neutraliser les accents
  libelle <- iconv(libelle, to = "ASCII//TRANSLIT")
  
  # Supprimer les points à la fin des phrases
  libelle <- gsub("\\.$", "", libelle)
  
  # Remplacer les points-virgules par un espace
  libelle <- gsub(";", " ", libelle)
  
  # Remplacer les virgules par un espace
  libelle <- gsub(",", " ", libelle)
  
  # Mettre en minuscules
  libelle <- tolower(libelle)
  
  # Supprimer les mots indésirables (par exemple : et, de, les, d, l, du)
  libelle <- gsub("\\b(et|de|du|des|les|d|l|le|la|a|en|ou|sauf|hors|n.c..|pour)\\b", "", libelle)
  
  # Remplacer les espaces multiples par un seul espace
  libelle <- gsub("\\s+", " ", libelle)
  
  # Supprimer les espaces de début et de fin
  libelle <- trimws(libelle)
  
  # Supprimer les mots en doublons
  mots <- unlist(strsplit(libelle, " ")) # Découpe en mots
  libelle <- paste(unique(mots), collapse = " ") # Garder les mots uniques et les recoller
  
  return(libelle)
}

# Appliquer la fonction à la colonne Libellé
naf_data$Libellé_bis <- sapply(naf_data$Libellé, nettoyer_libelle)
# Aperçu du tableau
# head(naf_data)
# Tester la fonction avec un exemple
# example_code <- "1071C"
# context <- get_context_from_code_naf(example_code, naf_data)
# print(context)

# colnames(naf_data)
# head(naf_data)
