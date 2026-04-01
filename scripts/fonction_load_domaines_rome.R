load_domaines_rome <- function(path = "data/cr_gd_dp_v4_utf8.csv") {
  if (!file.exists(path)) {
    stop("Fichier des domaines ROME introuvable : ", path)
  }
  
  df <- read.csv(
    path,
    sep = ",",
    fileEncoding = "UTF-8",
    stringsAsFactors = FALSE
  )
  
  names(df) <- trimws(names(df))
  
  cols_attendues <- c(
    "code_rome",
    "libelle_rome",
    "code_grand_domaine",
    "libelle_grand_domaine",
    "code_domaine_professionel",
    "libelle_domaine_professionel"
  )
  
  manquantes <- setdiff(cols_attendues, names(df))
  if (length(manquantes) > 0) {
    stop(
      "Colonnes manquantes dans le fichier des domaines ROME : ",
      paste(manquantes, collapse = ", ")
    )
  }
  
  df <- df[, cols_attendues]
  
  names(df) <- c(
    "CodeRome",
    "LibelleRomeRef",
    "CodeGrandDomaine",
    "LibelleGrandDomaine",
    "CodeDomaineProfessionnel",
    "LibelleDomaineProfessionnel"
  )
  
  df$CodeRome <- trimws(df$CodeRome)
  
  df
}