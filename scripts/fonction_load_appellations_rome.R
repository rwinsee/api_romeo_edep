load_appellations_rome <- function(path = "data/unix_referentiel_appellation_v460_utf8.csv") {
  if (!file.exists(path)) {
    stop("Fichier des appellations ROME introuvable : ", path)
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
    "code_ogr",
    "libelle_appellation_long",
    "libelle_appellation_court"
  )
  
  manquantes <- setdiff(cols_attendues, names(df))
  if (length(manquantes) > 0) {
    stop(
      "Colonnes manquantes dans le fichier des appellations : ",
      paste(manquantes, collapse = ", ")
    )
  }
  
  out <- df[, cols_attendues]
  
  names(out) <- c(
    "CodeRome",
    "CodeOGR",
    "LibelleAppellationLong",
    "LibelleAppellationCourt"
  )
  
  out$CodeRome <- trimws(out$CodeRome)
  out$CodeOGR <- trimws(as.character(out$CodeOGR))
  
  unique(out)
}