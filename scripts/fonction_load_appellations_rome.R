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
  names(df) <- tolower(names(df))
  
  # Normalisation souple des noms de colonnes possibles
  noms <- names(df)
  
  col_code_rome <- intersect(c("code_rome", "coderome"), noms)
  col_libelle_rome <- intersect(c("libelle_rome", "libellerome"), noms)
  col_code_ogr <- intersect(c("code_appellation", "code_ogr", "codeogr"), noms)
  col_libelle_app <- intersect(c("libelle_appellation", "libelle_ogr", "libelleappellation"), noms)
  
  if (length(col_code_rome) == 0 || length(col_code_ogr) == 0) {
    stop("Impossible d'identifier les colonnes CodeRome / CodeOGR dans le fichier des appellations.")
  }
  
  out <- data.frame(
    CodeRome = trimws(df[[col_code_rome[1]]]),
    CodeOGR = trimws(df[[col_code_ogr[1]]]),
    stringsAsFactors = FALSE
  )
  
  if (length(col_libelle_app) > 0) {
    out$LibelleAppellationRef <- df[[col_libelle_app[1]]]
  }
  
  out <- unique(out)
  out
}