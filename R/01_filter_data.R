filtrer_donnees <- function(df, pays = NULL, annee_min = NULL, annee_max = NULL) {
  res <- df
  if (!is.null(pays)) {
    res <- res %>% filter(Country %in% pays)
  }
  if (!is.null(annee_min)) {
    res <- res %>% filter(Year >= annee_min)
  }
  if (!is.null(annee_max)) {
    res <- res %>% filter(Year <= annee_max)
  }
  return(res)
}