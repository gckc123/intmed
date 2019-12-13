char2fac <- function(data, ...) {
  df <- dplyr::as_tibble(data)
  df[, !sapply(df, is.numeric) & !sapply(df, is.factor)] <- lapply(df[, !sapply(df, is.numeric) & !sapply(df, is.factor)], as.factor)
  return(df)
}

extract_analysis_vars <- function(data, fo, ...) {
  df <- dplyr::as_tibble(data)
  fo_vars <- base::all.vars(fo)
  df <- df[, fo_vars]
  return(df)
}

empirical_pvalue <- function(vec) {
  gtzero <- ifelse(vec > 0, 1, 0)
  p <- sum(gtzero)/length(gtzero)
  p <- ifelse(p > 0.5, 1-p, p)
  p <- 2*p
  return(p)
}
