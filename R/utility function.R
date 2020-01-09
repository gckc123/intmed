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

validate_input <- function(y, med, treat, mod, c, ymodel, mmodel, treat_lv, control_lv, incint, inc_mmint, sim, conf.level, out_scale, complete_analysis, digits) {
  if (sum(stringr::str_detect(y,names(data)) != 1)) {
    stop("The outcome variable does not exist in the dataset. Check the variable name.")
    return(FALSE)
  }
  for (i in 1:length(med)) {
    if (sum(stringr::str_detect(med[i],names(data)) != 1)) {
      stop(paste0("Mediator", i," does not exist in the dataset. Check the variable name."))
    }
  }
  if (sum(stringr::str_detect(treat,names(data)) != 1)) {
    stop("The treatment variable does not exist in the dataset. Check the variable name.")
    return(FALSE)
  }
  if (!is.null(c)) {
    for (i in 1:length(c)) {
      if (sum(stringr::str_detect(c[i],names(data)) != 1)) {
        stop(paste0("Covariate", i, " does not exist in the dataset. Check the variable name."))
      }
    }
  }
  if (sum(stringr::str_detect(ymodel, c("regression","poisson regression","logistic regression"))) != 1) {
    stop("Only regression, poisson regerssion and logistic are supported for the outcome variable.")
  }
}
