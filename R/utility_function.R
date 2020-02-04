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

validate_input <- function(y, med, treat, mod, c, ymodel, mmodel, treat_lv, control_lv, incint, inc_mmint, data, sim, conf.level, out_scale, complete_analysis, digits) {
  tmp <- NULL
  search_term = paste0("^",y,"$")
  if (sum(stringr::str_detect(names(data), search_term)) != 1) {
    stop("The outcome variable does not exist in the dataset. Check the variable name.")
    return(FALSE)
  }


  for (i in 1:length(med)) {
    search_term = paste0("^",med[i],"$")
    if (sum(stringr::str_detect(names(data), search_term)) != 1) {
      stop(paste0("Mediator ", i," does not exist in the dataset. Check the variable name."))
    }
  }

  search_term = paste0("^",treat,"$")
  if (sum(stringr::str_detect(names(data), search_term)) != 1) {
    stop("The treatment variable does not exist in the dataset. Check the variable name.")
    return(FALSE)
  }

  if (!is.null(c)) {
    for (i in 1:length(c)) {
      search_term = paste0("^",c[i],"$")
      if (sum(stringr::str_detect(names(data), search_term)) != 1) {
        stop(paste0("Covariate ", i, " does not exist in the dataset. Check the variable name."))
      }
    }
  }

  search_term = paste0("^",ymodel,"$")
  if (sum(stringr::str_detect(c("regression","poisson regression","logistic regression"),search_term)) != 1) {
    stop("Only regression, poisson regerssion and logistic are supported for the outcome variable.")
  }

  if (ymodel == "logistic regression") {

    expr = parse(text = paste0("tmp <- is.factor(data$",y,")"))
    eval(expr)
    if (tmp) {
      expr = parse(text = paste0("tmp <- length(levels(data$",y,"))"))
      eval(expr)
      if (tmp != 2) {
        stop("Logistic regression is specified for the outcome variable. The outcome must have 2 levels. Please check the variable.")
      }
    }else {
      stop("Logistic regression is specified for the outcome variable. The outcome must be a factor (categorical) variable. Please check the variable and convert it into factor using as.factor().")
    }
  }

  if (ymodel == "poisson regression") {
    expr = parse(text = paste0("tmp <- is.numeric(data$",y,")"))
    eval(expr)
    if (!tmp) {
      stop("Poisson regression is specified for the outcome variable. The outcome must be a numeric variable. Please check the variable.")
    }else {
      expr = parse(text = paste0("tmp <- sum(data$",y," %% 1 != 0, na.rm = TRUE)"))
      eval(expr)
      if (tmp != 0) {
        stop("Poisson regression is specified for the outcome variable. The outcome must be integers. Please check the variable.")
      }else {
        expr = parse(text = paste0("tmp <- sum(data$",y," < 0, na.rm = TRUE)"))
        eval(expr)
        if (tmp != 0) {
          stop("Poisson regression is specified for the outcome variable. The outcome must be non-negative. Please check the variable.")
        }
      }
    }
  }

  if (ymodel == "regression") {
    expr = parse(text = paste0("tmp <- is.numeric(data$",y,")"))
    eval(expr)
    if (!tmp) {
      stop("Regression is specified for the outcome variable. The outcome must be a numeric variable. Please check the variable.")
    }
  }

  #Need to test the following 10.1.2020
  if (length(med) != length(mmodel)) {
    stop("The number of mediators must be the same as the number of models specified for the mediators.")
  }

  for (i in 1:length(med)) {
    search_term = paste0("^",mmodel[i],"$")
    if (sum(stringr::str_detect(c("regression","poisson regression","logistic regression"),search_term)) != 1) {
      stop("Only regression, poisson regerssion and logistic are supported for the mediator(s).")
    }

    if (mmodel[i] == "logistic regression") {
      expr = parse(text = paste0("tmp <- is.factor(data$",med[i],")"))
      eval(expr)
      if (tmp) {
        expr = parse(text = paste0("tmp <- length(levels(data$",med[i],"))"))
        eval(expr)
        if (tmp != 2) {
          stop(paste0("Logistic regression is specified for mediator ", i,". This mediator must have 2 levels. Please check the variable."))
        }
      }else {
        stop(paste0("Logistic regression is specified for mediator ",i,". This mediator must be a factor (categorical) variable. Please check the variable and convert it into factor using as.factor()."))
      }
    }

    if (mmodel[i] == "poisson regression") {
      expr = parse(text = paste0("tmp <- is.numeric(data$",med[i],")"))
      eval(expr)
      if (!tmp) {
        stop("Poisson regression is specified for mediator ",i,". This mediator must be a numeric variable. Please check the variable.")
      }else {
        expr = parse(text = paste0("tmp <- sum(data$",med[i]," %% 1 != 0, na.rm = TRUE)"))
        eval(expr)
        if (tmp != 0) {
          stop("Poisson regression is specified for mediator ",i,". This mediator must be integers. Please check the variable.")
        }else {
          expr = parse(text = paste0("tmp <- sum(data$",med[i]," < 0, na.rm = TRUE)"))
          eval(expr)
          if (tmp != 0) {
            stop("Poisson regression is specified for mediator ",i,". The outcome must be non-negative. Please check the variable.")
          }
        }
      }
    }

    if (mmodel[i] == "regression") {
      expr = parse(text = paste0("tmp <- is.numeric(data$",med[i],")"))
      eval(expr)
      if (!tmp) {
        stop("Regression is specified for mediator ",i,". This mediator must be a numeric variable. Please check the variable.")
      }
    }
  }

}
