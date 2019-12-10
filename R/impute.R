mi_prepare_impute <- function(fo, data, maxit = 20) {
  df <- extract_analysis_vars(data, fo)
  fo_terms <- stats::terms(fo)
  fo_term_labels <- base::attr(fo_terms,"term.labels")
  fo_vars <- base::all.vars(fo)
  fo_length <- length(fo_vars)
  fo_interactions <- stringr::str_subset(fo_term_labels, ":")

  mi_formulas <- list()
  DV <- ""
  for (i in seq_along(fo_vars)) {
    DV <- fo_vars[i]
    search_term <- stringr::str_c("(^",DV,":)|(:",DV,":)|(:", DV, "$)|(^",DV,"$)")
    IVs <- fo_term_labels[!stringr::str_detect(fo_term_labels, search_term)]
    IVs <- paste0(IVs, collapse = " + ")
    tmp_formula <- as.formula(paste0(DV, " ~ " ,IVs))
    mi_formulas <- c(mi_formulas, tmp_formula)
  }

  mi_formulas <- mice::name.formulas(mi_formulas)
  max_missing_perc <- sum(ifelse(rowSums(as.data.frame(lapply(df, is.na))) == 0, 0 ,1))/nrow(df)*100

  if (max_missing_perc > 0 & max_missing_perc < 5) {
    max_missing_perc = 5
  }else {
    max_missing_perc = floor(max_missing_perc)
  }

  prepare_impute_obj <- list(DV = fo_vars[[1]], IVs = fo_term_labels, formulas = mi_formulas,
                             data = df, m = max_missing_perc, maxit = maxit)

  class(prepare_impute_obj) <- "prepare_impute_obj"
  return(prepare_impute_obj)
}


