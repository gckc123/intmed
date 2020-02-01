#' Estimating the mediation analysis based on the interventional effect
#'
#' @param y The outcome variable.
#' @param med A vector of the mediators.
#' @param treat The exposure variable.
#' @param mod A vector of moderators for moderated mediation analysis - Not yet implemented.
#' @param c A vector of covariates.
#' @param ymodel A character string specifying the outcome model. Current options are "regression" (for continuous variable), "logistic regression" (for binary variable), and "poisson regression" (for count variable)
#' @param mmodel A vector of character string specifying the mediator models. Current options are "regression" (for continuous variable), "logistic regression" (for binary variable), and "poisson regression" (for count variable)
#' @param treat_lv Value of the treatment variable used as the treatment condition. Default is 1.
#' @param contron_lv Value of the treatment variable used as the control condition. Default is 0.
#' @param incint A vector of boolean specifying if the exposure-mediator interactions are included into the outcome model. Default is NULL.
#' @param inc_mmint A boolean value specifying if the mediator-mediator interactions are included. Default is FALSE.
#' @param data A data frame containing all the analysis variables.
#' @param sim A numerical value specifying the number of simulation. Default is 1000.
#' @param conf.level A numerical value specifying the confidence interval the the estimates. Default is 0.95
#' @param out_scale A string specifying the scale of the analysis. Only difference scale is implemented.
#' @param complete_analysis Multiple imputation will be used to fill in missing value. Setting this flag to FALSE will force the analysis to be conducted on complete data.


mediate <- function(y, med , treat, mod = NULL, c = NULL, ymodel, mmodel, treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data, sim = 1000, conf.level = 0.95, out_scale = "difference", complete_analysis = FALSE, digits = 2) {

  validate_input(y = y, med = med, treat = treat, mod = mod, c = c, ymodel = ymodel, mmodel = mmodel, treat_lv = treat_lv, control_lv = control_lv, incint = incint, inc_mmint = inc_mmint, data = data, sim = sim, conf.level = conf.level, out_scale = out_scale, complete_analysis = complete_analysis, digits = digits)

  y_modelformula <- build_ymodel_formula(y, med = med, treat = treat, ymodel = ymodel, data = data, c = c, mod = mod, incint = incint, inc_mmint = inc_mmint)
  fo_vars <- base::all.vars(y_modelformula)
  data <- extract_analysis_vars(data, y_modelformula)
  data <- char2fac(data)

  descriptive_html <- descriptive(data = data, digits = digits, complete = complete_analysis)

  max_missing_perc <- sum(ifelse(rowSums(as.data.frame(lapply(data, is.na))) == 0, 0 ,1))/nrow(data)*100

  results = list()
  y_res = list()
  m_res = list()

  if (length(med) == 1) {
    inc_mmint = FALSE
  }

  if (max_missing_perc > 0 & complete_analysis == FALSE) {
    mi_prepare_obj <- mi_prepare_impute(y_modelformula, data)
    mids_obj <- mice::mice(data, formulas = mi_prepare_obj$formulas, m = mi_prepare_obj$m)
    for (i in 1:mi_prepare_obj$m) {
      results$individual[[i]] = medi(y = y, med = med, treat = treat, mod = mod, c = c, ymodel = ymodel, mmodel = mmodel, treat_lv = treat_lv, control_lv = control_lv, incint = incint, inc_mmint = inc_mmint, data = mice::complete(mids_obj, action = i), sim = sim, conf.level = conf.level, out_scale = out_scale)
    }
    indirect_list <- list()
    prop_list <- list()

    indirect1 = c()
    indirect2 = c()
    indirect3 = c()
    direct = c()
    total = c()
    prop1 = c()
    prop2 = c()
    prop3 = c()
    dependence = c()
    interaction = c()
    total_sim = sim*mi_prepare_obj$m

    for (i in 1:mi_prepare_obj$m) {
      if (length(med) == 1) {
        direct = c(direct, results$individual[[i]]$direct)
        indirect1 = c(indirect1, results$individual[[i]]$indirect1)
        total = c(total, results$individual[[i]]$total)
        prop1 = c(prop1, results$individual[[i]]$prop1)

      }else if (length(med) == 2) {
        indirect1 = c(indirect1, results$individual[[i]]$indirect1)
        indirect2 = c(indirect2, results$individual[[i]]$indirect2)
        direct = c(direct, results$individual[[i]]$direct)
        interaction = c(interaction, results$individual[[i]]$interaction)
        dependence = c(dependence, results$individual[[i]]$dependence)
        total = c(total, results$individual[[i]]$total)

      }else if (length(med) == 3) {
        indirect1 = c(indirect1, results$individual[[i]]$indirect1)
        indirect2 = c(indirect2, results$individual[[i]]$indirect2)
        indirect3 = c(indirect3, results$individual[[i]]$indirect3)
        direct = c(direct, results$individual[[i]]$direct)
        total = c(total, results$individual[[i]]$total)

      }
    }

    if (length(med) == 1) {

      indirect_list[[1]] = indirect1
      prop_list[[1]] = indirect_list[[1]]/total

      results$combined = list(indirect = indirect_list, direct = direct, total = total, prop = prop_list, interaction = NULL, dependence = NULL)

    }else if (length(med) == 2) {

      indirect_list[[1]] = indirect1
      indirect_list[[2]] = indirect2
      prop_list[[1]] = indirect_list[[1]]/total
      prop_list[[2]] = indirect_list[[2]]/total

      results$combined = list(indirect = indirect_list, direct = direct, total = total, prop = prop_list, interaction = interaction, dependence = dependence)

    }else if (length(med) == 3) {

      indirect_list[[1]] = indirect1
      indirect_list[[2]] = indirect2
      indirect_list[[3]] = indirect3
      prop_list[[1]] = indirect_list[[1]]/total
      prop_list[[2]] = indirect_list[[2]]/total
      prop_list[[3]] = indirect_list[[3]]/total

      results$combined = list(indirect = indirect_list, direct = direct, total = total, prop = prop_list, interaction = NULL, dependence = NULL)
    }
    results$mids = mids_obj

    for (i in 1:mi_prepare_obj$m) {
      y_res[[i]] <- results$individual[[i]]$ymodel
      for (j in 1:length(med)) {
        if (i == 1) {
          m_res[[j]] = list()
        }
        expr <- parse(text = paste0("m_res[[j]][[i]] <- results$individual[[i]]$m",j,"_model"))
        eval(expr)
      }
    }

    results$y_pooled_res <- mice::pool(y_res)
    results$m_pooled_res <- list()

    for (i in 1:length(med)) {
      results$m_pooled_res[[i]] <- mice::pool(m_res[[i]])
      expr = parse(text = paste0("indirect_list[[i]] = indirect",i))
      eval(expr)
    }
    results$model_summary <- gen_med_reg_table(y_res = results$y_pooled_res, m_res = results$m_pooled_res, ymodel = ymodel, mmodel = mmodel, conf.level = conf.level, digits = digits)
  }else {
    results$individual = medi(y = y, med = med, treat = treat, mod = mod, c = c, ymodel = ymodel, mmodel = mmodel, treat_lv = treat_lv, control_lv = control_lv, incint = incint, inc_mmint = inc_mmint, data = data, sim = sim, conf.level = conf.level, out_scale = out_scale)
    for (i  in 1:length(med)) {
      expr = parse(text = paste0("m_res[[i]] = results$individual$m",i,"_model"))
      eval(expr)
    }
    #These codes can be made more efficients by changing output of the medi function.
    if (length(med) == 1) {
      results$combined = list(indirect = list(results$individual$indirect1), direct = results$individual$direct, total = results$individual$total, prop = list(results$individual$prop1), interaction = NULL, dependence = NULL)
    }else if (length(med) == 2) {
      results$combined = list(indirect = list(results$individual$indirect1, results$individual$indirect2), direct = results$individual$direct, total = results$individual$total, prop = list(results$individual$prop1, results$individual$prop2), interaction = results$individual$interaction, dependence = results$individual$dependence)
    }else if (length(med) == 3) {
      results$combined = list(indirect = list(results$individual$indirect1, results$individual$indirect2, results$individual$indirect2), direct = results$individual$direct, total = results$individual$total, prop = list(results$individual$prop1, results$individual$prop2, results$individual$prop3), interaction = NULL, dependence = NULL)
    }
    results$model_summary <- gen_med_reg_table(y_res = results$individual$ymodel, m_res = m_res, ymodel = ymodel, mmodel = mmodel, conf.level = conf.level, digits = digits)
  }

  model_summary_html <- gen_med_reg_html(results$model_summary, y = y, med = med, treat = treat, c = c, ymodel = ymodel, mmodel = mmodel, incint = incint, inc_mmint = inc_mmint, conf.level, data_head = head(data, 1), treat_lv = treat_lv, control_lv = control_lv)
  mediation_res_html <- gen_med_table_html(med_res = results$combined, med = med, conf.level = conf.level, digits = digits)

  tmp_text <- paste0("<h4><u>Descriptive statistics</u></h4> The table below shows the descriptive statistics of all analyses variables. The overall sample size is ",nrow(data),". ")
  if (max_missing_perc > 0) {
    if (complete_analysis == TRUE) {
      tmp_text <- paste(tmp_text, "There were ", round(max_missing_perc,2),"% cases with missing data. Complete case analysis wass used for the subsequent mediation analysis.")
    }else {
      tmp_text <- paste(tmp_text, "There were ", round(max_missing_perc,2),"% cases with missing data. Multiple imputation was used to impute missing data (Rubin, 2004) and ", mi_prepare_obj$m, "datasets were imputed using the R package MICE (van Buuren, 2011).")
    }
  }
  tmp_text <- paste(tmp_text, "<br/>")

  results$res_html <- c(tmp_text, descriptive_html)

  results$res_html <- c(results$res_html, model_summary_html)

  results$res_html <- c(results$res_html, mediation_res_html)

  sink("res.html")
  cat(results$res_html)
  sink()

  shell.exec("res.html")
  return(results)

}

medi <- function(y, med , treat, mod = NULL, c = NULL, ymodel, mmodel, treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = TRUE, data, sim = 1000, conf.level = 0.95, out_scale = "difference") {
  data <- tibble::add_column(data, missing = rowSums(sapply(data, is.na)))
  data <- data[data$missing == 0, 1:length(data)-1]

  no_cores = parallel::detectCores() - 1
  cl <- parallel::makeCluster(no_cores, outfile=paste0('./info_parallel.log'))

  doParallel::registerDoParallel(cl)
  m2_modelformula = NULL
  m2_modelformula_cond = NULL
  m2res = NULL
  m2res_cond = NULL
  m2coeff = NULL
  m2coeff_cond = NULL
  m2_fo_terms = NULL
  m2_cat_var_dict = NULL
  m2_cond_fo_terms = NULL
  m2_cat_var_dict = NULL

  indirect1 = rep(0, sim)
  indirect2 = rep(0, sim)
  indirect3 = rep(0, sim)
  direct = rep(0, sim)
  total = rep(0, sim)

  prop1 = rep(0, sim)
  prop2 = rep(0, sim)
  prop3 = rep(0, sim)

  dependence = rep(0, sim)
  interaction = rep(0, sim)

  m1_modelformula <- build_mmodel_formula(med = med, medposition = 1, treat = treat, mmodel = mmodel, data = data, cond = NULL, c, mod = mod)
  m1res <- run_model(m1_modelformula, mmodel[1], data)

  m1_fo_terms  = random_draw_formula_terms(model = m1res, data_name = "data", coeff_name = "current_m1coeff")
  m1_cat_var_dict = gen_cat_var_dict(m1res)

  m1_0_formula <- random_draw_formula(m1_cat_var_dict, m1_fo_terms, target = treat, target_lv = control_lv)
  m1_1_formula <- random_draw_formula(m1_cat_var_dict, m1_fo_terms, target = treat, target_lv = treat_lv)

  y_modelformula <- build_ymodel_formula(y, med = med, treat = treat, ymodel = ymodel, data = data, c = c, mod = mod, incint = incint, inc_mmint = inc_mmint)
  yres <- run_model(y_modelformula, ymodel, data)

  y_temodelformula <- build_ymodel_formula(y, med = NULL, treat = treat, ymodel = ymodel, data = data, c = c, mod = NULL, incint = NULL)
  yres_te <- run_model(y_temodelformula, ymodel, data)

  if (length(med) >=  2) {
    m2_modelformula_marg <- build_mmodel_formula(med = med, medposition = 2, treat = treat, mmodel = mmodel, data = data, cond = NULL, c, mod = mod)
    m2_modelformula_cond <- build_mmodel_formula(med = med, medposition = 2, treat = treat, mmodel = mmodel, data = data, cond = 1, c, mod = mod)
    m2res_marg <- run_model(m2_modelformula_marg, mmodel[2], data)
    m2res_cond <- run_model(m2_modelformula_cond, mmodel[2], data)

    m2marg_fo_terms = random_draw_formula_terms(model = m2res_marg, data_name = "data", coeff_name = "current_m2marg_coeff")
    m2marg_cat_var_dict = gen_cat_var_dict(m2res_marg)
    m2marg_0_formula = random_draw_formula(m2marg_cat_var_dict, m2marg_fo_terms, target = treat, target_lv = control_lv)
    m2marg_1_formula = random_draw_formula(m2marg_cat_var_dict, m2marg_fo_terms, target = treat, target_lv = treat_lv)

    m2cond_fo_terms = random_draw_formula_terms(model = m2res_cond, data_name = "data", coeff_name = "current_m2cond_coeff")
    m2cond_cat_var_dict = gen_cat_var_dict(m2res_cond)

    m2cond_formula_00 = random_draw_formula(m2cond_cat_var_dict, m2cond_fo_terms, target = c(treat, med[1]), target_lv = c(control_lv, 0))
    m2cond_fixed_part = m2cond_formula_00$fixed_part
    m2cond_formula_00_part = m2cond_formula_00$target_part
    #double check this - the 10 and 01 part may not be necessary! 4.11.2019
    m2cond_formula_10 = random_draw_formula(m2cond_cat_var_dict, m2cond_fo_terms, target = c(treat, med[1]), target_lv = c(treat_lv, 0))
    m2cond_formula_10_part = m2cond_formula_10$target_part
    m2cond_formula_01 = random_draw_formula(m2cond_cat_var_dict, m2cond_fo_terms, target = c(treat, med[1]), target_lv = c(control_lv, 1))
    m2cond_formula_01_part = m2cond_formula_01$target_part
    m2cond_formula_11 = random_draw_formula(m2cond_cat_var_dict, m2cond_fo_terms, target = c(treat, med[1]), target_lv = c(treat_lv, 1))
    m2cond_formula_11_part = m2cond_formula_11$target_part
  }

  if (length(med) == 3) {

    m3_modelformula_marg <- build_mmodel_formula(med = med, medposition = 3, treat = treat, mmodel = mmodel, data = data, cond = NULL, c, mod = mod)
    m3_modelformula_cond_m1 <- build_mmodel_formula(med = med, medposition = 3, treat = treat, mmodel = mmodel, data = data, cond = 1, c, mod = mod)
    m3_modelformula_cond_m2 <- build_mmodel_formula(med = med, medposition = 3, treat = treat, mmodel = mmodel, data = data, cond = 2, c, mod = mod)
    m3_modelformula_cond_m1m2 <- build_mmodel_formula(med = med, medposition = 3, treat = treat, mmodel = mmodel, data = data, cond = c(1,2), c, mod = mod)
    m3res_marg <- run_model(m3_modelformula_marg, mmodel[3], data)
    m3res_cond_m1 <- run_model(m3_modelformula_cond_m1, mmodel[3], data)
    m3res_cond_m2 <- run_model(m3_modelformula_cond_m2, mmodel[3], data)
    m3res_cond_m1m2 <- run_model(m3_modelformula_cond_m1m2, mmodel[3], data)

    m3marg_fo_terms = random_draw_formula_terms(model = m3res_marg, data_name = "data", coeff_name = "current_m3marg_coeff")
    m3marg_cat_var_dict = gen_cat_var_dict(m3res_marg)
    m3marg_0_formula = random_draw_formula(m3marg_cat_var_dict, m3marg_fo_terms, target = treat, target_lv = control_lv)
    m3marg_1_formula = random_draw_formula(m3marg_cat_var_dict, m3marg_fo_terms, target = treat, target_lv = treat_lv)

    m3cond_m1_fo_terms = random_draw_formula_terms(model = m3res_cond_m1, data_name = "data", coeff_name = "current_m3cond_m1_coeff")
    m3cond_m1_cat_var_dict = gen_cat_var_dict(m3res_cond_m1)
    m3cond_formula_00A = random_draw_formula(m3cond_m1_cat_var_dict, m3cond_m1_fo_terms, target = c(treat, med[1]), target_lv = c(control_lv, 0))

    m3cond_m2_fo_terms = random_draw_formula_terms(model = m3res_cond_m2, data_name = "data", coeff_name = "current_m3cond_m2_coeff")
    m3cond_m2_cat_var_dict = gen_cat_var_dict(m3res_cond_m2)
    m3cond_formula_0A0 = random_draw_formula(m3cond_m2_cat_var_dict, m3cond_m2_fo_terms, target = c(treat, med[2]), target_lv = c(control_lv, 0))
    m3cond_formula_0A0$target_part = stringr::str_replace_all(m3cond_formula_0A0$target_part, "m1_0", "m2_0_marg")

    m3cond_m1m2_fo_terms = random_draw_formula_terms(model = m3res_cond_m1m2, data_name = "data", coeff_name = "current_m3cond_m1m2_coeff")
    m3cond_m1m2_cat_var_dict = gen_cat_var_dict(m3res_cond_m1m2)
    m3cond_formula_000 = random_draw_formula(m3cond_m1m2_cat_var_dict, m3cond_m1m2_fo_terms, target = c(treat, med[1], med[2]), target_lv = c(control_lv, 0,0))
    m3cond_formula_000$target_part = stringr::str_replace_all(m3cond_formula_000$target_part, "m2_0", "m2_0_cond")
    m3cond_formula_111 = random_draw_formula(m3cond_m1m2_cat_var_dict, m3cond_m1m2_fo_terms, target = c(treat, med[1], med[2]), target_lv = c(treat_lv, 1, 1))
    m3cond_formula_111$target_part = stringr::str_replace_all(m3cond_formula_111$target_part, "m2_1", "m2_1_cond")

  }

  y_fo_terms = random_draw_formula_terms(model = yres, data_name = "data", coeff_name = "current_ycoeff")
  y_cat_var_dict = gen_cat_var_dict(yres)

  y_te_fo_terms = random_draw_formula_terms(model = yres_te, data_name = "data", coeff_name = "current_ycoeff_te")
  y_te_cat_var_dict = gen_cat_var_dict(yres_te)
  y_te_formula_0 = random_draw_formula(y_te_cat_var_dict, y_te_fo_terms, target = treat, target_lv = control_lv)
  y_te_formula_fixed_part = y_te_formula_0$fixed_part
  y_te_formula_0_part = y_te_formula_0$target_part
  y_te_formula_1 = random_draw_formula(y_te_cat_var_dict, y_te_fo_terms, target = treat, target_lv = treat_lv)
  y_te_formula_1_part = y_te_formula_1$target_part

  if (length(med) == 2) {

    y_formula_000 = random_draw_formula(y_cat_var_dict, y_fo_terms, target = c(treat, med[1], med[2]), target_lv = c(control_lv, 0,0))
    y_formula_fixed_part = y_formula_000$fixed_part
    y_formula_000_cond_part = y_formula_000$target_part
    y_formula_000_cond_part = stringr::str_replace_all(y_formula_000_cond_part, "m2_0", "m2_0_cond")
    y_formula_100_cond = random_draw_formula(y_cat_var_dict, y_fo_terms, target = c(treat, med[1], med[2]), target_lv = c(treat_lv, 0, 0))
    y_formula_100_cond_part = y_formula_100_cond$target_part
    y_formula_100_cond_part = stringr::str_replace_all(y_formula_100_cond_part, "m2_0", "m2_0_cond")
    y_formula_110_marg = random_draw_formula(y_cat_var_dict, y_fo_terms, target = c(treat, med[1], med[2]), target_lv = c(treat_lv, 1, 0))
    y_formula_110_marg_part = y_formula_110_marg$target_part
    y_formula_110_marg_part = stringr::str_replace_all(y_formula_110_marg_part, "m2_0", "m2_0_marg")
    y_formula_100_marg = random_draw_formula(y_cat_var_dict, y_fo_terms, target = c(treat, med[1], med[2]), target_lv = c(treat_lv, 0, 0))
    y_formula_100_marg_part = y_formula_100_marg$target_part
    y_formula_100_marg_part = stringr::str_replace_all(y_formula_100_marg_part, "m2_0", "m2_0_marg")
    y_formula_101_marg = random_draw_formula(y_cat_var_dict, y_fo_terms, target = c(treat, med[1], med[2]), target_lv = c(treat_lv, 0, 1))
    y_formula_101_marg_part = y_formula_101_marg$target_part
    y_formula_101_marg_part = stringr::str_replace_all(y_formula_101_marg_part, "m2_1", "m2_1_marg")
    y_formula_111_cond = random_draw_formula(y_cat_var_dict, y_fo_terms, target = c(treat, med[1], med[2]), target_lv = c(treat_lv, 1, 1))
    y_formula_111_cond_part = y_formula_111_cond$target_part
    y_formula_111_cond_part = stringr::str_replace_all(y_formula_111_cond_part, "m2_1", "m2_1_cond")
    y_formula_111_marg_part = stringr::str_replace_all(y_formula_111_cond_part, "m2_1_cond", "m2_1_marg")

  }else if (length(med) == 1) {

    y_formula_00 = random_draw_formula(y_cat_var_dict, y_fo_terms, target = c(treat,med[1]), target_lv = c(control_lv, 0))
    y_formula_fixed_part = y_formula_00$fixed_part
    y_formula_00_part = y_formula_00$target_part
    y_formula_10 = random_draw_formula(y_cat_var_dict, y_fo_terms, target = c(treat,med[1]), target_lv = c(treat_lv, 0))
    y_formula_10_part = y_formula_10$target_part
    y_formula_01 = random_draw_formula(y_cat_var_dict, y_fo_terms, target = c(treat,med[1]), target_lv = c(control_lv, 1))
    y_formula_01_part = y_formula_01$target_part
    y_formula_11 = random_draw_formula(y_cat_var_dict, y_fo_terms, target = c(treat,med[1]), target_lv = c(treat_lv, 1))
    y_formula_11_part = y_formula_11$target_part

  }else if (length(med) == 3) {

    y_formula_0000_cond = random_draw_formula(y_cat_var_dict, y_fo_terms, target = c(treat, med[1], med[2], med[3]), target_lv = c(control_lv, 0, 0, 0))
    y_formula_fixed_part = y_formula_0000_cond$fixed_part
    y_formula_0000_cond_m1m2m3_part = y_formula_0000_cond$target_part
    y_formula_0000_cond_m1m2m3_part = stringr::str_replace_all(y_formula_0000_cond_m1m2m3_part, "m2_0", "m2_0_cond")
    y_formula_0000_cond_m1m2m3_part = stringr::str_replace_all(y_formula_0000_cond_m1m2m3_part, "m3_0", "m3_0_cond_m1m2")
    y_formula_1000_cond_m1m2m3 = random_draw_formula(y_cat_var_dict, y_fo_terms, target = c(treat, med[1], med[2], med[3]), target_lv = c(treat_lv, 0, 0, 0))
    y_formula_1000_cond_m1m2m3_part = y_formula_1000_cond_m1m2m3$target_part
    y_formula_1000_cond_m1m2m3_part = stringr::str_replace_all(y_formula_1000_cond_m1m2m3_part, "m2_0", "m2_0_cond")
    y_formula_1000_cond_m1m2m3_part = stringr::str_replace_all(y_formula_1000_cond_m1m2m3_part, "m3_0", "m3_0_cond_m1m2")

    y_formula_1111_cond = random_draw_formula(y_cat_var_dict, y_fo_terms, target = c(treat, med[1], med[2], med[3]), target_lv = c(treat_lv, 1, 1, 1))
    y_formula_1111_cond_m1m2m3_part = y_formula_1111_cond$target_part
    y_formula_1111_cond_m1m2m3_part = stringr::str_replace_all(y_formula_1111_cond_m1m2m3_part, "m2_1","m2_1_cond")
    y_formula_1111_cond_m1m2m3_part = stringr::str_replace_all(y_formula_1111_cond_m1m2m3_part, "m3_1", "m3_1_cond_m1m2")

    y_formula_1100_cond_m2m3 = random_draw_formula(y_cat_var_dict, y_fo_terms, target = c(treat, med[1], med[2], med[3]), target_lv = c(treat_lv, 1, 0, 0))
    y_formula_1100_cond_m2m3_part = y_formula_1100_cond_m2m3$target_part
    y_formula_1100_cond_m2m3_part = stringr::str_replace_all(y_formula_1100_cond_m2m3_part, "m2_0","m2_0_marg")
    y_formula_1100_cond_m2m3_part = stringr::str_replace_all(y_formula_1100_cond_m2m3_part, "m3_0", "m3_0_cond_m2")
    y_formula_1000_cond_m2m3 = random_draw_formula(y_cat_var_dict, y_fo_terms, target = c(treat, med[1], med[2], med[3]), target_lv = c(treat_lv, 0, 0, 0))
    y_formula_1000_cond_m2m3_part = y_formula_1000_cond_m2m3$target_part
    y_formula_1000_cond_m2m3_part = stringr::str_replace_all(y_formula_1000_cond_m2m3_part, "m2_0","m2_0_marg")
    y_formula_1000_cond_m2m3_part = stringr::str_replace_all(y_formula_1000_cond_m2m3_part, "m3_0", "m3_0_cond_m2")

    y_formula_1010_cond_m1m3 = random_draw_formula(y_cat_var_dict, y_fo_terms, target = c(treat, med[1], med[2], med[3]), target_lv = c(treat_lv, 0, 1, 0))
    y_formula_1010_cond_m1m3_part = y_formula_1010_cond_m1m3$target_part
    y_formula_1010_cond_m1m3_part = stringr::str_replace_all(y_formula_1010_cond_m1m3_part, "m2_1", "m2_1_marg")
    y_formula_1010_cond_m1m3_part = stringr::str_replace_all(y_formula_1010_cond_m1m3_part, "m3_0", "m3_0_cond_m1")
    y_formula_1000_cond_m1m3 = random_draw_formula(y_cat_var_dict, y_fo_terms, target = c(treat, med[1], med[2], med[3]), target_lv = c(treat_lv, 0, 0, 0))
    y_formula_1000_cond_m1m3_part = y_formula_1000_cond_m1m3$target_part
    y_formula_1000_cond_m1m3_part = stringr::str_replace_all(y_formula_1000_cond_m1m3_part, "m2_0", "m2_0_marg")
    y_formula_1000_cond_m1m3_part = stringr::str_replace_all(y_formula_1000_cond_m1m3_part, "m3_0", "m3_0_cond_m1")

    y_formula_1001_cond_m1m2 = random_draw_formula(y_cat_var_dict, y_fo_terms, target = c(treat, med[1], med[2], med[3]), target_lv = c(treat_lv, 0, 0, 1))
    y_formula_1001_cond_m1m2_part = y_formula_1001_cond_m1m2$target_part
    y_formula_1001_cond_m1m2_part = stringr::str_replace_all(y_formula_1001_cond_m1m2_part, "m3_1", "m3_1_marg")
    y_formula_1001_cond_m1m2_part = stringr::str_replace_all(y_formula_1001_cond_m1m2_part, "m2_0", "m2_0_cond")
    y_formula_1000_cond_m1m2 = random_draw_formula(y_cat_var_dict, y_fo_terms, target = c(treat, med[1], med[2], med[3]), target_lv = c(treat_lv, 0, 0, 0))
    y_formula_1000_cond_m1m2_part = y_formula_1000_cond_m1m2$target_part
    y_formula_1000_cond_m1m2_part = stringr::str_replace_all(y_formula_1000_cond_m1m2_part, "m3_0", "m3_0_marg")
    y_formula_1000_cond_m1m2_part = stringr::str_replace_all(y_formula_1000_cond_m1m2_part, "m2_0", "m2_0_cond")

  }

  ####################################################################################################################

  m1coeff <- MASS::mvrnorm(n = sim, m1res$coefficients, vcov(m1res))
  ycoeff <- MASS::mvrnorm(n = sim, yres$coefficients, vcov(yres))
  y_tecoeff <- MASS::mvrnorm(n = sim, yres_te$coefficients, vcov(yres_te))

  if (length(med) == 1) {
    m1_fixed_part_value = NULL
    y_fixed_part_value = NULL
    sim_res <- foreach(i=1:sim, .combine = rbind, .inorder = TRUE, .export = c("random_draw","generate_estimates")) %dopar% {
      current_m1coeff = m1coeff[i,]
      current_ycoeff = ycoeff[i,]
      current_ycoeff_te = y_tecoeff[i,]
      expr = parse(text = paste0("m1_fixed_part_value = ",paste(m1_0_formula$fixed_part, collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("m1_0 = ",paste(paste(m1_0_formula$target_part, collapse = "+"), "m1_fixed_part_value", sep = "+")))
      eval(expr)
      expr = parse(text = paste0("m1_1 = ", paste(paste(m1_1_formula$target_part, collapse = "+"), "m1_fixed_part_value", sep = "+")))
      eval(expr)

      if (length(m1_0) == 1) {
        m1_0 = rep(m1_0, nrow(data))
        m1_1 = rep(m1_1, nrow(data))
      }
      tmp = random_draw(list(m1_0, m1_1), m1res, mmodel[1])
      m1_0 = tmp[[1]]
      m1_1 = tmp[[2]]

      expr = parse(text = paste0("y_fixed_part_value = ", paste(y_formula_fixed_part, collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("y00 = ",paste(paste(y_formula_00_part, collapse = "+"), "y_fixed_part_value", sep = "+")))
      eval(expr)
      expr = parse(text = paste0("y10 = ",paste(paste(y_formula_10_part, collapse = "+"), "y_fixed_part_value", sep = "+")))
      eval(expr)
      expr = parse(text = paste0("y01 = ",paste(paste(y_formula_01_part, collapse = "+"), "y_fixed_part_value", sep = "+")))
      eval(expr)
      expr = parse(text = paste0("y11 = ",paste(paste(y_formula_11_part, collapse = "+"), "y_fixed_part_value", sep = "+")))
      eval(expr)

      expr = parse(text = paste0("y_te_fixed_part_value = ", paste(y_te_formula_fixed_part, collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("y0 = ", paste(paste(y_te_formula_0_part, collapse = "+"), "y_te_fixed_part_value", sep = "+")))
      eval(expr)
      expr = parse(text = paste0("y1 = ", paste(paste(y_te_formula_1_part, collapse = "+"), "y_te_fixed_part_value", sep = "+")))
      eval(expr)

      if (length(y0) == 1) {
        y0 = rep(y0, nrow(data))
        y1 = rep(y1, nrow(data))
      }

      generate_estimates(data.frame(y00,y01,y10,y11,y0,y1),ymodel, out_scale)

    }
  }else if (length(med) == 2) {
    m1_fixed_part_value = NULL
    m2cond_fixed_part_value = NULL
    m2marg_fixed_part_value = NULL
    y_fixed_part_value = NULL
    #NEED TO HAVE BETTER NAMING OF THE VARIABLES - m2coeff_cond vs m2cond_coeff??!
    m2coeff_marg = MASS::mvrnorm(n = sim, m2res_marg$coefficients, vcov(m2res_marg))
    m2coeff_cond = MASS::mvrnorm(n = sim, m2res_cond$coefficients, vcov(m2res_cond))

    sim_res <- foreach(i=1:sim, .combine = rbind, .inorder = TRUE, .export = c("random_draw","generate_estimates")) %dopar% {
      current_m1coeff = m1coeff[i,]
      current_ycoeff = ycoeff[i,]
      current_m2cond_coeff = m2coeff_cond[i,]
      current_m2marg_coeff = m2coeff_marg[i,]
      current_ycoeff_te = y_tecoeff[i,]

      expr = parse(text = paste0("m1_fixed_part_value = ", paste0(m1_0_formula$fixed_part, collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("m1_0 = ", paste(paste(m1_0_formula$target_part, collapse = "+"),"m1_fixed_part_value", sep = "+")))
      eval(expr)
      expr = parse(text = paste0("m1_1 = ", paste(paste(m1_1_formula$target_part, collapse = "+"),"m1_fixed_part_value", sep = "+")))
      eval(expr)

      if (length(m1_0) == 1) {
        m1_0 = rep(m1_0, nrow(data))
        m1_1 = rep(m1_1, nrow(data))
      }

      tmp = random_draw(list(m1_0, m1_1), m1res, mmodel[1])
      m1_0 = tmp[[1]]
      m1_1 = tmp[[2]]

      expr = parse(text = paste0("m2marg_fixed_part_value = ", paste0(m2marg_0_formula$fixed_part, collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("m2_0_marg = ", paste(paste(m2marg_0_formula$target_part, collapse = "+"), "m2marg_fixed_part_value", sep = "+")))
      eval(expr)
      expr = parse(text = paste0("m2_1_marg = ", paste(paste(m2marg_1_formula$target_part, collapse = "+"), "m2marg_fixed_part_value", sep = "+")))
      eval(expr)

      if (length(m2_0_marg) == 1) {
        m2_0_marg = rep(m2_0_marg, nrow(data))
        m2_1_marg = rep(m2_1_marg, nrow(data))
      }

      tmp = random_draw(list(m2_0_marg, m2_1_marg), m2res_marg, mmodel[[2]])
      m2_0_marg = tmp[[1]]
      m2_1_marg = tmp[[2]]

      expr = parse(text = paste0("m2cond_fixed_part_value = ", paste0(m2cond_formula_00$fixed_part, collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("m2_0_cond = ", paste(paste(m2cond_formula_00_part, collapse = "+"), "m2cond_fixed_part_value", sep = "+")))
      eval(expr)
      expr = parse(text = paste0("m2_1_cond = ", paste(paste(m2cond_formula_11_part, collapse = "+"), "m2cond_fixed_part_value", sep = "+")))
      eval(expr)

      tmp = random_draw(list(m2_0_cond, m2_1_cond), m2res_cond, mmodel[[2]])
      m2_0_cond = tmp[[1]]
      m2_1_cond = tmp[[2]]

      expr = parse(text = paste0("y_fixed_part_value = ", paste0(y_formula_fixed_part, collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("y_000_cond = ", paste(paste(y_formula_000_cond_part, collapse = "+"), "y_fixed_part_value", sep = "+")))
      eval(expr)
      expr = parse(text = paste0("y_100_cond = ", paste(paste(y_formula_100_cond_part, collapse = "+"), "y_fixed_part_value", sep = "+")))
      eval(expr)
      expr = parse(text = paste0("y_110_marg = ", paste(paste(y_formula_110_marg_part, collapse = "+"), "y_fixed_part_value", sep = "+")))
      eval(expr)
      expr = parse(text = paste0("y_100_marg = ", paste(paste(y_formula_100_marg_part, collapse = "+"), "y_fixed_part_value", sep = "+")))
      eval(expr)
      expr = parse(text = paste0("y_101_marg = ", paste(paste(y_formula_101_marg_part, collapse = "+"), "y_fixed_part_value", sep = "+")))
      eval(expr)
      expr = parse(text = paste0("y_111_cond = ", paste(paste(y_formula_111_cond_part, collapse = "+"), "y_fixed_part_value", sep = "+")))
      eval(expr)
      expr = parse(text = paste0("y_111_marg = ", paste(paste(y_formula_111_marg_part, collapse = "+"), "y_fixed_part_value", sep = "+")))
      eval(expr)

      expr = parse(text = paste0("y_te_fixed_part_value = ", paste(y_te_formula_fixed_part, collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("y0 = ", paste(paste(y_te_formula_0_part, collapse = "+"), "y_te_fixed_part_value", sep = "+")))
      eval(expr)
      expr = parse(text = paste0("y1 = ", paste(paste(y_te_formula_1_part, collapse = "+"), "y_te_fixed_part_value", sep = "+")))
      eval(expr)

      if (length(y0) == 1) {
        y0 = rep(y0, nrow(data))
        y1 = rep(y1, nrow(data))
      }

      generate_estimates(data.frame(y_000_cond, y_100_cond, y_110_marg, y_100_marg, y_101_marg, y_111_cond, y_111_marg, y0, y1), ymodel, out_scale = out_scale)
    }
  }else if (length(med) == 3) {
    m2coeff_marg = MASS::mvrnorm(n = sim, m2res_marg$coefficients, vcov(m2res_marg))
    m2coeff_cond = MASS::mvrnorm(n = sim, m2res_cond$coefficients, vcov(m2res_cond))

    m3coeff_marg = MASS::mvrnorm(n = sim, m3res_marg$coefficients, vcov(m3res_marg))
    m3coeff_cond_m1 = MASS::mvrnorm(n = sim, m3res_cond_m1$coefficients, vcov(m3res_cond_m1))
    m3coeff_cond_m2 = MASS::mvrnorm(n = sim, m3res_cond_m2$coefficients, vcov(m3res_cond_m2))
    m3coeff_cond_m1m2 = MASS::mvrnorm(n = sim, m3res_cond_m1m2$coefficients, vcov(m3res_cond_m1m2))

    sim_res <- foreach(i=1:sim, .combine = rbind, .inorder = TRUE, .export = c("random_draw","generate_estimates")) %dopar% {
      current_m1coeff = m1coeff[i,]
      current_ycoeff = ycoeff[i,]

      current_m2cond_coeff = m2coeff_cond[i,]
      current_m2marg_coeff = m2coeff_marg[i,]

      current_m3cond_m1_coeff = m3coeff_cond_m1[i,]
      current_m3cond_m2_coeff = m3coeff_cond_m2[i,]
      current_m3cond_m1m2_coeff = m3coeff_cond_m1m2[i,]
      current_m3marg_coeff = m3coeff_marg[i,]

      current_ycoeff_te = y_tecoeff[i,]

      expr = parse(text = paste0("m1_fixed_part_value = ", paste0(m1_0_formula$fixed_part, collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("m1_0 = ", paste(c(m1_0_formula$target_part, "m1_fixed_part_value"), collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("m1_1 = ", paste(c(m1_1_formula$target_part, "m1_fixed_part_value"), collapse = "+")))
      eval(expr)

      if (length(m1_0) == 1) {
        m1_0 = rep(m1_0, nrow(data))
        m1_1 = rep(m1_1, nrow(data))
      }

      tmp = random_draw(list(m1_0, m1_1), m1res, mmodel[1])
      m1_0 = tmp[[1]]
      m1_1 = tmp[[2]]

      expr = parse(text = paste0("m2marg_fixed_part_value = ", paste0(m2marg_0_formula$fixed_part, collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("m2_0_marg = ", paste(c(m2marg_0_formula$target_part, "m2marg_fixed_part_value"), collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("m2_1_marg = ", paste(c(m2marg_1_formula$target_part, "m2marg_fixed_part_value"), collapse = "+")))
      eval(expr)

      if (length(m2_0_marg) == 1) {
        m2_0_marg = rep(m2_0_marg, nrow(data))
        m2_1_marg = rep(m2_1_marg, nrow(data))
      }

      tmp = random_draw(list(m2_0_marg, m2_1_marg), m2res_marg, mmodel[[2]])
      m2_0_marg = tmp[[1]]
      m2_1_marg = tmp[[2]]

      expr = parse(text = paste0("m2cond_fixed_part_value = ", paste0(m2cond_formula_00$fixed_part, collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("m2_0_cond = ", paste(c(m2cond_formula_00_part, "m2cond_fixed_part_value"), collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("m2_1_cond = ", paste(c(m2cond_formula_11_part, "m2cond_fixed_part_value"), collapse = "+")))
      eval(expr)

      tmp = random_draw(list(m2_0_cond, m2_1_cond), m2res_cond, mmodel[[2]])
      m2_0_cond = tmp[[1]]
      m2_1_cond = tmp[[2]]

      expr = parse(text = paste0("m3marg_fixed_part_value = ", paste0(m3marg_0_formula$fixed_part, collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("m3_0_marg = ", paste(c(m3marg_0_formula$target_part, "m3marg_fixed_part_value"), collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("m3_1_marg = ", paste(c(m3marg_1_formula$target_part, "m3marg_fixed_part_value"), collapse = "+")))
      eval(expr)

      if (length(m3_0_marg) == 1) {
        m3_0_marg = rep(m3_0_marg, nrow(data))
        m3_1_marg = rep(m3_1_marg, nrow(data))
      }

      tmp = random_draw(list(m3_0_marg, m3_1_marg), m3res_marg, mmodel[[3]])
      m3_0_marg = tmp[[1]]
      m3_1_marg = tmp[[2]]

      expr = parse(text = paste0("m3cond_m1_fixed_part_value = ", paste0(m3cond_formula_00A$fixed_part, collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("m3_0_cond_m1 = ", paste(c(m3cond_formula_00A$target_part, "m3cond_m1_fixed_part_value"), collapse = "+")))
      eval(expr)
      tmp = random_draw(list(m3_0_cond_m1), m3res_cond_m1, mmodel[[3]])
      m3_0_cond_m1 = tmp[[1]]

      expr = parse(text = paste0("m3cond_m2_fixed_part_value = ", paste0(m3cond_formula_0A0$fixed_part, collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("m3_0_cond_m2 = ", paste(c(m3cond_formula_0A0$target_part, "m3cond_m2_fixed_part_value"), collapse = "+")))
      eval(expr)
      tmp = random_draw(list(m3_0_cond_m2), m3res_cond_m2, mmodel[[3]])
      m3_0_cond_m2 = tmp[[1]]

      expr = parse(text = paste0("m3cond_m1m2_fixed_part_value = ", paste0(m3cond_formula_000$fixed_part, collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("m3_0_cond_m1m2 = ", paste(c(m3cond_formula_000$target_part, "m3cond_m1m2_fixed_part_value"), collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("m3_1_cond_m1m2 = ", paste(c(m3cond_formula_111$target_part, "m3cond_m1m2_fixed_part_value"), collapse = "+")))
      eval(expr)
      tmp = random_draw(list(m3_0_cond_m1m2, m3_1_cond_m1m2), m3res_cond_m1m2, mmodel[[3]])
      m3_0_cond_m1m2 = tmp[[1]]
      m3_1_cond_m1m2 = tmp[[2]]

      expr = parse(text = paste0("y_fixed_part_value = ", paste0(y_formula_fixed_part, collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("y_0000_cond_m1m2m3 = ", paste(c(y_formula_0000_cond_m1m2m3_part,"y_fixed_part_value"), collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("y_1000_cond_m1m2m3 = ", paste(c(y_formula_1000_cond_m1m2m3_part,"y_fixed_part_value"), collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("y_1100_cond_m2m3 = ", paste(c(y_formula_1100_cond_m2m3_part,"y_fixed_part_value"), collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("y_1000_cond_m2m3 = ", paste(c(y_formula_1000_cond_m2m3_part,"y_fixed_part_value"), collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("y_1010_cond_m1m3 = ", paste(c(y_formula_1010_cond_m1m3_part,"y_fixed_part_value"), collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("y_1000_cond_m1m3 = ", paste(c(y_formula_1000_cond_m1m3_part,"y_fixed_part_value"), collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("y_1001_cond_m1m2 = ", paste(c(y_formula_1001_cond_m1m2_part,"y_fixed_part_value"), collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("y_1000_cond_m1m2 = ", paste(c(y_formula_1000_cond_m1m2_part,"y_fixed_part_value"), collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("y_1111_cond_m1m2m3 = ", paste(c(y_formula_1111_cond_m1m2m3_part,"y_fixed_part_value"), collapse = "+")))
      eval(expr)

      expr = parse(text = paste0("y_te_fixed_part_value = ", paste(y_te_formula_fixed_part, collapse = "+")))
      eval(expr)
      expr = parse(text = paste0("y0 = ", paste(paste(y_te_formula_0_part, collapse = "+"), "y_te_fixed_part_value", sep = "+")))
      eval(expr)
      expr = parse(text = paste0("y1 = ", paste(paste(y_te_formula_1_part, collapse = "+"), "y_te_fixed_part_value", sep = "+")))
      eval(expr)

      if (length(y0) == 1) {
        y0 = rep(y0, nrow(data))
        y1 = rep(y1, nrow(data))
      }

      generate_estimates(data.frame(y_0000_cond_m1m2m3, y_1000_cond_m1m2m3, y_1100_cond_m2m3, y_1000_cond_m2m3, y_1010_cond_m1m3, y_1000_cond_m1m3, y_1001_cond_m1m2, y_1000_cond_m1m2, y_1111_cond_m1m2m3, y0, y1), ymodel, out_scale = out_scale)
    }
  }
  parallel::stopCluster(cl)

  sim_res = as.data.frame(sim_res)

  if (length(med) == 1) {

    prop1 = sim_res$indirect1/sim_res$total

    output = list(indirect1 = sim_res$indirect1, direct = sim_res$direct, total = sim_res$total, prop1 = prop1, ymodel = yres, ymodel_te = yres_te, m1_model = m1res, total2 = sim_res$total2)

    return(output)
  }else if (length(med) == 2) {

    prop1 = sim_res$indirect1/sim_res$total
    prop2 = sim_res$indirect2/sim_res$total

    output = list(direct = sim_res$direct, indirect1 = sim_res$indirect1, indirect2 = sim_res$indirect2, dependence = sim_res$dependence, interaction = sim_res$interaction, total = sim_res$total, prop1 = prop1, prop2 = prop2, ymodel = yres, ymodel_te = yres_te, m1_model = m1res, m2_model_cond = m2res_cond, m2_model = m2res_marg, total2 = sim_res$total2)
    return(output)
  }else if (length(med) ==  3) {

    prop1 = sim_res$indirect1/sim_res$total
    prop2 = sim_res$indirect2/sim_res$total
    prop3 = sim_res$indirect3/sim_res$total

    output = list(direct = sim_res$direct, indirect1 = sim_res$indirect1, indirect2 = sim_res$indirect2, indirect3 = sim_res$indirect3, total = sim_res$total, prop1 = prop1, prop2 = prop2, prop3 = prop3, ymodel = yres, ymodel_te = yres_te, m1_model = m1res, m2_model_cond = m2res_cond, m2_model = m2res_marg, m3_model = m3res_marg, m3_model_cond_m1 = m3res_cond_m1, m3_model_cond_m2 = m3res_cond_m2, m3_model_cond_m1m2 = m3res_cond_m1m2, total2 = sim_res$total2)
    return(output)
  }
}

#for 1 mediator, the order of ys is y00, y01, y10, y11, y0 and y1
generate_estimates <- function(ys, model, out_scale = "difference") {
  if (length(ys) == 6) {
    if (model == "regression") {
      est1 = (mean(ys$y11 - ys$y10))
      est2 = (mean(ys$y10 - ys$y00))
      est3 = (mean(ys$y11 - ys$y00))
      est5 = (mean(ys$y1 - ys$y0))
      output = c(indirect1 = est1, direct = est2, total = est3, total2 = est5)
      return(output)
    }else if (model == "logistic regression") {
      ys$y00 = (1-1/(1+exp(ys$y00)))
      ys$y01 = (1-1/(1+exp(ys$y01)))
      ys$y10 = (1-1/(1+exp(ys$y10)))
      ys$y11 = (1-1/(1+exp(ys$y11)))
      ys$y1 = (1-1/(1+exp(ys$y1)))
      ys$y0 = (1-1/(1+exp(ys$y0)))

      if (out_scale == "difference") {
      #These are for difference scale.
        est1 = mean(ys$y11 - ys$y10)
        est2 = mean(ys$y10 - ys$y00)
        est3 = mean(ys$y11 - ys$y00)
        est5 = mean(ys$y1 - ys$y0)
      }else if (out_scale == "ratio") {

        #Generate results in OR scale
        est1 = (mean(ys$y11)/mean(1-ys$y11))/(mean(ys$y10)/mean(1-ys$y10))
        est2 = (mean(ys$y10)/mean(1-ys$y10))/(mean(ys$y00)/mean(1-ys$y00))
        est3 = (mean(ys$y11)/mean(1-ys$y11))/(mean(ys$y00)/mean(1-ys$y00))
        #Implement this later!
        #est4 = (mean(ys$y11)/mean(1-ys$y11))/(mean(ys$y00)/mean(1-ys$y00))
        est5 = (mean(ys$y1)/mean(1-ys$y1))/(mean(ys$y0)/mean(1-ys$y0))
      }
      output = c(indirect1 = est1, direct = est2, total = est3, total2 = est5)
      return(output)
    }else if (model == "poisson regression") {
      ys$y00 = exp(ys$y00)
      ys$y01 = exp(ys$y01)
      ys$y10 = exp(ys$y10)
      ys$y11 = exp(ys$y11)
      ys$y1 = exp(ys$y1)
      ys$y0 = exp(ys$y0)

      est1 = mean(ys$y11 - ys$y10)
      est2 = mean(ys$y10 - ys$y00)
      est3 = mean(ys$y11 - ys$y00)

      est5 = mean(ys$y1 - ys$y0)

      output = c(indirect1 = est1, direct = est2, total = est3, total2 = est5)
      return(output)
    }
  }else if (length(ys) == 9) {
    #for 2 mediators, the order of ys is y_000_cond, y_100_cond, y_110_marg, y_100_marg, y_101_marg, y_111_cond, y_111_marg, y0 and y1
    if (model == "regression") {

      #direct effect
      est1 = mean(ys$y_100_cond - ys$y_000_cond)

      #indirect effect through M1
      est2 = mean(ys$y_110_marg - ys$y_100_marg)
      #indirect effect through M2
      est3 = mean(ys$y_101_marg - ys$y_100_marg)
      #dependence
      est4 = mean(ys$y_111_cond - ys$y_111_marg - ys$y_100_cond + ys$y_100_marg)
      #mediated interaction
      est5 = mean(ys$y_111_marg - ys$y_110_marg - ys$y_101_marg + ys$y_100_marg)

      est6 = mean(ys$y_111_cond - ys$y_000_cond)
      est7 = mean(ys$y1 - ys$y0)

      output = c(direct = est1, indirect1 = est2, indirect2 = est3, dependence = est4, interaction = est5, total = est6, total2 = est7)
      return(output)
    }else if (model == "logistic regression") {
      #calculate the probabilities here
      ys$y_100_cond = (1-1/(1+exp(ys$y_100_cond)))
      ys$y_000_cond = (1-1/(1+exp(ys$y_000_cond)))
      ys$y_110_marg = (1-1/(1+exp(ys$y_110_marg)))
      ys$y_100_marg = (1-1/(1+exp(ys$y_100_marg)))
      ys$y_101_marg = (1-1/(1+exp(ys$y_101_marg)))
      ys$y_111_cond = (1-1/(1+exp(ys$y_111_cond)))
      ys$y_111_marg = (1-1/(1+exp(ys$y_111_marg)))
      ys$y1 = (1-1/(1+exp(ys$y1)))
      ys$y0 = (1-1/(1+exp(ys$y0)))

      if (out_scale == "difference") {
        #direct effect
        est1 = mean(ys$y_100_cond - ys$y_000_cond)
        #indirect effect through M1
        est2 = mean(ys$y_110_marg - ys$y_100_marg)
        #indirect effect through M2
        est3 = mean(ys$y_101_marg - ys$y_100_marg)
        #dependence
        est4 = mean(ys$y_111_cond - ys$y_111_marg - ys$y_100_cond + ys$y_100_marg)
        #mediated interaction
        est5 = mean(ys$y_111_marg - ys$y_110_marg - ys$y_101_marg + ys$y_100_marg)
        est6 = mean(ys$y_111_cond - ys$y_000_cond)
        est7 = mean(ys$y1 - ys$y0)

        output = c(direct = est1, indirect1 = est2, indirect2 = est3, dependence = est4, interaction = est5, total = est6, total2 = est7)
        return(output)

      }else if (out_scale == "ratio") {
        #Results in ratio scale seems to be problematic because of dependence between the direct and indirect effects
        est1 = (mean(ys$y_100_cond)/mean(1-ys$y_100_cond))/(mean(ys$y_000_cond)/mean(1-ys$y_000_cond))
        est2 = (mean(ys$y_110_marg)/mean(1-ys$y_110_marg))/(mean(ys$y_100_marg)/mean(1-ys$y_100_marg))
        est3 = (mean(ys$y_101_marg)/mean(1-ys$y_101_marg))/(mean(ys$y_100_marg)/mean(1-ys$y_100_marg))
        est4 = (mean(ys$y_111_cond)/mean(1-ys$y_111_cond)) / (mean(ys$y_111_marg)/mean(1-ys$y_111_marg)) / (mean(ys$y_100_cond)/mean(1 - ys$y_100_cond)) * (mean(ys$y_100_marg)/mean(1 - ys$y_100_marg))
        est5 = ((mean(ys$y_111_marg)/mean(1 - ys$y_111_marg)) / ((mean(ys$y_110_marg)/mean(1 - ys$y_110_marg)))) / ((mean(ys$y_101_marg)/mean(1 - ys$y_101_marg))/(mean(ys$y_100_marg)/mean(1 - ys$y_100_marg)))
        est6 = (mean(ys$y_111_cond)/mean(1-ys$y_111_cond)) / (mean(ys$y_000_cond)/mean(1-ys$y_000_cond))
        est7 = (mean(ys$y1)/mean(1-ys$y1))/(mean(ys$y0)/mean(1 - ys$y0))

        output = c(direct = est1, indirect1 = est2, indirect2 = est3, dependence = est4, interaction = est5, total = est6, total2 = est7)
        return(output)

      }
    }else if (model == "poisson regression") {
      ys$y_100_cond = exp(ys$y_100_cond)
      ys$y_000_cond = exp(ys$y_000_cond)
      ys$y_110_marg = exp(ys$y_110_marg)
      ys$y_100_marg = exp(ys$y_100_marg)
      ys$y_101_marg = exp(ys$y_101_marg)
      ys$y_111_cond = exp(ys$y_111_cond)
      ys$y_111_marg = exp(ys$y_111_marg)
      ys$y1 = exp(ys$y1)
      ys$y0 = exp(ys$y0)

      #direct effect
      est1 = mean(ys$y_100_cond - ys$y_000_cond)
      #indirect effect through M1
      est2 = mean(ys$y_110_marg - ys$y_100_marg)
      #indirect effect through M2
      est3 = mean(ys$y_101_marg - ys$y_100_marg)
      #dependence
      est4 = mean(ys$y_111_cond - ys$y_111_marg - ys$y_100_cond + ys$y_100_marg)
      #mediated interaction
      est5 = mean(ys$y_111_marg - ys$y_110_marg - ys$y_101_marg + ys$y_100_marg)
      est6 = mean(ys$y_111_cond - ys$y_000_cond)
      est7 = mean(ys$y1 - ys$y0)

      output = c(direct = est1, indirect1 = est2, indirect2 = est3, dependence = est4, interaction = est5, total = est6, total2 = est7)
      return(output)
    }
    #for 3 mediators, the order is y_0000_cond_m1m2m3, y_1000_cond_m1m2m3, y_1100_cond_m2m3, y_1000_cond_m2m3, y_1010_cond_m1m3, y_1000_cond_m1m3, y_1001_cond_m1m2, y_1000_cond_m1m2, y_1111_cond_m1m2m3, y0, y1
  }else if (length(ys) == 11) {
    if (model == "regression") {
      est1 = mean(ys$y_1000_cond_m1m2m3 - ys$y_0000_cond_m1m2m3)
      est2 = mean(ys$y_1100_cond_m2m3 - ys$y_1000_cond_m2m3)
      est3 = mean(ys$y_1010_cond_m1m3 - ys$y_1000_cond_m1m3)
      est4 = mean(ys$y_1001_cond_m1m2 - ys$y_1000_cond_m1m2)
      est5 = mean(ys$y_1111_cond_m1m2m3 - ys$y_0000_cond_m1m2m3)
      est6 = mean(ys$y1 - ys$y0)

      output = c(direct = est1, indirect1= est2, indirect2 = est3, indirect3 = est4, total = est5, total2 = est6)
      return(output)

    }else if (model == "logistic regression") {
      ys$y_0000_cond_m1m2m3 = (1-1/(1+exp(ys$y_0000_cond_m1m2m3)))
      ys$y_1000_cond_m1m2m3 = (1-1/(1+exp(ys$y_1000_cond_m1m2m3)))
      ys$y_1100_cond_m2m3 = (1-1/(1+exp(ys$y_1100_cond_m2m3)))
      ys$y_1000_cond_m2m3 = (1-1/(1+exp(ys$y_1000_cond_m2m3)))
      ys$y_1010_cond_m1m3 = (1-1/(1+exp(ys$y_1010_cond_m1m3)))
      ys$y_1000_cond_m1m3 = (1-1/(1+exp(ys$y_1000_cond_m1m3)))
      ys$y_1001_cond_m1m2 = (1-1/(1+exp(ys$y_1001_cond_m1m2)))
      ys$y_1000_cond_m1m2 = (1-1/(1+exp(ys$y_1000_cond_m1m2)))
      ys$y_1111_cond_m1m2m3 = (1-1/(1+exp(ys$y_1111_cond_m1m2m3)))
      ys$y1 = (1-1/(1+exp(ys$y1)))
      ys$y0 = (1-1/(1+exp(ys$y0)))

      if (out_scale == "difference") {

        est1 = mean(ys$y_1000_cond_m1m2m3 - ys$y_0000_cond_m1m2m3)
        est2 = mean(ys$y_1100_cond_m2m3 - ys$y_1000_cond_m2m3)
        est3 = mean(ys$y_1010_cond_m1m3 - ys$y_1000_cond_m1m3)
        est4 = mean(ys$y_1001_cond_m1m2 - ys$y_1000_cond_m1m2)
        est5 = mean(ys$y_1111_cond_m1m2m3 - ys$y_0000_cond_m1m2m3)
        est6 = mean(ys$y1 - ys$y0)

        output = c(direct = est1, indirect1= est2, indirect2 = est3, indirect3 = est4, total = est5, total2 = est6)
        return(output)

      }else if (out_scale == "ratio") {
        #implement this later 20.11.2019
      }
    }else if (model == "poisson regression") {
      ys$y_0000_cond_m1m2m3 = exp(ys$y_0000_cond_m1m2m3)
      ys$y_1000_cond_m1m2m3 = exp(ys$y_1000_cond_m1m2m3)
      ys$y_1100_cond_m2m3 = exp(ys$y_1100_cond_m2m3)
      ys$y_1000_cond_m2m3 = exp(ys$y_1000_cond_m2m3)
      ys$y_1010_cond_m1m3 = exp(ys$y_1010_cond_m1m3)
      ys$y_1000_cond_m1m3 = exp(ys$y_1000_cond_m1m3)
      ys$y_1001_cond_m1m2 = exp(ys$y_1001_cond_m1m2)
      ys$y_1000_cond_m1m2 = exp(ys$y_1000_cond_m1m2)
      ys$y_1111_cond_m1m2m3 = exp(ys$y_1111_cond_m1m2m3)
      ys$y1 = exp(ys$y1)
      ys$y0 = exp(ys$y0)

      est1 = mean(ys$y_1000_cond_m1m2m3 - ys$y_0000_cond_m1m2m3)
      est2 = mean(ys$y_1100_cond_m2m3 - ys$y_1000_cond_m2m3)
      est3 = mean(ys$y_1010_cond_m1m3 - ys$y_1000_cond_m1m3)
      est4 = mean(ys$y_1001_cond_m1m2 - ys$y_1000_cond_m1m2)
      est5 = mean(ys$y_1111_cond_m1m2m3 - ys$y_0000_cond_m1m2m3)
      est6 = mean(ys$y1 - ys$y0)

      output = c(direct = est1, indirect1= est2, indirect2 = est3, indirect3 = est4, total = est5, total2 = est6)
      return(output)
    }
  }
}

random_draw <- function(data,modelres,model) {
  output <- list()
  if (model == "regression") {
    resid_sd <- sd(modelres$residuals)
    err <- rnorm(length(data[[1]]), mean = 0, sd = resid_sd)
    for (i in 1:length(data)) {
      output[[i]] <- data[[i]] + err
    }
    return(output)

  }else if (model == "logistic regression") {
    expr = parse(text = paste0("tmp = levels(modelres$data$",all.vars(modelres$formula)[1],")"))
    eval(expr)

    for (i in 1:length(data)) {
      sim_data <- runif(length(data[[i]])) > 1/(1+exp(data[[i]]))
      sim_data = ifelse(sim_data == 0, tmp[1], tmp[2])
      sim_data = as.factor(sim_data)
      sim_data = relevel(sim_data, ref=tmp[1])
      output[[i]] = sim_data
    }
    return(output)
  }else if (model == "poisson regression") {
    for (i in 1:length(data)) {
      sim_data <- rpois(length(data[[i]]), exp(data[[i]]))
      output[[i]] = sim_data
    }
    return(output)
  }
}

random_draw_formula <- function(cat_var_dict, fo_terms, target, target_lv = NULL) {
  formula_terms = fo_terms
  search_term = NULL
  non_fixed_part = rep(FALSE, length(fo_terms))
  discarded_part = rep(FALSE, length(fo_terms))

  for (i in 1:length(target)) {
    if (i == 1) {
      tmp_dict = cat_var_dict[cat_var_dict$cat_var_terms ==  target[i],]
      if (nrow(tmp_dict) > 0) {
        for (j in 1:nrow(tmp_dict)) {
          search_term = tmp_dict$expanded_cat_var_terms[j]
          if (tmp_dict$level_labels[j] == target_lv[i]) {
            non_fixed_part = non_fixed_part | stringr::str_detect(fo_terms, search_term)
            search_term = paste0("\\(data\\$",target[i],"=='",target_lv[i],"'\\)")
            formula_terms = stringr::str_replace_all(formula_terms, paste0("\\*", search_term),"")
            formula_terms = stringr::str_replace_all(formula_terms, paste0(search_term,"\\*"),"")
          }else
          {
            discarded_part = discarded_part | stringr::str_detect(fo_terms, search_term)
          }
        }
      }else{
        search_term = paste0("\\$",target[i],"\\*")
        non_fixed_part = non_fixed_part | stringr::str_detect(fo_terms, search_term)
        #target level
        if (target_lv[i] == 0) {
          discarded_part = discarded_part | stringr::str_detect(fo_terms, search_term)
        }else if (target_lv[i] == 1){
          search_term = paste0("data\\$",target[i])
          formula_terms = stringr::str_replace_all(formula_terms,paste0("\\*",search_term),"")
          formula_terms = stringr::str_replace_all(formula_terms,paste0(search_term,"\\*"),"")
        }else {
          search_term = paste0("data\\$",target[i])
          formula_terms = stringr::str_replace_all(formula_terms,search_term,as.character(target_lv[i]))
        }
      }
    }else {
      search_term = paste0("data\\$",target[i])
      non_fixed_part = non_fixed_part | stringr::str_detect(fo_terms, search_term)
      formula_terms = stringr::str_replace_all(formula_terms, search_term, paste0("m",i-1,"_",target_lv[i]))
    }
  }
  fixed_part = formula_terms[!(non_fixed_part | discarded_part)]
  target_part = formula_terms[non_fixed_part & !discarded_part]

  formula_parts <- list(fixed_part = fixed_part, target_part = target_part)
  return(formula_parts)
}

gen_cat_var_dict <- function(model) {
  cat_var_dict = dplyr::tibble(cat_var_terms = character(), expanded_cat_var_terms = character(), level_labels = character())
  cat_var_list <- model$xlevels
  tmp = NULL
  expanded_cat_var_terms = NULL
  tmp2 = NULL
  cat_var_terms = NULL
  level_labels = NULL
  if (length(cat_var_list) != 0) {
    for (i in 1:length(cat_var_list)) {
      tmp <- stringr::str_c(names(cat_var_list[i]),cat_var_list[[i]])
      expanded_cat_var_terms = c(expanded_cat_var_terms, tmp)
      tmp2 <- rep(names(cat_var_list[i]), length(tmp))
      cat_var_terms = c(cat_var_terms, tmp2)
      level_labels = c(level_labels, cat_var_list[[i]])
    }
    cat_var_dict <- dplyr::tibble(cat_var_terms, expanded_cat_var_terms, level_labels)
  }
  return(cat_var_dict)
}

random_draw_formula_terms <- function(model, data_name, coeff_name) {
  expanded_cat_var_terms = NULL
  cat_var_terms = NULL
  level_labels = NULL
  fo_str = NULL
  int_var_str = NULL

  cat_var_dict <- gen_cat_var_dict(model)

  for (i in 1:length(model$coefficients)) {
    current_var = names(model$coefficients[i])
    if (current_var == "(Intercept)") {
      tmp = paste0(coeff_name,"['(Intercept)']")
      fo_str = c(fo_str, tmp)
    }else {
      #use another fuction to replace them when needed.
      if (!stringr::str_detect(current_var,":")) {
        if (length(cat_var_dict) > 0) {
          if (sum(stringr::str_detect(cat_var_dict$expanded_cat_var_terms, current_var)) > 0) {
            cat_var_name <- cat_var_dict$cat_var_terms[cat_var_dict$expanded_cat_var_terms == current_var]
            cat_var_label <- cat_var_dict$level_labels[cat_var_dict$expanded_cat_var_terms == current_var]
            tmp = paste0("(",data_name,"$",cat_var_name,"=='",cat_var_label,"')","*",coeff_name,"['",current_var,"']")
            fo_str = c(fo_str, tmp)
          }else {
            tmp = paste0(data_name,"$",current_var,"*",coeff_name,"['",current_var,"']")
            fo_str = c(fo_str, tmp)
          }
        }else {
          tmp = paste0(data_name,"$",current_var,"*",coeff_name,"['",current_var,"']")
          fo_str = c(fo_str, tmp)
        }
      }else {
        int_var = unlist(strsplit(current_var,":"))
        int_var_str = NULL
        for (j in 1:length(int_var)) {
          if (nrow(cat_var_dict) > 0) {
            if (sum(stringr::str_detect(cat_var_dict$expanded_cat_var_terms, int_var[j])) > 0) {
              cat_var_name <- cat_var_dict$cat_var_terms[cat_var_dict$expanded_cat_var_terms == int_var[j]]
              cat_var_label <- cat_var_dict$level_labels[cat_var_dict$expanded_cat_var_terms == int_var[j]]
              tmp = paste0("(",data_name,"$",cat_var_name,"=='",cat_var_label,"')")
              int_var_str = c(int_var_str, tmp)
            }else {
              tmp = paste0(data_name,"$",int_var[j])
              int_var_str = c(int_var_str, tmp)
            }
          }else {
            tmp = paste0(data_name,"$",int_var[j])
            int_var_str = c(int_var_str, tmp)
          }
        }
        int_var_str = c(int_var_str, paste0(coeff_name,"['",current_var,"']"))
        int_var_str = paste(int_var_str,collapse = "*")
        fo_str = c(fo_str,int_var_str)
      }
    }
  }
  return(fo_str)
}

run_model <- function(fo, model, data) {
  if (model == "regression") {
    res = lm(fo,data = data)
  }else if (model == "logistic regression") {
    res = glm(fo, data = data, family = "binomial")
  }else if (model == "poisson regression") {
    res = glm(fo, data = data, family = "poisson")
  }
  return(res)
}

build_ymodel_formula <- function(y, med, treat, ymodel, data, c = NULL, mod = NULL, incint = NULL, inc_mmint = TRUE)
{
  xvar <- paste(c(med,treat), collapse = "+")
  #xvar <- paste(paste(med, collapse = "+"), treat, sep = "+")
  if (!is.null(c)) {
    xvar <- paste(xvar,paste0(c,collapse = "+"), sep = "+")
  }

  if (!is.null(mod)) {
    if (length(mod) == 1 || length(mod) == length(med)) {

      target_str = paste0("\\+",treat,"\\+")
      unique_mod = unique(mod)
      replacement_str = paste0(paste0(treat,"*",unique_mod), collapse = "+")
      if (stringr::str_detect(xvar, target_str)) {
        replacement_str <- paste0("+",replacement_str,"+")
      }else {
        target_str = paste0("\\+",treat)
        replacement_str <- paste0("+", replacement_str)
      }
      xvar = stringr::str_replace(xvar, target_str, replacement_str)


      if (length(mod) == 1) {
        mod = rep(mod,length(med))

      }
      for (i in 1:length(med)) {
        target_str = paste0("\\+",med[i],"\\+")
        replacement_str = paste0("+",med[i],"*",mod[i],"+")
        if (stringr::str_detect(xvar,target_str)) {
          xvar = stringr::str_replace(xvar,target_str,replacement_str)
        }else
        {
          target_str = paste0(med[i],"\\+")
          replacement_str = paste0(med[i],"*", mod[i], "+")
          xvar = stringr::str_replace(xvar,target_str,replacement_str)
        }
      }
    }
    else {
      print("Number of moderators and mediators are not the same.")
      return(NULL)
    }
  }
  if (!is.null(incint)) {
    if (length(incint) == 1 || length(incint) == length(med)){
      if (length(incint) == 1) {
        incint = rep(incint, length(med))
      }
      if (sum(incint) != 0) {
        for (i in 1:length(med)) {
          if (incint[i] == TRUE) {
            xvar <- paste0(xvar,"+",treat,"*",med[i])
          }
        }
      }
    }else {
      print("Number of boolean value for incint and mediators are not the same.")
      return(NULL)
    }
  }
  if (inc_mmint) {
    if (length(med) > 1) {
      for (i in 1:length(med)) {
        for (j in i:length(med)) {
          if (i != j) {
            xvar <- paste0(xvar,"+",med[i],"*",med[j])
          }
        }
      }
    }
  }
  fo <- paste0(y,"~", xvar)
  fo <- as.formula(fo)
  return(fo)
}

build_mmodel_formula <- function(med, medposition, treat, mmodel, data, cond = NULL, c = NULL, mod = NULL) {
  if (is.null(c)) {
    xvar <- treat
  }else {
    xvar <- paste(paste0(c, collapse = "+"),treat, sep = "+")
  }

  if (!is.null(mod)) {
    target_str = paste0("\\+",treat)
    if (length(mod) == 1) {
      replacement_str = paste0(treat,"*",mod)
    }else if (length(mod) == length(med)) {
      replacement_str = paste0(treat,"*",mod[medposition])
    }else {
      print("Number of moderators and mediators are not the same.")
      return(NULL)
    }
    if (!str_detect(xvar,target_str)) {
      target_str = treat
      xvar = stringr::str_replace(xvar,target_str,replacement_str)
    }else {
      replacement_str = paste0("+",replacement_str)
      xvar = stringr::str_replace(xvar,target_str,replacement_str)
    }

  }
  #Only consider 3 mediators for now
  if (!is.null(cond)) {
    for (i in 1:length(cond)) {
      xvar = paste0(xvar,"+",med[cond[i]])
    }
  }
  fo <- paste0(med[medposition],"~", xvar)
  fo <- as.formula((fo))

  return(fo)
}

###############################################################################################################################
