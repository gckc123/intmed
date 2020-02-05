gen_med_reg_html <- function(res_df, y, med, treat, c, ymodel, mmodel, incint = NULL, inc_mmint = FALSE, conf.level = 0.95, data_head, treat_lv = NULL, control_lv = NULL) {
  html_output = "<br/><h4><u>Regression analysis</u></h4>The table below shows the estimates from the key regression models for the mediation analysis."
  html_line = paste0("<ul><li>The outcome variable, ",y,", was modelled with ", ymodel,"</li>")
  html_output = paste0(html_output, html_line)
  for (i in 1:length(med)) {
    html_line = paste0("<li>The mediator, ", med[i], ", was modelled with ", mmodel[i], "</li>")
    html_output = paste0(html_output, html_line)
  }
  html_output = paste0(html_output, "</ul>")

  alpha_lv = 1-conf.level
  coeff_lab = rep("b", length(med)+1)

  html_line = paste0("The mediator ", ifelse(length(med) > 1, "models ", "model ") ,"indicated that the exposure variable, ", treat,", was ")
  html_output = paste0(html_output, html_line)
  ##Stop here - need to take care of binary mediator and binary treatment 7.1.2020##
  expr = parse(text = paste0("tmp = is.factor(data_head$",treat,")"))
  eval(expr)
  treat_row_name = ifelse(tmp,paste0(treat,treat_lv), treat)
  html_line = "<ul>"
  for (i in 1:length(med)) {
    html_line = paste0(html_line, "<li>")
    tmp = paste0(ifelse(res_df[res_df$variables == treat_row_name,3*i+1] < alpha_lv, "significantly ","not significantly "), "associated with the mediator, ", med[[i]],", ")
    html_line = paste0(html_line, tmp)

    if (mmodel[[i]] == "logistic regression") {
      coeff_lab[i] = "OR"
    }else if (mmodel[[i]] == "poisson regression") {
      coeff_lab[[i]] = "IRR"
    }

    tmp = paste0(coeff_lab[i], " = ", stringr::str_replace_all(res_df[res_df$variables == treat_row_name, 3*(i-1)+2],"\\*",""),", ", round(conf.level*100, 0), "% CI = ", res_df[res_df$variables == treat_row_name, 3*(i-1)+3],"." )

    html_line = paste0(html_line, tmp, "</li>")
  }
  html_line = paste0(html_line, "</ul>")

  html_output = paste0(html_output, html_line)

  if (ymodel == "logistic regression") {
    coeff_lab[length(med)+1] = "OR"
  }else if (ymodel == "poisson regression") {
    coeff_lab[length(med)+1] = "IRR"
  }

  if (is.null(incint) | sum(incint) == 0) {
    html_line = paste0("After accounting for the effect of the ", ifelse(length(med) > 1, "mediators ", "mediator "), ", the outcome model indicated that the treatment variable, ", treat, ", was ")
    html_line = paste0(html_line, ifelse(res_df[res_df$variables == treat_row_name,3*length(med)+4] < alpha_lv, "significantly ","not significantly "),"associated with ", y, ", ")
    html_line = paste0(html_line, coeff_lab[length(med)+1], " = ", stringr::str_replace_all(res_df[res_df$variables == treat_row_name, 3*length(med)+2],"\\*",""),", ", round(conf.level*100, 0), "% CI = ", res_df[res_df$variables == treat_row_name, 3*length(med)+3],"." )
    html_output = paste0(html_output, html_line, "<br/>")
    if (inc_mmint == FALSE) {
      html_line = "The outcome model also indicated that <ul>"
      for (i in 1:length(med)) {
        expr = parse(text = paste0("tmp = is.factor(data_head$",med[i],")"))
        eval(expr)
        med_row_name <- NULL
        expr = parse(text = paste0("med_row_name = ifelse(tmp, paste0(med[i],tail(levels(data_head$",med[i],"),1)),med[i])"))
        eval(expr)
        html_line = paste0(html_line, "<li>",med[i]," is ", ifelse(res_df[res_df$variables == med_row_name,3*length(med)+4] < alpha_lv, "significantly ","not significantly "), "associated with the outcome, ", y, ", ")
        html_line = paste0(html_line, coeff_lab[length(med)+1], " = ", stringr::str_replace_all(res_df[res_df$variables == med_row_name,3*length(med)+2],"\\*",""), ", ", round(conf.level*100,0),"% CI = ", res_df[res_df$variables == med_row_name,3*length(med)+3],". </li>")
      }
      html_line = paste0(html_line, "</ul>")
      html_output = paste0(html_output, html_line)
    }else {
      html_line = "The outcome model includes the mediator-mediator interaction(s). The effect of the mediators and their interaction(s) are summarized in the three right-most columns in the table below. <br/>"
      html_output = paste0(html_output, html_line)
    }
  }else {
    if (inc_mmint == TRUE) {
      html_line = "The outcome model includes the treatment-mediator interaction(s) and also the mediator-mediator interaction(s). The effect of the treatment, mediator(s) and relevant interactions are summarized in the three right-most columns in the table below. <br/>"
      html_output = paste0(html_output, html_line)
    }else {
      html_line = "The outcome model includes the treatment-mediator interaction(s). The effect of the treatment, mediator(s) and relevant interaction(s) are summarized in the three right-most columns in the table below. <br/>"
      html_output = paste0(html_output, html_line)
    }
  }

  if (!is.null(c)) {
    if (length(c) == 1) {
      html_line = paste0("The effect of ", c," was adjusted for in the above analyses. <br/>")
    }else {
      tmp = paste(paste0(c[1:length(c)-1], collapse = ", "), c[length(c)], sep = " and ")
      html_line = paste0("The effect of ", tmp," were adjusted for in the above analyses. <br/>")
    }
    html_output = paste0(html_output, html_line)
  }

  html_output = c(html_output,"<br/><b>Table. Results from key regression analyses.</b><br/>")
  html_output = c(html_output, "<table style = \"text-align: left;border-bottom: 1px solid black; border-top: 1px solid black;\" cellspacing=\"0\" cellpadding = \"2\">")
  html_line = "<tr><td></td>"

  for (i in 1:length(med)) {
    html_line = paste0(html_line,"<td colspan = \"3\">",med[i],"</td>")
  }

  html_line = paste0(html_line, "<td colspan = \"3\">", y,"</td></tr>")
  html_output = c(html_output, html_line)
  html_line = "<tr><td style=\"padding-right: 1em;border-bottom: 1px solid black;\">Variables</td>"

  tmp = paste0(paste0("<td style=\"padding-right: 1em;border-bottom: 1px solid black;\">",coeff_lab,"</td><td style=\"padding-right: 1em;border-bottom: 1px solid black;\">", round(conf.level*100,0),"% CI","</td><td style=\"padding-right: 1em;border-bottom: 1px solid black;\">p-value</td>"), collapse = "")

  html_line = paste0(html_line, tmp, "</tr>")
  html_output = c(html_output, html_line)

  res_df[sapply(res_df, is.na)] = "-"
  for (i in 1:nrow(res_df)) {
    html_line = paste0("<tr><td style=\"padding-right: 1em\">",paste0(res_df[i,], collapse = "</td><td style=\"padding-right: 1em\">"),"</td></tr>")
    html_output = c(html_output, html_line)
  }

  html_output = c(html_output, "</table>")
  html_output = c(html_output, "***<i>p</i> <.001;**<i>p</i> <.01;*<i>p</i> <.05. <br/>")
  html_output = c(html_output, "<br/>Reference<br/>Rubin DB. Multiple imputation for nonresponse in surveys. New York: John Wiley & Sons; 2009.")
  html_output = c(html_output, "<br/>Buuren Sv, Groothuis-Oudshoorn K. mice: Multivariate imputation by chained equations in R. Journal of statistical software. 2010:1-68.")
  html_output = c(html_output, "<br/>Vansteelandt S, Daniel RM. Interventional effects for mediation analysis with multiple mediators. Epidemiology (Cambridge, Mass). 2017; 28(2):258.")
  html_output = c(html_output, "<br/>Chan GCK, Leung J. Causal mediation analysis using the interventional effect approach. A refined definition and software implementation in R. Paper under review. 2020.")

  return(html_output)
}

gen_med_table_html <- function(med_res, med, conf.level = 0.95, digits = 2, sim) {
  res_des = paste0("<br/><h4><u>Mediation analysis</u></h4>Mediation analysis was performed based on the counter-factual framework and the interventional effect (Vansteelandt and Daniel, 2017; Chan and Leung, 2020). The analysis was conducted in R using the intmed package (Chan and Leung, 2020) with ",sim, " simulations. <br/> Results from the mediation analysis indicated that <ul>")
  alpha_level = 1-conf.level
  html_output = "<br/><b>Table. Mediation analysis results.</b><br/>"
  html_output = c(html_output, "<table style = \"text-align: left;border-bottom: 1px solid black; border-top: 1px solid black;\" cellspacing=\"0\" cellpadding = \"2\">")
  html_line = paste0("<tr><td style=\"padding-right: 1em;border-bottom: 1px solid black;\"></td><td style=\"padding-right: 1em;border-bottom: 1px solid black;\">Estimates</td><td style=\"padding-right: 1em;border-bottom: 1px solid black;\">",round(conf.level*100, 0),"% CI</td><td style=\"padding-right: 1em;border-bottom: 1px solid black;\">p-value</td></tr>")
  html_output = c(html_output, html_line)

  for (i in 1:length(med_res$indirect)) {
    estimate <- mean(med_res$indirect[[i]])
    ci <- stats::quantile(med_res$indirect[[i]], c((1-conf.level)/2,1-(1-conf.level)/2))
    pvalue <- empirical_pvalue(med_res$indirect[[i]])
    formatted_p <- format(round(pvalue,3), nsmall = 3)
    html_line <- paste0("<tr><td style=\"padding-right: 1em\">Indirect effect mediated through ", med[i],"</td><td style=\"padding-right: 1em\">", format(round(estimate, digits), nsmall = digits),ifelse(pvalue < 0.05,"*",""),ifelse(pvalue < 0.01,"*",""),ifelse(pvalue < 0.001,"*",""),"</td><td style=\"padding-right: 1em\">(",format(round(ci[1], digits), nsmall = digits),", ",format(round(ci[2], digits), nsmall = digits),")</td><td>",formatted_p,"</td></tr>")
    html_output = c(html_output, html_line)
    res_des_line = paste0("<li> the indirect effect through ",med[i], " was ", ifelse(pvalue < alpha_level, "statistically significant","non-signifcant"),", p = ",formatted_p,"; ")
    res_des_line = paste0(res_des_line, format(round(stats::median(med_res$prop[[i]])*100, digits = digits),nsmall = digits), "% of the total effect was mediated through ", med[i], ".</li>")
    res_des = c(res_des, res_des_line)

  }

  if (!is.null(med_res$dependence)) {
    estimate <- mean(med_res$dependence)
    ci <- stats::quantile(med_res$dependence, c((1-conf.level)/2,1-(1-conf.level)/2))
    pvalue <- empirical_pvalue(med_res$dependence)
    formatted_p <- format(round(pvalue,3), nsmall = 3)
    html_line <- paste0("<tr><td style=\"padding-right: 1em\">Indirect effect mediated through the dependence between mediators</td><td style=\"padding-right: 1em\">", format(round(estimate, digits), nsmall = digits),ifelse(pvalue < 0.05,"*",""),ifelse(pvalue < 0.01,"*",""),ifelse(pvalue < 0.001,"*",""),"</td><td style=\"padding-right: 1em\">(",format(round(ci[1], digits), nsmall = digits),", ",format(round(ci[2], digits), nsmall = digits),")</td><td>",formatted_p,"</td></tr>")
    html_output = c(html_output, html_line)
    res_des = c(res_des, paste0("<li> the indirect effect through the dependence between mediators was ", ifelse(pvalue < alpha_level, "statistically significant","non-signifcant"),", p = ",formatted_p,".</li>"))
  }

  if (!is.null(med_res$interaction)) {
    estimate <- mean(med_res$interaction)
    ci <- stats::quantile(med_res$interaction, c((1-conf.level)/2,1-(1-conf.level)/2))
    pvalue <- empirical_pvalue(med_res$interaction)
    formatted_p <- format(round(pvalue,3), nsmall = 3)
    html_line <- paste0("<tr><td style=\"padding-right: 1em\">Indirect effect mediated through the interaction between mediators</td><td style=\"padding-right: 1em\">", format(round(estimate, digits), nsmall = digits),ifelse(pvalue < 0.05,"*",""),ifelse(pvalue < 0.01,"*",""),ifelse(pvalue < 0.001,"*",""),"</td><td style=\"padding-right: 1em\">(",format(round(ci[1], digits), nsmall = digits),", ",format(round(ci[2], digits), nsmall = digits),")</td><td>",formatted_p,"</td></tr>")
    html_output = c(html_output, html_line)
    res_des = c(res_des, paste0("<li> the indirect effect through the interaction between mediators was ", ifelse(pvalue < alpha_level, "statistically significant","non-signifcant"),", p = ",formatted_p,".</li>"))
  }

  estimate <- mean(med_res$direct)
  ci <- stats::quantile(med_res$direct, c((1-conf.level)/2,1-(1-conf.level)/2))
  pvalue <- empirical_pvalue(med_res$direct)
  formatted_p <- format(round(pvalue,3), nsmall = 3)
  html_line <- paste0("<tr><td style=\"padding-right: 1em\">Direct effect</td><td style=\"padding-right: 1em\">", format(round(estimate, digits), nsmall = digits),ifelse(pvalue < 0.05,"*",""),ifelse(pvalue < 0.01,"*",""),ifelse(pvalue < 0.001,"*",""),"</td><td style=\"padding-right: 1em\">(",format(round(ci[1], digits), nsmall = digits),", ",format(round(ci[2], digits), nsmall = digits),")</td><td>",formatted_p,"</td></tr>")
  html_output = c(html_output, html_line)
  res_des = c(res_des, paste0("<li> the direct effect was ", ifelse(pvalue < alpha_level, "statistically significant","non-signifcant"),", p = ",formatted_p,".</li>"))

  estimate <- mean(med_res$total)
  ci <- stats::quantile(med_res$total, c((1-conf.level)/2,1-(1-conf.level)/2))
  pvalue <- empirical_pvalue(med_res$total)
  formatted_p <- format(round(pvalue,3), nsmall = 3)
  html_line <- paste0("<tr><td style=\"padding-right: 1em\">Total effect</td><td style=\"padding-right: 1em\">", format(round(estimate, digits), nsmall = digits),ifelse(pvalue < 0.05,"*",""),ifelse(pvalue < 0.01,"*",""),ifelse(pvalue < 0.001,"*",""),"</td><td style=\"padding-right: 1em\">(",format(round(ci[1], digits), nsmall = digits),", ",format(round(ci[2], digits), nsmall = digits),")</td><td>",formatted_p,"</td></tr>")
  html_output = c(html_output, html_line)
  res_des = c(res_des, paste0("<li> the total effect was ", ifelse(pvalue < alpha_level, "statistically significant","non-signifcant"),", p = ",formatted_p,".</li>"))

  for (i in 1:length(med_res$prop)) {
    estimate <- stats::median(med_res$prop[[i]])
    ci <- stats::quantile(med_res$prop[[i]], c((1-conf.level)/2,1-(1-conf.level)/2))
    pvalue <- "-"
    html_line <- paste0("<tr><td style=\"padding-right: 1em\">Proportion of effect mediated through ", med[i],"</td><td style=\"padding-right: 1em\">", format(round(estimate, digits), nsmall = digits),"</td><td style=\"padding-right: 1em\"></td><td></td></tr>")
    html_output = c(html_output, html_line)
  }
  html_output <- c(html_output, "</table>")
  html_output <- c(html_output, "***<i>p</i> < .001;**<i>p</i> < .05***<i>p</i> < .05.")
  res_des <- c(res_des, "</ul>")
  html_output <- c(res_des, html_output)

  return(html_output)
}

gen_med_reg_table <- function(y_res, m_res, med, ymodel, mmodel, conf.level = 0.95, digits = 2) {

  table <- extract_reg_table(m_res[[1]], mmodel[[1]], conf.level, digits = digits)
  colnames(table) <- c("variables", "m1.b","m1.ci","m1.p")

  if (length(m_res) > 1) {
    for (i in 2:length(m_res)) {
      tmp <- extract_reg_table(m_res[[i]], mmodel[[i]], conf.level, digits =  digits)
      colnames(tmp) <- c("variables", paste0("m",i,".b"), paste0("m",i,".ci"), paste0("m",i,".p"))
      table <- merge(table, tmp, by = "variables", all = TRUE)
    }
  }

  tmp <- extract_reg_table(y_res, ymodel, conf.level, digits)

  colnames(tmp) <- c("variables","y.b","y.ci", "y.p")

  table <- merge(table, tmp , by = "variables", all = TRUE)
  table.part1 <- table[!is.na(table$m1.b),]
  table.part2 <- table[is.na(table$m1.b),]
  table <- rbind(table.part1,table.part2)

  return(table)

}

extract_reg_table <- function(res, model, conf.level = 0.95, digits = 2) {
  half_alpha = (1-conf.level)/2
  if (class(res)[[1]] == "mipo") {
    table <- summary(res)
    table$ts <- stats::qt(half_alpha, table$df)
  }else if (class(res)[[1]] == "glm" | class(res)[[1]] == "lm"){
    table <- as.data.frame(summary(res)$coefficients)
    table$df <- res$df.residual
    colnames(table)[which(colnames(table) == "Estimate")] = "estimate"
    colnames(table)[which(colnames(table) == "Std. Error")] = "std.error"
    if (class(res)[[1]] == "lm") {
      colnames(table)[which(colnames(table) == "Pr(>|t|)")] = "p.value"
      table$ts <- stats::qt(half_alpha, table$df)
    }else if (class(res)[[1]] == "glm") {
      colnames(table)[which(colnames(table) == "Pr(>|z|)")] = "p.value"
      table$ts <- stats::qnorm(half_alpha)
    }
  }


  table$lb <- table$estimate + table$ts*table$std.error
  table$ub <- table$estimate - table$ts*table$std.error

  if (model == "logistic regression" | model == "poisson regression") {
    table$lb = exp(table$lb)
    table$ub = exp(table$ub)
    table$estimate = exp(table$estimate)
  }

  table$ci <- paste0("(",format(round(table$lb, digits), nsmall = digits),", ",format(round(table$ub, digits), nsmall = digits),")")
  table$estimate <- format(round(table$estimate, digits), digits)
  table$estimate <- ifelse(table$p.value < .05 ,paste0(table$estimate,"*"),table$estimate)
  table$estimate <- ifelse(table$p.value < .01 ,paste0(table$estimate,"*"),table$estimate)
  table$estimate <- ifelse(table$p.value < .001 ,paste0(table$estimate,"*"),table$estimate)


  table$p.value <- format(round(table$p.value, 3), nsmall = 3)

  table$variables <- rownames(table)
  table <- table[,c("variables","estimate","ci","p.value")]
  return(table)
}


