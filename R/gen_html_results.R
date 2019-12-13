gen_med_reg_html <- function(res_df, y, med, treat, ymodel, mmodel, incint = NULL, inc_mmint = FALSE, conf.level = 0.95) {
  html_output = "<br/>Mediation analysis was performed based on the counter-factual framework and the interventional effect (Vansteelandt and Daniel, 2017; Chan and Leung, 2020). The analysis was conducted in R using the intmed package (Chan and Leung, 2020). The table below shows the estimates from the key regression models for the mediation analysis.<br/>"

  alpha_lv = 1-conf.level
  coeff_lab = rep("b", length(med)+1)

  html_line = paste0("<br/>The mediator ", ifelse(length(med) > 1, "models ", "model ") ,"indicated that the exposure variable, ", treat,", was ")
  html_output = paste0(html_output, html_line)

  html_line = "<ul>"
  for (i in 1:length(med)) {
    html_line = paste0(html_line, "<li>")
    tmp = paste0(ifelse(res_df[res_df$variables == treat,3*i+1] < alpha_lv, "significantly ","not significantly "), "associated with the mediator, ", med[[i]],", ")
    html_line = paste0(html_line, tmp)

    if (mmodel[[i]] == "logistic regression") {
      coeff_lab[i] = "OR"
    }else if (mmodel[[i]] == "poisson regression") {
      coeff_lab[[i]] = "IRR"
    }

    tmp = paste0(coeff_lab[i], " = ", stringr::str_replace_all(res_df[res_df$variables == treat, 3*(i-1)+2],"\\*",""),", ", round(conf.level*100, 0), "% CI = ", res_df[res_df$variables == treat, 3*(i-1)+3],"." )

    html_line = paste0(html_line, tmp, "</li>")
  }
  html_line = paste0(html_line, "</ul>")

  html_output = paste0(html_output, html_line)

  if (ymodel == "logistic regression") {
    coeff_lab[length(med)+1] = "OR"
  }else if (ymodel == "poisson regression") {
    coeff_lab[length(med)+1] = "IRR"
  }

  if (is.null(incint)) {
    html_line = paste0("After accounting for the effects of the ", ifelse(length(med) > 1, "mediators ", "mediator "), ", the outcome model indicated that the treatment variable, ", treat, ", was ")
    html_line = paste0(html_line, ifelse(res_df[res_df$variables == treat,3*length(med)+4] < alpha_lv, "significantly ","not significantly "),"associated with ", y, ", ")
    html_line = paste0(html_line, coeff_lab[length(med)+1], " = ", stringr::str_replace_all(res_df[res_df$variables == treat, 3*length(med)+2],"\\*",""),", ", round(conf.level*100, 0), "% CI = ", res_df[res_df$variables == treat, 3*length(med)+3],"." )
    html_output = paste0(html_output, html_line, "<br/>")
  }

  if (is.null(incint) && inc_mmint == FALSE) {
    html_line = "The outcome model also indicated that <ul>"
    for (i in 1:length(med)) {
      html_line = paste0(html_line, "<li>",med[i]," is ", ifelse(res_df[res_df$variables == med[i],3*length(med)+4] < alpha_lv, "significantly ","not significantly "), "associated with the outcome, ", y, ", ")
      html_line = paste0(html_line, coeff_lab[length(med)+1], " = ", stringr::str_replace_all(res_df[res_df$variables == med[i],3*length(med)+2],"\\*",""), ", ", round(conf.level*100,0),"% CI = ", res_df[res_df$variables == med[i],3*length(med)+3],". </li>")
    }
    html_line = paste0(html_line, "</ul>")
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
  return(html_output)
}

gen_med_table_html <- function(med_res, med, conf.level = 0.95, digits = 2) {
  html_output = "<br/><b>Table. Mediation analysis results.</b><br/>"
  html_output = c(html_output, "<table style = \"text-align: left;border-bottom: 1px solid black; border-top: 1px solid black;\" cellspacing=\"0\" cellpadding = \"2\">")
  html_line = paste0("<tr><td style=\"padding-right: 1em;border-bottom: 1px solid black;\"></td><td style=\"padding-right: 1em;border-bottom: 1px solid black;\">Estimates</td><td style=\"padding-right: 1em;border-bottom: 1px solid black;\">",round(conf.level*100, 0),"% CI</td></tr>")
  html_output = c(html_output, html_line)

  for (i in 1:length(med_res$indirect)) {
    estimate <- mean(med_res$indirect[[i]])
    ci <- quantile(med_res$indirect[[i]], c((1-conf.level)/2,1-(1-conf.level)/2))
    html_line <- paste0("<tr><td style=\"padding-right: 1em\">Indirect effect mediated through ", med[i],"</td><td style=\"padding-right: 1em\">", format(round(estimate, digits), nsmall = digits),"</td><td style=\"padding-right: 1em\">(",format(round(ci[1], digits), nsmall = digits),", ",format(round(ci[2], digits), nsmall = digits),")</td></tr>")
    html_output = c(html_output, html_line)
  }

  if (!is.null(med_res$dependence)) {
    estimate <- mean(med_res$dependence)
    ci <- quantile(med_res$dependence, c((1-conf.level)/2,1-(1-conf.level)/2))
    html_line <- paste0("<tr><td style=\"padding-right: 1em\">Indirect effect mediated through the dependence between mediators</td><td style=\"padding-right: 1em\">", format(round(estimate, digits), nsmall = digits),"</td><td style=\"padding-right: 1em\">(",format(round(ci[1], digits), nsmall = digits),", ",format(round(ci[2], digits), nsmall = digits),")</td></tr>")
    html_output = c(html_output, html_line)
  }

  if (!is.null(med_res$interaction)) {
    estimate <- mean(med_res$interaction)
    ci <- quantile(med_res$interaction, c((1-conf.level)/2,1-(1-conf.level)/2))
    html_line <- paste0("<tr><td style=\"padding-right: 1em\">Indirect effect mediated through the interaction between mediators</td><td style=\"padding-right: 1em\">", format(round(estimate, digits), nsmall = digits),"</td><td style=\"padding-right: 1em\">(",format(round(ci[1], digits), nsmall = digits),", ",format(round(ci[2], digits), nsmall = digits),")</td></tr>")
    html_output = c(html_output, html_line)
  }

  estimate <- mean(med_res$direct)
  ci <- quantile(med_res$direct, c((1-conf.level)/2,1-(1-conf.level)/2))
  html_line <- paste0("<tr><td style=\"padding-right: 1em\">Direct effect</td><td style=\"padding-right: 1em\">", format(round(estimate, digits), nsmall = digits),"</td><td style=\"padding-right: 1em\">(",format(round(ci[1], digits), nsmall = digits),", ",format(round(ci[2], digits), nsmall = digits),")</td></tr>")
  html_output = c(html_output, html_line)

  estimate <- mean(med_res$total)
  ci <- quantile(med_res$total, c((1-conf.level)/2,1-(1-conf.level)/2))
  html_line <- paste0("<tr><td style=\"padding-right: 1em\">Total effect</td><td style=\"padding-right: 1em\">", format(round(estimate, digits), nsmall = digits),"</td><td style=\"padding-right: 1em\">(",format(round(ci[1], digits), nsmall = digits),", ",format(round(ci[2], digits), nsmall = digits),")</td></tr>")
  html_output = c(html_output, html_line)

  for (i in 1:length(med_res$prop)) {
    estimate <- median(med_res$prop[[i]])
    ci <- quantile(med_res$prop[[i]], c((1-conf.level)/2,1-(1-conf.level)/2))
    html_line <- paste0("<tr><td style=\"padding-right: 1em\">Proportion of effect mediated through ", med[i],"</td><td style=\"padding-right: 1em\">", format(round(estimate, digits), nsmall = digits),"</td><td style=\"padding-right: 1em\">(",format(round(ci[1], digits), nsmall = digits),", ",format(round(ci[2], digits), nsmall = digits),")</td></tr>")
    html_output = c(html_output, html_line)
  }

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
    table$ts <- qt(half_alpha, table$df)
  }else if (class(res)[[1]] == "glm" | class(res)[[1]] == "lm"){
    table <- as.data.frame(summary(res)$coefficients)
    table$df <- res$df.residual
    colnames(table)[which(colnames(table) == "Estimate")] = "estimate"
    colnames(table)[which(colnames(table) == "Std. Error")] = "std.error"
    if (class(res)[[1]] == "lm") {
      colnames(table)[which(colnames(table) == "Pr(>|t|)")] = "p.value"
      table$ts <- qt(half_alpha, table$df)
    }else if (class(res)[[1]] == "glm") {
      colnames(table)[which(colnames(table) == "Pr(>|z|)")] = "p.value"
      table$ts <- qnorm(half_alpha)
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

empirical_pvalue <- function(vec) {

}
