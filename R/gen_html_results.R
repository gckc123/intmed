gen_med_reg_html <- function(res_df, y, med, conf.level = 0.95) {
  html_output = "<table style = \"text-align: left;border-bottom: 1px solid black; border-top: 1px solid black;\" cellspacing=\"0\" cellpadding = \"2\">"
  html_line = "<tr><td></td>"

  for (i in 1:length(med)) {
    html_line = paste0(html_line,"<td colspan = \"3\">",med[i],"</td>")
  }

  html_line = paste0(html_line, "<td colspan = \"3\">", y,"</td></tr>")
  html_output = c(html_output, html_line)
  html_line = "<tr><td style=\"padding-right: 1em;border-bottom: 1px solid black;\">Variables</td>"
  tmp = paste0(rep(paste0("<td style=\"padding-right: 1em;border-bottom: 1px solid black;\">b</td><td style=\"padding-right: 1em;border-bottom: 1px solid black;\">", round(conf.level*100,0),"% CI","</td><td style=\"padding-right: 1em;border-bottom: 1px solid black;\">p-value</td>"),length(med)+1), collapse = "")
  html_line = paste0(html_line, tmp, "</tr>")
  html_output = c(html_output, html_line)

  res_df[sapply(res_df, is.na)] = "-"
  for (i in 1:nrow(res_df)) {
    html_line = paste0("<tr><td style=\"padding-right: 1em\">",paste0(res_df[i,], collapse = "</td><td style=\"padding-right: 1em\">"),"</td></tr>")
    html_output = c(html_output, html_line)
  }

  html_output = c(html_output, "</table>")
  sink("med_reg_table.html")
  cat(html_output)
  sink()
  return(html_output)

}

gen_med_table <- function(indirect, direct, total, prop, dependence, interaction, med, conf.level = 0.95, digits = 2) {
  html_output = "<table style = \"text-align: left;border-bottom: 1px solid black; border-top: 1px solid black;\" cellspacing=\"0\" cellpadding = \"2\">"
  html_line = paste0("<tr><td></td><td>Estimates</td><td>",round(conf.level*100, 0),"% CI</td></tr>")
  html_output = c(html_output, html_line)
  for (i in 1:length(indirect)) {
    estimate <- mean(indirect[[i]])
    ci <- quantile(indirect[[i]], c((1-conf.level)/2,1-(1-conf.level)/2))
    html_line <- paste0("<tr><td>Indirect effect mediated through ", med[i],"</td><td>", format(round(estimate, digits), nsmall = digits),"</td><td>(",format(round(ci[1], digits), nsmall = digits),", ",format(round(ci[2], digits), nsmall = digits),")</td></tr>")
    html_output = c(html_output, html_line)
  }
  if (!is.null(dependence)) {
    estimate <- mean(dependence)
    ci <- quantile(dependence, c((1-conf.level)/2,1-(1-conf.level)/2))
    html_line <- paste0("<tr><td>Indirect effect mediated through the dependence between mediators</td><td>", format(round(estimate, digits), nsmall = digits),"</td><td>(",format(round(ci[1], digits), nsmall = digits),", ",format(round(ci[2], digits), nsmall = digits),")</td></tr>")
    html_output = c(html_output, html_line)
  }
  if (!is.null(interaction)) {
    estimate <- mean(interaction)
    ci <- quantile(interaction, c((1-conf.level)/2,1-(1-conf.level)/2))
    html_line <- paste0("<tr><td>Indirect effect mediated through the interaction between mediators</td><td>", format(round(estimate, digits), nsmall = digits),"</td><td>(",format(round(ci[1], digits), nsmall = digits),", ",format(round(ci[2], digits), nsmall = digits),")</td></tr>")
    html_output = c(html_output, html_line)
  }
  estimate <- mean(direct)
  ci <- quantile(direct, c((1-conf.level)/2,1-(1-conf.level)/2))
  html_line <- paste0("<tr><td>Direct effect</td><td>", format(round(estimate, digits), nsmall = digits),"</td><td>(",format(round(ci[1], digits), nsmall = digits),", ",format(round(ci[2], digits), nsmall = digits),")</td></tr>")
  html_output = c(html_output, html_line)
  for (i in 1:length(prop)) {
    estimate <- mean(prop[[i]])
    ci <- quantile(prop[[i]], c((1-conf.level)/2,1-(1-conf.level)/2))
    html_line <- paste0("<tr><td>Indirect effect mediated through ", med[i],"</td><td>", format(round(estimate, digits), nsmall = digits),"</td><td>(",format(round(ci[1], digits), nsmall = digits),", ",format(round(ci[2], digits), nsmall = digits),")</td></tr>")
    html_output = c(html_output, html_line)
  }
  html_output <- c(html_output,"</table>")
  return(html_output)
}

gen_med_reg_table <- function(y_res, m_res, med, conf.level = 0.95, digits = 2) {

  table <- extract_reg_table(m_res[[1]], conf.level)
  colnames(table) <- c("variables", "m1.b","m1.ci","m1.p")

  if (length(m_res) > 1) {
    for (i in 2:length(m_res)) {

      #need to fix this 6.12.2019
      tmp <- extract_reg_table(m_res[[i]], conf.level)
      colnames(tmp) <- c("variables", paste0("m",i,".b"), paste0("m",i,".ci"), paste0("m",i,".p"))
      table <- merge(table, tmp, by = "variables", all = TRUE)
    }
  }

  tmp <- extract_reg_table(y_res, conf.level, digits)

  colnames(tmp) <- c("variables","y.b","y.ci", "y.p")

  table <- merge(table, tmp , by = "variables", all = TRUE)
  table.part1 <- table[!is.na(table$m1.b),]
  table.part2 <- table[is.na(table$m1.b),]
  table <- rbind(table.part1,table.part2)

  return(table)

}


extract_reg_table <- function(res, conf.level = 0.95, digits = 2) {
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

  table$ci <- paste0("(",format(round(table$lb, digits), digits),", ",format(round(table$ub, digits), digits),")")
  table$estimate <- format(round(table$estimate, digits), digits)
  table$estimate <- ifelse(table$p.value < .05 ,paste0(table$estimate,"*"),table$estimate)
  table$estimate <- ifelse(table$p.value < .01 ,paste0(table$estimate,"*"),table$estimate)
  table$estimate <- ifelse(table$p.value < .001 ,paste0(table$estimate,"*"),table$estimate)


  table$p.value <- format(round(table$p.value, 3), 3)

  table$variables <- rownames(table)
  table <- table[,c("variables","estimate","ci","p.value")]
  return(table)
}
