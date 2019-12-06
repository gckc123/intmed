gen_reg_table <- function(y_res, m_res, digits = 2, conf.level = 0.95) {

  table <- extract_reg_table(m_res[[1]], conf.level)
  colnames(table) <- c("variables", "m1.b","m1.lb","m1.ub","m1.p")

  if (length(m_res) > 1) {
    for (i in 2:length(m_res)) {

      #need to fix this 6.12.2019
      tmp <- extract_reg_table(m_res[[i]], conf.level)
      colnames(tmp) <- c("variables", paste0("m",i,".b"), paste0("m",i,".lb"), paste0("m",i,".ub"), paste0("m",i,".p"))
      table <- merge(table, tmp, by = "variables", all = TRUE)
    }
  }

  tmp <- extract_reg_table(y_res, conf.level)

  colnames(tmp) <- c("variables","y.b","y.lb", "y.ub", "y.p")

  table <- merge(table, tmp , by = "variables", all = TRUE)
  table.part1 <- table[!is.na(table$m1.b),]
  table.part2 <- table[is.na(table$m1.b),]
  table <- rbind(table.part1,table.part2)

  return(table)

}


extract_reg_table <- function(res, conf.level) {
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
  table$variables <- rownames(table)
  table <- table[,c("variables","estimate","lb","ub","p.value")]
  return(table)
}
