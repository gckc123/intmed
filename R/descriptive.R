descriptive <- function(data, digits = 2, group = NULL, complete = FALSE, ...) {

  html_output <- ""
  df <- dplyr::as_tibble(data)

  #listwise deletion or not?
  if (complete == TRUE) {
    df <- tibble::add_column(df, missing = rowSums(sapply(df, is.na)))
    df <- df[df$missing == 0, 1:length(df)-1]
  }

  df <- char2fac(df)

  grouping_combo <- NULL

  #Display the descriptive statistics by the grouping variables
  #firstly construct the grouping_combo dataframe that enumerates all grouping combinations
  if (length(group) > 0) {
    for (i in seq_along(group)) {
      if (i == 1) {
        expr <- parse(text = paste0("grouping_combo <- tibble(",group[[i]]," = levels(df$", group[[i]],"))"))
        eval(expr)
      }else {
        eval(parse(text = paste0("tmp <- levels(df$", group[[i]],")")))
        tmp1 <- grouping_combo
        for (j in seq_along(tmp)) {
          expr <- parse(text = paste("tmp2 <- tibble::add_column(tmp1,", group[[i]]," = tmp[[j]])"))
          eval(expr)
          if (j == 1) {
            grouping_combo <- tmp2
          }else
          {
            grouping_combo <- dplyr::bind_rows(grouping_combo, tmp2)
          }
        }
      }
    }

    N_list <- list()
    Mean_list <- list()
    SD_list <- list()
    Median_list <- list()
    IQR_list <- list()

    NCat_list <- list()
    Perc_list <- list()

    for (k in 1:nrow(grouping_combo)) {
      selection = ""
      for (l in seq_along(names(grouping_combo))) {
        if ( l == 1) {
          selection = paste0("df$",names(grouping_combo)[[l]]," == \"", grouping_combo[[k,l]],"\"")
        } else {
          tmp = paste0("df$",names(grouping_combo)[[l]]," == \"", grouping_combo[[k,l]],"\"")
          selection = paste(selection, tmp, sep = " & ")
        }
      }

      expr <- parse(text = paste0("subdf <- df[", selection, ", !(names(df) %in% names(grouping_combo))]"))

      eval(expr)

      if (length(df[,sapply(df, is.numeric)]) > 0) {
        dfnumeric <- subdf[, sapply(subdf, is.numeric)]
        Variables <- colnames(dfnumeric)
        N_list[[k]] <- nrow(dfnumeric) - sapply(lapply(dfnumeric, is.na), sum)
        Mean_list[[k]] <- format(round(sapply(dfnumeric, mean, na.rm = TRUE), digits), nsmall = digits)
        SD_list[[k]] <- format(round(sapply(dfnumeric, sd, na.rm = TRUE), digits), nsmall = digits)
        Median_list[[k]] <- format(round(sapply(dfnumeric, median, na.rm = TRUE), digits), nsmall = digits)
        IQR_list[[k]] <- format(round(sapply(dfnumeric, stats::IQR, na.rm = TRUE), digits), nsmall = digits)
      }

      if (length(df[,!sapply(df, is.numeric)]) > 0) {
        dfnonnumeric <- subdf[, !sapply(subdf, is.numeric)]
        for (i in seq_along(dfnonnumeric)) {
          if (i == 1) {
            NCat <- table(dfnonnumeric[i])
            Perc <- format(round(prop.table(table(dfnonnumeric[i]))*100,digits),nsmall = digits)
            VariablesCat <- rep("", length(NCat))
            VariablesCat[1] <- colnames(dfnonnumeric[i])
            Levels <- names(NCat)
          }else {
            NCat <- c(NCat, table(dfnonnumeric[i]))
            Perc <- c(Perc, format(round(prop.table(table(dfnonnumeric[i]))*100,digits),nsmall = digits))
            tmp <- rep("", length(table(dfnonnumeric[i])))
            tmp[1] <- colnames(dfnonnumeric[i])
            VariablesCat <- c(VariablesCat, tmp)
            Levels <- c(Levels, names(table(dfnonnumeric[i])))
          }
        }
        NCat_list[[k]] <- NCat
        Perc_list[[k]] <- Perc
      }
    }


    if (length(df[,sapply(df, is.numeric)]) > 0) {
      html_output <- "<br/><b>Table. Descriptive statistics of continuous variables</b><br/>"
      html_output <- c(html_output, "<table style=\"text-align:left\">")
      html_output <- c(html_output, "<tr><td colspan=\"6\" style=\"border-bottom: 1px solid black\"></td></tr>")
      for (k in 1:nrow(grouping_combo)) {
        html_output <- c(html_output, paste0("<tr><td colspan=\"6\" style=\"text-align:left\">", paste0(grouping_combo[k,], collapse = " "),"</td></tr>"))
        html_output <- c(html_output, "<tr><td style=\"padding-right: 1em\">Variables</td><td style=\"padding-right: 1em\">N</td><td style=\"padding-right: 1em\">Mean</td><td style=\"padding-right: 1em\">SD</td><td style=\"padding-right: 1em\">Median</td><td style=\"padding-right: 1em\">IQR</td></tr>")
        html_output <- c(html_output, "<tr><td colspan=\"6\" style=\"border-bottom: 1px solid black\"></td></tr>")
        for (i in 1:length(Variables)) {
          html_output <- c(html_output, paste0("<tr><td style=\"padding-right: 1em\">",Variables[i],"</td><td style=\"padding-right: 1em\">",N_list[[k]][[i]],
                                               "</td><td style=\"padding-right: 1em\">",Mean_list[[k]][[i]],"</td><td style=\"padding-right: 1em\">",SD_list[[k]][[i]],
                                               "</td><td style=\"padding-right: 1em\">",Median_list[[k]][[i]],"</td><td style=\"padding-right: 1em\">",IQR_list[[k]][[i]],"</td></tr>"))

        }
        html_output <- c(html_output, "<tr><td colspan=\"6\" style=\"border-bottom: 1px solid black\"></td></tr>")
      }
      html_output <- c(html_output, "</table>")
    }

    if (length(df[,!sapply(df, is.numeric)]) > 0) {
      html_output <- c(html_output, "<br/><b>Table. Descriptive statistics of categorical variables</b><br/>")
      html_output <- c(html_output, "<table style=\"text-align:left\">")
      html_output <- c(html_output, paste0("<tr><td colspan=\"",length(Perc_list)*2+2,"\" style =\"border-bottom: 1px solid black\"></td></tr>"))
      tmp <- "<tr><td></td><td></td>"
      tmp1 <- "<tr><td style=\"padding-right: 1em\">Variables</td><td style=\"padding-right: 1em\">Levels</td>"
      for (i in 1:length(Perc_list)) {
        tmp <- paste0(tmp, "<td colspan = \"2\">", paste0(grouping_combo[i,], collapse = " "),"</td>")
        tmp1 <- paste0(tmp1,"<td>N</td><td>%</td>")
      }
      tmp <- paste0(tmp, "</tr>")
      tmp1 <- paste0(tmp1,"</tr>")
      html_output <- c(html_output, tmp, tmp1)
      html_output <- c(html_output, paste0("<tr><td colspan=\"",length(Perc_list)*2+2,"\" style =\"border-bottom: 1px solid black\"></td></tr>"))
      for (i in 1:length(VariablesCat)) {
        if (i != 1 & VariablesCat[[i]] != "") {
          html_output <- c(html_output, paste0("<tr><td></td><td colspan=\"",length(Perc_list)*2+1,"\" style =\"border-bottom: 1px solid black\"></td></tr>"))
        }

        tmp <- paste0("<tr><td style=\"padding-right: 1em\">",VariablesCat[[i]],"</td><td style=\"padding-right: 1em\">",Levels[[i]],"</td>")
        for (j in 1:length(NCat_list)) {
          tmp <- paste0(tmp,"<td style=\"padding-right: 1em\">",NCat_list[[j]][i],"</td><td style=\"padding-right: 1em\">",Perc_list[[j]][i],"</td>")
        }
        tmp <- paste0(tmp,"</tr>")
        html_output <- c(html_output, tmp)
      }
      html_output <- c(html_output, paste0("<tr><td colspan=\"",length(Perc_list)*2+2,"\" style =\"border-bottom: 1px solid black\"></td></tr>"))
      html_output <- c(html_output, "</table>")
    }
  }else {
    dfnumeric <- df[, sapply(df, is.numeric)]
    if (length(dfnumeric) > 0) {
      Variables <- colnames(dfnumeric)
      N <- nrow(dfnumeric) - sapply(lapply(dfnumeric, is.na), sum)
      Mean <- format(round(sapply(dfnumeric, mean, na.rm = TRUE), digits), nsmall = digits)
      SD <- format(round(sapply(dfnumeric, sd, na.rm = TRUE), digits), nsmall = digits)
      Median <- format(round(sapply(dfnumeric, median, na.rm = TRUE), digits), nsmall = digits)
      IQR <- format(round(sapply(dfnumeric, stats::IQR, na.rm = TRUE), digits), nsmall = digits)

      html_output <- "<br/><b>Table. Descriptive statistics of continuous variables</b><br/>"
      html_output <- c(html_output, "<table style=\"text-align:left\">")
      html_output <- c(html_output, "<tr><td colspan=\"6\" style=\"border-bottom: 1px solid black\"></td></tr>")
      html_output <- c(html_output, "<tr><td style=\"padding-right: 1em\">Variables</td><td style=\"padding-right: 1em\">N</td><td style=\"padding-right: 1em\">Mean</td><td style=\"padding-right: 1em\">SD</td><td style=\"padding-right: 1em\">Median</td><td style=\"padding-right: 1em\">IQR</td></tr>")
      html_output <- c(html_output, "<tr><td colspan=\"6\" style=\"border-bottom: 1px solid black\"></td></tr>")
      for (i in 1:length(Variables)) {
        html_output <- c(html_output, paste0("<tr><td style=\"padding-right: 1em\">",Variables[i],"</td><td style=\"padding-right: 1em\">",N[[i]],"</td><td style=\"padding-right: 1em\">",Mean[[i]],"</td><td style=\"padding-right: 1em\">",SD[[i]],"</td><td style=\"padding-right: 1em\">",Median[[i]],"</td><td style=\"padding-right: 1em\">",IQR[[i]],"</td></tr>"))
      }
      html_output <- c(html_output, "<tr><td colspan=\"6\" style=\"border-bottom: 1px solid black\"></td></tr>")
      html_output <- c(html_output, "</table>")

    }
    dfnonnumeric <- df[, !sapply(df, is.numeric)]
    if (length(dfnonnumeric) > 0) {
      for (i in seq_along(dfnonnumeric)) {
        if (i == 1) {
          NCat <- table(dfnonnumeric[i])
          Perc <- format(round(prop.table(table(dfnonnumeric[i]))*100,digits),nsmall = digits)
          VariablesCat <- rep("", length(NCat))
          VariablesCat[1] <- colnames(dfnonnumeric[i])
          Levels <- names(NCat)
        }else
        {
          NCat <- c(NCat, table(dfnonnumeric[i]))
          Perc <- c(Perc, format(round(prop.table(table(dfnonnumeric[i]))*100,digits),nsmall = digits))
          tmp <- rep("", length(table(dfnonnumeric[i])))
          tmp[1] <- colnames(dfnonnumeric[i])
          VariablesCat <- c(VariablesCat, tmp)
          Levels <- c(Levels, names(table(dfnonnumeric[i])))
        }
      }
      html_output <- c(html_output, "<br/><b>Table. Descriptive statistics of categorical variables</b><br/>")
      html_output <- c(html_output, "<table style=\"text-align:left\">")
      html_output <- c(html_output, "<tr><td colspan=\"4\" style =\"border-bottom: 1px solid black\"></td></tr>")
      html_output <- c(html_output, "<tr><td style=\"padding-right: 1em\">Variables</td><td style=\"padding-right: 1em\">Levels</td><td style=\"padding-right: 1em\">N</td><td style=\"padding-right: 1em\">%</td>")
      html_output <- c(html_output, "<tr><td colspan=\"4\" style =\"border-bottom: 1px solid black\"></td></tr>")
      for (i in 1:length(VariablesCat)) {
        if (i != 1 & VariablesCat[[i]] != "") {
          html_output <- c(html_output, "<tr><td></td><td colspan = \"3\" style = \"border-bottom: 1px solid black\"></td></tr>")
        }
        tmp <- paste0("<tr><td style=\"padding-right: 1em\">",VariablesCat[[i]],"</td><td style=\"padding-right: 1em\">",Levels[[i]],"</td><td style=\"padding-right: 1em\">",NCat[[i]],"</td><td style=\"padding-right: 1em\">",Perc[[i]],"</td></tr>")
        html_output <- c(html_output, tmp)
      }
      html_output <- c(html_output, "<tr><td colspan=\"4\" style =\"border-bottom: 1px solid black\"></td></tr>")
      html_output <- c(html_output, "</table>")
    }
  }
  #sink("descriptive.html")
  #cat(html_output)
  #sink()
  return(html_output)
}
