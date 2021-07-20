variables <- read.csv("inst/extdata/variables.csv", header = TRUE)
# variables$databaseStart[1]
i <- 1
#Loop for all variables in the variables.csv table
for (var_name_str in variables$variable) {
# Split the years into a list containing each cchs year
# var_name_str <- variables$variable[i]
  separated_vars <- strsplit(variables$databaseStart[i], split = ", ")
  separated_vars <- c(unlist(separated_vars))
  k <- 1
  # Loop for all years that data is available for a given variable
  for (cchs_str in separated_vars) {
    cchs_year <- eval(parse(text=paste(cchs_str)))
    rwt_data <- rec_with_table(cchs_year,
                               database_name = cchs_str,
                               var_name_str,
                               log = TRUE)
    # Define the year as only the year-number in the database name string
    if (nchar(cchs_str)>= 15)
    {
      yr_str <- substring(cchs_str, 5, 13)
      yr_str <- gsub("_","-", yr_str)
    } else {
      yr_str <- substring(cchs_str, 5, 8)
    }
    if (k == 1) {
      cchs_data <- as_label(rwt_data)
      cchs_data$year <- yr_str
    } else {
      cchs_data <- bind_rows(cchs_data,as_label(rwt_data))
      cchs_data$year[is.na(cchs_data$year)] <- yr_str
    }
    k <- k+1
  }

  if (ncol(cchs_data) > 1) {
    # Generate stacked barplot for categorical variables
    if (variables$variableType[i] == "Categorical") {
      # Create table with number of each categorical response for the plot
      agg <- count(cchs_data,eval(parse(text=paste(var_name_str))),year)
      names(agg)[1] <- var_name_str
      plot <- ggplot(agg, aes(x = year,
                              y = n,
                              fill = eval(parse(text=paste(var_name_str))))) + 
                geom_bar(position="stack", stat="identity") +
                labs(
                  fill = var_name_str,
                  x = "Year",
                  y = var_name_str
      )
      # Transform data with tidyr::spread() for the variable name
      agg_chi_sq <- spread(agg,var_name_str,n)
      rownames(agg_chi_sq) <- agg_chi_sq[,1]
      agg_chi_sq <- agg_chi_sq[,-1]
      agg_chi_sq[is.na(agg_chi_sq)] <- 0
      # Run chi squared and separate significance based on p value
      chisq <- chisq.test(agg_chi_sq)
      p <- chisq$p.value
    } else {
      # Create bar graphs for continuous variables for the means
      if (variables$variableType[i] == "Continuous") {
        plot <- ggplot(cchs_data, aes(x = year, 
                                      y = eval(parse(text=paste(var_name_str))), 
                                      group = year, 
                                      fill = year)) +
          geom_boxplot(alpha = .7) +
          geom_jitter(width = .05, alpha = .4) +
          guides(fill = "none") +
          theme_bw() +
          labs(
            x = "Year",
            y = var_name_str
          )
        # Run one-way ANOVA to determine significance for continuous variables
        res.aov <- aov(eval(parse(text=paste(var_name_str))) ~ year, data = cchs_data)
        p <- summary(res.aov)[[1]][["Pr(>F)"]]
      } else {
        warning("Variable type is neither categorical nor continuous.")
      }
    }
    # Save significant and ns plots in different folders
    if (p < 0.05) {
      img_label <- paste("man/figures/sig_plots/", var_name_str, ".png", 
                         sep = "", collapse = NULL)
    } else {
      img_label <- paste("man/figures/ns_plots/", var_name_str, ".png", 
                         sep = "", collapse = NULL)
    }
    img_label
    plot + ggsave(file=img_label, 
           width = 16, 
           height = 9)
  } else {
    p <- "Error - no data encoded"
  }
  # Append data to summary table
  if (i==1) {
    summary <- data.frame(var_name_str, p)
    names(summary)[1] <- "Variable Name"
    names(summary)[2] <- "p-value"
  } else {
    summary[i,1] <- var_name_str
    if (length(p) > 1) {
      p <- p[1]
    }
    summary[i,2] <- p
  }
  i <- i+1
}
write.csv(summary,"inst/extdata/summary_variable_p-values.csv")
summary
