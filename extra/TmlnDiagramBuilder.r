library(DiagrammeR)
library(tidyverse)
library(rsvg)
library(DiagrammeRsvg)

#' @title Timeline diagram builder
#'
#' @description This function creates a timeline diagram of the derived variables contained in the 
#' /Data/variable_details.RData file from cchsflow R package. It provides a quick flow reference of 
#' the variables involved to harmonize the derived variable of interest across the specified cchs cycles 
#' If vars_of_interest is equal to NA, the diagram will include all the derived variables.
#'
#' @param vars_of_interest names of the derived variables of interest
#'
#' @param cchs_db_names names of the cchs cycles.
#' 
#' @param output_name name of the output .png file to be saved in the working directory.
#'
#' @return A .png file in the working directory.
#'
#' @examples
#' # Using tmlndiag() to get the timeline diagram of time quit smoking.
#'
#' library(cchsflow)
#' vars_of_interest <- c("SMKDSTY_cat5", "time_quit_smoking", "smoke_simple")
#' cchs_db_names <- paste0(paste0("cchs",c(2001,2003,2005,"2007_2008","2009_2010","2011_2012","2013_2014")),"_p") 
#' tmlndiag(vars_of_interest, cchs_db_names, "TSQ")
#' 
#' # Using tmlndiag() to get the timeline diagram of all derived variables 
#' tmlndiag(NA, cchs_db_names, "All_DerivedVars")
#'  
#' @export
#' 
tmlndiag <- function(vars_of_interest, cchs_db_names, output_name) {
  # all variable names
  derived_vars0 <- unique(variable_details$variable)

  # Create the list of variables and labels based on the "list of variables of interest"
  nrs <- which((derived_vars0 %in% vars_of_interest) == T)

  if (!any(is.na(vars_of_interest))) {
    derived_vars <- derived_vars0[nrs]
  } else {
    derived_vars <- derived_vars0
  }

  listB <- list()
  longLabels <- derived_vars
  for (k in seq_along(derived_vars)) {
    derived_var <- derived_vars[k]
    variableStart <- variable_details$variableStart[variable_details$variable == derived_var]
    databaseStart <- variable_details$databaseStart[variable_details$variable == derived_var]
    longLabels[k] <- unique(variable_details$variableStartLabel[variable_details$variable == derived_var])
    listA <- list()
    for (j in seq_along(cchs_db_names)) {
      db_name <- cchs_db_names[j]

      # Filter rows that match the database name
      InDb <- grepl(db_name, databaseStart)
      var_start <- unique(variableStart[InDb])
      bracket_chunk <- str_extract(var_start, "(?<=\\[).*(?=\\])")
      final_chunk <- str_extract(bracket_chunk, ".*(?=\\])") 

      cond <- grepl(db_name, var_start)

      # Extract variable names
      ext_part1 <- str_extract(var_start, paste0("(?<=", db_name, "::).*?(?=\\,)"))
      vars0 <- ifelse(cond,
        ifelse(is.na(ext_part1), str_extract(var_start, paste0("(?<=", db_name, "::).*")), ext_part1),
        ifelse(grepl("\\[|\\]", bracket_chunk), final_chunk, bracket_chunk)
      )

      vars <- unique(vars0)
      listA[[j]] <- ifelse(!is.na(vars), paste0(paste0(db_name, "::"), vars), NA)
    }
    listB[[k]] <- na.omit(unlist(listA))
  }

  # Assign the list
  variables <- listB

  # Parse year and variable names for each derived var
  parsed_list <- lapply(variables, function(var_group) {
    years <- sub("cchs(\\d{4}(?:_\\d{4})?)_.*", "\\1", var_group)
    vars <- sub(".*::", "", var_group)
    data.frame(year = years, var = vars, stringsAsFactors = FALSE)
  })

  # Collect all unique years in order
  all_years <- sort(unique(unlist(lapply(parsed_list, function(df) df$year))))

  # Build a combined data frame
  df <- data.frame(year = all_years, stringsAsFactors = FALSE)
  for (i in seq_along(parsed_list)) {
    df[[paste0("var", i)]] <- parsed_list[[i]]$var[match(df$year, parsed_list[[i]]$year)]
  }

  # The graph will be laid out from left to right
  # Begin DOT graph
  dot <- "digraph G {
  graph [rankdir=LR, layout=dot];
  node [shape=box, style=filled, fontname=Helvetica, fontsize=12];
  edge [color=gray50];
  "

  for (i in seq_len(nrow(df))) {
    year_id <- paste0("y", i)
    dot <- paste0(dot, sprintf('  %s [label="%s", shape=plaintext, fillcolor=white];\n', year_id, df$year[i]))
  }

  # Build timeline by year row
  for (i in seq_len(nrow(df))) {
    year_id <- paste0("y", i)
    row_ids <- c(year_id)

    for (j in seq_along(derived_vars)) {
      var_value <- str_replace_all(df[[paste0("var", j)]][i], ", ", " \n")
      if (!is.na(var_value)) {
        node_id <- paste0("v", j, "_", i)
        row_ids <- c(row_ids, node_id)
        dot <- paste0(dot, sprintf(
          '  %s [label="%s", fillcolor=lightblue, fixedsize=FALSE];\n',
          node_id, var_value
        ))
      }
    }

    dot <- paste0(dot, "  { rank=same; ", paste(row_ids, collapse = "; "), " }\n")
  }

  # Final derived variable boxes
  for (j in seq_along(derived_vars)) {
    dot <- paste0(dot, sprintf(
      '  d%d [label="%s", shape=rect, style="rounded,filled", fillcolor=black, fontcolor=white, fixedsize=FALSE];\n',
      j, paste0(derived_vars[j], " \n", longLabels[j])
    ))
  }
  dot <- paste0(dot, "  { rank=same; ", paste0("d", seq_along(derived_vars), collapse = "; "), " }\n")

  # Draw edges
  for (j in seq_along(derived_vars)) {
    col <- paste0("var", j)
    valid_idx <- which(!is.na(df[[col]]))
    if (length(valid_idx) > 0) {
      for (k in seq_along(valid_idx)[-1]) {
        from <- paste0("v", j, "_", valid_idx[k - 1])
        to <- paste0("v", j, "_", valid_idx[k])
        dot <- paste0(dot, sprintf("  %s -> %s [arrowhead='none'];\n", from, to))
      }
      last_node <- paste0("v", j, "_", valid_idx[length(valid_idx)])
      dot <- paste0(dot, sprintf("  %s -> d%d;\n", last_node, j))
    }
  }

  # End graph
  dot <- paste0(dot, "}")
  # For manual debug: writeLines(dot, "raw_code.txt")
  
  # Render
  tmp0 <- DiagrammeR::grViz(dot)
  
  # Convert to SVG, then save as png
  tmp <- DiagrammeRsvg::export_svg(tmp0)
  tmp <- charToRaw(tmp)
  rsvg::rsvg_png(tmp, paste0(output_name, ".png"))
  
  return(tmp0)
}
