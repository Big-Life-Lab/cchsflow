#' @title Set Data Labels
#' @description sets labels for passed database, Uses the names of final
#' variables in variable_details/variables_sheet as well as the labels contained
#' in the passed dataframes
#'
#' @param data_to_label newly transformed dataset
#' @param variable_details variable_details.csv
#' @param variables_sheet variables.csv
#'
#' @return labeled data_to_label
#'
#' @examples
#' library(cchsflow)
#' library(sjlabelled)
#' bmi2001 <- rec_with_table(
#'  cchs2001_p, c(
#'     "HWTGHTM",
#'     "HWTGWTK", "HWTGBMI_der"
#'   )
#' )
#'
#' bmi2003 <- rec_with_table(
#'   cchs2003_p, c(
#'     "HWTGHTM",
#'     "HWTGWTK", "HWTGBMI_der"
#'   )
#' )
#'
#' combined_bmi <- bind_rows(bmi2001, bmi2003)
#'
#' get_label(combined_bmi)
#'
#' labeled_combined_data <- set_data_labels(combined_bmi,
#'  variable_details,
#'  variables)
#'
#' get_label(labeled_combined_data)
#' @export
set_data_labels <-
  function(data_to_label,
           variable_details,
           variables_sheet = NULL) {
    # Extract variables in the data
    variable_names <- unique(colnames(data_to_label))
    # extract only relevant variable info
    if (!is.null(variable_details)) {
      variable_details[[pkg.globals$argument.Variables]] <- sapply(
        variable_details[[pkg.globals$argument.Variables]], trimws)
      variable_details <-
        variable_details[variable_details[[pkg.globals$argument.Variables]]
                         %in% variable_names, ]
      if (is.null(variables_sheet)) {
        variable_details[[pkg.globals$MSW.Variables.Columns.Label]] <- NA
        variable_details[[pkg.globals$MSW.Variables.Columns.LabelLong]] <-
          NA
      }
    }
    if (!is.null(variables_sheet)) {
      variables_sheet[[pkg.globals$argument.Variables]] <- sapply(
        variables_sheet[[pkg.globals$argument.Variables]], trimws)
      variables_sheet <-
        variables_sheet[variables_sheet[[pkg.globals$argument.Variables]] %in%
                          variable_names, ]
      variable_details <-
        update_variable_details_based_on_variable_sheet(
          variable_sheet = variables_sheet,
          variable_details = variable_details
          )
    }
    label_list <- NULL
    for (variable_name in variable_names) {
      rows_to_process <-
        variable_details[variable_details[[
          pkg.globals$argument.Variables]] == variable_name, ]
      label_list[[variable_name]] <-
        create_label_list_element(rows_to_process)
    }
    data_to_label <- label_data(label_list, data_to_label)

    return(data_to_label)
  }

create_label_list_element <- function(variable_rows) {
  ret_list <- list(
    type = NULL,
    unit = NULL,
    label_long = NULL,
    label = NULL,
    values = c(),
    values_long = c()
  )
  first_row <- variable_rows[1, ]
  ret_list$type <-
    as.character(first_row[[pkg.globals$argument.ToType]])
  ret_list$unit <-
    as.character(first_row[[pkg.globals$argument.Units]])
  ret_list$label_long <-
    as.character(first_row[[pkg.globals$argument.VariableLabel]])
  ret_list$label <-
    as.character(first_row[[pkg.globals$argument.VariableLabelShort]])
  if (is_equal(ret_list$type, pkg.globals$ddiValueName.Cat)) {
    for (row_index in seq_len(nrow(variable_rows))) {
      single_row <- variable_rows[row_index, ]
      # Verify type stays the same
      if (!is_equal(ret_list$type,
                    as.character(single_row[[pkg.globals$argument.ToType]]))) {
        stop(
          paste(
            as.character(single_row[[pkg.globals$argument.Variables]]),
            "does not contain all identical",
            pkg.globals$argument.ToType,
            "variable cant change variable type for different values"
          )
        )
      }
      # Verify unit is identical
      if (!is_equal(ret_list$unit,
                    as.character(single_row[[pkg.globals$argument.Units]]))) {
        stop(
          paste(
            as.character(single_row[[pkg.globals$argument.Variables]]),
            "does not contain all identical",
            pkg.globals$argument.Units,
            "variable cant change unit type for different values"
          )
        )
      }
      # Verify variable label is identical
      if (!is_equal(
        ret_list$label_long,
        as.character(single_row[[pkg.globals$argument.VariableLabel]])
      )) {
        stop(
          paste(
            as.character(single_row[[pkg.globals$argument.Variables]]),
            "does not contain all identical",
            pkg.globals$argument.VariableLabel,
            "variable cant change variableLabel for different values. VAL1:",
            ret_list$label_long,
            "VAL2:",
            as.character(single_row[[pkg.globals$argument.VariableLabel]])
          )
        )
      }
      value_being_labeled <-
        as.character(single_row[[pkg.globals$argument.CatValue]])
      value_being_labeled <-
        recode_variable_NA_formating(value_being_labeled, ret_list$type)
      ret_list$values[[as.character(single_row[[
        pkg.globals$argument.CatLabel]])]] <-
        value_being_labeled
      ret_list$values_long[[as.character(single_row[[
        pkg.globals$argument.CatLabelLong]])]] <-
        value_being_labeled
    }
  }

  return(ret_list)
}

#' @title label_data
#'
#' @description Attaches labels to the DataToLabel to preserve metadata
#'
#' @param label_list the label list object that contains extracted labels
#' from variable details
#' @param data_to_label The data that is to be labeled
#' @importFrom sjlabelled set_labels set_label set_label<-
#'
#' @return Returns labeled data
label_data <- function(label_list, data_to_label) {
  for (variable_name in names(label_list)) {
    if (is.na(label_list[[variable_name]]$type)) {
      warning(paste(variable_name,
                    "is missing from variable_details or variables
                    (if it was also passed) please verify correct spelling"))
      next()
    }
    if (label_list[[variable_name]]$type == pkg.globals$argument.CatType) {
      if (!is.factor(data_to_label[, variable_name])) {
        data_to_label[, variable_name] <-
          factor(data_to_label[[variable_name]])
      }
      # List fix
      label_list[[variable_name]]$values <- unlist(label_list[[variable_name]]$values)
      label_list[[variable_name]]$values_long <- unlist(label_list[[variable_name]]$values_long)
      
      data_to_label[, variable_name] <-
        set_labels(data_to_label[, variable_name],
                   labels = label_list[[variable_name]]$values)
      attr(data_to_label[, variable_name], "labels_long") <-
        label_list[[variable_name]]$values_long
    } else {
      if (is.factor(data_to_label[, variable_name])) {
        data_to_label[, variable_name] <-
          as.numeric(levels(data_to_label[, variable_name])
                     [data_to_label[, variable_name]])
      } else {
        data_to_label[, variable_name] <-
          as.numeric(data_to_label[, variable_name])
      }
    }
    set_label(data_to_label[, variable_name]) <-
      label_list[[variable_name]]$label
    attr(data_to_label[, variable_name], "unit") <-
      label_list[[variable_name]]$unit
    attr(data_to_label[, variable_name], "label_long") <-
      label_list[[variable_name]]$label_long
  }

  return(data_to_label)
}
