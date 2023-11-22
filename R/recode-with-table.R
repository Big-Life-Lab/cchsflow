# Removing Note
. <- NULL
#' @title is equal
#' @description Function to compare even with NA present
#' This function returns TRUE wherever elements are the same, including NA's,
#' and false everywhere else.
#'
#' @param v1 variable 1
#' @param v2 variable 2
#'
#' @return boolean value of whether or not v1 and v2 are equal
#'
#' @examples
#' library(cchsflow)
#' is_equal(1,2)
#' # FALSE
#'
#' is_equal(1,1)
#' # TRUE
#'
#' 1==NA
#' # NA
#'
#' is_equal(1,NA)
#' # FALSE
#'
#' NA==NA
#' # NA
#'
#' is_equal(NA,NA)
#' # TRUE
#' @export
is_equal <- function(v1, v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  # anything compared to NA equals NA
  # replaces all instanses of NA with FALSE
  same[is.na(same)] <- FALSE

  return(same)
}

#' Recode with Table
#'
#' Recode with Table is responsible for recoding values of a dataset based on
#' the specifications in variable_details.
#'
#' The \href{https://github.com/Big-Life-Lab/cchsflow/blob/master/inst/extdata/variable_details.csv}{variable_details}
#'  dataframe needs the following variables to function:
#'  \describe{
#'   \item{variable}{name of new (mutated) variable that is recoded}
#'   \item{toType}{type the variable is being recoded to
#'   \emph{cat = categorical, cont = continuous}}
#'   \item{databaseStart}{name of dataframe with original variables to be
#'   recoded}
#'   \item{variableStart}{name of variable to be recoded}
#'   \item{fromType}{variable type of start variable.
#'   \emph{cat = categorical or factor variable}
#'   \emph{cont = continuous variable (real number or integer)}}
#'   \item{recTo}{Value to recode to}
#'   \item{recFrom}{Value/range being recoded from}
#'  }
#'  Each row in \emph{variable_details} comprises one category in a
#'  newly transformed variable. The rules for each category the new variable
#'  are a string in \emph{recFrom} and value in \emph{recTo}.
#'  These recode pairs are the same syntax as \emph{sjmisc::rec()},
#'  except in \emph{sjmisc::rec()} the pairs are a string for the function
#'  attribute \emph{rec =}, separated by '\emph{=}'.
#'  For example in \emph{rec_w_table}
#'  \emph{variable_details$recFrom = 2; variable_details$recTo = 4}
#'  is the same as \emph{sjmisc::rec(rec = "2=4")}.
#'  the pairs are obtained from the RecFrom and RecTo columns
#'   \describe{
#'     \item{recode pairs}{each recode pair is row. see above example
#'     or \emph{PBC-variableDetails.csv}}
#'     \item{multiple values}{multiple old values that should be recoded into
#'     a new single value may be separated with comma, e.g.
#'     \emph{recFrom = "1,2"; recTo = 1}}
#'     \item{value range}{a value range is indicated by a colon, e.g.
#'     \emph{recFrom= "1:4"; recTo = 1} (recodes all values from 1 to 4 into 1)}
#'     \item{value range for doubles}{for double vectors (with fractional part),
#'      all values within the specified range are recoded; e.g.
#'      \emph{recFrom = "1:2.5'; recTo = 1} recodes 1 to 2.5 into 1,
#'      but 2.55 would not be recoded
#'      (since it's not included in the specified range)}
#'     \item{\emph{"min"} and \emph{"max"}}{minimum and maximum values
#'     are indicates by \emph{min} (or \emph{lo}) and \emph{max} (or \emph{hi}),
#'      e.g. \emph{recFrom = "min:4"; recTo = 1} (recodes all values from
#'      minimum values of \emph{x} to 4 into 1)}
#'     \item{\emph{"else"}}{all other values, which have not been specified yet,
#'      are indicated by \emph{else}, e.g. \emph{recFrom = "else"; recTo = NA}
#'      (recode all other values (not specified in other rows) to "NA")}
#'     \item{\emph{"copy"}}{the \emph{"else"}-token can be combined with
#'     \emph{copy}, indicating that all remaining, not yet recoded values should
#'      stay the same (are copied from the original value), e.g.
#'      \emph{recFrom = "else"; recTo = "copy"}}
#'     \item{\emph{NA}'s}{\emph{NA} values are allowed both as old and
#'     new value, e.g.
#'     \emph{recFrom "NA"; recTo = 1. or "recFrom = "3:5"; recTo = "NA"}
#'     (recodes all NA into 1,
#'     and all values from 3 to 5 into NA in the new variable)}
#' }
#'
#' @param data A dataframe containing the variables to be recoded. Can also be a list of dataframes
#' @param variables character vector containing variable names to recode or
#' a variables csv containing additional variable info
#' @param database_name String, the name of the dataset containing the variables
#' to be recoded. Can also be a vector of strings if data is a list
#' @param variable_details A dataframe containing the specifications (rules)
#' for recoding.
#' @param else_value Value (string, number, integer, logical or NA) that is used
#' to replace any values that are outside the specified ranges
#' (no rules for recoding).
#' @param append_to_data Logical, if \code{TRUE} (default), recoded variables
#' will be appended to the data.
#' @param log Logical, if \code{FALSE} (default), a log of recoding will
#' not be printed.
#' @param notes Logical, if \code{FALSE} (default), will not print the
#' content inside the `Note`` column of the variable being recoded.
#' @param var_labels labels vector to attach to variables in variables
#' @param custom_function_path path to location of the function to load
#' @param attach_data_name to attach name of database to end table
#'
#' @return a dataframe that is recoded according to rules in variable_details.
#'
#' @examples
#' library(cchsflow)
#' bmi2001 <- rec_with_table(
#'   data = cchs2001_p, c(
#'     "HWTGHTM",
#'     "HWTGWTK", "HWTGBMI_der"
#'   )
#' )
#'
#' head(bmi2001)
#'
#' bmi2011_2012 <- rec_with_table(
#'   data = cchs2011_2012_p,  c(
#'     "HWTGHTM",
#'     "HWTGWTK", "HWTGBMI_der"
#'   )
#' )
#'
#' tail(bmi2011_2012)
#'
#' combined_bmi <- bind_rows(bmi2001, bmi2011_2012)
#' head(combined_bmi)
#' tail(combined_bmi)
#' @importFrom haven tagged_na
#' @importFrom stringr str_match
#' @importFrom dplyr rowwise select do
#' @importFrom magrittr %>%
#' @export

rec_with_table <-
  function(data,
           variables = NULL,
           database_name = NULL,
           variable_details = NULL,
           else_value = NA,
           append_to_data = FALSE,
           log = FALSE,
           notes = TRUE,
           var_labels = NULL,
           custom_function_path = NULL,
           attach_data_name = FALSE) {
    # If custom Functions are passed create new environment and source
    if (!is.null(custom_function_path)) {
      source(custom_function_path)
    }
    if (is.null(variable_details)) {
      message("No variable_details detected.
              Loading cchsflow variable_details")
      data(variable_details, package = "cchsflow", envir = environment())
    }
    if (is.null(variables)) {
      message("No variables detected.
              Loading cchsflow variables")
      data(variables, package = "cchsflow", envir = environment())
    }
    if (is.null(database_name)) {
      message("Using the passed data variable name as database_name")
      database_name <- deparse(substitute(data))
    }
    # ---- Step 1: Detemine if the passed data is a list or single database
    append_non_db_columns <- FALSE
    if (is.list(data) &&
        length(database_name) == length(data)) {
      for (data_name in database_name) {
        # ---- Step 2A: Verify that the passed name exists in the passed data
        
        if (!is.null(data[[data_name]])) {
          data[[data_name]] <- recode_call(
            variables = variables,
            data = data[[data_name]],
            database_name = database_name,
            print_note = notes,
            else_value = else_value,
            variable_details = variable_details,
            append_to_data = append_to_data,
            append_non_db_columns = append_non_db_columns,
            log = log,
            var_labels = var_labels
          )
        } else {
          stop(
            paste(
              "The data",
              data_name,
              "is missing from the passed list please verify the names are
              correct in the data list and the database_name list"
            )
          )
        }
      }
    } else if ("data.frame" %in% class(data) &&
               length(database_name) == 1) {
      data <- recode_call(
        variables = variables,
        data = data,
        database_name = database_name,
        print_note = notes,
        else_value = else_value,
        variable_details = variable_details,
        append_to_data = append_to_data,
        append_non_db_columns = append_non_db_columns,
        log = log,
        var_labels = var_labels
      )
      if (attach_data_name) {
        data[["data_name"]] <- database_name
      }
    } else {
      stop(
        paste(
          "The passed number of data does not match the passed number of
          data_names. Please verify that the number of databases matches the number
          of passed names.
          Aborting operation!"
        ),
        call. = FALSE
      )
    }
    
    return(data)
  }

# Creates inputs and runs recode functions
recode_call <-
  function(variables,
           data,
           database_name,
           print_note,
           else_value,
           variable_details,
           append_to_data,
           append_non_db_columns,
           log,
           var_labels) {
    variable_details[[pkg.globals$argument.Variables]] <-
      trimws(variable_details[[pkg.globals$argument.Variables]])
    if (!is.null(variables) && "data.frame" %in% class(variables)) {
      variables[[pkg.globals$argument.Variables]] <-
        trimws(variables[[pkg.globals$argument.Variables]])
      variable_details <-
        update_variable_details_based_on_variable_sheet(
          variable_sheet = variables,
          variable_details = variable_details
        )
    } else {
      if (!is.null(variables)) {
        variable_details <-
          variable_details[trimws(variable_details[[
            pkg.globals$argument.Variables]]) %in% variables, ]
        vars_being_recoded <-
          as.character(unique(variable_details[[
            pkg.globals$argument.Variables]]))
        if (length(vars_being_recoded) != length(variables)) {
          missing_vars <- setdiff(variables, vars_being_recoded)
          warning(
            paste(
              missing_vars,
              "is missing from variable details therefore cannot be recoded"
            )
          )
        }
      }
      if (is.null(variable_details[[pkg.globals$argument.VariableLabel]])) {
        variable_details[[pkg.globals$argument.VariableLabel]] <- NA
      }
      if (is.null(variable_details[[
        pkg.globals$argument.VariableLabelShort]])) {
        variable_details[[pkg.globals$argument.VariableLabelShort]] <- NA
      }
    }
    if (!is.null(var_labels)) {
      if (is.null(names(var_labels))) {
        stop(
          "The passed labels was not a named vector please follow
          the c(var_name = varLalbel) format"
        )
      } else {
        if (is.factor(variable_details[[
          pkg.globals$argument.VariableLabelShort]])) {
          variable_details[[pkg.globals$argument.VariableLabelShort]] <-
            as.character(variable_details[[
              pkg.globals$argument.VariableLabelShort]])
        }
        for (var_name in names(var_labels)) {
          variable_details[variable_details[[
            pkg.globals$argument.Variables]] == var_name,
            pkg.globals$argument.VariableLabelShort] <-
            var_labels[[var_name]]
        }
      }
    }
    
    all_possible_var_names <-
      unique(as.character(variable_details[[pkg.globals$argument.Variables]]))
    all_variables_detected <-
      variable_details[grepl(database_name, variable_details[[
        pkg.globals$argument.DatabaseStart]]), ]
    
    rec_data <-
      recode_columns(
        data = data,
        variables_to_process = all_variables_detected,
        data_name = database_name,
        log = log,
        print_note = print_note,
        else_default = else_value
      )
    if (append_non_db_columns) {
      missed_variables <-
        all_possible_var_names[!all_possible_var_names %in%
                                 unique(as.character(
                                   all_variables_detected[
                                     ,
                                     pkg.globals$argument.Variables]))]
      for (missed_variable_name in missed_variables) {
        rec_data[[missed_variable_name]] <- NA
      }
    }
    
    if (append_to_data) {
      data <- cbind(data, rec_data)
    } else {
      data <- rec_data
    }
    
    return(data)
  }

#' @title Get Data Variable Name
#'
#' @name get_data_variable_name
#'
#' @description Retrieves the name of the column inside data to
#' use for calculations
#'
#' @param data_name name of the database being checked
#' @param data database being checked
#' @param row_being_checked the row from variable details that contains
#' information on this variable
#' @param variable_being_checked the name of the recoded variable
#'
#' @return the data equivalent of variable_being_checked
#' 
#' @keywords internal
get_data_variable_name <-
  function(data_name,
           data,
           row_being_checked,
           variable_being_checked) {
    data_variable_being_checked <- character()
    var_start_names <-
      as.character(row_being_checked[[pkg.globals$argument.VariableStart]])
    
    if (grepl(data_name, var_start_names)) {
      var_start_names_list <- as.list(strsplit(var_start_names, ",")[[1]])
      # Find exact var Name
      for (var_name in var_start_names_list) {
        if (grepl(data_name, var_name)) {
          # seperate dataname from the var name
          data_variable_being_checked <-
            as.list(strsplit(var_name, "::")[[1]])[[2]]
        }
      }
      # Check for default variable name
    } else if (grepl("\\[", var_start_names)) {
      # Strip default var name tags: []
      data_variable_being_checked <-
        str_match(var_start_names, "\\[(.*?)\\]")[, 2]
    } else {
      stop(
        paste(
          "The row
          ",
          row,
          "for the variable",
          variable_being_checked,
          "
          Does not contain the database being checked(",
          data_name,
          ") in its variable start the default is also missing.
          Please double check if this variable should have this",
          data_name,
          "included in its databaseStart"
        )
      )
    }
    data_variable_being_checked <- trimws(data_variable_being_checked)
    return(data_variable_being_checked)
  }

#' recode_columns
#'
#' Recodes columns from passed row and returns just table with those columns
#' and same rows as the data
#'
#' @param data The source database
#' @param variables_to_process rows from variable details that are applicable
#' to this DB
#' @param data_name Name of the database being passed
#' @param log The option of printing log
#' @param print_note the option of printing the note columns
#' @param else_default default else value to use if no else is present
#'
#' @return Returns recoded and labeled data
#' 
#' @keywords internal
recode_columns <-
  function(data,
           variables_to_process,
           data_name,
           log,
           print_note,
           else_default) {
    # Split variables to process into recode map and func
    map_variables_to_process <-
      variables_to_process[grepl("map::", variables_to_process[[
        pkg.globals$argument.CatValue]]), ]
    
    func_variables_to_process <-
      variables_to_process[grepl("DerivedVar::", variables_to_process[[
        pkg.globals$argument.VariableStart]]), ]
    
    rec_variables_to_process <-
      variables_to_process[(!grepl("Func::|map::", variables_to_process[[
        pkg.globals$argument.CatValue]])) & (!grepl("DerivedVar::",
                                                    variables_to_process[[
                                                      pkg.globals$argument.VariableStart]])), ]
    
    label_list <- list()
    # Set interval if none is present
    valid_intervals <- c("[,]", "[,)", "(,]")
    interval_default <- "[,]"
    recoded_data <- data[, 0]
    # Loop through the rows of recode vars
    while (nrow(rec_variables_to_process) > 0) {
      variable_being_checked <-
        as.character(rec_variables_to_process[1,
                                              pkg.globals$argument.Variables])
      rows_being_checked <-
        rec_variables_to_process[rec_variables_to_process[[
          pkg.globals$argument.Variables]] == variable_being_checked, ]
      rec_variables_to_process <-
        rec_variables_to_process[!rec_variables_to_process[[
          pkg.globals$argument.Variables]] == variable_being_checked, ]
      first_row <- rows_being_checked[1, ]
      # Check for varialbe existance in data
      data_variable_being_checked <-
        get_data_variable_name(
          data_name = data_name,
          row_being_checked = first_row,
          variable_being_checked = variable_being_checked,
          data = data
        )
      if (is.null(data[[data_variable_being_checked]])) {
        warning(
          paste(
            "Data",
            data_name,
            "does not contain the variable",
            data_variable_being_checked
          )
        )
      } else {
        # Check for From column duplicates
        all_from_values_for_variable <-
          rows_being_checked[[pkg.globals$argument.From]]
        if (length(unique(
          all_from_values_for_variable)) != length(
            all_from_values_for_variable)) {
          for (single_from in all_from_values_for_variable) {
            # Check if value is repeated more then once
            if (sum(all_from_values_for_variable == single_from) > 1) {
              stop(
                paste(
                  single_from,
                  "was detected more then once in",
                  variable_being_checked,
                  "please make sure only one from value is being recoded"
                )
              )
            }
          }
        }
        
        # Set factor for all recode values
        label_list[[variable_being_checked]] <-
          create_label_list_element(rows_being_checked)
        else_value <-
          as.character(rows_being_checked[rows_being_checked[[
            pkg.globals$argument.From]] == "else",
            pkg.globals$argument.CatValue])
        if (length(else_value) == 1 &&
            !is_equal(else_value, "character(0)")) {
          else_value <-
            recode_variable_NA_formating(else_value, label_list[[
              variable_being_checked]]$type)
          if (is_equal(else_value, "copy")) {
            recoded_data[variable_being_checked] <-
              data[data_variable_being_checked]
          } else {
            recoded_data[variable_being_checked] <- else_value
          }
          # Catch multiple else rows
        } else if (length(else_value) > 1) {
          stop(
            paste(
              variable_being_checked,
              " contains",
              length(else_value),
              "rows of else only one else value is allowed"
            )
          )
        }
        else {
          recoded_data[variable_being_checked] <- else_default
        }
        rows_being_checked <-
          rows_being_checked[!rows_being_checked[[
            pkg.globals$argument.From]] == "else", ]
        if (nrow(rows_being_checked) > 0) {
          log_table <- rows_being_checked[, 0]
          log_table$value_to <- NA
          log_table$From <- NA
          log_table$rows_recoded <- NA
          levels(recoded_data[[variable_being_checked]]) <-
            c(levels(recoded_data[[variable_being_checked]]),
              levels(rows_being_checked[[pkg.globals$argument.CatValue]]))
          
          for (row in seq_len(nrow(rows_being_checked))) {
            row_being_checked <- rows_being_checked[row, ]
            # If cat go check for label and obtain it
            
            # regardless obtain unit and attach
            
            # find var name for this database
            data_variable_being_checked <-
              get_data_variable_name(
                data_name = data_name,
                row_being_checked = row_being_checked,
                variable_being_checked = variable_being_checked,
                data = data
              )
            
            # Recode the variable
            from_values <- list()
            interval <- ""
            if (grepl("\\[*\\]", as.character(row_being_checked[[
              pkg.globals$argument.From]])) ||
              grepl("\\(*\\)", as.character(row_being_checked[[
                pkg.globals$argument.From]]))) {
              from_values <-
                strsplit(as.character(row_being_checked[[
                  pkg.globals$argument.From]]), ",")[[1]]
              from_values[[1]] <- trimws(from_values[[1]])
              from_values[[2]] <- trimws(from_values[[2]])
              interval_left <- substr(from_values[[1]], 1, 1)
              interval_right <- substr(from_values[[2]], nchar(from_values[[2]]), nchar(from_values[[2]]))
              interval <- paste0(interval_left,",",interval_right)
              if(grepl("\\[", from_values[[1]])){
                from_values[[1]] <- gsub("\\[", "", from_values[[1]])
              }
              else{
                from_values[[1]] <- gsub("\\(", "", from_values[[1]])
              }
              if(grepl("\\]", from_values[[2]])){
                from_values[[2]] <- gsub("|]", "", from_values[[2]])
              }
              else{
                from_values[[2]] <- gsub("|)", "", from_values[[2]])
              }
            } 
            else {
              temp_from <-
                as.character(row_being_checked[[pkg.globals$argument.From]])
              from_values[[1]] <- temp_from
              from_values[[2]] <- from_values[[1]]
            }
            value_recorded <-
              as.character(row_being_checked[[pkg.globals$argument.CatValue]])
            if (from_values[[1]] == from_values[[2]]) {
              interval <- "[,]"
            }else if (!interval %in% valid_intervals) {
              message(paste("For variable", variable_being_checked, "invalid interval was passed.\nDefault interval will be used:", interval_default))
              interval <- interval_default
            }
            valid_row_index <- compare_value_based_on_interval(
              compare_columns = data_variable_being_checked,
              data = data,
              left_boundary = from_values[[1]],
              right_boundary = from_values[[2]],
              interval = interval
            )
            
            # Start construction of dataframe for log
            log_table[row, "value_to"] <- value_recorded
            log_table[row, "From"] <-
              as.character(row_being_checked[[pkg.globals$argument.From]])
            log_table[row, "rows_recoded"] <-
              sum(valid_row_index, na.rm = TRUE)
            
            value_recorded <-
              recode_variable_NA_formating(value_recorded, label_list[[
                variable_being_checked]]$type)
            if (is_equal(value_recorded, "copy")) {
              value_recorded <-
                data[valid_row_index, data_variable_being_checked]
            }
            recoded_data[valid_row_index, variable_being_checked] <-
              value_recorded
            if (print_note &&
                !is.null(row_being_checked[[pkg.globals$argument.Notes]]) &&
                !is_equal(row_being_checked[[pkg.globals$argument.Notes]],
                          "") &&
                !is.na(row_being_checked[[pkg.globals$argument.Notes]])) {
              message("NOTE for ", variable_being_checked,
                      ": ",
                      as.character(row_being_checked[[
                        pkg.globals$argument.Notes]]))
            }
          }
          # if log was requested print it
          if (log) {
            message(
              "The variable ",
              data_variable_being_checked,
              " was recoded into ",
              variable_being_checked,
              " for the database ",
              data_name,
              " the following recodes were made: "
            )
            # Reset rowCount to avoid confusion
            rownames(log_table) <- NULL
            print(log_table)
          }
        }
      }
    }
    
    # Process funcVars
    while (nrow(func_variables_to_process) > 0) {
      first_row <- func_variables_to_process[1, ]
      first_row_variable_name <-
        as.character(first_row[[pkg.globals$argument.Variables]])
      # get name of var pass to
      derived_return <-
        recode_derived_variables(
          variable_being_processed = first_row_variable_name,
          recoded_data = recoded_data,
          variables_to_process = func_variables_to_process,
          log = log,
          print_note = print_note,
          else_default = else_default,
          label_list = label_list,
          var_stack = c()
        )
      label_list <- derived_return$label_list
      recoded_data <- derived_return$recoded_data
      func_variables_to_process <-
        derived_return$variables_to_process
    }
    # Populate data Labels
    recoded_data <-
      label_data(label_list = label_list, data_to_label = recoded_data)
    
    return(recoded_data)
  }

#' Compare Value Based On Interval
#'
#' Compare values on the scientific notation interval
#' 
#' @param left_boundary the min value
#' @param right_boundary the max value
#' @param data the data that contains values being compared
#' @param compare_columns The columns inside data being checked
#' @param interval The scientific notation interval
#'
#' @return a boolean vector containing true for rows where the
#' comparison is true
#' @keywords internal
compare_value_based_on_interval <-
  function(left_boundary,
           right_boundary,
           data,
           compare_columns,
           interval) {
    return_boolean <- vector()
    if (suppressWarnings(is.na(as.numeric(left_boundary)))) {
      return_boolean <-
        data[[compare_columns]] %in% data[[
          compare_columns]][
            which(left_boundary == data[[compare_columns]])]
    } else {
      if (interval == "[,]") {
        return_boolean <-
          data[[compare_columns]] %in% data[[
            compare_columns]][which(
              as.numeric(left_boundary) <= data[[compare_columns]] &
                data[[compare_columns]] <= as.numeric(right_boundary)
            )]
      } else if (interval == "[,)") {
        return_boolean <-
          data[[compare_columns]] %in% data[[
            compare_columns]][which(
              as.numeric(left_boundary) <= data[[compare_columns]] &
                data[[compare_columns]] < as.numeric(right_boundary)
            )]
      } else if (interval == "(,]") {
        return_boolean <-
          data[[compare_columns]] %in% data[[
            compare_columns]][which(
              as.numeric(left_boundary) < data[[compare_columns]] &
                data[[compare_columns]] <= as.numeric(right_boundary)
            )]
      } 
      else if (interval == "(,)") {
        return_boolean <-
          data[[compare_columns]] %in% data[[
            compare_columns]][which(
              as.numeric(left_boundary) < data[[compare_columns]] &
                data[[compare_columns]] < as.numeric(right_boundary)
            )]
      }
      else {
        stop("Invalid Argument was passed")
      }
    }
    
    return(return_boolean)
  }

# Parse out variables csv
update_variable_details_based_on_variable_sheet <-
  function(variable_sheet, variable_details) {
    # remove conflicting columns from variable details
    variable_details <-
      variable_details[, !(
        names(variable_details) %in% c(
          pkg.globals$MSW.Variables.Columns.VariableType,
          pkg.globals$MSW.Variables.Columns.Label,
          pkg.globals$MSW.Variables.Columns.LabelLong,
          pkg.globals$MSW.Variables.Columns.Units
        )
      )]
    # Only keep the needed columns
    variable_sheet <-
      variable_sheet[, c(
        pkg.globals$MSW.Variables.Columns.Variable,
        pkg.globals$MSW.Variables.Columns.VariableType,
        pkg.globals$MSW.Variables.Columns.Label,
        pkg.globals$MSW.Variables.Columns.LabelLong,
        pkg.globals$MSW.Variables.Columns.Units
      )]
    # merge the labels and data
    variable_details <-
      merge(
        variable_details,
        variable_sheet,
        by.x = pkg.globals$argument.Variables,
        by.y = pkg.globals$MSW.Variables.Columns.Variable,
        all.x = TRUE
      )
    # remove variables not present in variable_sheet
    variable_details <-
      variable_details[variable_details[[pkg.globals$argument.Variables]] %in%
                         variable_sheet[[
                           pkg.globals$MSW.Variables.Columns.Variable]], ]
    
    return(variable_details)
  }

#' Recode NA formatting
#' 
#' Recodes the NA depending on the var type
#' 
#' @param cell_value The value inside the recTo column
#' @param var_type the toType of a variable
#' 
#' @return an appropriately coded tagged NA
#' 
#' @keywords internal
recode_variable_NA_formating <- function(cell_value, var_type) {
  recode_value <- NULL
  if (grepl("NA", cell_value)) {
    na_value_list <- strsplit(cell_value, ":")[[1]]
    if (is_equal(var_type, pkg.globals$argument.CatType)) {
      recode_value <- paste("NA(", na_value_list[[3]], ")", sep = "")
    } else {
      recode_value <- tagged_na(as.character(na_value_list[[3]]))
    }
  } else {
    if (!is_equal(var_type, pkg.globals$argument.CatType) &&
        !is_equal(cell_value, "copy")) {
      cell_value <- as.numeric(cell_value)
    }
    recode_value <- cell_value
  }
  
  return(recode_value)
}

recode_derived_variables <-
  function(recoded_data,
           variable_being_processed,
           variables_to_process,
           log,
           print_note,
           else_default,
           label_list,
           var_stack) {
    if (nrow(variables_to_process) <= 0) {
      stop(paste(
        variable_being_processed,
        "is missing from variable_details"
      ))
    }
    var_stack <- c(var_stack, variable_being_processed)
    # obtain rows to process and updated variables to Process
    variable_rows <-
      variables_to_process[variables_to_process[[
        pkg.globals$argument.Variables]] == variable_being_processed, ]
    fun_variable_rows <-
      variable_rows[grepl("Func::", variable_rows[[
        pkg.globals$argument.CatValue]]), ]
    variables_to_process <-
      variables_to_process[variables_to_process[[
        pkg.globals$argument.Variables]] != variable_being_processed, ]
    for (row_num in seq_len(nrow(fun_variable_rows))) {
      # Check for presence of feeder variables in data and in the
      # variable being processed stack
      feeder_vars <-
        as.list(strsplit(as.character(variable_rows[row_num, ][[
          pkg.globals$argument.VariableStart]]), "::"))[[1]][[2]]
      feeder_vars <- gsub("\\[|\\]", "", feeder_vars)
      feeder_vars <- as.list(strsplit(feeder_vars, ","))[[1]]
      feeder_vars <- sapply(feeder_vars, trimws)
      used_feeder_vars <- feeder_vars
      feeder_vars <- setdiff(feeder_vars, names(recoded_data))
      
      # Check if the variable has a function to recode
      non_func_missing_variables <-
        setdiff(feeder_vars, unique(as.character(variables_to_process[[
          pkg.globals$argument.Variables]])))
      if (length(non_func_missing_variables) > 0) {
        warning(
          paste(
            variable_being_processed,
            "could not be derived because",
            feeder_vars,
            "was never specified nor is it a function variable,
            therefore it was not recoded \n"
          )
        )
        var_stack <-
          var_stack[!(var_stack == variable_being_processed)]
        
        return(
          list(
            var_stack = var_stack,
            label_list = label_list,
            recoded_data = recoded_data,
            variables_to_process = variables_to_process
          )
        )
      }
      # Check for presense in var_stack
      if (length(intersect(feeder_vars, var_stack)) > 0) {
        conflict_vars <- intersect(feeder_vars, var_stack)
        stop(
          paste(
            conflict_vars,
            "is required to create",
            variable_being_processed,
            "but",
            variable_being_processed,
            "is needed to create",
            conflict_vars
          )
        )
      }
      
      # Update var_stack and recurse to get the feeder vars
      for (one_feeder in feeder_vars) {
        # Need to check recoded data again in case a recursion added it
        if (!one_feeder %in% names(recoded_data)) {
          derived_return <-
            recode_derived_variables(
              variable_being_processed = one_feeder,
              recoded_data = recoded_data,
              variables_to_process = variables_to_process,
              log = log,
              print_note = print_note,
              else_default = else_default,
              label_list = label_list,
              var_stack = var_stack
            )
          var_stack <- derived_return$var_stack
          label_list <- derived_return$label_list
          recoded_data <- derived_return$recoded_data
          variables_to_process <-
            derived_return$variables_to_process
        }
      }
      
      # Obtain the function for each row
      row_being_checked <- variable_rows[row_num, ]
      func_cell <-
        as.character(row_being_checked[[pkg.globals$argument.CatValue]])
      function_being_used <-
        as.list(strsplit(func_cell, "::"))[[1]][[2]]
      
      column_value <-
        recoded_data %>%
        rowwise() %>%
        select(used_feeder_vars) %>%
        do(
          column_being_added = calculate_custom_function_row_value(
            .,
            variable_names = used_feeder_vars,
            custom_function_name = function_being_used
          )
        )
      # Set type of var
      if (as.character(row_being_checked[[pkg.globals$argument.ToType]]) !=
          pkg.globals$argument.CatType) {
        column_value <- as.numeric(unlist(column_value[["column_being_added"]]))
      }else{
        column_value <- as.factor(unlist(column_value[["column_being_added"]]))
      }
      recoded_data[[variable_being_processed]] <-
        column_value
      
      var_stack <-
        var_stack[!(var_stack == variable_being_processed)]
    }
    
    label_list[[as.character(variable_being_processed)]] <-
      assign(variable_being_processed, create_label_list_element(variable_rows))
    
    return(
      list(
        var_stack = var_stack,
        label_list = label_list,
        recoded_data = recoded_data,
        variables_to_process = variables_to_process
      )
    )
  }
calculate_custom_function_row_value <-
  function(row_values,
           variable_names,
           custom_function_name) {
    row_values <- unname(row_values)
    custom_function_return_value <-
      do.call(get(custom_function_name), row_values)
    
    return(custom_function_return_value)
  }