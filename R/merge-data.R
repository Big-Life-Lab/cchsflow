merge_data <- function(data, ...) {
  # Step 1: bind datasets
  new_data <- bind_rows(data, ...)
  
  # Step 2: apply NA(c) to untagged NA's
  for (variable in new_data$columns) {
    NA_index <- is.na(new_data[[variable]])
    tagged_NA_index <- haven::is_tagged_na(new_data[[variable]])
    true_NA_index <- !(NA_index == tagged_NA_index)
    if (is.numeric(new_data[[variable]])) {
      new_data[true_NA_index, variable] <-
        "NA(c)"
    } else {
      if (!"NA(c)" %in% levels(new_data[[variable]])) {
        levels(new_data[[variable]]) <-
          c(levels(new_data[[variable]]),
            "NA(c)")
      }
      new_data[true_NA_index, variable] <-
        "NA(c)"
    }
  }
  
  # Step 3: apply labels to combined dataset
  labelled_data <- set_data_labels(new_data, variable_details,
                                   variables)
  return(labelled_data)
}