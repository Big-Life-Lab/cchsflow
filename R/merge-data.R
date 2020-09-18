merge_data <- function(...) {
  # Step 1: bind datasets
  new_data <- bind_rows(...)
  
  # Step 2: apply NA(c) to untagged NA's
  for (i in names(new_data)) {
    NA_index <- is.na(new_data[[i]])
    tagged_NA_index <- haven::is_tagged_na(new_data[[i]])
    true_NA_index <- !(NA_index == tagged_NA_index)
    if (is.numeric(new_data[[i]])) {
      new_data[true_NA_index, i] <-
        "NA(c)"
    } else {
      if (!"NA(c)" %in% levels(new_data[[i]])) {
        levels(new_data[[i]]) <-
          c(levels(new_data[[i]]),
            "NA(c)")
      }
      new_data[true_NA_index, i] <-
        "NA(c)"
    }
  }
  # Step 3: apply labels to combined dataset
  labelled_data <- set_data_labels(new_data, variable_details,
                                   variables)
  return(labelled_data)
}