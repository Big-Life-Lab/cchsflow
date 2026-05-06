#' Load schema configuration from YAML
#'
#' @description Loads the YAML schema configuration for a given file type.
#'   The schema contains the expected column order and other metadata used
#'   for validating CSV worksheets.
#'
#' @param file_type Either "variables" or "variable_details"
#'
#' @return List containing schema configuration:
#'   \itemize{
#'     \item expected_column_order: Character vector of column names in expected order
#'     \item id_column_name: Name of the ID column used for row sorting, or NULL
#'       if not defined in the schema (present in variables.yaml, absent in
#'       variable_details.yaml)
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' schema <- load_schema("variables")
#' schema$expected_column_order
#' schema$id_column_name
#' }
load_schema <- function(file_type) {
  file_type <- match.arg(file_type, c("variables", "variable_details"))

  schema_file <- paste0(file_type, ".yaml")
  schema_path <- system.file(
    "metadata", "schemas", "core", schema_file,
    package = "cchsflow",
    mustWork = TRUE
  )

  tryCatch(
    yaml::read_yaml(schema_path),
    error = function(e) {
      stop("Failed to load schema for '", file_type,
           "'. The schema file at ", schema_path, " may be corrupted: ",
           e$message)
    }
  )
}
