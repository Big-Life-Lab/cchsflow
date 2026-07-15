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

#' Load the database-token registry
#'
#' @description Loads the registry of valid CCHS database identifiers used to
#'   validate databaseStart tokens in the worksheets. The registry file name
#'   comes from the worksheet schema's `database_registry_file` key.
#'
#' @param registry_file File name of the registry YAML (default
#'   "database_registry.yaml"), resolved inside the package's
#'   metadata/schemas/core directory.
#'
#' @return Character vector of valid database identifiers.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_database_registry()
#' }
load_database_registry <- function(registry_file = "database_registry.yaml") {
  registry_path <- system.file(
    "metadata", "schemas", "core", registry_file,
    package = "cchsflow",
    mustWork = TRUE
  )

  registry <- tryCatch(
    yaml::read_yaml(registry_path),
    error = function(e) {
      stop("Failed to load database registry. The file at ", registry_path,
           " may be corrupted: ", e$message)
    }
  )

  if (is.null(registry$valid_databases) ||
      length(registry$valid_databases) == 0) {
    stop("Database registry at ", registry_path,
         " has no 'valid_databases' entries.")
  }

  as.character(registry$valid_databases)
}
