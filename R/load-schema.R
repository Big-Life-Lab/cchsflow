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

#' Load the CCHS missing-data pattern schema
#'
#' @description Loads the machine-actionable CCHS missing-data pattern
#'   definitions (inst/metadata/schemas/cchs/cchs_missing_data.yaml): the
#'   single/double/triple-digit code families with their decimal-era
#'   variants and the not-applicable-over-missing priority hierarchy.
#'
#'   Consumers read the normative blocks only (`pattern_definitions` and
#'   `transformation_rules$na_category_definitions`); the file's
#'   variable-level assignments are reference documentation -- the
#'   per-variable source of truth remains variable_details.csv.
#'
#' @return Named list parsed from the YAML schema.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' schema <- load_cchs_missing_data()
#' names(schema$pattern_definitions$patterns)
#' }
load_cchs_missing_data <- function() {
  schema_path <- system.file(
    "metadata", "schemas", "cchs", "cchs_missing_data.yaml",
    package = "cchsflow",
    mustWork = TRUE
  )

  tryCatch(
    yaml::read_yaml(schema_path),
    error = function(e) {
      stop("Failed to load the CCHS missing-data schema at ", schema_path,
           ": ", e$message)
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
