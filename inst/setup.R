# Generate a test DB with synthetic omop data
testdb <- NULL
my_directory <- system.file(package = 'quantvariabledistribution')
my_file_folder <- system.file('extdata', package = 'quantvariabledistribution')
# Function to generate omop test db
mk_testdb_omop <- function(){
  if (! is.null(testdb)) return(testdb)

  # Create an ephemeral in-memory RSQLite database
  testdb <<- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  # Read all csv files in testdata folder
  col_type_mapping <- list(
    'drug_exposure' = 'iiiii',
    'person' = 'i',
    'visit_occurrence' = 'iiii')
  for (file_name in list.files(path=system.file('extdata',
                                                package = 'quantvariabledistribution'),
                               pattern = "\\.csv$")) {
    # Get table_name from csv file_name without extension
    table_name <- sub('\\.csv$', '', file_name)
    tbl <- readr::read_csv(
      file = file.path(system.file('extdata', package = 'quantvariabledistribution'),
                       file_name),
      col_types = col_type_mapping[[table_name]],
      show_col_types = FALSE
    )
    # Write to db
    DBI::dbWriteTable(testdb, table_name, tbl)
  }
  testdb
}
