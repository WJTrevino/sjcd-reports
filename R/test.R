test <- function(data) {
  tbl <- tidyr::as_tibble(jsonlite::fromJSON(data))
  print(tbl)
}