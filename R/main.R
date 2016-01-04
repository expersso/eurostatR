base_url <- "http://ec.europa.eu/eurostat/SDMX/diss-web/rest/"

#' Retrieve a data frame of all Eurostat data sets
#'
#' @param lang The language for variable descriptions ('en', 'de', or 'fr')
#'
#' @return A dataframe
#' @export
#'
#' @examples
#' df <- get_dataflows()
#' head(df)
#' df[grep("doctorate", df$description),]
get_dataflows <- function(lang = "en") {

  query_url <- paste0(base_url, "dataflow/ESTAT/all/latest")

  req <- make_request(query_url, "metadata")
  res <- xml2::read_xml(httr::content(req, "text"), verbose = TRUE)
  estat_ns <- xml2::xml_ns(res) # xml namespace

  data_flows_nodes <- xml2::xml_find_all(res, "//str:Dataflow", estat_ns)
  name_nodes <- xml2::xml_find_all(res,
                sprintf("//str:Dataflow//com:Name[@xml:lang='%s']", lang),
                estat_ns)

  id <- xml2::xml_attr(data_flows_nodes, "id", estat_ns)
  description <- xml2::xml_text(name_nodes, trim = TRUE)

  df <- data.frame(id, description, stringsAsFactors = FALSE)
  structure(df, class = c("tbl_df", "tbl", "data.frame"))
}

#' Retrieve data from the Eurostat API
#'
#' @param key A character string identifying the series to be retrieved
#' @param filter A named list with additional filters (see \code{details})
#'
#' @details
#' The \code{filter} option of \code{get_data()} takes a named list of key-value
#' pairs. If left blank, it returns all data for the current version.
#'
#' Available filter parameters:
#'
#' \itemize{
#' \item \code{startPeriod} & \code{endPeriod}
#'  \itemize{
#'    \item \code{YYYY} for annual data (e.g.: 2013)
#'    \item \code{YYYY-S[1-2]} for semi-annual data (e.g.: 2013-S1)
#'    \item \code{YYYY-Q[1-4]} for quarterly data (e.g.: 2013-Q1)
#'    \item \code{YYYY-MM} for monthly data (e.g.: 2013-01)
#'    \item \code{YYYY-W[01-53]} for weekly data (e.g.: 2013-W01)
#'    \item \code{YYYY-MM-DD} for daily data (e.g.: 2013-01-01)
#'    }
#'  }
#'
#' @return A data frame
#' @export
#'
#' @examples
#' # Get data on doctorates in US and DE
#' df <- get_data("cdh_e_occ1.A.TOTAL.PC..DE+US")
#' head(df)
get_data <- function(key, filter = NULL) {

  query_url <- create_query_url(key, filter = filter)

  req <- make_request(query_url, "data")

  tmp <- tempfile()
  writeLines(httr::content(req, "text"), tmp)

  result <- rsdmx::readSDMX(tmp, FALSE)

  unlink(tmp)

  df <- as.data.frame(result)
  df <- structure(df,
                  class = c("tbl_df", "tbl", "data.frame"),
                  names = tolower(names(df)))
  df
}

#' Retrieve dimensions of Eurostat series
#'
#' @param id A character string identifying the series to be retrieved
#'
#' @return A list of data frames, one for each series retrieved
#' @export
#'
#' @examples
#' dims <- get_dimensions("cdh_e_occ1")
#' str(dims)
get_dimensions <- function(id) {

  query_url <- paste0(base_url, "datastructure/ESTAT/DSD_", id)

  resp <- rsdmx::readSDMX(query_url)

  dfs <- lapply(resp@codelists@codelists, function(x) {

    data.frame(
      id = vapply(x@Code, slot, "id", FUN.VALUE = character(1)),
      description = vapply(x@Code, function(x) slot(x, "label")$en, character(1)),
      stringsAsFactors = FALSE
      )
  })

  name <- vapply(resp@codelists@codelists, slot, "id", FUN.VALUE = character(1))

  names(dfs) <- substring(name, 4)
  dfs_list <- list()

  dfs_list$dimensions <- dfs[grep("OBS_", names(dfs), invert = TRUE)]
  dfs_list$other <- dfs[grep("OBS_", names(dfs))]

  dims <- vapply(resp@datastructures@datastructures[[1]]@Components@Dimensions,
                 slot, "conceptRef", FUN.VALUE = character(1))

  key <- paste(id, paste(dims, collapse = "."), sep = ".")

  dfs_list$key <- key
  dfs_list
}

#' Format date variable into a proper date variable
#'
#' @param x A vector of dates
#'
#' @return A date-formatted vector
#' @export
convert_dates <- function(x) {

  if(grepl("^[0-9]{4}$", x[1])) {
    return(as.Date(paste0(x, "-01-01"), "%Y-%m-%d"))
  }

  if(grepl("^[0-9]{4}-[0-9]{2}$", x[1])) {
    return(as.Date(paste0(x, "-01"), "%Y-%m-%d"))
  }

  if(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x[1])) {
    return(as.Date(x, "%Y-%m-%d"))
  }
}

create_query_url <- function(key, filter = NULL) {

  url <- paste0(base_url, "data")

  # Get flow reference (= dataset abbreviation, e.g. ICP or BOP)
  flow_ref <- extract_flowref(key)
  key_q <- regmatches(key, regexpr("^[[:alnum:]_]+\\.", key),
                      invert = TRUE)[[1]][2]

  if(any(names(filter) == "")) {
    stop("All filter parameters must be named!")
  }

  if("updatedAfter" %in% names(filter)) {
    filter$updatedAfter <- curl::curl_escape(filter$updatedAfter)
  }

  # Create parameter part of query string
  if(!is.null(filter)) {
    names <- curl::curl_escape(names(filter))
    values <- curl::curl_escape(as.character(filter))
    query <- paste0(names, "=", values, collapse = "&")
    query <- paste0("?", query)
  } else {
    query <- ""
  }

  query_url <- paste(url, flow_ref, key_q, query, sep = "/")
  query_url
}

check_status <- function(req) {
  if(req$status_code >= 400)
    stop("HTTP failure: ", req$status_code, "\n", httr::content(req, "text"))
}

make_request <- function(query_url, header_type) {

  accept_headers <-
    c("metadata" = "application/vnd.sdmx.genericdata+xml;version=2.1",
      "data" = "application/vnd.sdmx.structurespecificdata+xml;version=2.1")

  req <- httr::GET(query_url, httr::add_headers(
    "Accept" = accept_headers[header_type],
    "Accept-Encoding" = "gzip, deflate"))

  check_status(req)
  req
}

extract_flowref <- function(key) {
  regmatches(key, regexpr("^[[:alnum:]_]+", key))
}

# parse_too_large_request <- function() {
#   url <- ""
#   tmp_dir <- tempdir()
#   tmp_file <- tempfile(tmpdir = tmp_dir, fileext = ".zip")
#   download.file(url, tmp_file, mode = "wb")
#   unzipped_file <- unzip(tmp_file, list = TRUE)$Name
#   unzip(tmp_file, exdir = tmp_dir)
#   file <- paste(tmp_dir, unzipped_file, sep = "\\")
#
#   resp <- rsdmx::readSDMX(file, FALSE)
#   df <- as.data.frame(resp)
#
# }