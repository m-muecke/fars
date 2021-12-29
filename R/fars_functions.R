#' Load US National Highway Traffic data
#'
#' \code{fars_read} loads-in the US National Highway Traffic Safety
#' Administration's Fatality Analysis Reporting System data from a provided csv
#' file. \code{fars_read} will throw an error if the provided file does not
#' exist.
#'
#' @param filename A character string providing the file path of the csv to load
#'
#' @return This function returns a tibble containing the US National Highway
#'   Traffic Safety Administration's Fatality Analysis Reporting System data.
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @export
#'
#' @examples
#' \dontrun{
#' df <- fars_read("accident_2013.csv.bz2")
#' }
fars_read <- function(filename) {
  if (!file.exists(filename)) {
    stop("file '", filename, "' does not exist")
  }
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Create a file name for a specific year
#'
#' \code{make_filename} creates a file name in the accident data format for the
#' specific input year.
#'
#' @param year A numeric scalar. The year being used for the file name.
#'
#' @return A character string. The created file name.
#' @export
#'
#' @examples
#' make_filename(2013)
#' make_filename(2014)
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read-in fars data (Fatality Analysis Reporting System)
#'
#' \code{fars_read_years} reads-in fars (Fatality Analysis Reporting System)
#' data for specified years.
#'
#' @param years A numeric vector. The years of the traffic data.
#'
#' @return A list of tibbles containing the traffic data for each year.
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select
#' @export
#' @section Warning:
#' If an invalid year has been given an error will be thrown and \code{NULL}
#'   returned.
#'
#' @examples
#' \dontrun{
#' fars_read_years(2014)
#' fars_read_years(c(2013, 2014))
#' }
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch(
      {
        dat <- fars_read(file)
        dplyr::mutate(dat, year = year) %>%
          dplyr::select(MONTH, year)
      },
      error = function(e) {
        warning("invalid year: ", year)
        return(NULL)
      }
    )
  })
}

#' Summarizes yearly fars data
#'
#' \code{fars_summarize_years} loads-in fars data file for the specified years
#' and joins them to one tibble.
#'
#' @param years A numeric vector. The years of the traffic data.
#'
#' @return A tibble withe fars data, each year represented as a column.
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @export
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2014)
#' fars_summarize_years(c(2013, 2014, 2015))
#' }
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plot accidents in a state
#'
#' \code{fars_map_state} plots the accidents in a state for a given year. The
#' state is plotted as a map and the accidents as points on the map. Throws an
#' error if the state number does not exist in the data and NULL if the
#' accidents are zero for the given year in the specified state.
#'
#' @param state.num A numeric scalar. The state number of the accidents.
#' @param year A numeric scalar. The year of the accidents.
#'
#' @return NULL
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @export
#'
#' @examples
#' \dontrun{
#' fars_map_state(1, 2014)
#' fars_map_state(5, 2014)
#' }
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if (!(state.num %in% unique(data$STATE))) {
    stop("invalid STATE number: ", state.num)
  }
  data.sub <- dplyr::filter(data, STATE == state.num)
  if (nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state",
      ylim = range(LATITUDE, na.rm = TRUE),
      xlim = range(LONGITUD, na.rm = TRUE)
    )
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
