#' Sends a call to the Kvalitetsindikatorkatalog API to retrieve information
#' about the specified measure ID
#'
#' @param measure_id the measure ID to search for
#'
#' @return the first result of the API call
search_measure_id <- function(measure_id) {

  res <- httr::POST(
    url = "https://kvalitetsindikatorkatalog.se/services/api/1.0/measureversions/searchlatest/withparams",
    config = httr::add_headers("Content-Type" = "application/json"),
    body = jsonlite::toJSON(
      list(
        searchValue = measure_id,
        index = 0,
        expiresBeforeNrOfMonths = NA,
        keywords = I(character()),
        measureOwner = "",
        measurementSource = "",
        measuringFrequencies = I(character()),
        organizationalLevels = I(character()),
        pageSize = 20,
        reportingSystem = "",
        sortDirection = "ASCENDING",
        sortfield = "TITLE",
        status = I(character(0)),
        textSearchSubset = "ALL",
        validFrom = NA,
        validTo = NA
      ),
      auto_unbox = TRUE
    ),
    encode = "json"
  ) |>
    httr::content()

  if (length(res$pageResults) == 0) {
    return(NULL)
  } else {
    return(res$pageResults[[1]])
  }

}

#' Gets the specified measure frequencies from a measure config
#'
#' @param measure_config the measure config from which to extract the measure
#' frequencies, the result of [search_measure_id()]
#'
#' @return vector of measure frequencies
get_measuring_frequency <- function(measure_config) {

  sapply(
    measure_config$measuringFrequencies,
    \(x) x$name
  )

}

#' Gets the specified measure organizational levels from a measure config
#'
#' @param measure_config the measure config from which to extract the measure
#' organizational levels, the result of [search_measure_id()]
#'
#' @return vector of measure organizational levels
get_organization_level <- function(measure_config) {

  sapply(
    measure_config$organizationalLevels,
    \(x) x$name
  )

}

#' Gets a minimal representation of a measure config for a specific measure ID
#'
#' @param measure_id the measure ID to search for
#'
#' @return a list containing the version, measure frequencies, organizational
#' levels, title, and status
#' @export
get_measure_config <- function(measure_id) {

  checkmate::assert_string(
    measure_id,
    pattern = "[a-z0-9]+-[a-z0-9]+-[a-z0-9]+-[a-z0-9]+-[a-z0-9]+"
  )

  cfg <- search_measure_id(measure_id)

  if (!is.null(cfg)) {
    cfg_freq <- get_measuring_frequency(cfg)
    cfg_org <- get_organization_level(cfg)
    return(
      list(
        version = cfg$versionNr,
        freq = cfg_freq,
        org = cfg_org,
        title = cfg$title,
        status = cfg$status,
        valid_from = as.POSIXct(
          cfg$validFrom / 1000,
          origin = "1970-01-01",
          tz = "Europe/Berlin"
        ),
        type = cfg$unitType
      )
    )
  } else {
    return(NULL)
  }

}

#' Aggregate a VIS indicator
#'
#' Creates an aggregated indicator ready for VIS with appropiate variable names
#' and metadata
#'
#' @details
#' The config defining the indicator is loaded automatically from the
#' Kvalitetsindikatorkatalog API ensuring the indicators are always up-to-date.
#'
#' Note that only the reported periods and organizational levels as well as
#' historic indicator validity is dynamically applied. The actual definition
#' of the indicator can change in the config and should be then changed in
#' `indicator_function` to reflect this.
#'
#' It's important to note that all relevant filtering on, for example, gender,
#' and care type has to be performed before passing `df` to this function.
#'
#' The `indicator_function` has to accept the following arguments `group_cols`,
#' `obfuscate_data`, `aggregate`, `marginal_cols` and is almost surely defined
#' in the [rcdm::rcdm] package.
#'
#' @param df a data.frame
#' @param measure_id the measure ID
#' @param indicator_function the function responsible for defining and
#' aggregating the indicator
#' @param date_var the name of the date variable
#' @param unit_var the name of the unit variable
#' @param county_var the name of the county variable
#' @param register_id the register ID
#'
#' @return an aggregated data.frame with VIS appropriate names and metadata
#' @export
aggregate_vis <- function(df,
                          measure_id,
                          indicator_function,
                          date_var,
                          unit_var,
                          county_var,
                          register_id) {

  cfg <- get_measure_config(measure_id)

  if (is.null(cfg)) {
    stop(
      sprintf("No config found for measure ID %s", measure_id)
    )
  }

  if (cfg$status == "UNPUBLISHED") {
    rlang::warn(sprintf("MeasureID '%s' is unpublished, skipping", measure_id))
    return(NULL)
  }

  checkmate::assert_function(
    indicator_function,
    args = c(
      "group_cols",
      "obfuscate_data",
      "aggregate",
      "marginal_cols"
    )
  )

  res <- df |>
    dplyr::filter(
      dplyr::if_any(dplyr::all_of(date_var)) >= as.Date(cfg$valid_from)
    ) |>
    add_groups_long(
      purrr::keep_at(cfg, c("freq", "org")),
      date_var,
      unit_var,
      county_var
    ) |>
    indicator_function(
      obfuscate_data = TRUE,
      aggregate = TRUE,
      marginal_cols = NULL,
      group_cols = c(
        "PeriodReportedStartDate",
        "PeriodReportedEndDate",
        "unit",
        "county",
        "unit_type"
      )
    ) |>
    postprocess_indicator(cfg, register_id) |>
    dplyr::mutate(MeasureID = measure_id)

  return(res)

}


#' Adds appropriate groups in long format
#'
#' @param df a data.frame
#' @param cfg the measure config, the result of [get_measure_config()]
#' @param date_var the name of the date variable
#' @param unit_var the name of the unit variable
#' @param county_var the name of county variable
#' @param type the type of date or organizational level to be added
#'
#' @return a data.frame with extra rows added
add_groups_long <- function(df, cfg, date_var, unit_var, county_var) {

  df <- df |>
    dplyr::mutate(dplyr::across(unit_var, as.character))

  out <- dplyr::bind_rows(
    purrr::map(
      cfg$freq,
      \(x) add_groups_long_date(df, date_var, x)
    )
  )

  out <- dplyr::bind_rows(
    purrr::map(
      cfg$org,
      \(x) add_groups_long_organization(out, unit_var, county_var, x)
    )
  )

  return(out)
}

#' @describeIn add_groups_long adds long date groups
add_groups_long_date <- function(df, date_var, type) {

  checkmate::assert_choice(
    type,
    c("Kvartal", "\u00c5r", "M\u00e5nad", "Halv\u00e5r")
  )

  lubridate_type <- dplyr::case_match(
    type,
    "Kvartal" ~ "quarter",
    "\u00c5r" ~ "year",
    "M\u00e5nad" ~ "month",
    "Halv\u00e5r" ~ "halfyear"
  )


  df |>
    dplyr::mutate(
      PeriodReportedStartDate = lubridate::floor_date(
        .data[[date_var]],
        lubridate_type
      ),
      PeriodReportedEndDate = lubridate::ceiling_date(
        .data[[date_var]],
        lubridate_type
      ) - 1
    ) |>
    dplyr::filter(.data$PeriodReportedEndDate < lubridate::today())
}

#' @describeIn add_groups_long adds long organization (unit and county) groups
add_groups_long_organization <- function(df, unit_var, county_var, type) {

  checkmate::assert_choice(
    type,
    c(
      "Riksniv\u00e5",
      "L\u00e4nsniv\u00e5",
      "Mottagningsniv\u00e5",
      "Region",
      "V\u00e5rdenhet"
    )
  )

  if (type == "Riksniv\u00e5") {
    df |>
      dplyr::mutate(
        unit = "Riket",
        unit_type = "country"
      )
  } else if (type %in% c("L\u00e4nsniv\u00e5", "Region")) {
    df |>
      dplyr::mutate(
        unit = .data[[county_var]],
        unit_type = "county"
      )
  } else if (type %in% c("Mottagningsniv\u00e5", "V\u00e5rdenhet")) {
    df |>
      dplyr::mutate(
        unit = as.character(.data[[unit_var]]),
        unit_type = "unit",
        county = .data[[county_var]]
      )
  }

}

#' Handles the necessary postprocessing of an aggregated indicator by renaming
#' and adding necessary variables as well as adding the version
#'
#' @param df an aggregated indicator
#' @param cfg the measure config, the result of [get_measure_config()]
#' @param register_id the register ID
#'
#' @return renamed data.frame
postprocess_indicator <- function(df, cfg, register_id) {

  checkmate::assert_choice(cfg$type, c("Andel", "Dagar", "Median", "Antal"))

  if (cfg$type == "Andel") {
    df <- postprocess_indicator_prop(df)
  } else if (cfg$type %in% c("Dagar", "Median")) {
    df <- postprocess_indicator_median(df)
  } else if (cfg$type == "Antal") {
    df <- postprocess_indicator_count(df)
  }

  df |>
    postprocess_indicator_org(register_id) |>
    postprocess_indicator_register(register_id) |>
    dplyr::mutate(Version = cfg$version)
}

#' @describeIn postprocess_indicator renames columns if the indicator is a
#' proportion
postprocess_indicator_prop <- function(df) {

  if (any(stringr::str_detect(names(df), "_total_non_missing$"))) {
    df |>
      dplyr::rename(
        Rate = dplyr::ends_with("_prop"),
        Denominator = dplyr::ends_with("_n"),
        Numerator = dplyr::ends_with("_total_non_missing")
      )
  } else {
    df |>
      dplyr::rename(
        Rate = dplyr::ends_with("_prop"),
        Denominator = dplyr::ends_with("_n"),
        Numerator = dplyr::all_of("total")
      )
  }
}

#' @describeIn postprocess_indicator renames columns if the indicator is a
#' median
postprocess_indicator_median <- function(df) {

  if (any(stringr::str_detect(names(df), "_total_non_missing$"))) {
    df |>
      dplyr::rename(
        Value = dplyr::ends_with("_median"),
        Measurepopulation = dplyr::ends_with("_total_non_missing")
      )
  } else {
    df |>
      dplyr::rename(
        Value = dplyr::ends_with("_median"),
        Measurepopulation = dplyr::all_of("total")
      )
  }
}

#' @describeIn postprocess_indicator renames columns if the indicator is a
#' count
postprocess_indicator_count <- function(df) {

  if (all(c("n", "total") %in% names(df))) {
    stop("A count indicator should only contain one of 'n' and 'total")
  }

  df |>
    dplyr::rename(
      Value = dplyr::any_of(c("n", "total"))
    )
}

#' @describeIn postprocess_indicator renames county and unit variables
postprocess_indicator_org <- function(df, register_id) {

  units <- api_register_units(register_id)
  counties <- api_domain(3003)

  df |>
    dplyr::mutate(
      RegionOrganisationId = dplyr::case_when(
        .data$unit_type == "county" ~ .data$unit,
        .data$unit_type == "unit" ~ .data$county,
        .default = NA
      ),
      UnitName = dplyr::case_when(
        .data$unit_type == "unit" ~ .data$unit,
        .default = NA
      )
    ) |>
    dplyr::left_join(
      units |>
        dplyr::mutate(UnitCode = as.character(.data$UnitCode)) |>
        dplyr::select("UnitCode", name = "UnitName", UnitHSAID = "HSAID"),
      by = dplyr::join_by("UnitName" == "UnitCode")
    ) |>
    dplyr::mutate(UnitName = .data$name) |>
    dplyr::select(-"name") |>
    dplyr::left_join(
      counties |>
        dplyr::select("ValueCode", "ValueName"),
      by = dplyr::join_by("RegionOrganisationId" == "ValueCode")
    ) |>
    dplyr::rename(RegionName = "ValueName") |>
    dplyr::select(-"unit", -"county", -"unit_type")
}

#' @describeIn postprocess_indicator adds the register name and HSAID variables
postprocess_indicator_register <- function(df, register_id) {

  register <- api_register(register_id)

  df |>
    dplyr::mutate(
      RegisterNamn = register$data$RegisterName,
      RegisterHSAID = register$data$HSAID %||% NA
    )
}
