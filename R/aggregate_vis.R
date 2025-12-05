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

    res <- res$pageResults[[1]]
    res$submeasures <- search_submeasures(res$id)

    return(res)
  }

}

#' Search for submeasures related to a measure
#'
#' @param id the key `id` as returned by [search_measure_id], separate from a
#' MeasureID
#'
#' @return the result of the API call
search_submeasures <- function(id) {
  url <- sprintf(
    "https://kvalitetsindikatorkatalog.se/services/api/1.0/measureversions/%s/submeasures/",
    id
  )

  res <- httr::GET(
    url = url,
    config = httr::add_headers("Content-Type" = "application/json"),
    encode = "json"
  ) |>
    httr::content()

  res
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

#' Gets the specified measure submeasures from a measure config
#'
#' @param measure_config the measure config from which to extract the
#' submeasures, the result of [search_measure_id()]
#'
#' @return list of gender codes and submeasure IDs
get_submeasures <- function(measure_config) {
  purrr::map(
    measure_config$submeasures,
    ~ list(gender_code = .x$gender$codeValue, measure_id = .x$id)
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
    pattern = "[a-z0-9]+-[a-z0-9]+-[a-z0-9]+-[a-z0-9]+-[a-z0-9]+|\\d{5}"
  )

  cfg <- search_measure_id(measure_id)

  if (!is.null(cfg)) {
    cfg_freq <- get_measuring_frequency(cfg)
    cfg_org <- get_organization_level(cfg)
    cfg_sub <- get_submeasures(cfg)
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
        type = cfg$unitType,
        submeasures = cfg_sub
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
#' @param gender_var the name of the gender variable, if `NULL` the variable
#' `SubjectKey` must be present in `df`
#'
#' @return an aggregated data.frame with VIS appropriate names and metadata
#' @export
aggregate_vis <- function(df,
                          measure_id,
                          indicator_function,
                          date_var,
                          unit_var,
                          county_var,
                          register_id,
                          gender_var = NULL) {

  cfg <- get_measure_config(measure_id)

  if (is.null(cfg)) {
    rlang::warn(
      sprintf("No config found for MeasureID '%s', skipping", measure_id)
    )
    return(NULL)
  }

  if (cfg$status == "UNPUBLISHED") {
    rlang::warn(sprintf("MeasureID '%s' is unpublished, skipping", measure_id))
    return(NULL)
  }

  if (length(cfg$submeasures) > 0) {
    if (is.null(gender_var) && !"SubjectKey" %in% names(df)) {
      stop("gender_var can not be NULL if SubjectKey is not present in df")
    }
  }

  checkmate::assert_function(
    indicator_function,
    args = c(
      "df",
      "group_cols",
      "obfuscate_data",
      "aggregate",
      "marginal_cols",
      "add_reason_col",
      "censored_value"
    )
  )

  df <- df |>
    dplyr::filter(
      dplyr::if_any(dplyr::all_of(date_var)) >= as.Date(cfg$valid_from)
    ) |>
    add_groups_long(
      purrr::keep_at(cfg, c("freq", "org", "submeasures")),
      date_var,
      unit_var,
      county_var,
      gender_var
    )

  res <- indicator_function(
    df = df,
    obfuscate_data = TRUE,
    aggregate = TRUE,
    marginal_cols = NULL,
    add_reason_col = TRUE,
    group_cols = c(
      "PeriodReportedStartDate",
      "PeriodReportedEndDate",
      "unit",
      "county",
      "unit_type",
      "gender"
    ),
    censored_value = NA
  ) |>
    postprocess_indicator(cfg, register_id, measure_id)

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
add_groups_long <- function(df,
                            cfg,
                            date_var,
                            unit_var,
                            county_var,
                            gender_var) {

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
  ) |>
    add_groups_long_gender(cfg$submeasures, gender_var)

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
      "V\u00e5rdenhet",
      "H\u00e4lso-och sjukv\u00e5rdsregion"
    )
  )

  if (type == "Riksniv\u00e5") {
    df |>
      dplyr::mutate(
        unit = "Riket",
        unit_type = "country",
        county = "Riket"
      )
  } else if (type %in% c("L\u00e4nsniv\u00e5", "Region")) {
    df |>
      dplyr::mutate(
        unit = .data[[county_var]],
        unit_type = "county",
        county = .data[[county_var]]
      )
  } else if (type %in% c("Mottagningsniv\u00e5", "V\u00e5rdenhet")) {
    df |>
      dplyr::mutate(
        unit = as.character(.data[[unit_var]]),
        unit_type = "unit",
        county = .data[[county_var]]
      )
  } else if (type == "H\u00e4lso-och sjukv\u00e5rdsregion") {
    cli::cli_alert_warning(
      sprintf("Type '%s' is not yet supported, ignoring", type)
    )
    NULL
  }

}

#' Adds a variable called `gender` to a data.frame
#'
#' @param df the data.frame
#' @param gender_var the optional name of the gender variable
#'
#' @return a data.frame with the variable `gender`
add_gender <- function(df, gender_var) {
  if (is.null(gender_var)) {
    df <- dplyr::mutate(df, gender = RCStat::gender(.data$SubjectKey))
  } else {
    df <- dplyr::mutate(df, gender = .data[[gender_var]])
  }
  dplyr::mutate(df, gender = as.character(.data$gender))
}

#' @describeIn add_groups_long adds long gender groups
#' @param submeasures the submeasures key from [get_measure_config]
#' @param gender_var the name of the gender variable
add_groups_long_gender <- function(df, submeasures, gender_var) {

  gender_codes <- purrr::map_chr(submeasures, ~ .x$gender_code)

  df <- add_gender(df, gender_var)
  out <- dplyr::mutate(df, gender = "all")

  if (length(submeasures) == 0) return(out)

  checkmate::assert_subset(gender_codes, c("1", "2"))

  dplyr::bind_rows(
    out,
    purrr::map(
      gender_codes,
      \(x) dplyr::filter(df, .data$gender == x)
    )
  )

}

#' Handles the necessary postprocessing of an aggregated indicator by renaming
#' and adding necessary variables as well as adding the version
#'
#' @param df an aggregated indicator
#' @param cfg the measure config, the result of [get_measure_config()]
#' @param register_id the register ID
#'
#' @return renamed data.frame
postprocess_indicator <- function(df, cfg, register_id, measure_id) {

  checkmate::assert_choice(
    cfg$type,
    c("Andel", "Dagar", "Median", "Antal", "Medelv\u00e4rde")
  )

  if (cfg$type == "Andel") {
    df <- postprocess_indicator_prop(df)
  } else if (cfg$type %in% c("Dagar", "Median")) {
    df <- postprocess_indicator_median(df)
  } else if (cfg$type == "Antal") {
    df <- postprocess_indicator_count(df)
  } else if (cfg$type == "Medelv\u00e4rde") {
    df <- postprocess_indicator_mean(df)
  }

  df |>
    postprocess_indicator_org(register_id) |>
    postprocess_indicator_register(register_id) |>
    postprocess_indicator_reason() |>
    postprocess_indicator_mid(cfg$submeasures, measure_id) |>
    postprocess_indicator_names() |>
    dplyr::mutate(Version = cfg$version)
}

#' @describeIn postprocess_indicator renames columns if the indicator is a
#' proportion
postprocess_indicator_prop <- function(df) {

  if ("estimate" %in% names(df)) {
    df <- df |>
      dplyr::rename(
        Rate = "estimate",
        Numerator = dplyr::any_of("cum_events"),
        Denominator = dplyr::any_of("total")
      )
  } else if (any(stringr::str_detect(names(df), "_total_non_missing$"))) {
    df <- df |>
      dplyr::rename(
        Rate = dplyr::ends_with("_prop"),
        Numerator = dplyr::ends_with("_n"),
        Denominator = dplyr::ends_with("_total_non_missing")
      )
  } else if ("total" %in% names(df)) {
    df <- df |>
      dplyr::rename(
        Rate = dplyr::ends_with("_prop"),
        Numerator = dplyr::ends_with("_n"),
        Denominator = dplyr::all_of("total")
      )
  } else {
    cli::cli_abort("Unable to determine correct columns")
  }
  df |>
    dplyr::mutate(
      Rate = round(.data$Rate, 4)
    )
}

#' @describeIn postprocess_indicator renames columns if the indicator is a
#' median
postprocess_indicator_median <- function(df) {

  if (any(stringr::str_detect(names(df), "_total_non_missing$"))) {
    df <- df |>
      dplyr::rename(
        Value = dplyr::ends_with("_median"),
        Measurepopulation = dplyr::ends_with("_total_non_missing")
      )
  } else {
    df <- df |>
      dplyr::rename(
        Value = dplyr::ends_with("_median"),
        Measurepopulation = dplyr::all_of("total")
      )
  }

  df |>
    dplyr::mutate(
      Value = round(.data$Value, 2)
    )
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
      ),
      CountryName = "Riket"
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

#' @describeIn postprocess_indicator adds remaining variables
postprocess_indicator_names <- function(df) {

  name_diff <- setdiff(get_vis_names(), names(df))

  purrr::reduce(
    name_diff,
    ~ dplyr::mutate(.x, !!.y := NA),
    .init = df
  )
}

#' @describeIn postprocess_indicator adds ReasonCode
postprocess_indicator_reason <- function(df) {

  df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::matches("obfuscated_reason"),
        ~ dplyr::case_when(
          stringr::str_detect(.x, "[Nn] < \\d+") ~ "MSK",
          .default = NA
        ),
        .names = "ReasonCode"
      )
    ) |>
    dplyr::select(-dplyr::matches("obfuscated_reason"))

}

#' @describeIn postprocess_indicator adds MeasureID
#' @param submeasures the submeasures key from [get_measure_config]
#' @param measure_id the MeasureID passed to [aggregate_vis]
postprocess_indicator_mid <- function(df, submeasures, measure_id) {

  if (length(submeasures) == 0) {
    df <- dplyr::mutate(df, MeasureID = measure_id)
  } else {
    df <- df |>
      dplyr::left_join(
        submeasures |>
          dplyr::bind_rows() |>
          dplyr::add_row(gender_code = "all", measure_id = .env$measure_id),
        by = dplyr::join_by("gender" == "gender_code")
      ) |>
      dplyr::rename(MeasureID = "measure_id")
  }
  dplyr::select(df, -"gender")
}

#' @describeIn postprocess_indicator renames columns if the indicator is a
#' mean
postprocess_indicator_mean <- function(df) {

  if (any(stringr::str_detect(names(df), "_total_non_missing$"))) {
    df <- df |>
      dplyr::rename(
        Value = dplyr::ends_with("_mean"),
        Measurepopulation = dplyr::ends_with("_total_non_missing")
      )
  } else {
    df <- df |>
      dplyr::rename(
        Value = dplyr::ends_with("_mean"),
        Measurepopulation = dplyr::all_of("total")
      )
  }

  df |>
    dplyr::mutate(
      Value = round(.data$Value, 2)
    )
}
