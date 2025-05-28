#' Validates a VIS data.frame
#'
#' Checks if:
#' * all variables are present
#' * the instruction on the first line is correct
#' * register HSAIDs are correct
#' * dates are correct
#' * Value and Rate are correct
#' * confidence intervals are correct
#' * values have not been censored to an unusually high degree
#' * county and unit variables are correct
#' * no rows are duplicated
#'
#' If any variables are found to be missing the function exists early because
#' then we can not be sure remaining validations will be possible.
#'
#' @param df VIS data.frame
#' @param overall_rate the overall censor rate to warn at
#' @param measure_rate the per-MeasureID censor rate to warn at
#' @param as_data_frame whether to return the errors in a data.frame
#'
#' @return a character vector of error messages
#' @export
#' @md
validate_vis <- function(df, overall_rate = 0.25, measure_rate = 0.75, as_data_frame = FALSE) {
  name_errors <- check_names(df)

  if (!is.null(name_errors)) {
    return(return_errors(name_errors, as_data_frame))
  }

  errors <- c()

  errors <- c(
    errors,
    check_instruction(df)
  )

  if (!is.null(errors)) {
    return(return_errors(errors, as_data_frame))
  }

  errors <- c(
    errors,
    check_register_hsaid(df)
  )

  df <- dplyr::slice(df, -1)

  errors <- c(
    check_dates(df),
    check_value_rate(df),
    check_ci(df),
    check_censor_rate(
      df,
      overall_rate = overall_rate,
      measure_rate = measure_rate
    ),
    check_counties_and_units(df),
    check_duplicate_rows(df),
    check_unit_hospital(df)
  )

  return(return_errors(errors, as_data_frame))
}

#' Format errors before return
#'
#' @param errors the vector of errors
#' @param as_data_frame logical indicating whether to return a data.frame or vector
#'
#' @return a data.frame or vector of errors
return_errors <- function(errors, as_data_frame) {
  if (as_data_frame) {
    return(data.frame(errors = errors))
  } else {
    return(errors)
  }
}

#' Checks if the first cell in the row contains a KEEP or DELETE existing
#' instruction
#'
#' @param df VIS data.frame
#'
#' @return vector of error messages
#' @md
check_instruction <- function(df) {
  first <- df |>
    dplyr::slice(1) |>
    dplyr::pull(1) |>
    grepl(pattern = "^(KEEP|DELETE)_EXISTING_DATA_FOR_REGISTER_HSAID:.+$")

  if (!first) {
    return("First cell does not contain correct keep or delete existing instruction")
  }
}

#' Checks if all variable names are present
#'
#' @param df VIS data.frame
#'
#' @return vector of error messages
#' @md
check_names <- function(df) {
  nms <- get_vis_names()

  errors <- c()

  if (!setequal(nms, names(df))) {
    nms_missing <- setdiff(nms, names(df))

    if (length(nms_missing) > 0) {
      errors <- c(
        errors, paste0(
          "Variables missing: ", paste0(nms_missing, collapse = ", ")
        )
      )
    }

    nms_superfluous <- setdiff(names(df), nms)

    if (length(nms_superfluous) > 0) {
      errors <- c(
        errors, paste0(
          "Variables that should not be included: ",
          paste0(
            nms_superfluous,
            collapse = ", "
          )
        )
      )
    }
  }

  return(errors)
}

#' Checks if all dates are correct
#'
#' * Ensures that no PeriodReportedStartDate is greater than
#' PeriodReportedEndDate
#' * Ensures that all PeriodReportedEndDate have passed
#' * Ensures that no PeriodReportedStartDate or PeriodReportedEndDate are
#' missing
#'
#' @param df VIS data.frame
#'
#' @return vector of error messages
#' @md
check_dates <- function(df) {
  errors <- c()

  tmp <- df |>
    dplyr::mutate(
      name = dplyr::coalesce(.data$UnitName, .data$HospitalName),
      hsaid = dplyr::coalesce(.data$UnitHSAID, .data$HospitalHSAID, .data$HospitalOrganisationId)
    ) |>
    dplyr::filter(
      .data$PeriodReportedEndDate <= df$PeriodReportedStartDate
    ) |>
    dplyr::distinct(
      .data$MeasureID,
      .data$RegionOrganisationId,
      .data$PeriodReportedStartDate,
      .data$PeriodReportedEndDate,
      .data$name
    )

  if (nrow(tmp) > 0) {
    errors <- c(
      errors,
      sprintf(
        paste0("MeasureID %s, RegionOrganisationId %s, unit/hospital name %s has",
               " PeriodReportedEndDate <= PeriodReportedStartDate"),
        tmp$MeasureID,
        tmp$RegionOrganisationId,
        tmp$name
      )
    )
  }

  tmp <- df |>
    dplyr::mutate(
      name = dplyr::coalesce(.data$UnitName, .data$HospitalName),
      hsaid = dplyr::coalesce(.data$UnitHSAID, .data$HospitalHSAID, .data$HospitalOrganisationId)
    ) |>
    dplyr::filter(
      .data$PeriodReportedEndDate > lubridate::today()
    ) |>
    dplyr::distinct(
      .data$MeasureID,
      .data$RegionOrganisationId,
      .data$PeriodReportedStartDate,
      .data$PeriodReportedEndDate,
      .data$name
    )

  if (nrow(tmp) > 0) {
    errors <- c(
      errors,
      sprintf(
        paste0("MeasureID %s, RegionOrganisationId %s, unit/hospital name %s has",
               " PeriodReportedEndDate > lubridate::today()"),
        tmp$MeasureID,
        tmp$RegionOrganisationId,
        tmp$name
      )
    )
  }

  errors <- c(
    errors,
    check_na(df, "PeriodReportedStartDate"),
    check_na(df, "PeriodReportedEndDate")
  )

  return(errors)
}

#' Utility function to ensure that `var` contains no missing values
#'
#' @param df VIS data.frame
#' @param var the name of the variable
#' @param var_name what to call the variable in the error message
#'
#' @return vector of error messages
#' @md
check_na <- function(df, var, var_name = var) {
  errors <- c()
  if (any(is.na(df[[var]]))) {
    errors <- c(
      errors,
      sprintf(
        "Some %ss are missing",
        var_name
      )
    )
  }
  return(errors)
}

#' Checks if register HSAID is correct
#'
#' * Ensures that the HSAID in the instruction cell matches all other observed
#' register HSAIDs
#' * Ensures that there is only one register HSAID
#'
#' @param df VIS data.frame
#'
#' @return vector of error messages
#' @md
check_register_hsaid <- function(df) {
  first_row_hsaid <- df |>
    dplyr::slice(1) |>
    dplyr::pull(1) |>
    stringr::str_extract(":(.+)$", group = 1)

  hsaids <- df |>
    dplyr::slice(-1) |>
    dplyr::pull("RegisterHSAID") |>
    unique()

  errors <- c()

  if (!all(first_row_hsaid == hsaids)) {
    errors <- c(
      errors,
      "RegisterHSAID in instruction does not equal other RegisterHSAIDs"
    )
  }

  if (length(hsaids) != 1) {
    errors <- c(
      errors,
      "Multiple RegisterHSAIDs observed"
    )
  }

  return(errors)
}

#' Checks if all rows have Value or Rate
#'
#' * Ensures that all rows that are not censored have Value or Rate
#' * Ensures that all Rates have Numerator and Denominator
#'
#' @param df VIS data.frame
#'
#' @return vector of error messages
#' @md
check_value_rate <- function(df) {
  errors <- c()

  tmp <- df |>
    dplyr::mutate(
      name = dplyr::coalesce(.data$UnitName, .data$HospitalName)
    ) |>
    dplyr::filter(is.na(.data$ReasonCode))

  na_value_rate <- tmp |>
    dplyr::filter(is.na(.data$Value), is.na(.data$Rate)) |>
    dplyr::distinct(
      .data$MeasureID,
      .data$PeriodReportedStartDate,
      .data$PeriodReportedEndDate,
      .data$RegionOrganisationId,
      .data$name
    )

  if (nrow(na_value_rate) > 0) {
    errors <- c(
      errors,
      sprintf(
        paste0("MeasureID %s, period %s-%s, RegionOrganisationId %s, unit/hospital name %s",
               " is missing both Value and Rate with no ReasonCode"),
        na_value_rate$MeasureID,
        na_value_rate$PeriodReportedStartDate,
        na_value_rate$PeriodReportedEndDate,
        na_value_rate$RegionOrganisationId,
        na_value_rate$name
      )
    )
  }


  tmp <- dplyr::filter(tmp, !is.na(.data$Rate))

  na_numerator <- tmp |>
    dplyr::filter(is.na(.data$Numerator)) |>
    dplyr::distinct(
      .data$MeasureID,
      .data$PeriodReportedStartDate,
      .data$PeriodReportedEndDate,
      .data$RegionOrganisationId,
      .data$name
    )

  if (nrow(na_numerator) > 0) {
    errors <- c(
      errors,
      sprintf(
        paste0("MeasureID %s, period %s-%s, RegionOrganisationId %s, unit/hospital name %s",
               " is missing Numerator where Rate is present"),
        na_numerator$MeasureID,
        na_numerator$PeriodReportedStartDate,
        na_numerator$PeriodReportedEndDate,
        na_numerator$RegionOrganisationId,
        na_numerator$name
      )
    )
  }

  na_denominator <- tmp |>
    dplyr::filter(is.na(.data$Denominator)) |>
    dplyr::distinct(
      .data$MeasureID,
      .data$PeriodReportedStartDate,
      .data$PeriodReportedEndDate,
      .data$RegionOrganisationId,
      .data$name
    )

  if (nrow(na_denominator) > 0) {
    errors <- c(
      errors,
      sprintf(
        paste0("MeasureID %s, period %s-%s, RegionOrganisationId %s, unit/hospital name %s",
               " is missing Denominator where Rate is present"),
        na_denominator$MeasureID,
        na_denominator$PeriodReportedStartDate,
        na_denominator$PeriodReportedEndDate,
        na_denominator$RegionOrganisationId,
        na_denominator$name
      )
    )
  }

  return(errors)
}

#' Checks that all confidence intervals are correct
#'
#' * Ensures that no ConfidenceIntervalLower is larger than
#' ConfidenceIntervalHigher
#' * Ensures that ConfidenceIntervalLower and ConfidenceIntervalHigher always
#' appear together
#'
#' @param df VIS data.frame
#'
#' @return vector of error messages
#' @md
check_ci <- function(df) {
  tmp <- df |>
    dplyr::mutate(
      name = dplyr::coalesce(.data$UnitName, .data$HospitalName)
    ) |>
    dplyr::filter(is.na(.data$ReasonCode))

  errors <- c()

  na_ci <- tmp |>
    dplyr::filter(is.na(.data$ConfidenceIntervalLower) != is.na(.data$ConfidenceIntervalHigher)) |>
    dplyr::distinct(
      .data$MeasureID,
      .data$PeriodReportedStartDate,
      .data$PeriodReportedEndDate,
      .data$RegionOrganisationId,
      .data$name
    )

  if (nrow(na_ci) > 0) {
    errors <- c(
      errors,
      sprintf(
        paste0("MeasureID %s, period %s-%s, RegionOrganisationId %s, unit/hospital name %s",
               " is missing one of ConfidenceIntervalLower/Higher"),
        na_ci$MeasureID,
        na_ci$PeriodReportedStartDate,
        na_ci$PeriodReportedEndDate,
        na_ci$RegionOrganisationId,
        na_ci$name
      )
    )
  }

  ci_incorrect <- tmp |>
    dplyr::filter(.data$ConfidenceIntervalLower > .data$ConfidenceIntervalHigher) |>
    dplyr::distinct(
      .data$MeasureID,
      .data$PeriodReportedStartDate,
      .data$PeriodReportedEndDate,
      .data$RegionOrganisationId,
      .data$name
    )

  if (nrow(ci_incorrect) > 0) {
    errors <- c(
      errors,
      sprintf(
        paste0("MeasureID %s, period %s-%s, RegionOrganisationId %s, unit/hospital name %s",
               " has a higher ConfidenceIntervalLower than ConfidenceIntervalHigher"),
        ci_incorrect$MeasureID,
        ci_incorrect$PeriodReportedStartDate,
        ci_incorrect$PeriodReportedEndDate,
        ci_incorrect$RegionOrganisationId,
        ci_incorrect$name
      )
    )
  }

  return(errors)
}

#' Checks that data is not censored to an usually high degree
#'
#' * Ensures that the overall censor rate does not exceed `overall_rate`
#' * Ensures that each MeasureID is not censored to a degree above
#' `measure_rate`
#'
#' @param df VIS data.frame
#' @param overall_rate the overall censor rate to warn at
#' @param measure_rate the per-MeasureID censor rate to warn at
#'
#' @return vector of error messages
#' @md
check_censor_rate <- function(df, overall_rate = 0.25, measure_rate = 0.75) {
  overall <- df |>
    dplyr::summarise(
      n = sum(!is.na(.data$ReasonCode) & .data$ReasonCode != ""),
      total = dplyr::n(),
      prop = .data$n / .data$total
    ) |>
    dplyr::pull("prop")

  errors <- c()

  if (!is.na(overall) && overall >= overall_rate) {
    errors <- c(
      errors,
      sprintf("The overall censored rate is %.1f%%", overall * 100)
    )
  }

  measure <- df |>
    dplyr::group_by(.data$MeasureID) |>
    dplyr::summarise(
      n = sum(!is.na(.data$ReasonCode) & .data$ReasonCode != ""),
      total = dplyr::n(),
      prop = .data$n / .data$total
    ) |>
    dplyr::filter(.data$prop >= measure_rate)

  if (nrow(measure) > 0) {
    errors <- c(
      errors,
      sprintf(
        "MeasureID %s is censored to an abnormally high degree (%.1f%%)",
        measure$MeasureID,
        measure$prop * 100
      )
    )
  }

  return(errors)
}

#' Checks that all unit and county variables are correct
#'
#' * Ensures that RegionOrganisationId, RegionName and at least one of
#' UnitHSAID, HospitalHSAID, and HospitalOrganisationId as well as UnitName and HospitalName is not
#' missing
#' * Ensures that each unit/hospital HSA/PAR-id belongs to at most one
#' unit/hospital name
#' * Ensures that each unit/hospital name has exactly one HSA/PAR-id
#' * Ensures that each combination of unit/hospital name and HSA/PAR-id belongs to
#' exactly one county
#'
#' @param df VIS data.frame
#'
#' @return vector of error messages
#' @md
check_counties_and_units <- function(df) {
  errors <- c()

  na_county <- df |>
    dplyr::group_by(
      .data$MeasureID,
      .data$PeriodReportedStartDate,
      .data$PeriodReportedEndDate
    ) |>
    dplyr::summarise(n = sum(is.na(.data$RegionOrganisationId))) |>
    dplyr::filter(.data$n > 1)

  if (nrow(na_county) > 0) {
    errors <- c(
      errors,
      sprintf(
        "MeasureID %s, period %s-%s is missing RegionOrganistionId in %d rows, should be exactly 1",
        na_county$MeasureID,
        na_county$PeriodReportedStartDate,
        na_county$PeriodReportedEndDate,
        na_county$n
      )
    )
  }

  errors <- c(
    errors,
    check_na(
      dplyr::filter(df, !is.na(.data$RegionOrganisationId)),
      "RegionName"
    )
  )

  tmp <- df |>
    dplyr::mutate(
      name = dplyr::coalesce(.data$UnitName, .data$HospitalName),
      hsaid = dplyr::coalesce(.data$UnitHSAID, .data$HospitalHSAID, .data$HospitalOrganisationId)
    )

  na_name_hsa <- tmp |>
    dplyr::group_by(
      .data$MeasureID,
      .data$RegionOrganisationId,
      .data$PeriodReportedStartDate,
      .data$PeriodReportedEndDate
    ) |>
    dplyr::filter(is.na(.data$name) & is.na(.data$hsaid)) |>
    dplyr::summarise(n = sum(is.na(.data$name) & is.na(.data$hsaid))) |>
    dplyr::filter(.data$n > 1)

  if (nrow(na_name_hsa) > 0) {
    errors <- c(
      errors,
      sprintf(
        paste0("MeasureID %s, RegionOrganisationId %s, period %s-%s has %d rows ",
               "missing both unit/hospital name and HSA/PAR-id, should be exactly 1"),
        na_name_hsa$MeasureID,
        na_name_hsa$RegionOrganisationId,
        na_name_hsa$PeriodReportedStartDate,
        na_name_hsa$PeriodReportedEndDate,
        na_name_hsa$n
      )
    )
  }

  name_na <- tmp |>
    dplyr::filter(
      is.na(.data$name) & !is.na(.data$hsaid)
    ) |>
    dplyr::distinct(.data$hsaid)

  if (nrow(name_na) > 0) {
    errors <- c(
      errors,
      sprintf(
        "HSA/PAR-id %s is missing name", name_na$hsaid
      )
    )
  }

  county_na <- tmp |>
    dplyr::filter(!is.na(.data$name), is.na(.data$RegionOrganisationId)) |>
    dplyr::distinct(.data$name)

  if (nrow(county_na) > 0) {
    errors <- c(
      errors,
      sprintf(
        "Unit %s is missing RegionOrganisationId", county_na$name
      )
    )
  }

  hsaid_na <- tmp |>
    dplyr::filter(
      is.na(.data$hsaid) & !is.na(.data$name)
    ) |>
    dplyr::distinct(.data$name)

  if (nrow(hsaid_na) > 0) {
    errors <- c(
      errors,
      sprintf(
        "Unit %s is missing HSA/PAR-id", hsaid_na$name
      )
    )
  }

  hsaid_count <- tmp |>
    dplyr::group_by(.data$hsaid) |>
    dplyr::mutate(n = dplyr::n_distinct(.data$name)) |>
    dplyr::filter(.data$n > 1) |>
    dplyr::summarise(values = list(unique(.data$name))) |>
    tibble::deframe()

  if (length(hsaid_count) > 0) {
    errors <- c(
      errors,
      sprintf(
        "The unit/hospital HSA/PAR-id %s belongs to several units: %s",
        names(hsaid_count),
        sapply(hsaid_count, \(x) paste0(x, collapse = ", "))
      )
    )
  }

  name_count <- tmp |>
    dplyr::group_by(.data$name) |>
    dplyr::mutate(n = dplyr::n_distinct(.data$hsaid)) |>
    dplyr::filter(.data$n > 1) |>
    dplyr::summarise(values = list(unique(.data$hsaid))) |>
    tibble::deframe()

  if (length(name_count) > 0) {
    errors <- c(
      errors,
      sprintf(
        "The unit/hospital %s has several HSA/PAR-ids %s",
        names(name_count),
        sapply(name_count, \(x) paste0(x, collapse = ", "))
      )
    )
  }

  county_count <- tmp |>
    dplyr::filter(!is.na(.data$name)) |>
    dplyr::group_by(unit = sprintf("%s (%s)", .data$name, .data$hsaid)) |>
    dplyr::mutate(n = dplyr::n_distinct(.data$RegionOrganisationId)) |>
    dplyr::filter(.data$n > 1) |>
    dplyr::summarise(values = list(unique(.data$RegionOrganisationId))) |>
    tibble::deframe()

  if (length(county_count) > 0) {
    errors <- c(
      errors,
      sprintf(
        "The unit/hospital %s belongs to several counties: %s",
        names(county_count),
        sapply(county_count, \(x) paste0(x, collapse = ", "))
      )
    )
  }

  return(errors)
}

#' Checks that no rows are duplicated
#'
#' * Ensures that each combination of PeriodReportedStartDate,
#' PeriodReportedEndDate, MeasureID, unit/hospital name, and unit/hospital
#' HSA/PAR-id appear exactly once
#'
#' @param df VIS data.frame
#'
#' @return vector of error messages
#' @md
check_duplicate_rows <- function(df) {
  errors <- c()

  count <- df |>
    dplyr::mutate(
      name = dplyr::coalesce(.data$UnitName, .data$HospitalName),
      hsaid = dplyr::coalesce(.data$UnitHSAID, .data$HospitalHSAID, .data$HospitalOrganisationId)
    ) |>
    dplyr::count(
      .data$PeriodReportedStartDate,
      .data$PeriodReportedEndDate,
      .data$MeasureID,
      .data$name,
      .data$hsaid,
      .data$RegionOrganisationId
    ) |>
    dplyr::filter(.data$n > 1) |>
    dplyr::distinct(
      .data$PeriodReportedStartDate,
      .data$PeriodReportedEndDate,
      .data$MeasureID,
      .data$name,
      .data$hsaid,
      .data$RegionOrganisationId
    )

  if (nrow(count) > 0) {
    errors <- c(
      errors,
      sprintf(
        paste0("MeasureID %s, period %s-%s, RegionOrganisation %s, unit/hospital name %s,",
               " unit/hospital HSA/PAR-id %s is duplicated"),
        count$MeasureID,
        count$PeriodReportedStartDate,
        count$PeriodReportedEndDate,
        count$RegionOrganisationId,
        count$name,
        count$hsaid
      )
    )
  }

  return(errors)
}

#' Checks that all ReasonCodes are correct
#'
#' * Ensures that each ReasonCode is NA, "MSK", "NI", "INV", or "UNK"
#'
#' @param df VIS data.frame
#'
#' @return vector of error messages
#' @md
check_reasoncode <- function(df) {
  errors <- c()

  if (!all(df$ReasonCode %in% c(NA, "MSK", "NI", "INV", "UNK"))) {
    codes <- df |>
      dplyr::filter(
        !.data$ReasonCode %in% c(NA, "MSK", "NI", "INV", "UNK")
      ) |>
      dplyr::distinct(.data$ReasonCode) |>
      dplyr::pull(.data$ReasonCode)

    errors <- c(
      errors,
      sprintf(
        "ReasonCode contains disallowed values: %s",
        paste0(codes, collapse = ", ")
      )
    )
  }

  return(errors)
}

#' Checks that all censored rows have values removed
#'
#' * Ensures that Value, Rate, ConfidenceIntervalLower and
#' ConfidenceIntervalHigher are missing if a ReasonCode is present
#'
#' @param df VIS data.frame
#'
#' @return vector of error messages
#' @md
check_censored <- function(df) {
  tmp <- df |>
    dplyr::filter(!is.na(.data$ReasonCode)) |>
    dplyr::mutate(
      name = dplyr::coalesce(.data$UnitName, .data$HospitalName)
    )

  errors <- c()

  na_value <- tmp |>
    dplyr::filter(!is.na(.data$Value)) |>
    dplyr::distinct(
      .data$MeasureID,
      .data$PeriodReportedStartDate,
      .data$PeriodReportedEndDate,
      .data$RegionOrganisationId,
      .data$name
    )

  if (nrow(na_value) > 0) {
    errors <- c(
      errors,
      sprintf(
        paste0("MeasureID %s, period %s-%s, RegionOrganisationId %s, unit/hospital name %s",
               " has Value where a ReasonCode is present"),
        na_value$MeasureID,
        na_value$PeriodReportedStartDate,
        na_value$PeriodReportedEndDate,
        na_value$RegionOrganisationId,
        na_value$name
      )
    )
  }

  na_rate <- tmp |>
    dplyr::filter(!is.na(.data$Rate)) |>
    dplyr::distinct(
      .data$MeasureID,
      .data$PeriodReportedStartDate,
      .data$PeriodReportedEndDate,
      .data$RegionOrganisationId,
      .data$name
    )

  if (nrow(na_rate) > 0) {
    errors <- c(
      errors,
      sprintf(
        paste0("MeasureID %s, period %s-%s, RegionOrganisationId %s, unit/hospital name %s",
               " has Rate where a ReasonCode is present"),
        na_rate$MeasureID,
        na_rate$PeriodReportedStartDate,
        na_rate$PeriodReportedEndDate,
        na_rate$RegionOrganisationId,
        na_rate$name
      )
    )
  }

  na_ci_lower <- tmp |>
    dplyr::filter(!is.na(.data$ConfidenceIntervalLower)) |>
    dplyr::distinct(
      .data$MeasureID,
      .data$PeriodReportedStartDate,
      .data$PeriodReportedEndDate,
      .data$RegionOrganisationId,
      .data$name
    )

  if (nrow(na_ci_lower) > 0) {
    errors <- c(
      errors,
      sprintf(
        paste0("MeasureID %s, period %s-%s, RegionOrganisationId %s, unit/hospital name %s",
               " has ConfidenceIntervalLower where a ReasonCode is present"),
        na_ci_lower$MeasureID,
        na_ci_lower$PeriodReportedStartDate,
        na_ci_lower$PeriodReportedEndDate,
        na_ci_lower$RegionOrganisationId,
        na_ci_lower$name
      )
    )
  }

  na_ci_higher <- tmp |>
    dplyr::filter(!is.na(.data$ConfidenceIntervalHigher)) |>
    dplyr::distinct(
      .data$MeasureID,
      .data$PeriodReportedStartDate,
      .data$PeriodReportedEndDate,
      .data$RegionOrganisationId,
      .data$name
    )

  if (nrow(na_ci_higher) > 0) {
    errors <- c(
      errors,
      sprintf(
        paste0("MeasureID %s, period %s-%s, RegionOrganisationId %s, unit/hospital name %s",
               " has ConfidenceIntervalHigher where a ReasonCode is present"),
        na_ci_higher$MeasureID,
        na_ci_higher$PeriodReportedStartDate,
        na_ci_higher$PeriodReportedEndDate,
        na_ci_higher$RegionOrganisationId,
        na_ci_higher$name
      )
    )
  }

  return(errors)
}

#' Checks that all censored rows have values removed
#'
#' * Ensures that only one of UnitHSAID, HospitalHSAID, and HospitalOrganisationID is present
#' * Ensures that only one of UnitName and HospitalName is present
#'
#' @param df VIS data.frame
#'
#' @return vector of error messages
#' @md
check_unit_hospital <- function(df) {
  errors <- c()

  tmp <- df |>
    dplyr::mutate(
      id_sum = rowSums(
        dplyr::across(
          dplyr::all_of(
            c(
              "UnitHSAID",
              "HospitalHSAID",
              "HospitalOrganisationId"
            )
          ),
          ~ !is.na(.x)
        )
      ),
      name_sum = rowSums(
        dplyr::across(
          dplyr::all_of(
            c(
              "UnitName",
              "HospitalName"
            )
          ),
          ~ !is.na(.x)
        )
      )
    )

  name_tmp <- tmp |>
    dplyr::filter(.data$name_sum > 1)

  if (nrow(name_tmp) > 0) {
    errors <- c(
      errors,
      sprintf(
        paste0("MeasureID %s, period %s-%s, RegionOrganisationId %s contains rows where",
               " both UnitName and HospitalName are present"),
        name_tmp$MeasureID,
        name_tmp$PeriodReportedStartDate,
        name_tmp$PeriodReportedEndDate,
        name_tmp$RegionOrganisationId
      )
    )
  }

  id_tmp <- tmp |>
    dplyr::filter(.data$id_sum > 1)

  if (nrow(id_tmp) > 0) {
    errors <- c(
      errors,
      sprintf(
        paste0("MeasureID %s, period %s-%s, RegionOrganisationId %s contains rows where",
               " more than one of UnitHSAID, HospitalHSAID, and HospitalOrganisationId are present"),
        id_tmp$MeasureID,
        id_tmp$PeriodReportedStartDate,
        id_tmp$PeriodReportedEndDate,
        id_tmp$RegionOrganisationId
      )
    )
  }

  return(errors)

}

#' Checks that MeasureIDs have a value
#'
#' @param df VIS data.frame
#'
#' @return vector of error messages
#' @md
check_measureid <- function(df) {
  errors <- c()
  if (any(is.na(df$MeasureID))) {
    errors <- c(
      errors,
      "MeasureID is NA in at least one row"
    )
  }
  return(errors)
}

#' Checks that CountryName only contains the value "Riket"
#'
#' @param df VIS data.frame
#'
#' @return vector of error messages
#' @md
check_countryname <- function(df) {
  errors <- c()
  tmp <- df |>
    dplyr::filter(tolower(.data$CountryName) != "riket" | is.na(.data$CountryName))

  if (nrow(tmp) > 0) {
    values <- tmp |>
      dplyr::distinct(.data$CountryName) |>
      dplyr::pull(.data$CountryName)
    errors <- c(
      errors,
      sprintf(
        "CountryName contains disallowed values: %s",
        paste0(values, collapse = ", ")
      )
    )
  }

  return(errors)
}

#' Checks that Version is correct
#'
#' @param df VIS data.frame
#'
#' @return vector of error messages
#' @md
check_version <- function(df) {
  errors <- c()
  tmp <- df |>
    dplyr::filter(!grepl("^\\d+$", .data$Version) | is.na(.data$Version))

  if (nrow(tmp) > 0) {
    values <- tmp |>
      dplyr::distinct(.data$Version) |>
      dplyr::pull(.data$Version)
    errors <- c(
      errors,
      sprintf(
        "Version contains disallowed values: %s",
        paste0(values, collapse = ", ")
      )
    )
  }

  return(errors)
}

#' Get a vector of expect variable names for a VIS file
#'
#' @return a vector of variable names
#' @export
get_vis_names <- function() {
  c(
    "RegionOrganisationId",
    "Numerator",
    "Rate",
    "Denominator",
    "RegionName",
    "PeriodReportedStartDate",
    "PeriodReportedEndDate",
    "MeasureID",
    "HospitalName",
    "HospitalHSAID",
    "HospitalOrganisationId",
    "CountryName",
    "Value",
    "Cohort",
    "Version",
    "UnitHSAID",
    "UnitName",
    "ReferenceIntervalRate",
    "ReferenceIntervalValue",
    "MunicipalityOrganisationID",
    "MunicipalityName",
    "RegisterHSAID",
    "RegisterNamn",
    "Coverage",
    "ConfidenceIntervalLower",
    "ConfidenceIntervalHigher",
    "Standarddeviation",
    "Measurepopulation",
    "Exclusions",
    "ReasonCode"
  )
}
