#' Get API data for register, forms and questions
#'
#' Access open data from the Stratum API.
#'
#' @param RegisterID,FormID Register and form ID numbers.
#'   See https://medicor.github.io/metaviewer/app.html
#' @param x character. Relevant part to paste into URL (mostly for internal use)
#' @param all logical. Should all columns be included?
#' @param only_mapped logical. Should only questions with mapped domains be included?
#' @param var character. Column name in table.
#' @references https://stratum.docs.apiary.io/
#'
#' @export
#' @rdname api
api_get <- function(x) {
  root <- "https://stratum.registercentrum.se/api"
  sprintf("%s/%s?apikey=qO9KWTLfKAI=", root, x) |>
    jsonlite::fromJSON()
}



# Helpers -----------------------------------------------------------------

api_domain <- function(DomainID) {
  api_get(sprintf("metadata/domains/%d", DomainID))$data$DomainValues |>
    tibble::as_tibble()
}


#' @describeIn api Convert RegisterID to RegisterName
#' @export
api_registerid_2_registername <- function(RegisterID) {
  api_register_meta(RegisterID)$ShortName
}

#' @describeIn api Convert FormID to FormName
#' @export
api_formid_2_formname <- function(FormID) {
  api_get(sprintf("metadata/forms/%d", FormID))$data$FormName
}

#' @describeIn api Get RegisterID for FormID
#' @export
api_formid_2_registerid <- function(FormID) {
  api_get(sprintf("metadata/forms/%d", FormID))$data$Register$RegisterID
}

#' @describeIn api Get a list of registers
#' @export
api_registers <- function() {
  api_get("metadata/registers")
}

# Register data -----------------------------------------------------------

#' @export
#' @describeIn api All data for register from API
api_register <- function(RegisterID) {
  api_get(sprintf("metadata/registers/%d", RegisterID))
}

#' @export
#' @describeIn api Meta data for register
api_register_meta <- function(RegisterID) {
  x <- api_register(RegisterID)$data
  x[c("RegisterName", "ShortName")]
}

#' @export
#' @describeIn api All forms in register
api_register_forms <- function(RegisterID) {
  x <-
    tibble::as_tibble(api_register(RegisterID)$data$Forms)

  if (utils::hasName(x, "Parent")) {
    x <- tidyr::unnest(x, "Parent", names_sep = "_")
  }
  if (utils::hasName(x, "Parent_Parent")) {
    x <- tidyr::unnest(x, "Parent_Parent", names_sep = "_")
  }

  dplyr::select(x, tidyselect::matches("FormID|FormName|FormTitle"))
}

#' @export
#' @describeIn api All units in register
api_register_units <- function(RegisterID) {
  api_register(RegisterID)$data$Units |>
    tibble::as_tibble() |>
    dplyr::select(
      tidyselect::any_of(
        c("UnitCode", "UnitName", "HSAID", "IsActive")
      )
    )
}

# Form data ---------------------------------------------------------------

#' @export
#' @describeIn api All data for register from API
api_form <- function(FormID) {
  api_get(sprintf("metadata/forms/%d", FormID))
}

#' @export
#' @describeIn api Meta data for form
api_form_meta <- function(FormID) {
  x <- api_form(FormID)$data
  x$RegisterID <- x$Register$RegisterID
  x$ShortName <- x$Register$ShortName
  x[c("FormName", "FormTitle", "TableName", "RegisterID", "ShortName")]
}

#' @export
#' @describeIn api All questions in form
api_form_questions <- function(FormID, all = FALSE, only_mapped = FALSE) {
  x <-
    api_get(sprintf("metadata/forms/%d", FormID))$data$Questions |>
    tibble::as_tibble() |>
    # Remove section labels (not questions)
    dplyr::filter(.data$Domain$DomainID != 1080) |>
    tidyr::unnest(cols = "Domain")

  if (!all) {
    incl <- c("ColumnName", "QuestionID", "MappedTo")
    x <- dplyr::select(x, tidyselect::any_of(incl), tidyselect::starts_with("Domain"))

    if (only_mapped) {
      x <-
        dplyr::filter(x, !is.na(.data$MappedTo)) |>
        dplyr::select("ColumnName", "MappedTo")
    }
  }
  x
}

# Questions -----------------------------------------------------------

#' @export
#' @describeIn api get map table attribute for variable in form
api_map <- function(var, FormID) {
  x <- api_form_questions(FormID)
  if (!var %in% x$ColumnName) {
    stop(sprintf("Column %s not found in form %s", var, FormID))
  }
  x <- dplyr::filter(x, .data$ColumnName == var)

  # UnitCodes are found in register-specific tables if DomainID >= 3000
  if (is.na(x$MappedTo)) {
    if (as.numeric(x$DomainID) < 3000L) {
      stop(
        paste0(
          var, " is not mapped (domain: ", x$DomainID, "). ",
          "See: https://stratum.docs.apiary.io/#reference/metadata/doman"
        )
      )
    }
    api_domain(x$DomainID) |>
      dplyr::select(levels := "ValueCode", labels := "ValueName")
    # Most domains are question-specific
  } else if (x$MappedTo == "UnitCode") {
    api_register_units(api_formid_2_registerid(FormID)) |>
      dplyr::select(levels := "UnitCode", labels := "UnitName")
  } else if (grepl("^[0-9]{4}$", "1273", x$MappedTo)) {
    stop("This variable is just a reference to FormID ", x$MappedTo)
  } else {
    stop("This variable can not be maped!")
  }
}
