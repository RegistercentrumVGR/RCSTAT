#' @export
#' @describeIn api Count number of registrations by form
api_form_count <- function(FormID) {
  reg <- api_form_meta(FormID)$ShortName
  form <- api_formid_2_formname(FormID)
  api_get(sprintf("aggregate/%s/%s/total/count", reg, form))$data
}
