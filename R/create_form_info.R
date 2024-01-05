#' Rolling average over vector
#'
#' @param register_id numeric value for the id-number for a register
#' @param api_key API-key for the API-call to Stratum to fetch information about form-IDs and whether they use PROM or not.
#' @param add_var_prefix Boolean. If the data frames have the same names as in Stratum, then this function can add a column 
#' telling you what prefix the variables in each form has.
#'
#' @export create_form_info
create_form_info <- function(
  register_id,
  api_key = "MpuYxfbtp5I=",
  add_var_prefix
) {
  
  form_info <- httr::content(
    httr::GET(
      paste('https://stratum.registercentrum.se/api/metadata/forms/register/', 
            register_id,
            '?apikey=',
            api_key, 
            sep = ''),
    ),
    as = 'text',
    encoding = 'UTF-8'
  ) %>%
  jsonlite::fromJSON()
  
  form_info <- form_info[["data"]]
  
  form_info <- form_info[c(
    "FormID", 
    "FormName",
    "FormScope",
    "Parent.FormID"
  )]
  
  form_info$isProm <- FALSE
  form_info$isProm[!is.na(form_info$FormScope) & form_info$FormScope == 3] <- TRUE
  
  
  top_forms <- form_info$FormID[is.na(form_info$Parent.FormID)]
  parent_forms <- unique(form_info$Parent.FormID)
  top_parents <- top_forms[top_forms %in% parent_forms]
  
  form_info$run_deep <- 0
  
  tmp <- form_info[form_info$FormID %in% top_parents,]
  tmp$run_deep <- 1
  
  form_info <- rbind(
    tmp,
    form_info
  )
  
  form_info$suffix <- NA
  form_info$suffix[formids$run_deep == 0] <- "_no_run_deep"
  form_info$suffix[formids$run_deep == 1] <- ""
  
  return(form_info)
}
