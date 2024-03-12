#' Get current user info
#' @export
get_user_info <- function() {

  scope_register <- .GlobalEnv[["Scope.Register"]]
  scope_role <- .GlobalEnv[["Scope.Role"]]
  scope_user <- .GlobalEnv[["Scope.User"]]
  scope_unit <- .GlobalEnv[["Scope.Unit"]]

  list(
    "register_id" = scope_register,
    "user_id" = scope_user,
    "unit_id" = scope_unit,
    "role_id" = scope_role
  )
}
