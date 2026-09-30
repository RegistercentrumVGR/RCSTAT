#' Run RCStat package checks
#'
#' Runs a configurable sequence of package-development checks for RCStat.
#' The available steps are documentation, linting, tests, and `R CMD check`.
#'
#' Steps are always executed in the following order:
#' \enumerate{
#'   \item documentation
#'   \item lint
#'   \item test
#'   \item check
#' }
#'
#' The `steps` argument can be used to select which checks to run. The order
#' in which steps are supplied does not affect their execution order.
#'
#' @param steps Character vector specifying the checks to run. Valid values
#'   are `"document"`, `"lint"`, `"test"`, and `"check"`.
#'   Defaults to all checks.
#' @param lint_cache Character; directory used by `lintr` for its cache.
#'   Defaults to `"lintr_cache"`.
#'
#' @return Invisibly returns `TRUE` if all selected steps complete successfully.
#'
#' @examples
#' \dontrun{
#' # Run all checks
#' rccheck()
#'
#' # Only run tests and R CMD check
#' rccheck(steps = c("test", "check"))
#'
#' # Run documentation and linting
#' rccheck(steps = c("document", "lint"))
#'
#' # Use a custom lintr cache directory
#' rccheck(lint_cache = "my-lintr-cache")
#' }
#'
#' @export
rccheck <- function(
  steps = c("document", "lint", "test", "check"),
  lint_cache = "lintr_cache"
) {
  valid_steps <- c("document", "lint", "test", "check")

  if (!is.character(steps)) {
    stop("`steps` must be a character vector.", call. = FALSE)
  }

  invalid_steps <- setdiff(steps, valid_steps)

  if (length(invalid_steps) > 0) {
    stop(
      sprintf(
        "Unknown step(s): %s. Valid steps are: %s.",
        paste(invalid_steps, collapse = ", "),
        paste(valid_steps, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Remove duplicates while preserving the canonical execution order.
  steps <- intersect(valid_steps, unique(steps))

  if (length(steps) == 0) {
    message("No checks selected.")
    return(invisible(TRUE))
  }

  run_step <- function(name, fn) {
    message("\n", strrep("=", 60))
    message("Running ", name, "...")
    message(strrep("=", 60))

    tryCatch(
      fn(),
      error = function(e) {
        stop(
          sprintf(
            "Step '%s' failed: %s",
            name,
            conditionMessage(e)
          ),
          call. = FALSE
        )
      }
    )

    message("Completed ", name, ".")
    invisible(NULL)
  }

  if ("document" %in% steps) {
    run_step(
      "documentation",
      function() {
        devtools::document()
      }
    )
  }

  if ("lint" %in% steps) {
    run_step(
      "lint",
      function() {
        lintr::lint_package(cache = lint_cache)
      }
    )
  }

  if ("test" %in% steps) {
    run_step(
      "tests",
      function() {
        devtools::test()
      }
    )
  }

  if ("check" %in% steps) {
    run_step(
      "check",
      function() {
        devtools::check()
      }
    )
  }

  message("\nAll selected checks completed successfully.")

  invisible(TRUE)
}
