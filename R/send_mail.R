#' Function to send e-mail from Registercentrum
#' office 365 mail.
#'
#' @param to e-mail to send to
#' @param from Name of sender, included in signature
#' @param subject Subject of message
#' @param body Text to include in body of message.
#' Please note that any line breaks must be done in
#' windows style to be accepted by mail server (eg.
#' use \\r\\n to break a line.).
#' @param password The password to the statistik\@registercentrum.se email
#'
#' @examples
#' \dontrun{
#' rc_mail(
#'   to = "rikard.isaksson@vgregion.se",
#'   from = "Rikard Isaksson",
#'   subject = "Hej test",
#'   body = "Test!"
#' )
#' }
#' @export

rc_mail <- function(to, from, subject, body, password) {
  requireNamespace("curl")
  requireNamespace("httr")

  httr::set_config(
    httr::config(
      ssl_verifypeer = 0L,
      ssl_verifyhost = 0L,
      ssl_version = 7
    )
  )

  message <- paste0(
    "From: statistik@registercentrum.se",
    "\r\nTo: ", to,
    "\r\nSubject: ", subject, "\r\n\r\n",
    body,
    "\r\n\r\nHälsningar\r\n",
    from,
    "\r\nStatistiker\r\nRegistercentrum Västra Götaland"
  )

  curl::send_mail(
    mail_from = "statistik@registercentrum.se",
    mail_rcpt = to,
    message = message,
    smtp_server = "smtp.office365.com:587",
    use_ssl = "try",
    verbose = TRUE,
    username = "statistik@registercentrum.se",
    password = password
  )
}

#' Function to generate bash-script that sends
#' e-mail with attachment from Registercentrum
#' office 365 mail.
#'
#' @param to e-mail to send to
#' @param subject Subject of message
#' @param file file to include. Currently supports
#' one file of type csv, xlsx, pdf, zip, doc or docx.
#' @param password The password to the statistik@registercentrum.se email
#' @param run_bash Whether or not to run and then delete the created bash script
#'
#' @examples
#' \dontrun{
#' sh_mail(
#'   to = "rikard.isaksson@vgregion.se",
#'   subject = "Hej test",
#'   file = "data/test.csv"
#' )
#'
#' sh_mail(
#'   to = c(
#'     "alexander.thoren@vgregion.se",
#'     "oskar.e.johansson@vgregion.se"
#'   ),
#'   subject = "test",
#'   password = ...,
#'   file = "test.xlsx"
#' )
#' }
#' @export

sh_mail <- function(to, subject, password, file = NULL, run_bash = TRUE) {
  requireNamespace("curl")
  requireNamespace("httr")


  # https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Common_types

  if (!is.null(file)) {
    if (grepl(".csv$", file)) {
      mimetype <- "text/csv"
    } else if (grepl(".xlsx$", file)) {
      mimetype <- "application/vnd.ms-excel"
    } else if (grepl(".pdf$", file)) {
      mimetype <- "application/pdf"
    } else if (grepl(".doc$", file)) {
      mimetype <- "application/msword"
    } else if (grepl(".docx$", file)) {
      mimetype <- "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
    } else if (grepl(".zip$", file)) {
      mimetype <- "application/zip"
    } else {
      stop("Filetype not supported")
    }
  }

  curl_command <-
    paste0(
      "curl smtp://smtp.office365.com:587 --ssl ",
      "--mail-from statistik@registercentrum.se ",
      paste("--mail-rcpt", to, collapse = " "),
      " --user statistik@registercentrum.se:",
      password,
      " -H 'From: statistik@registercentrum.se' ",
      "-H 'To: ", paste(to, collapse = ","), "' ",
      "-H 'Subject: ", subject, "' ",
      "-F '=(;type=multipart/mixed' ",
      "-F 'file=@", file,
      ";type=", mimetype, ";encoder=base64' ",
      "-F '=)'"
    )

  con <- file("send.sh")

  writeLines(paste0("#!/bin/bash \n\n", curl_command), con)
  close(con)

  if (run_bash) {
    system("bash send.sh")
    system("rm send.sh")
  }
}
