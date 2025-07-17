
sendUsername <- function(email_add) {
  ## Connect to the database
  conn <- DBI::dbConnect(RMariaDB::MariaDB(),
                    host = Sys.getenv("DB_HOST"),
                    port = Sys.getenv("DB_PORT"),
                    user = Sys.getenv("DB_USER"),
                    password = Sys.getenv("DB_PASSWORD"),
                    dbname = Sys.getenv("DB_NAME")
  )

  ## Get list of emails for all accounts
  emails <- as.character(DBI::dbGetQuery(conn,
                                    "SELECT EMAIL FROM ACCOUNT_INFO")[,1])

  ## Check if inputted email is associated with existing account. If not, show a
  ## Notification.
  if (!email_add %in% emails) {
    showNotification("That email is not associated with an account", type = "error")
    DBI::dbDisconnect(conn)
  } else {
    ## Get email address entered by user to use in a SQL query retrieve the
    ## username associated with that email
    user <- DBI::dbGetQuery(conn,
                               "SELECT USERNAME FROM ACCOUNT_INFO WHERE EMAIL = ?;",
                               params = list(email_add)
    )[,1]

    ## Disconnect from the data base and compose password reset email
    DBI::dbDisconnect(conn)
    message <- blastula::compose_email(
      glue::glue("The username for your flyfishr account is: {user}")
    )

    ## Email credentials. Use creds_envvar() to read from environment variable
    blastula::gmail_creds <- creds_envvar(
      user = "flyfishrapp@gmail.com",
      pass_envvar = "GMAIL_APP_PASSWORD",
      host = "smtp.gmail.com",
      port = 587,
      use_ssl = TRUE
    )

    ## Use in smtp_send to send password rest email
    blastula::smtp_send(
      email = message,
      to = email_add,
      from = "flyfishrapp@gmail.com",
      subject = "Username request",
      credentials = gmail_creds
    )
    removeModal()
    showNotification(glue::glue("An email containing your username was sent to {email_add}"))
  }

}
