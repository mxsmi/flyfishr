
passwordReset <- function(reset_token, email_add) {

  ## Connect to the database
  conn <- dbConnect(MariaDB(),
                    host = Sys.getenv("DB_HOST"),
                    port = Sys.getenv("DB_PORT"),
                    user = Sys.getenv("DB_USER"),
                    password = Sys.getenv("DB_PASSWORD"),
                    dbname = Sys.getenv("DB_NAME")
  )

  ## Get email address entered by user to use in a SQL query to retrieve
  ## the password reset token for that user
  dbExecute(conn,
             "UPDATE ACCOUNT_INFO
             SET RESET_TOKEN = ?
             WHERE EMAIL = ?;",
             params = list(reset_token, email_add))

   ## Disconnect from the data base and compose password reset email
   dbDisconnect(conn)
    message <- blastula::compose_email(
      glue::glue("You requested a password reset for your flyfishr account. Here is your
      password reset token: \n '{reset_token}'.")
    )

  ## Email credentials. Use creds_envvar() to read from environment variable
  gmail_creds <- blastula::creds_envvar(
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
    subject = "Password Reset",
    credentials = gmail_creds
  )
}
