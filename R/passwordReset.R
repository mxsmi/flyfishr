## Function for sending an email with a password reset token to the user when
## they need to reset their password

passwordReset <- function(pool, reset_token, email_add) {

  ### Get email address entered by user to use to fetch password reset token
  ### for that user
  dbExecute(pool,
             "UPDATE ACCOUNT_INFO
             SET RESET_TOKEN = ?, RT_EXPIRATION = ?
             WHERE EMAIL = ?;",
             params = list(reset_token, Sys.time() + hours(1), email_add))

   ### Compose password reset email
    message <- blastula::compose_email(
      glue::glue("You requested a password reset for your flyfishr account. Here is your
      password reset token: \n '{reset_token}'. It is valid for one hour.")
    )
  ### Email credentials. Use creds_envvar() to read from environment variable
  gmail_creds <- blastula::creds_envvar(
    user = "flyfishrapp@gmail.com",
    pass_envvar = "GMAIL_APP_PASSWORD",
    host = "smtp.gmail.com",
    port = 587,
    use_ssl = TRUE
  )
  ### Use in smtp_send to send password rest email
  blastula::smtp_send(
    email = message,
    to = email_add,
    from = "flyfishrapp@gmail.com",
    subject = "Password Reset",
    credentials = gmail_creds
  )

}
