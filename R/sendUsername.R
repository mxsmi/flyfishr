## Function for sending an email to the user with their username if they forgot it

sendUsername <- function(pool, email_add) {

  ### Fetch emails for all accounts
  emails <- as.character(dbGetQuery(pool,
                                    "SELECT EMAIL FROM ACCOUNT_INFO;")[,1])
  ### Check if entered email is associated with existing account. If not, show a
  ### Notification.
  if (!email_add %in% emails) {
    showNotification("That email is not associated with an account", type = "error")
  } else {
    ### Get email address entered by user fetch username associated with that email
    ### from the database
    user <- dbGetQuery(pool,
                        "SELECT USERNAME FROM ACCOUNT_INFO WHERE EMAIL = ?;",
                        params = list(email_add)
    )[,1]
    ### Compose password reset email
    message <- blastula::compose_email(
      glue::glue("The username for your flyfishr account is: {user}")
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
      subject = "Username request",
      credentials = gmail_creds
    )
    ### Remove modal
    removeModal()
    ### Show notification that email was sent
    showNotification(glue::glue("An email containing your username was sent to {email_add}"), type = "message")
  }

}
