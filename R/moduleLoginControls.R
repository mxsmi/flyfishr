
loginControlsUI <- function(id) {
  fluidRow(

    ## login button
    div(style = "text-align: right; margin-top: 10px;",
      uiOutput(NS(id, "login_logout_btn")),
      textOutput(NS(id, "login_status"))
    )
  )
}

loginControlsServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    ## Reactive value to keep track of logged in status
    logged_in <- reactiveVal(FALSE)

    ## Conditionally display either log-in or log-out button
    output$login_logout_btn <- renderUI({
      if (logged_in()) {
        actionButton(NS(id, "logout"), "Logout",
                     class = "btn-secondary",
                     style = "background-color: #0EA5E9; color: black !important; border-color: #000000; border-width: 2px;")
      } else {
        actionButton(NS(id, "login"), "Login",
                     class = "btn-secondary",
                     style = "background-color: #0EA5E9; color: black !important; border-color: #000000; border-width: 2px;")
      }
    })

    ## Show modalDialog for logging in
    observeEvent(input$login, {
      showModal(modalDialog(
        title = "Enter credentials",
        div(
          textInput(NS(id, "username"), "Username:", placeholder = "Enter username"),
          passwordInput(NS(id, "password"), "Password:", placeholder = "Enter password")
        ),
        footer = tagList(
          actionButton(NS(id, "submit_login"), "Login"),
          actionButton(NS(id, "create_account"), "Create an account"),
          modalButton("Cancel"),
          br(),
          actionLink(NS(id, "forgot_password"), "Forgot password?"),
          actionLink(NS(id, "forgot_username"), "Forgot username?")
        )
      ))
    })

    ## Get email from user to send password reset token
    observeEvent(input$forgot_password, {
      removeModal()
      showModal(modalDialog(
        title = "Enter email address",
        textInput(NS(id, "email_for_pw_reset"), "Email:"),
        footer = tagList(
          actionButton(NS(id, "get_pw_reset_token"), "Get password reset token"),
          modalButton("Cancel")
        )
      ))
    })

    ## Generate reset token
    generate_reset_token <- function() {
      paste0(sample(c(letters, LETTERS, 0:9), 32, replace = TRUE), collapse = "")
    }

    ## Reactive value that stores the password reset token
    reset_token <- reactiveVal()
    reset_token(generate_reset_token())

    # ## Send email to user with password reset token when the
    # ## "Get password reset token" is pressed.
    observeEvent(input$get_pw_reset_token, {

      ## Get the user supplied email address to pass into passwordReset function
      email_add <- input$email_for_pw_reset
      passwordReset(reset_token(), email_add)

      ## Notify the user that the email has been sent, and show modal dialog
      ## to allow the user to enter the password reset token
      showNotification("Password reset request submitted. Please check email for token")
      removeModal()
      showModal(modalDialog(
        title = "Enter password reset token",
        textInput(NS(id, "token_submitted"), "Reset token:"),
        footer = tagList(
          actionButton(NS(id, "submit_reset_token"), "Submit"),
          modalButton("cancel")
        )
        ))
    })

    ## When the user inputs the password reset token, show modal dialog to allow
    ## the user to create a new password.
    observeEvent(input$submit_reset_token, {
      if (input$token_submitted == reset_token()) {

        removeModal()
        showModal(modalDialog(
          title = "Create new password",
          div(
            passwordInput(NS(id, "reset_password1"), "Password:", placeholder = "Pick a password"),
            passwordInput(NS(id, "reset_password2"), "Re-enter password:", placeholder = "Confirm password"),
          ),
          footer = tagList(
            actionButton(NS(id, "submit_new_password"), "Change password"),
            modalButton("Cancel")
          )
        ))
      }
    })

    ## When the user submits their new password, update the ACCOUNT_INFO SQL database
    observeEvent(input$submit_new_password, {
      if (input$token_submitted == reset_token()) {
        conn <- DBI::dbConnect(MariaDB(),
                          host = Sys.getenv("DB_HOST"),
                          port = Sys.getenv("DB_PORT"),
                          user = Sys.getenv("DB_USER"),
                          password = Sys.getenv("DB_PASSWORD"),
                          dbname = Sys.getenv("DB_NAME")
        )

        rt <- reset_token()
        DBI::dbExecute(conn,
           "UPDATE ACCOUNT_INFO
           SET PASSWORD = ?
           WHERE RESET_TOKEN = ?;",
           params = list(input$reset_password1, rt))
        DBI::dbDisconnect(conn)
        showNotification("Password updated!")
        removeModal()
      } else {
        showNotification("Incorrect token entered", type = "error")
      }
    })

    ## Get email from user to send forgot username email
    observeEvent(input$forgot_username, {
      removeModal()
      showModal(modalDialog(
        title = "Enter email address",
        textInput(NS(id, "email_for_forgot_username"), "Email:"),
        footer = tagList(
          actionButton(NS(id, "request_username"), "Enter"),
          modalButton("Cancel")
        )
      ))
    })

    ## When the user enters their email address, send them their username
    observeEvent(input$request_username, {
      email_add <- input$email_for_forgot_username
      sendUsername(email_add)
    })

    ## Handle login attempt
    observeEvent(input$submit_login, {
      username <- input$username
      password <- input$password

      ## Authenticate credentials
      if (authenticate_user(username, password)) {
        removeModal()
        logged_in(TRUE)
        showNotification("Login successful!", type = "message")
        ## Update UI for logged-in state
      } else {
        showNotification("Invalid credentials", type = "error")
        ## Keep modal open for retry
      }
    })

    ## Handle create new account
    observeEvent(input$create_account, {
      removeModal()
      showModal(modalDialog(
        title = "Create new account",
        div(
          textInput(NS(id, "new_username"), "Username:", placeholder = "Pick a username"),
          passwordInput(NS(id, "new_password1"), "Password:", placeholder = "Pick a password"),
          passwordInput(NS(id, "new_password2"), "Re-enter password:", placeholder = "Confirm password"),
          textInput(NS(id, "email"), "Email:", placeholder = "Enter email addresss")
        ),
        footer = tagList(
          actionButton(NS(id, "submit_new_credentials"), "Create account"),
          modalButton("Cancel")
        )
      ))
    })

    ## Check new credentials
    observeEvent(input$submit_new_credentials, {
      new_username <- input$new_username
      new_password1 <- input$new_password1
      new_password2 <- input$new_password2
      new_email <- as.character(input$email)

      if (new_password1 == new_password2) {

        conn <- DBI::dbConnect(MariaDB(),
                          host = Sys.getenv("DB_HOST"),
                          port = Sys.getenv("DB_PORT"),
                          user = Sys.getenv("DB_USER"),
                          password = Sys.getenv("DB_PASSWORD"),
                          dbname = Sys.getenv("DB_NAME")
        )

        usernames <- as.character(DBI::dbGetQuery(conn,
                                             "SELECT USERNAME FROM ACCOUNT_INFO")[,1])
        emails <- as.character(DBI::dbGetQuery(conn,
                             "SELECT EMAIL FROM ACCOUNT_INFO")[,1])

        if (new_username %in% usernames) {
          showNotification("That username is already taken. Please choose another one",
                           type = "error")
        } else if (new_email %in% emails) {
          showNotification("There is already an account for that email", type = "error")
        } else {
          user_num <- DBI::dbGetQuery(conn,
            "SELECT USER_ID
            FROM ACCOUNT_INFO
            ORDER BY USER_ID")[,1]

          DBI::dbExecute(conn,
                      "INSERT INTO ACCOUNT_INFO
                      (USER_ID, USERNAME, PASSWORD, EMAIL, DATE_CREATED, ACCESS)
                      VALUES (?, ?, ?, ?, ?, ?)",
                      params = list(user_num[length(user_num)] + 1,
                      new_username,
                      new_password1,
                      new_email,
                      as.character(Sys.Date()),
                      'user'
                    )
          )

          DBI::dbDisconnect(conn)
          showNotification("Account created! You can now login", type = "message")
          removeModal()
        }
      } else {
        showNotification("Password entries do not match", type = "error")
      }
    })

    ## Authentication function
    authenticate_user <- function(username, password) {

      conn <- DBI::dbConnect(MariaDB(),
                        host = Sys.getenv("DB_HOST"),
                        port = Sys.getenv("DB_PORT"),
                        user = Sys.getenv("DB_USER"),
                        password = Sys.getenv("DB_PASSWORD"),
                        dbname = Sys.getenv("DB_NAME")
      )

      usernames <- as.character(DBI::dbGetQuery(conn,
                              "SELECT USERNAME FROM ACCOUNT_INFO")[,1])

      passwords <- as.character(DBI::dbGetQuery(conn,
                              "SELECT PASSWORD FROM ACCOUNT_INFO")[,1])

      username_auth <- FALSE
      password_auth <- FALSE

      if (username %in% usernames) {
        username_auth = TRUE
      }
      if (password %in% passwords) {
        password_auth = TRUE
      }
      DBI::dbDisconnect(conn)
      return(username_auth && password_auth)
    }

    ## Display login status
    output$login_status <- renderText({
      if (logged_in()) {
        paste0("Logged in as: ", input$username)
      } else {
        ""
      }
    })

    ## Handle log out
    observeEvent(input$logout, {
      logged_in(FALSE)
    })
  })
}
