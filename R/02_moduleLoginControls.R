## Module for the login UI and server code

loginControlsUI <- function(id) {
  fluidRow(
    ### Login button UI
    div(style = "text-align: right; margin-top: 10px;",
      ### Button
      uiOutput(NS(id, "login_logout_btn")),
      ### Display logged in status
      textOutput(NS(id, "login_status")),
      uiOutput(NS(id, "delete_account"))
    )
  )
}

loginControlsServer <- function(id, pool) {
  moduleServer(id, function(input, output, session) {

    ### Reactive value to keep track of logged in status
    logged_in <- reactiveVal(list(FALSE, NULL))
    ### Conditionally display either log-in or log-out button
    output$login_logout_btn <- renderUI({
      if (logged_in()[[1]]) {
        actionButton(NS(id, "logout"), "Logout",
                     style ="background-color: #0EA5E9BF; border-color: #000000;")
      } else {
        actionButton(NS(id, "login"), "Login",
                     style = "background-color: #0EA5E9BF; border-color: #000000;")
      }
    })

    ### Conditionally show delete account link if user is logged in
    output$delete_account <- renderUI({
      if (logged_in()[[1]]) {
        actionLink(NS(id, "delete_account_btn"), "Delete account")
      }
    })

    ### Show modalDialog for logging in
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

    ### Get email from user to send password reset token
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

    ### Generate reset token
    generate_reset_token <- function() {
      sample(100000:999999, 1)
    }
    ### Reactive value that stores the password reset token
    reset_token <- reactiveVal()
    reset_token(generate_reset_token())

    ### Send email to user with password reset token when the
    ### "Get password reset token" button is pressed.
    observeEvent(input$get_pw_reset_token, {
      ## Get the user supplied email address to pass into passwordReset function
      email_add <- input$email_for_pw_reset
      passwordReset(pool, reset_token(), email_add)
      ## Notify the user that the email has been sent, and show modal dialog
      ## to allow the user to enter the password reset token
      showNotification("Password reset email sent. The reset token expires in one hour",
                       type = "message")
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
        ### Remove previous modal
        removeModal()
        ### Show new modal for creating new password
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

    ### When the user submits their new password, update the ACCOUNT_INFO database
    observeEvent(input$submit_new_password, {
      ### If the reset token entered matches the reset token in the database,
      ### update the password
      if (input$token_submitted == reset_token()) {
        ### Hash the password
        hashed_pw <- hashpw(input$reset_password1, salt = gensalt())
        ### Capture reset token
        rt <- reset_token()
        ### Update password in the database
        dbExecute(pool,
           "UPDATE ACCOUNT_INFO
           SET PASSWORD = ?
           WHERE RESET_TOKEN = ?;",
           params = list(hashed_pw, rt))
        ### Show notification saying password was updated
        showNotification("Password updated!", type = "message")
        ### Remove modal
        removeModal()
      } else {
        ### If the reset token entered does not match the reset token in the
        ### database, show notification saying that the reset toke in incorrect
        showNotification("Incorrect token entered", type = "error")
      }
    })

    ### Get email from user to send forgot username email
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

    ### When the user enters their email address, send them their username
    observeEvent(input$request_username, {
      email_add <- input$email_for_forgot_username
      sendUsername(pool, email_add)
    })

    ### Handle login attempt
    observeEvent(input$submit_login, {
      username <- input$username
      password <- input$password

      ### Authenticate credentials. If authenticated log in the user.
      if (authenticate_user(username, password)) {
        removeModal()
        ### Capture user id entered
        user_id <- dbGetQuery(pool,
                              "SELECT USER_ID FROM ACCOUNT_INFO
                              WHERE USERNAME = ?;",
                              params = list(username)
                              )[1,1]
        ### Update logged_in() reactive with user id and logged in status
        logged_in(list(TRUE, user_id))
        ### Show notification saying that log in was successfull
        showNotification("Login successful!", type = "message")
      } else {
        ### If user is not authenticated, show notification saying invalid credentials
        showNotification("Invalid credentials", type = "error")
      }
    })

    ### Handle create new account
    observeEvent(input$create_account, {
      removeModal()
      showModal(modalDialog(
        title = "Create new account",
        div(
          textInput(NS(id, "new_username"), "Username:", placeholder = "Pick a username"),
          passwordInput(NS(id, "new_password1"), "Password:", placeholder = "Pick a password"),
          passwordInput(NS(id, "new_password2"), "Confirm password:", placeholder = "Confirm password"),
          textInput(NS(id, "email"), "Email:", placeholder = "Enter email addresss")
        ),
        footer = tagList(
          actionButton(NS(id, "submit_new_credentials"), "Create account"),
          modalButton("Cancel")
        )
      ))
    })

    ### Check new credentials
    observeEvent(input$submit_new_credentials, {
      ### If any of the required fields for account creation are not filled out,
      ### show an error message.
      if (input$new_username == "") {
        showNotification("Username is required", type = "error")
      } else if (input$new_password1 == "" | input$new_password2 == "") {
        showNotification("You must fill out both the Password and Confirm Password fields", type = "error")
      } else if (input$email == "") {
        showNotification("Email is required", type = "error")
      } else {
        ### Capture the username, password, password confirmation, and email for
        ### a new account
        new_username <- input$new_username
        new_password1 <- input$new_password1
        new_password2 <- input$new_password2
        new_email <- as.character(input$email)
        ### If the password and password confirmation that were entered match,
        ### update the database
        if (new_password1 == new_password2) {
          ### Fetch all usernames from the database
          usernames <- as.character(dbGetQuery(pool,
                                               "SELECT USERNAME FROM ACCOUNT_INFO;")[,1])
          ### Fetch all emails from the database
          emails <- as.character(dbGetQuery(pool,
                                            "SELECT EMAIL FROM ACCOUNT_INFO;")[,1])
          ### If the username entered matches the username for an existing account,
          ### show notification that the username is already taken
          if (new_username %in% usernames) {
            showNotification("That username is already taken. Please choose another one",
                             type = "error")
            ### If the email entered matches the email for an existing account,
            ### show notification that the email is already associated with an account
          } else if (new_email %in% emails) {
            showNotification("There is already an account for that email", type = "error")
            ### If the username and email entered are available, create the new account
          } else {
            ### Hash the password entered
            hashed_pw <- bcrypt::hashpw(new_password1, salt = gensalt())
            ### Insert account information into the database
            dbExecute(pool,
                      "INSERT INTO ACCOUNT_INFO
                      (USERNAME, PASSWORD, EMAIL, DATE_CREATED, ACCESS)
                      VALUES (?, ?, ?, ?, ?);",
                      params = list(
                        new_username,
                        hashed_pw,
                        new_email,
                        as.character(Sys.Date()),
                        'user'
                      )
            )
            ### Show notification that the account was created
            showNotification("Account created! You can now login", type = "message")
            ### Remove modal
            removeModal()
          }
          ### If the password and the password confirmation that were entered do not
          ### match, show notification
        } else {
          showNotification("Password entries do not match", type = "error")
        }
      }
    })

    ### Authentication function
    authenticate_user <- function(username, password) {
      ### Fetch all usernames from the database
      usernames <- as.character(dbGetQuery(pool,
                              "SELECT USERNAME FROM ACCOUNT_INFO;")[,1])
      ### Fetch all passwords from the database
      passwords <- as.character(dbGetQuery(pool,
                              "SELECT PASSWORD FROM ACCOUNT_INFO;")[,1])
      ### Fetch the hash for the password
      hash <- as.character(dbGetQuery(pool,
                                           "SELECT PASSWORD FROM ACCOUNT_INFO
                                           WHERE USERNAME = ?;",
                                           params = list(username)))
      ### Set logical authentication status values for username and password
      username_auth <- FALSE
      password_auth <- FALSE
      ### Check if username is in the database and set authentication status to TRUE
      ### if it is
      if (username %in% usernames) {
        username_auth = TRUE
      }
      ### Check if the password entered matches the hash fetched from the database
      ### and set authentication status to TRUE if it does
      if (checkpw(password, hash)) {
        password_auth = TRUE
      }
      ### Return authentication status
      return(username_auth && password_auth)
    }

    ### Display login status in the UI
    output$login_status <- renderText({
      if (logged_in()[[1]]) {
        paste0("Logged in as: ", input$username)
      } else {
        ""
      }
    })

    ### Handle log out
    observeEvent(input$logout, {
      logged_in(list(FALSE, NULL))
    })

    ### Show modal asking user if they are sure they want to delete their account
    observeEvent(input$delete_account_btn, {
      showModal(modalDialog(
        title = "Are you sure you want to delete your account?",
        "Deleting your account will remove all of your account information and Fish Log data.
        This action cannot be undone.",
        footer = tagList(
          actionButton(NS(id, "confirm_delete_account"), "Yes, delete my account",
                       style = "background-color: #F87171BF; border-color: #000000;"),
          modalButton("cancel")
        )
      ))
    })

    ### Handle deletion of account
    observeEvent(input$confirm_delete_account, {
      ### Capture user id
      user_id <- logged_in()[[2]]
      ### Delete data from FISH_LOG table for that user id
      dbExecute(pool,
                "DELETE FROM FISH_LOG
                WHERE USER_ID = ?;",
                params = list(user_id)
                )
      ### Delete data from ACCOUNT_INFO table for that user id
      dbExecute(pool,
                "DELETE FROM ACCOUNT_INFO
                WHERE USER_ID = ?;",
                params = list(user_id)
                )
      ### Log them out of the app
      logged_in(list(FALSE, NULL))
      removeModal()
      ### Show notification that account has been deleted
      showNotification("You account has been deleted and you have been logged out.", type = "message")
    })

    ### Return logged_in() reactive so that it can be used in other module
    return(reactive({
      logged_in()
    }))
  })
}
