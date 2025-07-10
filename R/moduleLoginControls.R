
loginControlsUI <- function(id) {
  fluidRow(
    ## Log in button
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
          modalButton("Cancel")
        )
      ))
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
          passwordInput(NS(id, "new_password2"), "Re-enter password:", placeholder = "Confirm password")
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

      if (new_password1 == new_password2) {
        showNotification("Account created! You can now login", type = "message")
        removeModal()
      } else {
        showNotification("Password entries do not match", type = "error")
      }
    })

    ## Authentication function
    authenticate_user <- function(username, password) {
      return(username == "flyfishr" && password == "flyfishr")
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
