
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
