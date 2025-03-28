# app.R
library(shiny)


# Set up future plan for asynchronous operations
future::plan(multisession)

# Source your UI and server files
source('ui.R')
source('server.R')

shinyApp(ui = ui, server = server)
