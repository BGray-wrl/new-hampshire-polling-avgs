# app.R

library(shiny)



# Source the UI and server from external files
source("ui.R")
source("server.R")

# Run the Shiny app
shinyApp(ui = ui, server = server)


