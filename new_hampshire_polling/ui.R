#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)


# Read in the data
source("data_processing.R")

# Define UI for application that draws a histogram
fluidPage(theme = shinythemes::shinytheme("flatly"),

    # Application title
    tags$div(
      titlePanel("New Hampshire's National Electoral Contests"),
      style = "text-align: center;"
    ),
    tags$h4("In the 2024 Election Cycle",style = "text-align: center;"),

    # fluidRow(
    #   column(2,selectInput("state_select_poll_avg", "Select Average:",
    #                        choices = c("Pres","Gov","CD1","CD2"), selected = "Pres")),
    #   # column(8, tags$div( style = "text-align: center;",tags$p(uiOutput("poll_title")))
    #   # )
    # ),
    fluidRow(
      column(8,offset=2,plotOutput("polling_avg_master",height=650))
    ),
    fluidRow(column(8,offset=2,sliderInput("avg_date_x", "Select Date:",label=NULL, min = min(poll_avg_master$date),max = max(poll_avg_master$date),
                                           value = max(poll_avg_master$date),step=1, timeFormat = "%b %d",ticks=FALSE)
    )),
    # fluidRow(column(8,offset=2,
    #                 # Section for allstate_averages data frame
    #                 HTML("<h4>State Polling Averages</h4>"),
    #                 DTOutput("allstate_averages_table"),
    #                 downloadButton("download_allstate_averages", "Download State Averages Data")
    # )),
    # fluidRow(column(8,offset=2,
    #                 HTML("<h4>Raw Poll Margins</h4>"),
    #                 DTOutput("poll_margins_table"),
    #                 downloadButton("download_poll_margins", "Download Poll Margins Data"),
    # )),
    fluidRow(style = "height: 70px;")
    
)
