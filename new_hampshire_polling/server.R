#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shadowtext)
library(scales)
library(tidyverse)

# source("data_processing.R")


# Define server logic required to draw a histogram
function(input, output, session) {
  
  output$polling_avg_master <- renderPlot({
    
    poll_avg_data<-poll_avg_master
    margins_data<-margins_master%>%
      filter(middle_date>=as.Date("2024-09-01"))
    
    x_coord<-max(input$avg_date_x)
    # x_coord<-max(poll_avg_data$date)
    
    # Y_coord<-poll_avg_pres[poll_avg_pres$date==x_coord,]$margin
    ymax = max(-1,max(poll_avg_data$cd1,na.rm=T),max(poll_avg_data$cd2,na.rm=T),
               max(poll_avg_data$gov,na.rm=T),max(poll_avg_data$pres,na.rm=T)
               ,max(margins_data$MARGIN,na.rm = T),na.rm=T)
    
    p<-poll_avg_data%>%
      ggplot(aes(x=date))+
      geom_line(aes(y = cd1, color = "U.S. House District 1")) +
      geom_line(aes(y = cd2, color = "U.S. House District 2")) +
      geom_line(aes(y = gov, color = "Governor")) +
      geom_line(aes(y = pres, color = "U.S. President")) +
      geom_hline(yintercept = 0)+
      geom_point(data=margins_data,aes(x=middle_date,y=MARGIN,color=contest),shape=1) +
      scale_y_continuous(labels=margin_label)+
      theme_minimal() +
      labs(x = "Date", color=" ", title="New Hampshire Polling Averages",
           subtitle="unadjusted and weighted by size, recency, and pollster quality",
           y = "Margin/Estimate")+ 
      
      geom_vline(xintercept = election_date, linetype = "solid", size = 0.4) +
      annotate("text", x = election_date-2, y = ymax, label = "Election", vjust = 1, hjust = 1, angle = 0, size = 5)+
      
      theme_minimal() +
      theme(
        axis.title.y = element_blank(),
        axis.text.x = element_text(hjust = 0.5, size=12,color="black"),
        axis.text.y = element_text(vjust = 0.5,hjust=0, size=16,color="black"),
        axis.text.y.right = element_text(size=15,color="black"),
        legend.position = "bottom",
        panel.grid.major.x = element_blank(),  # Remove vertical grid lines
        panel.grid.minor.x = element_blank(),  # Remove minor vertical grid lines
        panel.grid.minor.y = element_blank(),  # Optional: Remove minor horizontal grid lines
        panel.grid.major.y = element_line(color = "lightgray"),  # Change horizontal grid line color for visibility
        axis.ticks.x = element_line(color = "black"),  # Add axis ticks
        axis.line.y = element_blank(),  # Add left y-axis line
        axis.line.x = element_line(color="black")  # Add bottom x-axis line
      )
    
    contest_short <- c("cd1","cd2","gov","pres")
    contest_colors<-c("#7CAE00","#00BFC4","#F8766D","#C77CFF")
    
    for (i in 1:4) {
      y_value <- poll_avg_data[[contest_short[i]]][poll_avg_data$date == x_coord]
      if (!is.na(y_value)) {
        p <- p + annotate("point", shape = 21, x = x_coord, y = y_value, fill = contest_colors[i], color = "black", size = 4) +
          annotate("shadowtext", x = x_coord, y = y_value, color = contest_colors[i], bg.color = "white", size = 4.5,
                   label = paste0(margin_label(round(y_value, 2))), hjust = -0.25)
      }
    }
    
    p
  })

  # output$polling_avg_pres <- renderPlot({
  #   
  #   poll_avg_pres
  #   
  #   # x_coord<-max(input$avg_date_x)
  #   # Y_coord<-poll_avg_pres[poll_avg_pres$date==x_coord,]$margin
  #   ymax = max(-1,max(poll_avg_pres$margin,na.rm=T),max(pres_margins$MARGIN,na.rm = T),na.rm=T)
  #   
  #   p<-poll_avg_pres%>%
  #     ggplot(aes(x=date))+
  #     geom_line(aes(y = margin, color = "avg")) +
  #     geom_hline(yintercept = 0)+
  #     geom_point(data=pres_margins,aes(x=middle_date,y=MARGIN,color="poll"),shape=1) +
  #     scale_y_continuous(labels=margin_label)+
  #     theme_minimal() +
  #     labs(x = "Date", color=" ",
  #          y = "Margin/Estimate")+ 
  #     scale_color_manual(values = c("avg" = "darkblue","poll" = "gray"))+
  #     # geom_vline(xintercept = election_date, linetype = "solid", size = 0.4) +
  #     # annotate("text", x = election_date-2, y = ymax, label = "Election", vjust = 1, hjust = 1, angle = 0, size = 5)+
  #     theme_minimal() +
  #     theme(
  #       axis.title.y = element_blank(),
  #       axis.text.x = element_text(hjust = 0.5, size=12,color="black"),
  #       axis.text.y = element_text(vjust = 0.5,hjust=0, size=16,color="black"),
  #       axis.text.y.right = element_text(size=15,color="black"),
  #       legend.position = "bottom",
  #       panel.grid.major.x = element_blank(),  # Remove vertical grid lines
  #       panel.grid.minor.x = element_blank(),  # Remove minor vertical grid lines
  #       panel.grid.minor.y = element_blank(),  # Optional: Remove minor horizontal grid lines
  #       panel.grid.major.y = element_line(color = "lightgray"),  # Change horizontal grid line color for visibility
  #       axis.ticks.x = element_line(color = "black"),  # Add axis ticks
  #       axis.line.y = element_blank(),  # Add left y-axis line
  #       axis.line.x = element_line(color="black")  # Add bottom x-axis line
  #     )
  #   
  #   # p<- p+   geom_line(aes(y = avg_margin,color=" Polling Average"),size=1) +
  #   #   annotate("point",shape=21,x = x_coord, y=Yme,fill="darkblue",color="black",size=4)+
  #   #   annotate("shadowtext",x = x_coord,y=Yme,color="darkblue",bg.color="white", size=4.5,
  #   #            label=paste0(margin_label(round(Yme,2))),hjust=-0.25)
  #   
  #   p
  #   
  # })  
  # 

}
  
  
