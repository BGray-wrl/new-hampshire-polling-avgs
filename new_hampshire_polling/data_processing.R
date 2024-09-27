



## Function to format margin labels:
margin_label <- function(x) {
  sapply(x, function(val) {
    if (is.na(val)) {
      return(NA)  # Handle NA values
    } else if (val > 0) {
      paste0("R+", val)
    } else if (val < 0) {
      paste0("D+", abs(val))
    } else {
      "0"  # Label for zero
    }
  })
}

# Read in Data
poll_avg_master<-read.csv("data/poll_avg_master.csv")%>%
  mutate(date=as.Date(date))
poll_avg_master<-poll_avg_master[,-1]

margins_master<-read.csv("data/margins_master.csv")%>%
  mutate(middle_date=as.Date(middle_date),
         start_date=as.Date(start_date),
         end_date=as.Date(end_date))
margins_master<-margins_master[,-1]

# Magic Numbers/Vars
election_date<-as.Date("2024-11-05")



