
library(tidyverse)

source("nh_project_reference.R")


## Read in polling data from 538
house_polls<-read.csv("data/house_polls.csv")
senate_polls<-read.csv("data/senate_polls.csv")
governor_polls<-read.csv("data/governor_polls.csv")
pres_polls<-read.csv("data/president_polls.csv")

# Set params
CURRENT_DATE<-Sys.Date()
smooth<-0.9


# Clean the polls
house_polls_CD1<-clean_multitype_polls(house_polls, "Russell Prescott", "Chris Pappas", "New Hampshire")
house_polls_CD2<-clean_multitype_polls(house_polls,"Lily Tang Williams", "Maggie Goodlander", "New Hampshire")
governor_polls<-clean_multitype_polls(governor_polls, "Kelly Ayotte", "Joyce Craig", "New Hampshire")
pres_polls<-clean_multitype_polls(pres_polls, "Donald Trump", "Kamala Harris", "New Hampshire")


# Calculate Margins and Weights
house_margins_CD1<-turn_clean_multitype_polls_into_margins(house_polls_CD1,CURRENT_DATE)
house_margins_CD2<-turn_clean_multitype_polls_into_margins(house_polls_CD2,CURRENT_DATE)
governor_margins<-turn_clean_multitype_polls_into_margins(governor_polls,CURRENT_DATE)
pres_margins<-turn_clean_multitype_polls_into_margins(pres_polls,CURRENT_DATE)

margins_master<-rbind(house_margins_CD1,house_margins_CD2,governor_margins,pres_margins)

# Calculate poll averages (going back 3-ish weeks)
poll_avg_CD1<-calculate_multitype_margins_backtrack(house_margins_CD1,CURRENT_DATE,21)
poll_avg_CD2<-calculate_multitype_margins_backtrack(house_margins_CD2,CURRENT_DATE,21)
poll_avg_gov<-calculate_multitype_margins_backtrack(governor_margins,CURRENT_DATE,21)
poll_avg_pres<-calculate_multitype_margins_backtrack(pres_margins,CURRENT_DATE,21)

poll_avg_master<-data.frame(date=poll_avg_CD1$date,
                            cd1=poll_avg_CD1$margin,
                            cd2=poll_avg_CD2$margin,
                            gov=poll_avg_gov$margin,
                            pres=poll_avg_pres$margin)


# write.csv(margins_master,"new_hampshire_polling/data/margins_master.csv")
# write.csv(poll_avg_master,"new_hampshire_polling/data/poll_avg_master.csv")


