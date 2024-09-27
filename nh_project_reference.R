

## Reference file for polling averages calculation for New Hampshire


## Function to clean polls, of all types
clean_multitype_polls<-function(pres_polls_master,republican_candidate,democratic_candidate,state_filter){
  pres_polls<-pres_polls_master%>%
    filter(state==state_filter)%>%
    filter(partisan=="")%>%                         # Only Non-Partisan Polls
    filter(party=="DEM"|party=="REP")%>%               # Only DEM and REP polls
    filter(ranked_choice_reallocated=="FALSE"|ranked_choice_reallocated=="false")%>%   # only head-to-head polls
    # filter(population_full!="a")%>%                 # Only Registered or Likely voters
    mutate(pct=as.numeric(pct),state=ifelse(state=="","National",state))%>% #Normal cleaning
    mutate(numeric_grade=ifelse(is.na(numeric_grade),0.5,numeric_grade))%>% # Give all pollsters without a numeric grade the minimum grade
    mutate(grade_weight=numeric_grade+0.5)%>%
    mutate(end_date=as.Date(end_date, format = "%m/%d/%y"), start_date=as.Date(start_date, format = "%m/%d/%y"))%>% ##
    mutate(middle_date = start_date + (end_date - start_date) / 2)%>% # Calculate the middle date between start and end
    mutate(keep=ifelse(candidate_name==republican_candidate&lag(candidate_name)==democratic_candidate,1,0))
  
  keep<-pres_polls$keep
  for(i in 2:nrow(pres_polls)){
    if(keep[i]==1){
      keep[i-1]=1
    }
  }
  
  pres_polls$keep=keep
  pres_polls<-pres_polls%>%filter(keep==1)
  
  med_sample_size<-median(pres_polls$sample_size,na.rm=T)
  # min_sample_size<-min(pres_polls$sample_size,na.rm=T)
  min_sample_size<-250
  
  pres_polls$sample_size=ifelse(is.na(pres_polls$sample_size),min_sample_size,pres_polls$sample_size)
  # pres_polls$size_weight=sqrt(pres_polls$sample_size)/sqrt(med_sample_size)
  return(pres_polls)
}  


## Function to turn cleaned polls into margins with weighted averages
turn_clean_multitype_polls_into_margins<-function(clean_polls,sim_date){
  
  med_sample_size<-median(clean_polls$sample_size,na.rm=T)
  clean_polls%>%
    group_by(pollster,poll_id,state,,start_date,office_type,seat_name,
             middle_date,end_date,grade_weight)%>%
    # mutate()%>%
    summarize(pct=mean(pct[party=="REP"])-mean(pct[party=="DEM"]),
              # office_type=ifelse(office_type=="U.S. House",
              #                    paste0(office_type," CD",seat_number),
              #                    ifelse(office_type=="U.S. Senate",paste0(office_type," ", seat_name),
              #                           office_type)
              #                    ),
              contest=paste0(office_type,ifelse(is.na(seat_name),"",paste0(" ",seat_name))),
              dem_candidate=answer[party=="DEM"][1],
              rep_candidate=answer[party=="REP"][1],
              sample_size=mean(sample_size),
              count=n(),
              days_behind=as.numeric(sim_date-middle_date),
              size_weight=sqrt(sample_size)/sqrt(med_sample_size),
              date_weight=round(smooth^days_behind,2),
              weight=round(date_weight*size_weight*grade_weight,2),
              .groups = "drop")%>%  # This will drop the grouping after summarizing)
    group_by(poll_id)%>%
    summarize(pollster=pollster[1],state=state[1],office_type=office_type[1],seat_name=seat_name[1], contest=contest[1],
              start_date=start_date[1],middle_date=middle_date[1],end_date=end_date[1],
              sample_size=sample_size[1],
              size_weight=size_weight[1],grade_weight=grade_weight[1],date_weight=date_weight[1],
              MARGIN=pct[1], dem_candidate=dem_candidate[1],rep_candidate=rep_candidate[1]
    )%>%
    arrange(desc(middle_date))
}


## Function to calculate the 'current' weighted average margin, if given a simulation date
calculate_multitype_margin_given_sim_date<-function(pres_margins,sim_date){
  
  poll_average<-pres_margins%>%
    filter(middle_date<=sim_date)%>%
    mutate(days_behind=as.numeric(sim_date-middle_date))%>%
    mutate(date_weight=0.9^days_behind)%>%
    mutate(weight=date_weight*size_weight*grade_weight)
  
  avg<-sum(poll_average$MARGIN*poll_average$weight)/sum(poll_average$weight)
  
  return(avg)
}


## Wrapper function to calculate the 'current' weighted average margins for a past number of days
calculate_multitype_margins_backtrack<-function(pres_margins,sim_date,num_back=14){
  
  back_vals<-c()
  for(i in 0:num_back){
    avg<-calculate_multitype_margin_given_sim_date(pres_margins,sim_date-i)
    # res<-c(res,avg)
    back_vals<-rbind(back_vals,c(sim_date-i,avg))
  }
  
  back_vals<-as.data.frame(back_vals)
  colnames(back_vals)=c("date","margin")
  back_vals$date=as.Date(back_vals$date)
  
  return(back_vals)
}


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






