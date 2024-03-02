library(dplyr)
library(ggplot2)

sess_to_summed <- function(sess_path){
  
  desired <- c("fast", "slow", "focus", "striving", "premark", "mark")
  undesired <- c("stop", "FALSE", "out_of_site", "distraction")
  nuetral <- c("obstacle", "back_to_op", "with_op", "between")
  
  df <- read.csv(sess_path)
  
  df$type <- ifelse(df$behavior %in% desired, "desired", ifelse(df$behavior %in% undesired, "undesired", "neutral"))
  
  if("mark" %in% df$behavior){
    for (i in 1:nrow(df)) {
      if (df[i, "behavior"] == "mark") {
        mark_time <- df[i, "time"]
        break
      }
    }
    row_mark <- mark_time - 3
    until_mark_df <- df[1:row_mark, ]
  } else{
    until_mark_df <- df
    row_mark <- nrow(df)
  }
  
  #makes new df - until the mark
  
  desired_num <- 0
  undesired_num <- 0
  neutral_num <- 0

  for(i in 1:nrow(until_mark_df)){
    if(df[i, "type"] == "desired"){
      desired_num = desired_num+1
    }
    if(df[i, "type"] == "undesired"){
      undesired_num = undesired_num+1
    }
    if(df[i, "type"] == "neutral"){
      neutral_num = neutral_num+1
    }
  }
  desired_prop = desired_num/row_mark
  undesired_prop = undesired_num/row_mark
  neutral_prop = neutral_num/row_mark
  
  #making variables
  behavior <- c("desired", "undesired", "neutral")
  frequency_in_sess<-c(desired_num,undesired_num,neutral_num)   
  prop<-c(desired_prop,undesired_prop,neutral_prop) 
  total <- row_mark
  behaviors_df <- data.frame(behavior, frequency_in_sess,prop,total)
  
  return(behaviors_df)
}

big_df <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(big_df) <- c("behavior", "frequency_in_sess", "prop", "total")


behaviors_dir_path <- "./behaviors"
dates <- list.files(behaviors_dir_path)

for(date in dates){
  date_path <- paste0(behaviors_dir_path, "/", date)
  runs <- list.files(date_path)
  
  for(run in runs){
    run_path <- paste0(date_path, "/", run)
    sesses <- list.files(run_path)
    
    for(sess in sesses){
      if(sess != "template.csv"){
        sess_path <- paste0(run_path, "/", sess)
        behaviors_df <- sess_to_summed(sess_path)
        
        behaviors_df$sess_name <- sess
        
        big_df <- rbind(big_df, behaviors_df)
        
      }
    }
  }
}

united_path <- "united.csv"
united_df <- read.csv(united_path)
united_df$sess_name <- paste0(united_df$dog, "_", united_df$date, "_", as.character(united_df$run), "_", as.character(united_df$sess), ".csv")

merged_df <- merge(big_df, united_df, on = sess_name)

write.csv(merged_df, "good_or_bad_behaviors.csv", row.names = F)

ggplot(merged_df,aes(x=radiation, y=prop, color=site))+
  geom_point() +
  facet_wrap(vars(behavior))






