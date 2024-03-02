library(dplyr)
library(ggplot2)

sess_to_summed <- function(sess_path){
  df <- read.csv(sess_path)
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
  
  dist_num <- 0
  fast_num <- 0
  slow_num <- 0
  focus_num <- 0
  striving_num <- 0
  premark_num <- 0
  out_of_site_num <- 0
  back_to_op_num <- 0
  with_op_num <- 0
  false_num <- 0
  obstacle_num <- 0
  stop_num <- 0
  for(i in 1:nrow(until_mark_df)){
    if(df[i, "behavior"] == "distraction"){
      dist_num = dist_num+1
    }
    if(df[i, "behavior"] == "fast"){
      fast_num = fast_num+1
    }
    if(df[i, "behavior"] == "slow"){
      slow_num = slow_num+1
    }
    if(df[i, "behavior"] == "focus"){
      focus_num = focus_num+1
    }
    if(df[i, "behavior"] == "striving"){
      striving_num = striving_num+1
    }
    if(df[i, "behavior"] == "premark"){
      premark_num = premark_num+1
    }
    if(df[i, "behavior"] == "out_of_site"){
      out_of_site_num = out_of_site_num+1
    }
    if(df[i, "behavior"] == "back_to_op"){
      back_to_op_num = back_to_op_num+1
    }
    if(df[i, "behavior"] == "with_op"){
      with_op_num = with_op_num+1
    }
    if(df[i, "behavior"] == "false"){
      false_num = false_num+1
    }
    if(df[i, "behavior"] == "obstacle"){
      obstacle_num = obstacle_num+1
    }
    if(df[i, "behavior"] == "stop"){
      stop_num = stop_num+1
    }
  }
  dist_prop = dist_num/row_mark
  fast_prop = fast_num/row_mark
  slow_prop = slow_num/row_mark
  focus_prop = focus_num/row_mark
  striving_prop = striving_num/row_mark
  premark_prop = premark_num/row_mark
  out_of_site_prop = out_of_site_num/row_mark
  back_to_op_prop = back_to_op_num/row_mark
  with_op_prop = with_op_num/row_mark
  false_prop = false_num/row_mark
  obstacle_prop = obstacle_num/row_mark
  stop_prop =stop_num/row_mark
  
  #making variables
  behavior <- c("distraction", "fast", "slow", "focus", "striving", "premark", "out_of_site", "back_to_op", "with_op", "false", "obstacle", "stop")
  frequency_in_sess<-c(dist_num,fast_num,slow_num,focus_num,striving_num,premark_num,out_of_site_num,back_to_op_num,with_op_num,false_num,obstacle_num,stop_num)   
  prop<-c(dist_prop,fast_prop,slow_prop,focus_prop,striving_prop,premark_prop,out_of_site_prop,back_to_op_prop,with_op_prop,false_prop,obstacle_prop,stop_prop) 
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

write.csv(merged_df, "../../processed data/merged_spec_behaviors.csv", row.names = F)


 ggplot(merged_df,aes(x=speed, y=prop, color=site))+
   geom_point() +
   xlim(0,25)+
   facet_wrap(vars(behavior))
    
  
  
