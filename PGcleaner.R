##This file will attempt to clean a raw csv downloaded from the PG website. The goal will be to produce a product that shows the doctor's name, the date of the evaluation, and the time, as well. This will list each evaluation separately and will indicate via a T/F if that eval was considered 'Top Box'. This will also display any potential demographics, like the race, sex, and age. In order to input the correct format, the specifications are:

PG_cleaner <- function(path = "", first_skip = 0, second_skip = 0, top_until = 0, comment_path = 0, comm_skip = 0) {
  #Loads libraries.
  library(lubridate)
  library(dplyr)
  #Gets the csv and skips the extra data at the top.
  top_rows <- top_until - (first_skip + 1)
  top_frame <- read.csv(path, skip = first_skip, nrows = top_rows, stringsAsFactors = FALSE)
  #Removes total values.
  top_frame <- filter(top_frame, !((IT.Admit.time == 'Total') | (IT.admit.date == 'Total')))
  #Removes percentage and total columns. 
  top_frame <- select(top_frame, -Service, -Questions, -Total.n, -(Very.Poor..:Very.Good..))
  #Now we will change the time into hours and minutes.
  top_frame[, 'IT.Admit.time'] <- as.numeric(top_frame[, 'IT.Admit.time'])
  top_frame[, 'IT.Admit.time'] <- hm(top_frame[, 'IT.Admit.time']/100)
  #Next we will format the times to create a single datetime object and use that as a column of data. This also assigns even and odd days.
  day.vector <- vector()
  for (i in 1:nrow(top_frame)) {
    temp.char.list <- strsplit(top_frame$IT.admit.date[i], ' ')
    temp.char.vector <- temp.char.list[[1]]
    #day.vector <- c(day.vector, day(mdy(temp.char.vector[1])))
    top_frame$IT.admit.date[i] <- as_date(as.numeric(mdy(temp.char.vector[1])))
  }
  top_frame[, 'IT.admit.date'] <- as_date(as.numeric(top_frame[, 'IT.admit.date']))
  top_frame <- mutate(top_frame, Timedate = as.POSIXct(IT.admit.date + IT.Admit.time))
  top_frame <- select(top_frame, -IT.admit.date, -IT.Admit.time)
  top_frame <- mutate(top_frame, TopBox = (Very.Good.n > 0))
  top_frame <- arrange(top_frame, Timedate)
  #Next we will need to make sure that there are no two timedates that are the same.
  cleaned_top_frame <- distinct(top_frame, Timedate, .keep_all = TRUE)
  #Now we will add columns for our next several variables - Attending, Age, Sex, Race, Time Spent in the ED.
  joined_frame <- mutate(cleaned_top_frame, Doc = NA, Patient.Age = NA, Sex = NA, Race = NA, PGLOS = NA)
  #Now we bring in the demographics frame from the bottom part of the csv file.
  demo_frame <- PG_demo_cleaner(path, second_skip)
  #Finally, we will create loop functions that assign values to each of the new variables within the joined frame.
  for (i in 1:nrow(joined_frame)) {
    temp_time_frame <- filter(demo_frame, Timedate == joined_frame[i, 'Timedate'])
    for (j in  1:nrow(temp_time_frame)) {
      if (temp_time_frame[j, 'Demographic'] == 'Age') {
        joined_frame[i, 'Patient.Age'] <- temp_time_frame[j, 'Value']
      }
      else if (temp_time_frame[j, 'Demographic'] == 'IT ATTNNAME') {
        joined_frame[i, 'Doc'] <- temp_time_frame[j, 'Value']
      }
      else if (temp_time_frame[j, 'Demographic'] == 'IT Race') {
        joined_frame[i, 'Race'] <- temp_time_frame[j, 'Value']
      }
      else if (temp_time_frame[j, 'Demographic'] == 'Sex') {
        joined_frame[i, 'Sex'] <- temp_time_frame[j, 'Value']
      }
      else if (temp_time_frame[j, 'Demographic'] == 'Time spent in the ED') {
        joined_frame[i, 'PGLOS'] <- temp_time_frame[j, 'Value']
      }
    }
  }
  #Now we will standardize the races listed.
  white <- c('White', 'WHITE', 'W')
  black <- c('B', 'BLACK AFRICAN AMERICAN', 'Black or African American')
  asian <- c('ASIAN', 'S')
  pac_isl <- c('N', 'NATIVE HAWAIIAN OTH PACIFIC IS')
  for (i in 1:nrow(joined_frame)) {
    if(joined_frame[i, 'Race'] %in% white) {
      joined_frame[i, 'Race'] <- 'W'
    }
    else if(joined_frame[i, 'Race'] %in% black) {
      joined_frame[i, 'Race'] <- 'B'
    }
    else if(joined_frame[i, 'Race'] %in% asian) {
      joined_frame[i, 'Race'] <- 'S'
    }
    else if(joined_frame[i, 'Race'] %in% pac_isl) {
      joined_frame[i, 'Race'] <- 'N'
    }
    else {joined_frame[i, 'Race'] <- NA}
  }
  #Now we will change the column names for easier joining. This should work with a full_join function now.
  joined_frame <- rename(joined_frame, ARR = Timedate)
  #Now we add in the designation of odd and even days.
  for (i in 1:nrow(joined_frame)) {
    temp_day <- day(joined_frame[i, 'ARR'])
    if (hour(joined_frame[i, 'ARR']) < 7) {
      temp_day <- temp_day + 1
    }
    day.vector <- c(day.vector, temp_day)
  }
  day.vectorTF <- (day.vector%%2 == 0)
  joined_frame <- mutate(joined_frame, "Even.Day" = day.vectorTF)
  if(comment_path != 0) {
    comment_frame <- PG_comment_cleaner(comment_path, comm_skip)
  }
  return(joined_frame)
}

#This function cleans the demographic part of the PG. Make sure to only start the csv file where the demographics begin!

PG_demo_cleaner <- function(path, skip_num = 0) {
  #This function will take the demographic part of the PG data and reorganize and clean it so that it can be matched up with the question analysis.
  #Loads libraries.
  library(lubridate)
  library(dplyr)
  #Gets the csv and skips the extra data at the top.
  uncleaned_frame <- read.csv(path, skip = skip_num, stringsAsFactors = FALSE)
  #Removes total values.
  uncleaned_frame <- filter(uncleaned_frame, !((IT.Admit.time == 'Total') | (IT.admit.date == 'Total')))
  #Removes excess columns.
  uncleaned_frame <- select(uncleaned_frame, -Service, -(contains('X.')))
  uncleaned_frame <- filter(uncleaned_frame, n != 0)
  part_cleaned_frame <- select(uncleaned_frame, -n)
  #Now we will change the time into hours and minutes.
  part_cleaned_frame[, 'IT.Admit.time'] <- as.numeric(uncleaned_frame[, 'IT.Admit.time'])
  part_cleaned_frame[, 'IT.Admit.time'] <- hm(part_cleaned_frame[, 'IT.Admit.time']/100)
  #Next we will format the times to create a single datetime object and use that as a column of data.
  for (i in 1:nrow(part_cleaned_frame)) {
    temp.char.list <- strsplit(part_cleaned_frame$IT.admit.date[i], ' ')
    temp.char.vector <- temp.char.list[[1]]
    part_cleaned_frame$IT.admit.date[i] <- as_date(as.numeric(mdy(temp.char.vector[1])))
  }
  part_cleaned_frame[, 'IT.admit.date'] <- as_date(as.numeric(part_cleaned_frame[, 'IT.admit.date']))
  final_cleaned_frame <- mutate(part_cleaned_frame, Timedate = as.POSIXct(IT.admit.date + IT.Admit.time))
  final_cleaned_frame <- select(final_cleaned_frame, -IT.admit.date, -IT.Admit.time)
  #final_cleaned_frame[,'Patient.Age'] <- as.integer(final_cleaned_frame[,'Patient.Age'])
  final_cleaned_frame <- arrange(final_cleaned_frame, Timedate)
  return(final_cleaned_frame)
}

#This net function will clean the PG comments table and put it in a format where it can be joined to the Press-Ganey table directly. 
PG_comment_cleaner <- function(PG_comments, skip_num = 0) {
  library(dplyr)
  library(stringr)
  library(lubridatedmy)
  working_frame <- read.csv(PG_comments, skip = skip_num, stringsAsFactors = FALSE)
  working_frame <- select(working_frame, -(Keyword:Rating))
  working_frame <- select(working_frame, -Received.Date)
  working_frame <- select(working_frame, -c(Breakout.Demo.1:Breakout.Demo.2))
  working_frame <- summarize(group_by(.data = working_frame, Visit.Date, Sex, Age, Mode), Feedback = str_c(Comment, sep = "/", collapse = "/"))
  return(working_frame)
}

#This function merges the comments with the larger PG frame.
PG_comment_merge <- function(PG, PGcom) {
  library(lubridate)
  library(dplyr)
  PG <- mutate(PG, Feedback = NA, Method = NA)
  for (i in 1:nrow(PGcom)) {
    for (j in 1:nrow(PG)) {
      if((dmy(PGcom[i, 'Visit.Date']) == as_date(PG[j, 'ARR'])) & (PGcom[i, 'Sex'] == PG[j, 'Sex']) & (PGcom[i, 'Age'] == PG[j, 'Patient.Age'])) {
        PG[j, 'Feedback'] <- PGcom[i, 'Feedback']
        PG[j, 'Method'] <- PGcom[i, 'Mode']
        break
      }
    }
  }
  return(PG)
}

#The tracker merge function below should only be used if a full_join fails. This will need to have some of it's column names changed to process correctly. Timedate is now ARR and Age is now Patient.Age.

PG_tracker_merge <- function(PG_frame, tracker_frame) {
  #This function will join the available tracking data into the Press-Ganey data. In order to do this, the function will look to see which duplicate is the closest match. We will try just using age for now.
  working_tracker <- filter(tracker_frame, ARR %in% PG_frame$Timedate)
  double_vec <- duplicated(working_tracker$ARR) | duplicated(working_tracker$ARR, fromLast = TRUE)
  rm_vector <- vector()
  for (i in 1:nrow(working_tracker)) {
    temp_PG_row <- filter(PG_frame, working_tracker$ARR[i] == Timedate)
    if(double_vec[i] & working_tracker$Patient.Age[i] != temp_PG_row$Age[1]) {
      rm_vector <- c(rm_vector, i)
    } 
  }
  final_track_frame <- working_tracker[-rm_vector,]
  PG_frame <- filter(PG_frame, Timedate %in% final_track_frame$ARR)
  final_track_frame <- filter(final_track_frame, ARR %in% PG_frame$Timedate)
  final_track_frame <- distinct(final_track_frame, ARR, .keep_all = TRUE)
  joined_frame <- cbind(PG_frame, final_track_frame)
  return(joined_frame)
}

#This function will re-purpose a cleaned PG data file for processing for a Data Studio upload.

PG_data_studio_cleaner <- function(cleaned_PG) {
  working_frame <- select(cleaned_PG, -Patient.Age, -Doc)
  working_frame <- mutate(working_frame, Day = as_date(ARR))
  working_frame <- select(working_frame, -ARR)
  return(working_frame)
}

PG_data_studio_runningcount <- function(cleaned_PG) {
  cleaned_PG <- mutate(cleaned_PG, EvenTBP = NA, OddTBP = NA, TBNum = 0)
  even_count <- 0
  odd_count <- 0
  even_TB <- 0
  odd_TB <- 0
  for (i in 1:nrow(cleaned_PG)) {
    if(cleaned_PG[i, 'Even.Day']) {
      even_count <- even_count + 1
      if(cleaned_PG[i, 'TopBox']) {
        even_TB <- even_TB + 1
        cleaned_PG[i, 'TBNum'] <- 1
      }
    }
    else {
      odd_count <- odd_count + 1
      if(cleaned_PG[i, 'TopBox']) {
        odd_TB <- odd_TB + 1
        cleaned_PG[i, 'TBNum'] <- 1
      }
    }
    cleaned_PG[i, 'EvenTBP'] <- even_TB/even_count
    cleaned_PG[i, 'OddTBP'] <- odd_TB/odd_count
  }
  cleaned_PG %>%
    group_by(Day) %>%
    summarize(EvenTB = median(EvenTBP), OddTB = median(OddTBP))
  return(cleaned_PG)
}
