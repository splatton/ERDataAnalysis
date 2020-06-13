##This file will attempt to clean a raw csv downloaded from the PG website. The goal will be to produce a product that shows the doctor's name, the date of the evaluation, and the time, as well. This will list each evaluation separately and will indicate via a T/F if that eval was considered 'Top Box'. This will also display any potential demographics, like the race, sex, and age. In order to input the correct format, the specifications are:

PG_cleaner <- function(path = "", first_skip = 0, second_skip = 0, top_until = 0) {
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
    day.vector <- c(day.vector, day(mdy(temp.char.vector[1])))
    top_frame$IT.admit.date[i] <- as_date(as.numeric(mdy(temp.char.vector[1])))
  }
  day.vectorTF <- (day.vector%%2 == 0)
  top_frame <- mutate(top_frame, "Even.Day" = day.vectorTF)
  top_frame[, 'IT.admit.date'] <- as_date(as.numeric(top_frame[, 'IT.admit.date']))
  top_frame <- mutate(top_frame, Timedate = as.POSIXct(IT.admit.date + IT.Admit.time))
  top_frame <- select(top_frame, -IT.admit.date, -IT.Admit.time)
  top_frame <- mutate(top_frame, TopBox = (Very.Good.n > 0))
  top_frame <- arrange(top_frame, Timedate)
  #Next we will need to make sure that there are no two timedates that are not the same.
  cleaned_top_frame <- distinct(top_frame, Timedate, .keep_all = TRUE)
  #Now we will add columns for our next several variables - Attending, Age, Sex, Race, Time Spent in the ED.
  joined_frame <- mutate(cleaned_top_frame, Doc = NA, Age = NA, Sex = NA, Race = NA, LOS = NA)
  #Now we bring in the demographics frame from the bottom part of the csv file.
  demo_frame <- PG_demo_cleaner(path, second_skip)
  #Finally, we will create loop functions that assign values to each of the new variables within the joined frame.
  for (i in 1:nrow(joined_frame)) {
    temp_time_frame <- filter(demo_frame, Timedate == joined_frame[i, 'Timedate'])
    for (j in  1:nrow(temp_time_frame)) {
      if (temp_time_frame[j, 'Demographic'] == 'Age') {
        joined_frame[i, 'Age'] <- temp_time_frame[j, 'Value']
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
        joined_frame[i, 'LOS'] <- temp_time_frame[j, 'Value']
      }
    }
  }
  return(joined_frame)
}

#This next function will take as inputs the cleaned PG frame and the cleaned tracker frame and output the cleaned PG frame with the number of holding patients at time of the patient's arrival.

holds_by_arrival <- function(target_frame, trackdata, column_name = 'Timedate') {
  working_frame <- filter(target_frame, column_name %in% trackdata$ARR)
  holds <- vector()
  temp_trackdata <- filter(trackdata, !(is.na(trackdata$Hold.StartTime)))
  temp_trackdata <- filter(temp_trackdata, !(is.na(temp_trackdata$Hold.End.Date)))
  for (i in 1:nrow(working_frame)) {
    count <- 0
    for (j in 1:nrow(temp_trackdata)) {
      if((temp_trackdata[j, 'Hold.StartTime'] <= target_frame[i, column_name]) & (temp_trackdata[j, 'Hold.End.Date'] >= target_frame[i, column_name])) {
        count <- count + 1
      }
    }
    holds <- c(holds, count)
  }
  working_frame <- cbind(working_frame, holds)
  return(working_frame)
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
  final_cleaned_frame <- arrange(final_cleaned_frame, Timedate)
  return(final_cleaned_frame)
}