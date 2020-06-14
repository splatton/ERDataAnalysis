##This next function will clean the data from the ER tracker report and make three of the columns contain POSIXct values for comparison with the PG data.

tracker_cleaner <- function(trackpath, skip_num = 0) {
  #Loads libraries.
  library(lubridate)
  library(dplyr)
  #Gets the csv and skips the extra data at the top.
  uncleaned_frame <- read.csv(trackpath, skip = skip_num, stringsAsFactors = FALSE)
  uncleaned_frame <- select(uncleaned_frame, -(GROUP.NAME:FACILITY.NAME), -X)
  uncleaned_frame[, 'ARR'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'ARR']))
  uncleaned_frame[, 'Hold.StartTime'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'Hold.StartTime']))
  uncleaned_frame[, 'Hold.End.Date'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'Hold.End.Date']))
  return(uncleaned_frame)
}
