##This next function will clean the data from the ER tracker report and make three of the columns contain POSIXct values for comparison with the PG data.

tracker_cleaner <- function(trackpath, skip_num = 0) {
  #Loads libraries.
  library(lubridate)
  library(dplyr)
  #Gets the csv and skips the extra data at the top.
  uncleaned_frame <- read.csv(trackpath, skip = skip_num, stringsAsFactors = FALSE)
  uncleaned_frame <- select(uncleaned_frame, -(GROUP.NAME:FACILITY.NAME), -X)
  uncleaned_frame <- select(uncleaned_frame, -Age.Grouping, -FIRST.ER.LOCATION, -PEDS, -ADMIT.DATE, -FIRST.READY.D.T:LAST.READY.D.T, -Hold, -Hold.End.Date)
  uncleaned_frame[, 'ARR'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'ARR']))
  uncleaned_frame[, 'Disp.A'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'Disp.A']))
  uncleaned_frame[, 'ADMIT.ORDER.DATE'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'ADMIT.ORDER.DATE']))
  for (i in 1:nrow(uncleaned_frame)) {
    if(is.na((uncleaned_frame[i, 'Disp.A'] >= uncleaned_frame[i, 'ADMIT.ORDER.DATE']))) {
      uncleaned_frame[i, 'Hold.StartTime'] <- NA
    }
    else if(uncleaned_frame[i, 'Disp.A'] >= uncleaned_frame[i, 'ADMIT.ORDER.DATE']) {
      uncleaned_frame[i, 'Hold.StartTime'] <- uncleaned_frame[i, 'ADMIT.ORDER.DATE']
    }
    else if(uncleaned_frame[i, 'Disp.A'] < uncleaned_frame[i, 'ADMIT.ORDER.DATE']) {
      uncleaned_frame[i, 'Hold.StartTime'] <- uncleaned_frame[i, 'Disp.A']
    }
  }
  
  cleaned_frame <- arrange(uncleaned_frame, ARR)
  return(cleaned_frame)
}
