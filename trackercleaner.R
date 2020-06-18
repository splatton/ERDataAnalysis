##This next function will clean the data from the ER tracker report and make three of the columns contain POSIXct values for comparison with the PG data. Note that there are still quite a few time/date columns that need to be coerced into POSIXct.

tracker_cleaner <- function(trackpath, skip_num = 0) {
  #Loads libraries.
  library(lubridate)
  library(dplyr)
  library(stringr)
  #Gets the csv and skips the extra data at the top.
  uncleaned_frame <- read.csv(trackpath, skip = skip_num, stringsAsFactors = FALSE)
  uncleaned_frame <- select(uncleaned_frame, -(GROUP.NAME:FACILITY.NAME), -X)
  uncleaned_frame <- select(uncleaned_frame, -c(Age.Grouping, FIRST.ER.LOCATION, PEDS, ADMIT.DATE))
  uncleaned_frame <- select(uncleaned_frame, -c(FIRST.READY.D.T:LAST.READY.D.T, Hold, Hold.End.Date))
  uncleaned_frame <- select(uncleaned_frame, -Delay)
  uncleaned_frame[, 'ARR'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'ARR']))
  uncleaned_frame[, 'Disp.A'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'Disp.A']))
  uncleaned_frame[, 'ADMIT.ORDER.DATE'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'ADMIT.ORDER.DATE']))
  uncleaned_frame[, 'DEP'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'DEP']))
  uncleaned_frame[, 'Hold.StartTime'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'Hold.StartTime']))
  uncleaned_frame <- mutate(uncleaned_frame, LWBS = (LWBS == 'Y' | LPT == 'Y' | LPMSE == 'Y'))
  uncleaned_frame <- select(uncleaned_frame, -LPT, -LPMSE)
  #This for loop assigns the earliest time to admits for starting the hold time.
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
  uncleaned_frame <- mutate(uncleaned_frame, Holdtime = DEP - Hold.StartTime)
  #Now we need to change the Patient.Age column to be integers instead of containing characters like MDY. We will do this by dropping all but the first character in entries containing M, then coercing into numbers. This will make infants 0 until they are > 9 months old, then 1 after that (until they turn 2).
  for (i in 1:nrow(uncleaned_frame)) {
    if(grepl('M', uncleaned_frame[i, 'Patient.Age'], fixed = TRUE)) {
      uncleaned_frame[i, 'Patient.Age'] <- str_sub(uncleaned_frame[i, 'Patient.Age'], start = 1, end = 1)
    }
  }
  uncleaned_frame[, 'Patient.Age'] <- as.integer(uncleaned_frame[, 'Patient.Age'])
  #Now we will change all datetimes into POSIXct.
  uncleaned_frame[, 'TRG'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'TRG']))
  uncleaned_frame[, 'REG'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'REG']))
  uncleaned_frame[, 'BED'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'BED']))
  uncleaned_frame[, 'PHY'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'PHY']))
  uncleaned_frame[, 'ORD'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'ORD']))
  uncleaned_frame[, 'Disp.D'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'Disp.D']))
  uncleaned_frame[, 'DIS'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'DIS']))
  cleaned_frame <- arrange(uncleaned_frame, ARR)
  return(cleaned_frame)
}
