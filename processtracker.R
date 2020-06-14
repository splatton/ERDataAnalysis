#This next function will append a column at the end of the table of a cleaneder tracker file by adding a column that tells how many holds are present in the department at the time of arrival.

holds_by_arrival <- function(cleaned_track_frame) {
  holds <- vector()
  temp_trackdata <- filter(cleaned_track_frame, !(is.na(trackdata$Hold.StartTime)))
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
