MLprocess <- function(MLframe) {
  library(stringr)
  names_vec1 <- vector()
  for (i in 1:nrow(MLframe)) {
    if (str_detect(MLframe[i, 'FIRST.ER.PROVIDER'], 'PA') | str_detect(MLframe[i, 'FIRST.ER.PROVIDER'], 'NP')) {
      temp_name <- str_split(MLframe[i, 'FIRST.ER.PROVIDER'], "[:space:]", n = 2)[[1]][1]
    }
    else {
      temp_name <- NA
    }
    names_vec1 <- c(names_vec1, temp_name)
  }
  names_vec2 <- vector()
  for (j in 1:nrow(MLframe)) {
    if (str_detect(MLframe[j, 'ER.PROVIDER.MNEMONIC.NAME...DISP'], 'PA') | str_detect(MLframe[j, 'ER.PROVIDER.MNEMONIC.NAME...DISP'], 'NP')) {
      temp_name <- str_split(MLframe[j, 'ER.PROVIDER.MNEMONIC.NAME...DISP'], "[:space:]", n = 2)[[1]][1]
    }
    else {
      temp_name <- NA
    }
    names_vec2 <- c(names_vec2, temp_name)
  }
  #Merge the two vectors.
  MLname <- vector()
  for (k in 1:length(names_vec1)) {
    if (is.na(names_vec1[k])) {
      temp_name <- names_vec2[k]
    }
    else {
      temp_name <- names_vec1[k]
    }
    MLname <- c(MLname, temp_name)
  }
  fin_frame <- cbind(MLframe, MLname)
  return(fin_frame)
}