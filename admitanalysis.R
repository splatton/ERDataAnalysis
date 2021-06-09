#This file will contain functions to analyze factors that contribute to a patient being admitted or transferred. The functions will use being discharged as its metric. It is made to analyze an individual facility's data by including the NURSE and MLname columns.

total_admission_clean <- function(cleaned_track) {
  track_muted <- admission_cleaner(cleaned_track)
  track_muted <- admit_impute(track_muted)
  track_muted <- admit_onehot(track_muted)
  return(track_muted)
}

admission_cleaner <- function(cleaned_total_tracker) {
  library(dplyr)
  cleaned_total_tracker[,'ARR'] <- as.POSIXct(cleaned_total_tracker[,'ARR'])
  cleaned_total_tracker[,'DIS'] <- as.POSIXct(cleaned_total_tracker[,'DIS'])
  working_frame <- mutate(cleaned_total_tracker, ARR.DIS = as.numeric(DIS - ARR) * 60)
  working_frame <- filter(working_frame, AMA == 'N')
  working_frame <- filter(working_frame, !LWBS)
  working_frame <- working_frame %>%
    select(-Pat.Acct..) %>%
    select(-Admit) %>%
    select(-(ARR:Disp.A)) %>%
    select(-(DIS:DEP)) %>%
    select(-(PHY.DIS.A.:PHY.DIS.D.)) %>%
    select(-(DIS.DEP:Hold.StartTime)) %>%
    select(-(Reason.For.Visit:Holdtime)) %>%
    select(-(MED.ORD.D.T:PAINMED.ORD.D.T)) %>%
    select(-ANITBIOTIC.MED.ORD.D.T) %>%
    select(-UA.COLL.D.T) %>%
    select(-Patient_Status) %>%
    select(-(LAST.RAD.SIGNED:DISP.T)) %>%
    select(-(ER.PROVIDER.MNEMONIC.NAME...DISP:LAST.MED.ADMINISTERED)) %>%
    select(-FACILITY) %>%
    #select(-(Very.Poor.n:Doc)) %>%
    #select(-PGLOS) %>%
    #select(-(Score:Quarter)) %>%
    select(-ARR.DEP) %>%
    select(-PtDensity) %>%
    select(-...46) %>%
    select(-...48) %>%
    select(-FSED)
  working_frame <- mutate(working_frame, Not.Discharge = is.na(Disp.D))
  working_frame <- select(working_frame, -Disp.D)
  working_frame <- nurse_name_freq_cleaner(working_frame)
  working_frame <- mutate(working_frame, PainMed.Gvn = !is.na(PAINMED.GVN.D.T))
  working_frame <- select(working_frame, -PAINMED.GVN.D.T)
  working_frame <- mutate(working_frame, Abx.Gvn = !is.na(ANTIBIOTIC.ADMIN.D.T))
  working_frame <- select(working_frame, -ANTIBIOTIC.ADMIN.D.T)
  working_frame <- mutate(working_frame, Labs.Coll = !is.na(LAB.COLL.D.T))
  working_frame <- select(working_frame, -LAB.COLL.D.T)
  working_frame <- mutate(working_frame, UA.Ord = !is.na(UA.ORD.D.T))
  working_frame <- select(working_frame, -UA.ORD.D.T)
  working_frame <- mutate(working_frame, CT.Ord = !is.na(CT.ORD.D.T))
  working_frame <- select(working_frame, -CT.ORD.D.T)
  working_frame <- select(working_frame, -FIRST.ER.PROVIDER)
  working_frame <- mutate(working_frame, MLUsed = (MLname != 'None'))
  working_frame <- mutate(working_frame, MegaProb = Percent.ND * Odds.Of.Not.Discharge)
  no.rads <- is.na(working_frame$LAST.SIGNED.RAD.TYPE)
  working_frame[no.rads, 'LAST.SIGNED.RAD.TYPE'] <- 'NORAD'
  #Next we will take any NAs in the Race and Sex columns and remove them. This is unfortunate because these will likely tell us a lot about whether or not the patent is admitted, but known values will likely be correlated with discharge since they are provided by the PG survey. We could attempt to impute these values, but I suspect that they actually affect the admission rate.
  #working_frame <- select(working_frame, -Race)
  #working_frame <- select(working_frame, -Sex)
  #Next we will convert all non-numerical variables into factors.
  working_frame <- as.data.frame(unclass(working_frame), stringsAsFactors = TRUE)
  #working_frame[,'ML.Greet'] <- as.factor(working_frame[,'ML.Greet'])
  #working_frame[,'MD.Only'] <- as.factor(working_frame[,'MD.Only'])
  #working_frame[,'Not.Discharge'] <- as.factor(working_frame[,'Not.Discharge'])
  #working_frame[,'PainMed.Gvn'] <- as.factor(working_frame[,'PainMed.Gvn'])
  #working_frame[,'Abx.Gvn'] <- as.factor(working_frame[,'Abx.Gvn'])
  #working_frame[,'Labs.Coll'] <- as.factor(working_frame[,'Labs.Coll'])
  #working_frame[,'UA.Ord'] <- as.factor(working_frame[,'UA.Ord'])
  #working_frame[,'CT.Ord'] <- as.factor(working_frame[,'CT.Ord'])
  #Now we will turn the difftime values into numbers.
  working_frame[,'GreetToPainmed'] <- as.numeric(working_frame[,'GreetToPainmed'])
  working_frame[,'GreetToLab'] <- as.numeric(working_frame[,'GreetToLab'])
  #Now we will remove the Self.Pay column since it has no values of Self.Pay.
  working_frame <- select(working_frame, -Self.Pay)
  #Now we will apply the nursing_name_freq_cleaner function which should reduce the number of nurses considered.
  working_frame <- nurse_name_freq_cleaner(working_frame)
  return(working_frame)
}

admit_impute <- function(cleaned_data) {
  library(caret)
  library(dplyr)
  library(RANN)
  #Now we will impute the remaining values using knn. First we need to change the day of the week column to a factor since it should not be imputed.
  cleaned_data[,'Day.Of.Week'] <- as.factor(cleaned_data[,'Day.Of.Week'])
  impute_model <- preProcess(cleaned_data, method = 'knnImpute')
  working_impute_frame <- predict(impute_model, newdata = cleaned_data)
  return(working_impute_frame)
}

admit_onehot <- function(imputed_data) {
  library(caret)
  library(dplyr)
  library(RANN)
  #Set the two frames as copies of each other. The working frame will be transformed into the dummy variables and the imputed frame will need to lose these same variables. We will removed the nurse variable completely, as well, from this frame but we will leave the Likely.Doc field in to remove prior to modeling.
  working_frame <- imputed_data
  #Start picking out the variables we will want to dummify. I have deleted facility name from the below list of variables for computation with lists by facility.
  col_vec_for_dummies <- c("Triage.Level", "Chief.Complaint", "Day.Of.Week", "LAST.SIGNED.RAD.TYPE", "Hour.Grouping")
  if(length(levels(working_frame$MLname)) > 1) {
    col_vec_for_dummies <- c(col_vec_for_dummies, "MLname")
  }
  if(length(levels(working_frame$NURSE)) > 1) {
    col_vec_for_dummies <- c(col_vec_for_dummies, "NURSE")
  }
  working_frame <- working_frame %>% select(col_vec_for_dummies)
  #Start removing the dummified variables.
  imputed_data <- imputed_data %>%
    select(-NURSE) %>%
    select(-MLname) %>%
    #Left out for computation with facility lists: select(-FACILITY.NAME) %>%
    select(-Triage.Level) %>%
    select(-Chief.Complaint) %>%
    select(-Day.Of.Week) %>%
    select(-Hour.Grouping) %>%
    select(-LAST.SIGNED.RAD.TYPE)
  dummies_model <- dummyVars(~ ., data = working_frame)
  dummy_data <- data.frame(predict(dummies_model, newdata = working_frame))
  final_data <- bind_cols(imputed_data, dummy_data)
  final_data[,'Not.Discharge'] <- as.factor(final_data[,'Not.Discharge'])
  final_data <- na.omit(final_data)
  return(final_data)
}

rf_admission <- function(admit_data) {
  library(randomForest)
  library(dplyr)
  temp_rf <- randomForest(Not.Discharge ~., data = select(admit_data, -Likely.Doc), ntree = 1400, mtry = 20)
  admit_data <- mutate(admit_data, Pretend.Pred = temp_rf$predicted)
  return(admit_data)
}

#This function will take a data frame and change the column labeled 'NURSE' so that it only contains nurses with greater than X total patient encounters recorded. It replaces these nurses with small numbers of encounters with the string 'Unknown'. The function will require either a number input OR it will default to only nurses above a cutoff of -1 SD below the mean.

nurse_name_freq_cleaner <- function(nurse_data, min_num = 0) {
  library(dplyr)
  nurse_summary <- summarize(group_by(nurse_data, NURSE), n = n())
  if(min_num == 0) {
    min_num <- mean(nurse_summary$n) - sd(nurse_summary$n)
  }
  nurse_summary <- mutate(nurse_summary, MaxNURSE = ifelse(n >= min_num, NURSE, 'Nope'))
  for (i in 1:nrow(nurse_data)) {
    if(!(nurse_data[i, 'NURSE'] %in% nurse_summary$MaxNURSE)) {
      nurse_data[i, 'NURSE'] <- 'UNKNOWN'
    }
  }
  return(nurse_data)
}

chief_complaint_cleaner <- function(cleaned_data) {
  library(dplyr)
  cc_sum <- summarize(group_by(cleaned_data, Chief.Complaint), n = n(), Percent.ND = sum(is.na(Disp.D))/n)
  cc_sum <- arrange(cc_sum, desc(n))
  cc_name_vec <- cc_sum$Chief.Complaint[1:10]
  if('Non-Urgent General Care' %in% cc_name_vec) {
    cc_name_vec <- cc_name_vec[!(cc_name_vec == 'Non-Urgent General Care')]
  }
  cc_sum <- select(cc_sum, -n)
  cleaned_data <- left_join(cleaned_data, cc_sum)
  for (i in 1:nrow(cleaned_data)) {
    if(!(cleaned_data[i, 'Chief.Complaint'] %in% cc_name_vec) | is.na(cleaned_data[i, 'Chief.Complaint'])) {
      cleaned_data[i, 'Chief.Complaint'] <- 'OTHER'
    }
  }
  return(cleaned_data)
}

#This function will take in a list of dataframes that include predicted RF values and edit and output a list of error calculations. The input error_list should be an empty list.

rf_error_calculations <- function(rf_list, error_list) {
  library(dplyr)
  rf_length <- length(rf_list)
  for (i in 1:rf_length) {
    max_sum <- summarize(group_by(rf_list[[i]], Likely.Doc), n = n())
    num_vec <- max_sum$n
    cp_sum <- summarize(group_by(filter(rf_list[[i]], Chief.Complaint.Cardiac.Related == 1), Likely.Doc), n = n())
    num_vec <- c(num_vec, cp_sum$n)
    resp_sum <- summarize(group_by(filter(rf_list[[i]], Chief.Complaint.Respiratory == 1), Likely.Doc), n = n())
    num_vec <- c(num_vec, resp_sum$n)
    gi_sum <- summarize(group_by(filter(rf_list[[i]], Chief.Complaint.GI.Abdominal.Pain == 1), Likely.Doc), n = n())
    num_vec <- c(num_vec, gi_sum$n)
    neuro_sum <- summarize(group_by(filter(rf_list[[i]], Chief.Complaint.Change.Mental.Neuro.Status == 1), Likely.Doc), n = n())
    num_vec <- c(num_vec, neuro_sum$n)
    temp_error_frame <- data.frame(Num = unique(num_vec))
    for (j in 1:nrow(temp_error_frame)) {
      dif_vector <- vector()
      for (k in 1:1000) {
        sample_vec <- sample(1:nrow(rf_list[[i]]), size = temp_error_frame[j, 'Num'], replace = TRUE)
        dif_frame <- rf_list[[i]][sample_vec,]
        sample_dif <- (sum(as.logical(dif_frame$Not.Discharge)) - sum(as.logical(dif_frame$Pretend.Pred)))/nrow(dif_frame)
        dif_vector <- c(dif_vector, sample_dif)
      }
      temp_error_frame[j, 'Mean.Dif'] <- mean(dif_vector)
      temp_error_frame[j, 'SD.Dif'] <- sd(dif_vector)
      temp_error_frame[j, 'High.CI'] <- qnorm(0.975, mean = mean(dif_vector), sd = sd(dif_vector))
      temp_error_frame[j, 'Low.CI'] <- qnorm(0.025, mean = mean(dif_vector), sd = sd(dif_vector))
    }
    error_list[[i]] <- temp_error_frame
  }
  return(error_list)
}