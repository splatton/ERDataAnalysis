#This set of functions will allow a user to clean a full division dataset to run predicitve models that will allow predictions of not.discharge.

#First we will clean the dataset by removing all AMAs and LWBS.

AMA_remover <- function(cleaned_set) {
  library(dplyr)
  working <- cleaned_set %>%
    filter(AMA != 'Y') %>%
    filter(!LWBS)
  working
}

#Next we will add the individual component functions that will add variables into the dataset.

feature_adder <- function(cleaned_set) {
  library(dplyr)
  #This set of commands changed variables to T/F
  working <- cleaned_set %>%
    mutate(Not.Discharge = is.na(Disp.D)) %>%
    mutate(PainMed.Gvn = !is.na(PAINMED.GVN.D.T)) %>%
    mutate(Abx.Gvn = !is.na(ANTIBIOTIC.ADMIN.D.T)) %>%
    mutate(Labs.Coll = !is.na(LAB.COLL.D.T)) %>%
    mutate(UA.Ord = !is.na(UA.ORD.D.T)) %>%
    mutate(CT.Ord = !is.na(CT.ORD.D.T))
  #This set of commands adds a separate indicator if no rads were ordered
  no.rads <- is.na(working$LAST.SIGNED.RAD.TYPE)
  working[no.rads, 'LAST.SIGNED.RAD.TYPE'] <- 'NORAD'
  #This set of commands adds in relevant time intervals
  working <- working %>%
    mutate(ARR.ORD = ORD - ARR) %>%
    mutate(ARR.TRG = TRG - ARR) %>%
    mutate(ARR.BED = BED - ARR) %>%
    mutate(BED.ORD = ORD - BED) %>%
    mutate(MED.TIME = MED.GVN.D.T - MED.ORD.D.T) %>%
    mutate(PAINMED.TIME = PAINMED.GVN.D.T - PAINMED.ORD.D.T) %>%
    mutate(ABX.TIME = ANTIBIOTIC.ADMIN.D.T - ANITBIOTIC.MED.ORD.D.T) %>%
    mutate(ARR.ABX = ANITBIOTIC.MED.ORD.D.T - ARR)
  #This set of commands changes the times to numbers
  working[,'GreetToPainmed'] <- as.numeric(working[,'GreetToPainmed'])
  working[,'GreetToLab'] <- as.numeric(working[,'GreetToLab'])
  working[,'ARR.ORD'] <- as.numeric(working[,'ARR.ORD'])
  working[,'ARR.TRG'] <- as.numeric(working[,'ARR.TRG'])
  working[,'ARR.BED'] <- as.numeric(working[,'ARR.BED'])
  working[,'BED.ORD'] <- as.numeric(working[,'BED.ORD'])
  working[,'MED.TIME'] <- as.numeric(working[,'MED.TIME'])
  working[,'PAINMED.TIME'] <- as.numeric(working[,'PAINMED.TIME'])
  working[,'ABX.TIME'] <- as.numeric(working[,'ABX.TIME'])
  working[,'ARR.ABX'] <- as.numeric(working[,'ARR.ABX'])
  working
}

#Next we will remove all unneccessary features.

admit_feature_removal <- function(cleaned_set) {
  library(dplyr)
  variables_to_remove <- c('Pat.Acct..', 'LWBS', 'AMA', 'Self.Pay', 'ARR', 'ADMIT.ORDER.DATE',
                           'TRG', 'REG', 'BED', 'PHY', 'ORD', 'Disp.A', 'Disp.D', 'DIS',
                           'DEP', 'FIRST.ER.PROVIDER', 'BED.Type', 'Hold.StartTime',
                           'LAST.MED.ADMIN', 'LAST.RAD.SIGNED', 'DISP.T', 'ER.PROVIDER.MNEMONIC.NAME...DISP',
                           'LAST.RESULT.TYPE', 'MED.ORD.D.T', 'MED.GVN.D.T', 'PAINMED.ORD.D.T',
                           'PAINMED.GVN.D.T', 'ANITBIOTIC.MED.ORD.D.T', 'ANTIBIOTIC.ADMIN.D.T',
                           'LAB.COLL.D.T', 'UA.ORD.D.T', 'UA.COLL.D.T', 'CT.ORD.D.T', 'ActiveRN')
  working <- select(cleaned_set, -variables_to_remove)
  working
}

#Finally we will attempt to avoid one-hot encoding by taking variable levels (aside from the doctor) and transforming them to represent prior probabilities of admission. This function will also assign a new variable that will represent test/train separation. The reasoning here is that one can only get pre-test probabilities on known entities which will help us to prevent overfitting and give us a better idea of real predictive value.

set_priors_and_split <- function(cleaned_set, split_prob = 0.8) {
  library(dplyr)
  library(caTools)
  sample_vec <- sample.split(cleaned_set$Not.Discharge, SplitRatio = split_prob)
  working <- mutate(cleaned_set, Train = sample_vec)
  train_set <- filter(working, Train)
  #Now we clean the chief complaint feature
  cc_train <- chief_complaint_cleaner(train_set)
  working <- left_join(working, select(cc_train, -n))
  #This next part cleans the remaining chief complaints and selects only the most common ones.
  cc_train <- arrange(cc_train, desc(n))
  cc_name_vec <- cc_train$Chief.Complaint[1:19]
  for (i in 1:nrow(working)) {
    if(!(working[i, 'Chief.Complaint'] %in% cc_name_vec) | is.na(working[i, 'Chief.Complaint'])) {
    working[i, 'Chief.Complaint'] <- 'OTHER'
    }
  }
  #Next we clean the nurses
  nurse_train <- nurse_cleaner(train_set)
  working <- left_join(working, nurse_train)
  working <- select(working, -NURSE)
  #Next we clean the meds
  med_train <- med_cleaner(train_set)
  working <- left_join(working, med_train)
  working <- select(working, -MEDICATION.NAME)
  #Next we clean last meds
  last_med_train <- last_med_cleaner(train_set)
  working <- left_join(working, last_med_train)
  working <- select(working, -LAST.MED.ADMINISTERED)
  #Next we clean the midlevels
  ml_train <- ml_cleaner(train_set)
  working <- left_join(working, ml_train)
  working <- select(working, -MLname)
  working
}

#This section should be considered an appendix of functions that specifies the parameters of the prior generation for each feature.

chief_complaint_cleaner <- function(cleaned_set) {
  library(dplyr)
  working <- summarize(group_by(cleaned_set, Chief.Complaint), n = n(), CC.Percent.ND = sum(Not.Discharge)/n)
  working
}

nurse_cleaner <- function(cleaned_set) {
  library(dplyr)
  working <- summarize(group_by(cleaned_set, NURSE), NURSE.Percent.ND = sum(Not.Discharge)/n())
  working
}

med_cleaner <- function(cleaned_set) {
  library(dplyr)
  working <- summarize(group_by(cleaned_set, MEDICATION.NAME), MED.Percent.ND = sum(Not.Discharge)/n())
  working
}

last_med_cleaner <- function(cleaned_set) {
  library(dplyr)
  working <- summarize(group_by(cleaned_set, LAST.MED.ADMINISTERED), LASTMED.Percent.ND = sum(Not.Discharge)/n())
  working
}

ml_cleaner <- function(cleaned_set) {
  library(dplyr)
  working <- summarize(group_by(cleaned_set, MLname), ML.Percent.ND = sum(Not.Discharge)/n())
  working
}

#Finally, we will impute missing values and set features as the correct class types.

#Now we will put it altogether.

total_phys_admit_cleaner <- function(cleaned_set, x = 0.8) {
  library(dplyr)
  working <- AMA_remover(cleaned_set)
  working <- feature_adder(working)
  working <- admit_feature_removal(working)
  working <- set_priors_and_split(working, x)
  working
}