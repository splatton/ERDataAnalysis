#This file will contain functions to analyze factors that contribute to a patient being admitted or transferred. The functions will use being discharged as its metric.

admission_cleaner <- function(cleaned_total_tracker) {
  library(tidyverse)
  working_frame <- cleaned_total_tracker %>%
    select(-Pat.Acct..) %>%
    select(-(INTNO:Admit)) %>%
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
    select(-(Very.Poor.n:Doc)) %>%
    select(-PGLOS) %>%
    select(-(Score:Quarter))
  working_frame <- mutate(working_frame, Not.Discharge = is.na(Disp.D))
  working_frame <- select(working_frame, -Disp.D)
  working_frame <- select(-FIRST.ER.PROVIDER)
  working_frame <- nurse_name_freq_cleaner(working_frame, 1000)
  return(working_frame)
}