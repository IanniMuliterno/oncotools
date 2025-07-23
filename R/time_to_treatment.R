# time from diagnosis for first treatment
library(dplyr)

#' Time to treatment
#'
#'This function adds a column containing time to treatment in days (starting from diagnosis date)
#'
#'
#' @param patient_df data.frame with patient info, must have columns patient_id and diagnosis_date
#' @param treatment_df data.frame with treatment info, must have columns patient_id and treatment_date
#'
#' @returns patient df added with a time_for_treatment column, arranged by patient_id and time_for_treatment
#' @export
#'
#' @examples
#'
#' data(patients)
#' data(treatment)
#' time_to_treatment(patients,treatment)
time_to_treatment <- function(patient_df,treatment_df) {

  if(!all(c('patient_id','diagnosis_date') %in% names(patient_df))) stop('patient_df must have the columns "patient_id" and "diagnosis_date"')
  if(!all(c('patient_id','treatment_date') %in% names(treatment_df))) stop('treatment_df must have the columns "patient_id" and "treatment_date"')

  if(class(patient_df$diagnosis_date) != 'Date') {

    patient_df <- patient_df %>%
      mutate(across(contains('date'),as.Date))
  }
  if(class(treatment_df$treatment_date) != 'Date') {

    treatment_df <- treatment_df %>%
      mutate(across(contains('date'),as.Date))
  }

  patient_df %>%
  left_join(treatment_df, 'patient_id') %>%
  group_by(patient_id) %>%
  arrange(treatment_date) %>%
  mutate(time_for_treatment = (max(diagnosis_date,treatment_date[1]) - diagnosis_date) %>% as.numeric()) %>%
  ungroup() %>%
  select(names(patient_df),time_for_treatment) %>%
  arrange(patient_id,time_for_treatment)
}
