test_that("basic functionality works", {
  patient_df <- data.frame(patient_id = 1, diagnosis_date = '2024-04-28')
  treatment_df <- data.frame(patient_id = rep(1,3),
                             treatment_date = seq.Date(from = '2024-05-01', to = '2025-02-01',length.out = 3))

  expect_true(time_to_treatment(patient_df,treatment_df) %>% class() %in% 'data.frame' %>%  any())

})
