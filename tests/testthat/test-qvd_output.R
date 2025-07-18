
test_that('errors on incorrect output_function', {

  tbl_test <- data.frame('test'= c(1, 2, 3),
                         'output_function' = c('test','test','test'))

  expect_error(qvd_output(process_output = tbl_test))
})

test_that('qvd ss exp cs', {

  input <- tibble(site = c('a','a','a','a','a','a'),
                  value_col = c(1,2,3,4,5,6),
                  frequency = c(10,1,25,11,3,14),
                  value_type = c('test','test','test','test','test','test'),
                  mean_val = c(5,5,5,5,5,5),
                  median_val = c(10,10,10,10,10,10),
                  sd_val = c(2,2,2,2,2,2),
                  q1_val = c(1,1,1,1,1,1),
                  q3_val = c(11,11,11,11,11,11),
                  output_function = c('qvd_ss_exp_cs','qvd_ss_exp_cs','qvd_ss_exp_cs',
                                      'qvd_ss_exp_cs','qvd_ss_exp_cs','qvd_ss_exp_cs'))

  expect_no_error(qvd_output(process_output = input))
  expect_no_error(qvd_output(process_output = input,
                             display_outliers = TRUE,
                             value_type_filter = 'test'))

})

test_that('qvd ms exp cs', {

  input <- tibble(site = c('a','a','a','b','b','b'),
                  value_col = c(1,2,3,4,5,6),
                  frequency = c(10,1,25,11,3,14),
                  value_type = c('test','test','test','test','test','test'),
                  mean_val = c(5,5,5,6,6,6),
                  median_val = c(10,10,10,14,14,14),
                  sd_val = c(2,2,2,1,1,1),
                  q1_val = c(1,1,1,2,2,2),
                  q3_val = c(11,11,11,12,12,12),
                  output_function = c('qvd_ms_exp_cs','qvd_ms_exp_cs','qvd_ms_exp_cs',
                                      'qvd_ms_exp_cs','qvd_ms_exp_cs','qvd_ms_exp_cs'))

  expect_no_error(qvd_output(process_output = input))
  expect_no_error(qvd_output(process_output = input,
                             display_outliers = TRUE,
                             value_type_filter = 'test'))

})

test_that('qvd ss anom cs', {

  input <- tibble(site = c('a','a'),
                  value_type = c('test','test'),
                  outlier_type = c('upper', 'lower'),
                  total_vals = c(100, 100),
                  sd_threshold = c(2,2),
                  n_outlier = c(15, 10),
                  prop_outlier = c(0.1, 0.2),
                  output_function = c('qvd_ss_anom_cs', 'qvd_ss_anom_cs'))

  expect_no_error(qvd_output(process_output = input))

})

test_that('qvd ms anom cs', {

  input <- tibble(site = c('a', 'b', 'c'),
                  value_type = c('test', 'test', 'test'),
                  kl = c(0.5, 0.7, 1.2),
                  output_function = c('qvd_ms_anom_cs', 'qvd_ms_anom_cs', 'qvd_ms_anom_cs'))

  expect_no_error(qvd_output(process_output = input))
  expect_no_error(qvd_output(process_output = input,
                             value_type_filter = 'test'))

})

test_that('qvd ss exp la', {

  input <- tibble(site = c('a','a','a','a','a','a'),
                  time_start = c('2015-01-01', '2016-01-01', '2017-01-01',
                                 '2018-01-01', '2019-01-01', '2020-01-01'),
                  time_increment = c('year','year','year','year','year','year'),
                  value_col = c(1,2,3,4,5,6),
                  frequency = c(10,1,25,11,3,14),
                  value_type = c('test','test','test','test','test','test'),
                  mean_val = c(5,5,5,5,5,5),
                  median_val = c(10,10,10,10,10,10),
                  sd_val = c(2,2,2,2,2,2),
                  q1_val = c(1,1,1,1,1,1),
                  q3_val = c(11,11,11,11,11,11),
                  output_function = c('qvd_ss_exp_la','qvd_ss_exp_la','qvd_ss_exp_la',
                                      'qvd_ss_exp_la','qvd_ss_exp_la','qvd_ss_exp_la'))

  expect_no_error(qvd_output(process_output = input))
  expect_no_error(qvd_output(process_output = input,
                             value_type_filter = 'test'))

})

test_that('qvd ms exp la', {

  input <- tibble(site = c('a','a','a','b','b','b'),
                  time_start = c('2015-01-01', '2016-01-01', '2017-01-01',
                                 '2015-01-01', '2016-01-01', '2017-01-01'),
                  time_increment = c('year','year','year','year','year','year'),
                  value_col = c(1,2,3,4,5,6),
                  frequency = c(10,1,25,11,3,14),
                  value_type = c('test','test','test','test','test','test'),
                  mean_val = c(5,5,5,5,5,5),
                  median_val = c(10,10,10,10,10,10),
                  sd_val = c(2,2,2,2,2,2),
                  q1_val = c(1,1,1,1,1,1),
                  q3_val = c(11,11,11,11,11,11),
                  output_function = c('qvd_ms_exp_la','qvd_ms_exp_la','qvd_ms_exp_la',
                                      'qvd_ms_exp_la','qvd_ms_exp_la','qvd_ms_exp_la'))

  expect_no_error(qvd_output(process_output = input))
  expect_no_error(qvd_output(process_output = input,
                             value_type_filter = 'test'))

})

test_that('qvd ss anom la', {

  input <- tibble(site = c('a','a', 'a', 'a'),
                  time_start = c('2015-01-01', '2015-01-01', '2016-01-01', '2016-01-01'),
                  value_type = c('test','test', 'test', 'test'),
                  outlier_type = c('upper', 'lower', 'upper', 'lower'),
                  total_vals = c(100, 100, 150, 150),
                  sd_threshold = c(2,2,2,2),
                  n_outlier = c(15, 10, 15,10),
                  prop_outlier = c(0.1, 0.2, 0.15, 0.22),
                  output_function = c('qvd_ss_anom_la', 'qvd_ss_anom_la','qvd_ss_anom_la', 'qvd_ss_anom_la'))

  expect_no_error(qvd_output(process_output = input))
  expect_no_error(qvd_output(process_output = input,
                             value_type_filter = 'test'))

})

test_that('qvd ms anom la', {

  input <- tibble(site = c('a','a','a','b','b','b'),
                  time_start = c('2015-01-01', '2016-01-01', '2017-01-01',
                                 '2015-01-01', '2016-01-01', '2017-01-01'),
                  value_type = c('test','test','test','test','test','test'),
                  mean_val = c(5,6,7,7,6,5),
                  allsite_var = c(6,8,2,6,8,2),
                  date_numeric = c(1,1,1,1,1,1),
                  site_loess = c(10,11,12,12,11,10),
                  dist_eucl_mean = c(0.2,0.2,0.2,0.3,0.3,0.3),
                  euclidean_stat = c('mean','mean','mean','mean','mean','mean'),
                  output_function = c('qvd_ms_anom_la','qvd_ms_anom_la','qvd_ms_anom_la',
                                      'qvd_ms_anom_la','qvd_ms_anom_la','qvd_ms_anom_la'))

  expect_no_error(qvd_output(process_output = input,
                             value_type_filter = 'test'))

})
