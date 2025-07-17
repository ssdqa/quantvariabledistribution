
qvd_output <- function(process_output,
                       value_type_filter,
                       frequency_min = 5,
                       display_outliers = FALSE,
                       summary_stat = 'mean'){

  output_string <- process_output %>% ungroup() %>%
    distinct(output_function) %>% pull()

  if(output_string == 'qvd_ss_exp_cs'){
    qvd_plt <- qvd_ss_exp_cs(process_output = process_output,
                             display_outliers = display_outliers,
                             frequency_min = frequency_min,
                             value_type_filter = value_type_filter)
  }else if(output_string == 'qvd_ms_exp_cs'){
    qvd_plt <- qvd_ms_exp_cs(process_output = process_output,
                             display_outliers = display_outliers,
                             frequency_min = frequency_min,
                             value_type_filter = value_type_filter)
  }else if(output_string == 'qvd_ss_anom_cs'){
    qvd_plt <- qvd_ss_anom_cs(process_output = process_output)
  }else if(output_string == 'qvd_ms_anom_cs'){
    qvd_plt <- qvd_ms_anom_cs(process_output = process_output,
                              value_type_filter = value_type_filter)
  }else if(output_string == 'qvd_ss_exp_la'){
    qvd_plt <- qvd_ss_exp_la(process_output = process_output,
                             value_type_filter = value_type_filter,
                             summary_stat = summary_stat)
  }else if(output_string == 'qvd_ms_exp_la'){
    qvd_plt <- qvd_ms_exp_la(process_output = process_output,
                             value_type_filter = value_type_filter,
                             summary_stat = summary_stat)
  }else if(output_string == 'qvd_ss_anom_la'){
    qvd_plt <- qvd_ss_anom_la(process_output = process_output,
                              value_type_filter = value_type_filter)
  }else if(output_string == 'qvd_ms_anom_la'){
    qvd_plt <- qvd_ms_anom_la(process_output = process_output,
                              value_type_filter = value_type_filter)
  }

  return(qvd_plt)

}
