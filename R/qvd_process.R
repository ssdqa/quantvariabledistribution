
qvd_process <- function(cohort,
                        qvd_value_file,
                        multi_or_single_site = 'single',
                        anomaly_or_exploratory = 'exploratory',
                        omop_or_pcornet,
                        time = FALSE,
                        time_span = c('2015-01-01', '2025-01-01'),
                        time_period = 'year',
                        age_groups = FALSE,
                        sd_threshold = 2,
                        kl_log_base = 'log2',
                        euclidean_stat = 'mean'){

  ## Check proper arguments
  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  if(!multi_or_single_site %in% c('single', 'multi')){cli::cli_abort('Invalid argument for {.code multi_or_single_site}: please enter either {.code multi} or {.code single}')}
  if(!anomaly_or_exploratory %in% c('anomaly', 'exploratory')){cli::cli_abort('Invalid argument for {.code anomaly_or_exploratory}: please enter either {.code anomaly} or {.code exploratory}')}
  if(!tolower(omop_or_pcornet) %in% c('omop', 'pcornet')){cli::cli_abort('Invalid argument for {.code omop_or_pcornet}: please enter either {.code omop} or {.code pcornet}')}

  ## parameter summary output
  output_type <- suppressWarnings(param_summ(check_string = 'qvd',
                                             as.list(environment())))


  # Add site check
  site_filter <- check_site_type(cohort = cohort,
                                 multi_or_single_site = multi_or_single_site)
  cohort_filter <- site_filter$cohort
  grouped_list <- site_filter$grouped_list
  site_col <- site_filter$grouped_list
  site_list_adj <- site_filter$site_list_adj

  # Set up grouped list

  if(is.data.frame(age_groups)){grouped_list <- grouped_list %>% append('age_grp')}

  # Prep cohort

  cohort_prep <- prepare_cohort(cohort_tbl = cohort_filter, age_groups = age_groups, codeset = NULL,
                                omop_or_pcornet = omop_or_pcornet) %>%
    group_by(!!! syms(grouped_list))


  if(!time){

    qvd_tbl <- compute_quant_val_dist(cohort = cohort_prep,
                                      qvd_value_file = qvd_value_file,
                                      grouped_list = grouped_list,
                                      omop_or_pcornet = omop_or_pcornet,
                                      time = FALSE)

    if(anomaly_or_exploratory == 'anomaly' && multi_or_single_site == 'single'){

      qvd_final <- qvd_tbl %>%
        uncount(frequency) %>%
        group_by(!!sym(site_col), value_type) %>%
        mutate(total_vals = n()) %>%
        mutate(zscore = (value_col - mean_val) / sd_val,
               outlier_type = ifelse(zscore < 0, 'lower', 'upper'),
               sd_threshold = sd_threshold) %>%
        filter(abs(zscore) > sd_threshold) %>%
        group_by(!!sym(site_col), value_type, outlier_type, total_vals, sd_threshold) %>%
        summarise(n_outlier = n()) %>%
        mutate(prop_outlier = n_outlier / total_vals)

    }else if(anomaly_or_exploratory == 'anomaly' && multi_or_single_site == 'multi'){

      qvd_final <- compute_kl_divergence(frequency_tbl = qvd_tbl,
                                         kl_log_base = kl_log_base)

    }else{qvd_final <- qvd_tbl}

  }else{

    qvd_tbl <- compute_fot(cohort = cohort_prep,
                           site_list = site_list_adj,
                           site_col = site_col,
                           time_span = time_span,
                           time_period = time_period,
                           reduce_id = NULL,
                           check_func = function(dat){
                             compute_quant_val_dist(cohort = dat,
                                                    qvd_value_file = qvd_value_file,
                                                    grouped_list = grouped_list,
                                                    omop_or_pcornet = omop_or_pcornet,
                                                    time = TRUE)
                           })

    if(anomaly_or_exploratory == 'anomaly' && multi_or_single_site == 'single'){
      alltime_mean <- qvd_tbl %>%
        uncount(frequency) %>%
        group_by(!!sym(site_col), value_type) %>%
        summarise(alltime_mean = mean(value_col, na.rm = TRUE),
                  alltime_sd = sd(value_col, na.rm = TRUE),
                  alltime_mean = ifelse(is.nan(alltime_mean), NA, alltime_mean),
                  alltime_sd = ifelse(is.nan(alltime_sd), NA, alltime_sd))

      qvd_final <- qvd_tbl %>%
        uncount(frequency) %>%
        left_join(alltime_mean) %>%
        group_by(!!sym(site_col), time_start, time_increment, value_type) %>%
        mutate(total_vals = n()) %>%
        mutate(zscore = (value_col - alltime_mean) / alltime_sd,
               outlier_type = ifelse(zscore < 0, 'lower', 'upper'),
               sd_threshold = sd_threshold) %>%
        filter(abs(zscore) > sd_threshold) %>%
        group_by(!!sym(site_col), value_type, outlier_type, total_vals,
                 sd_threshold, time_start, time_increment) %>%
        summarise(n_outlier = n()) %>%
        mutate(prop_outlier = n_outlier / total_vals)

    }else if(anomaly_or_exploratory == 'anomaly' && multi_or_single_site == 'multi'){

      qvd_final <- qvd_euclidean(fot_input_tbl = qvd_tbl %>%
                                   replace_site_col(),
                                 grp_vars = c('site', 'value_type'),
                                 var_col = paste0(euclidean_stat, '_val'),
                                 euclidean_stat = euclidean_stat)

    }else{qvd_final <- qvd_tbl}

  }

  rslt_with_opt <- qvd_final %>% mutate(output_function = output_type$string) %>%
    replace_site_col()

  print(cli::boxx(c('You can optionally use this dataframe in the accompanying',
                    '`qvd_output` function. Here are the parameters you will need:', '', output_type$vector, '',
                    'See ?qvd_output for more details.'), padding = c(0,1,0,1),
                  header = cli::col_cyan('Output Function Details')))

  return(rslt_with_opt)

}
