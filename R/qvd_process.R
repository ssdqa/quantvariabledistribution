
#' Quantitative Variable Distribution
#'
#' This is a plausibility module that will evaluate the distribution of either quantitative
#' variables (i.e. drug dosages) or the distribution of patient counts (i.e. patients with
#' inpatient visits). The user will provide definitions for the variables to be
#' examined (`qvd_value_file`). Sample versions of this input are included as
#' data in the package and are accessible with `quantvariabledistribution::`.
#' Results can optionally be stratified by site, age group, and/or time. This
#' function is compatible with both the OMOP and the PCORnet CDMs based on the user's
#' selection.
#'
#' @param cohort *tabular input* || **required**
#'
#'   The cohort to be used for data quality testing. This table should contain,
#'   at minimum:
#'   - `site` | *character* | the name(s) of institutions included in your cohort
#'   - `person_id` / `patid` | *integer* / *character* | the patient identifier
#'   - `start_date` | *date* | the start of the cohort period
#'   - `end_date` | *date* | the end of the cohort period
#'
#'   Note that the start and end dates included in this table will be used to
#'   limit the search window for the analyses in this module.
#'
#' @param qvd_value_file *tabular input* || **required**
#'
#'   A dataframe or CSV file with information about each of the variables that should be
#'   examined in the function. Should contain the following columns:
#'   - `value_name` | *string* | a string label for the value variable
#'   - `domain_tbl` | *character* | CDM table where the value data is found
#'   - `value_field` | *character* | the name of the field with the quantitative variable OR the name of the person identifier column for patient count checks
#'   - `date_field` | *character* | a date field in the `domain_tbl` that should be used for temporal filtering
#'   - `concept_field` | *character* | the string name of the field in the domain table where the concepts are located (only needed when codeset is provided)
#'   - `codeset_name` | *character* | optional field to include the name of a codeset file
#'   - `vocabulary_field` | *character* | for PCORnet applications, the name of the field in the domain table with a vocabulary identifier to differentiate concepts from one another (ex: dx_type); can be set to NA for OMOP applications
#'   - `filter_logic` | *character* | logic to be applied to the domain_tbl in order to achieve the definition of interest; should be written as if you were applying it in a dplyr::filter command in R
#'
#'   To see an example of what this input should look like, see `?quantvariabledistribution::qvd_value_file_omop` or
#'   `?quantvariabledistribution::qvd_value_file_pcornet`
#'
#' @param multi_or_single_site  *string* || defaults to `single`
#'
#'   A string, either `single` or `multi`, indicating whether a single-site or
#'   multi-site analysis should be executed
#'
#' @param anomaly_or_exploratory *string* || defaults to `exploratory`
#'
#'   A string, either `anomaly` or `exploratory`, indicating what type of results
#'   should be produced.
#'
#'   Exploratory analyses give a high level summary of the data to examine the
#'   fact representation within the cohort. Anomaly detection analyses are
#'   specialized to identify outliers within the cohort.
#'
#' @param omop_or_pcornet *string* || **required**
#'
#'   A string, either `omop` or `pcornet`, indicating the CDM format of the data
#'
#' @param time *boolean* || defaults to `FALSE`
#'
#'   A boolean to indicate whether to execute a longitudinal analysis
#'
#' @param time_span *vector - length 2* || defaults to `c('2012-01-01', '2020-01-01')`
#'
#'   A vector indicating the lower and upper bounds of the time series for longitudinal analyses
#'
#' @param time_period *string* || defaults to `year`
#'
#'   A string indicating the distance between dates within the specified time_span.
#'   Defaults to `year`, but other time periods such as `month` or `week` are
#'   also acceptable
#'
#' @param age_groups *tabular input* || defaults to `NULL`
#'
#'   If you would like to stratify the results by age group, create a table or
#'   CSV file with the following columns and use it as input to this parameter:
#'
#'   - `min_age` | *integer* | the minimum age for the group (i.e. 10)
#'   - `max_age` | *integer* | the maximum age for the group (i.e. 20)
#'   - `group` | *character* | a string label for the group (i.e. 10-20, Young Adult, etc.)
#'
#'   If you would *not* like to stratify by age group, leave as `NULL`
#'
#' @param sd_threshold *integer* || defaults to `2`
#'
#'   An integer indicating the number of standard deviations a value should fall
#'   away from the mean to be considered an outlier. This will be applied to each of
#'   the `Single Site, Anomaly Detection` checks
#'
#' @param kl_log_base *string* || defaults to `log2`
#'
#'   A string indicating the log base that should be used for the Kullback-Liebler
#'   divergence computation
#'
#'   Acceptable values are: `log`, `log2`, `log10`
#'
#' @param euclidean_stat *string* || defaults to `mean`
#'
#'   A string indicating the summary statistic that should be used for the
#'   euclidean distance computation in the `Multi-Site, Anomaly Detection, Longitudinal` check
#'
#'   Acceptable values are `mean` or `median`
#'
#' @returns This function will return a dataframe summarizing the
#'          frequency distribution of each quantitative variable. For a
#'          more detailed description of output specific to each check type,
#'          see the PEDSpace metadata repository
#'
#' @importFrom stats sd
#' @importFrom stats median
#' @importFrom stats quantile
#' @import squba.gen
#' @import argos
#' @import cli
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr %>%
#'
#' @example inst/example-qvd_process_output.R
#'
#' @export
#'
qvd_process <- function(cohort,
                        qvd_value_file,
                        multi_or_single_site = 'single',
                        anomaly_or_exploratory = 'exploratory',
                        omop_or_pcornet,
                        time = FALSE,
                        time_span = c('2012-01-01', '2020-01-01'),
                        time_period = 'year',
                        age_groups = NULL,
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
        uncount(value_freq) %>%
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
        uncount(value_freq) %>%
        group_by(!!sym(site_col), value_type) %>%
        summarise(alltime_mean = mean(value_col, na.rm = TRUE),
                  alltime_sd = sd(value_col, na.rm = TRUE),
                  alltime_mean = ifelse(is.nan(alltime_mean), NA, alltime_mean),
                  alltime_sd = ifelse(is.nan(alltime_sd), NA, alltime_sd))

      qvd_final <- qvd_tbl %>%
        uncount(value_freq) %>%
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
