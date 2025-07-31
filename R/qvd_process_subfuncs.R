
#' Compute quantitative variable distribution
#'
#' @param qvd_value_file *tabular input* | dataframe or CSV file with information about each of the variables that should be
#' examined in the function. contains the following columns:
#' - `value_name` | *string* |
#' - `domain_tbl` | *character* | CDM table where the value data is found
#' - `value_field` | *character* | the name of the field with the quantitative variable OR the name of the person identifier column for patient count checks
#' - `date_field` | *character* | a date field in the `domain_tbl` that should be used for over time analyses
#' - `concept_field` | *character* | concept_id field with codes from the associated codeset (only needed when codeset is provided)
#' - `codeset_name` | *character* | the name of the codeset file; DO NOT include the file extension; optional
#' - `filter_logic` | *character* | a string indicating filter logic that should be applied to achieve the desired variable; optional
#' @param cohort *tabular input* | A dataframe with the cohort of patients for your study. Should include the columns:
#' - `person_id` / `patid` | *integer* / *character*
#' - `start_date` | *date*
#' - `end_date` | *date*
#' - `site` | *character*
#' @param grouped_list *vector* | the list of columns that should be used to group the variable table
#' @param omop_or_pcornet *string* | Option to run the function using the OMOP or PCORnet CDM as the default CDM
#'                        accepts `omop` or `pcornet`
#' @param time *boolean* | a logical that tells the function whether you would like to look at the output over time
#'
#' @returns a dataframe with the frequency distribution for each value associated with the variable
#'          of interest and summary statistics (mean, median, q1, q3, sd) describing the distribution
#'
#' @importFrom purrr set_names
#' @importFrom purrr reduce
#' @importFrom rlang parse_expr
#' @importFrom rlang :=
#'
compute_quant_val_dist <- function(qvd_value_file,
                                   cohort = results_tbl('scd_cohort'),
                                   grouped_list = 'site',
                                   omop_or_pcornet = 'omop',
                                   time = FALSE){

  qvd_list <- split(qvd_value_file, seq(nrow(qvd_value_file)))

  freq_rslt <- list()
  stat_rslt <- list()

  for(i in 1:length(qvd_list)){

    message(paste0('Starting ', qvd_list[[i]]$value_name))

    if(omop_or_pcornet == 'omop'){
      join_cols <- purrr::set_names('concept_id', qvd_list[[i]]$concept_field)
      person_col <- 'person_id'
    }else{
      join_cols <- purrr::set_names('concept_code', qvd_list[[i]]$concept_field)

      if(!is.na(qvd_list[[i]]$vocabulary_field)){
        join_cols2 <- set_names('vocabulary_id', qvd_list[[i]]$vocabulary_field)
        join_cols <- join_cols %>% append(join_cols2)
      }

      person_col <- 'patid'
    }

    ## Build table
    domain_tbl <- cdm_tbl(qvd_list[[i]]$domain_tbl) %>%
      inner_join(cohort) %>%
      filter(!!sym(qvd_list[[i]]$date_field) >= start_date &
               !!sym(qvd_list[[i]]$date_field) <= end_date) %>%
      group_by(!!!syms(grouped_list))

    if(time){
      domain_tbl <- domain_tbl %>%
        filter(!!sym(qvd_list[[i]]$date_field) >= time_start &
                 !!sym(qvd_list[[i]]$date_field) <= time_end) %>%
        group_by(time_start, time_increment, .add = TRUE)
    }

    if(!is.na(qvd_list[[i]]$filter_logic)){
      tbl_use <- domain_tbl %>%
        filter(!! rlang::parse_expr(qvd_list[[i]]$filter_logic))
    }else{tbl_use <- domain_tbl}

    if(!is.na(qvd_list[[i]]$codeset_name)){
      tbl_use <- tbl_use %>%
        inner_join(load_codeset(qvd_list[[i]]$codeset_name), by = join_cols)
    }else{tbl_use <- tbl_use}

    ## Frequencies by patient count or value count
    if(is.na(qvd_list[[i]]$value_field) || qvd_list[[i]]$value_field == person_col){

      pt_cts <- tbl_use %>%
        group_by(!!sym(person_col), .add = TRUE) %>%
        summarise(value_col = n()) %>%
        collect()

      get_values <- pt_cts %>%
        ungroup(!!sym(person_col)) %>%
        group_by(value_col, .add = TRUE) %>%
        summarise(value_freq = n()) %>%
        mutate(value_type = qvd_list[[i]]$value_name)

    }else{

      get_values <- tbl_use %>%
        group_by(!!sym(qvd_list[[i]]$value_field), .add = TRUE) %>%
        summarise(value_freq = n()) %>%
        collect() %>%
        rename('value_col' := !!sym(qvd_list[[i]]$value_field)) %>%
        mutate(value_type = qvd_list[[i]]$value_name)

    }

    ## Summarise numerical distribution
    stat_sum <- get_values %>%
      uncount(value_freq) %>%
      group_by(value_type, .add = TRUE) %>%
      summarise(mean_val = mean(value_col, na.rm = TRUE),
                median_val = median(value_col, na.rm = TRUE),
                sd_val = sd(value_col, na.rm = TRUE),
                q1_val = quantile(value_col, 0.25, na.rm = TRUE),
                q3_val = quantile(value_col, 0.75, na.rm = TRUE)) %>%
      mutate(across(where(is.numeric), ~replace_na(., NA)))

    freq_rslt[[i]] <- get_values
    stat_rslt[[i]] <- stat_sum

    rm(domain_tbl)
    rm(tbl_use)

  }

  freq_red <- purrr::reduce(.x = freq_rslt,
                            .f = dplyr::union)
  stat_red <- purrr::reduce(.x = stat_rslt,
                            .f = dplyr::union)

  final_rslt <- freq_red %>%
    left_join(stat_red)

  return(final_rslt)

}



#' Compute Kullback-Liebler divergence
#'
#' @param frequency_tbl the output of `compute_quant_val_dist`
#' @param kl_log_base a string indicating the log base to be used in the computations;
#'                    defaults to `log2`, `log` and `log10` are also acceptable
#'
#' @returns a dataframe with the kullback-liebler divergence value for the site's
#' distribution compared to the overall, all-site distribution
#'
#' @importFrom philentropy KL
#' @importFrom tibble column_to_rownames
#'
compute_kl_divergence <- function(frequency_tbl,
                                  kl_log_base = 'log2'){

  val_list <- frequency_tbl %>% ungroup() %>% distinct(value_type) %>% pull()

  val_rslt <- list()

  for(i in val_list){

    site_list <- frequency_tbl %>% distinct(site) %>% pull()

    network_distribution <- frequency_tbl %>%
      ungroup() %>%
      filter(value_type == i) %>%
      arrange(value_col) %>%
      select(value_col, value_freq) %>%
      group_by(value_col) %>%
      summarise(value_freq = sum(value_freq)) %>%
      ungroup() %>%
      rename('network_frequency' = value_freq) %>%
      mutate(network_total = sum(network_frequency),
             network_prop = network_frequency / network_total) %>%
      select(value_col, network_prop)

    site_rslt <- list()

    for(k in site_list){

      site_distribution <- frequency_tbl %>%
        ungroup() %>%
        filter(value_type == i, site == k) %>%
        arrange(value_col) %>%
        select(value_col, value_freq) %>%
        rename('site_frequency' = value_freq) %>%
        mutate(site_total = sum(site_frequency),
               site_prop = site_frequency / site_total) %>%
        select(value_col, site_prop)

      kl_matrix <- network_distribution %>%
        left_join(site_distribution) %>%
        filter(!is.na(value_col)) %>%
        mutate(site_prop = ifelse(is.na(site_prop), 0, site_prop),) %>%
        mutate(value_col = as.character(value_col)) %>%
        column_to_rownames(var = 'value_col')

      kl_vector_test <- kl_matrix %>% pull(site_prop)

      if(!all(kl_vector_test == 0)){
        test <- as.data.frame(t(kl_matrix))
        test <- as.matrix(test)

        val <- philentropy::KL(test, est.prob = 'empirical',
                               unit = kl_log_base)

        site_tibble <- tibble(site = k,
                              value_type = i,
                              kl = unname(val))

      }else{site_tibble <- tibble(site = k,
                                  value_type = i,
                                  kl = NA)}

      site_rslt[[k]] <- site_tibble
    }

    site_red <- purrr::reduce(.x = site_rslt,
                              .f = dplyr::union)

    val_rslt[[i]] <- site_red

  }

  val_red <- purrr::reduce(.x = val_rslt,
                           .f = dplyr::union)

  return(val_red)
}


#' Euclidean distance for QVD
#'
#' @param fot_input_tbl the output of `compute_fot` when `compute_quant_val_dist` is
#' used as the check_func input
#' @param grp_vars list of columns that should be used to group the table
#' @param euclidean_stat the statistic that should be computed across all sites to be used
#' as a comparison to the same site-specific value (i.e. the distance from the site mean to the all site mean)
#' defaults to `mean` but `median` is also accepted
#'
#' @returns a dataframe with the original stat value, the newly computed all site stat value,
#'          and the euclidean distance between the two distributions stratified by each time point
#'          and the additional grp_vars provided
#'
#' @importFrom stats loess
#' @importFrom stats predict
#' @importFrom stats frequency
#'
qvd_euclidean <- function(fot_input_tbl,
                          grp_vars,
                          euclidean_stat = 'mean'){

  update_grpvr <- grp_vars[!grp_vars %in% 'site']

  var_col <- paste0(euclidean_stat, '_val')

  ms_at_cj <- squba.gen:::compute_at_cross_join(cj_tbl=fot_input_tbl,
                                                cj_var_names = grp_vars)

  allsite_stats <- ms_at_cj %>%
    uncount(value_freq) %>%
    group_by(!!!syms(update_grpvr), time_start, time_increment) %>%
    summarise(allsite_mean = mean(value_col, na.rm = TRUE),
              allsite_median = median(value_col, na.rm = TRUE),
              allsite_sd = sd(value_col, na.rm = TRUE),
              allsite_q1 = quantile(value_col, 0.25, na.rm = TRUE),
              allsite_q3 = quantile(value_col, 0.75, na.rm = TRUE)) %>%
    mutate(across(where(is.numeric), ~replace_na(., NA)))

  site_summ <- ms_at_cj %>%
    select(-c(value_col, value_freq)) %>%
    distinct()

  allsite_stat <- paste0('allsite_', euclidean_stat)
  output_var <- paste0(euclidean_stat, '_val')

  ## Euclidean comp
  grp_tbls <- group_split(site_summ %>% unite(facet_col, !!!syms(grp_vars), sep = '_', remove = FALSE) %>%
                            group_by(facet_col))

  euclidean_dist <- function(x, y) sqrt(sum((x - y)^2))

  overall <- list()

  for(i in 1:length(grp_tbls)) {

    site_datenumeric <-
      grp_tbls[[i]] %>%
      left_join(allsite_stats) %>%
      mutate(date_numeric = as.numeric(time_start),
             output_var = !!sym(output_var),
             allsite_var = ifelse(is.na(!!sym(allsite_stat)), 0, !!sym(allsite_stat)))
    site_loess <- loess(output_var ~ date_numeric, data=site_datenumeric)
    site_loess_df <- as_tibble(predict(site_loess)) %>% rename(site_loess=1)
    euclidean_site_loess <- euclidean_dist(predict(site_loess), site_datenumeric$allsite_var)
    ms_witheuclidean <-
      cbind(site_datenumeric,site_loess_df) %>%
      mutate(dist_eucl_mean=euclidean_site_loess) #%>%
    # mutate(loess_predicted=predict(site_loess))

    overall[[i]] <- ms_witheuclidean

  }

  overall_reduce <- reduce(.x=overall,
                           .f=dplyr::union) %>% as_tibble() %>%
    mutate(dist_eucl_mean=round(dist_eucl_mean,2),
           site_loess=round(site_loess,2)) %>%
    select(-facet_col) %>%
    select(site,time_start, grp_vars, var_col,
           allsite_var, date_numeric,
           site_loess,dist_eucl_mean
    ) %>%
    mutate(euclidean_stat = euclidean_stat)

  return(overall_reduce)


}
