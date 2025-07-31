

#' *Single Site, Exploratory, Cross-Sectional*
#'
#' @param process_output the output of the `qvd_process` function
#' @param display_outliers for boxplot output, a boolean to indicate whether outliers should be displayed; defaults to FALSE
#' @param frequency_min an integer to establish a minimum amount of times a value should occur to be included in the output;
#'                      aimed at trimming infrequently occurring outliers for a cleaner plot; defaults to 5
#' @param value_type_filter a string or vector of strings to filter the graph to specific variables of interest
#'
#' @returns a halfeye density plot showing the density of the distribution for each variable with a boxplot underneath
#'          showing the summary of the distribution
#'
qvd_ss_exp_cs <- function(process_output,
                          display_outliers = FALSE,
                          frequency_min = 5,
                          value_type_filter = NULL){

  freq_dist <- process_output %>%
    mutate(tooltip = paste0('Variable: ', value_type,
                            '\nRaw Mean: ', round(mean_val, 3),
                            '\nRaw Median: ', round(median_val, 3),
                            '\nRaw Q1,Q3: ', round(q1_val, 3), ', ', round(q3_val,3),
                            '\nRaw SD: ', round(sd_val, 3))) %>%
    select(site, value_type, value_col, tooltip, value_freq) %>%
    filter(!is.na(value_col), value_freq > frequency_min) %>%
    uncount(value_freq)

  mean_input <- process_output %>%
    distinct(site, value_type, mean_val)

  if(display_outliers){
    outlier_color = 'lightgray'
  }else{outlier_color = NA}

  if(is.null(value_type_filter)){
    variable_list <- process_output %>% distinct(value_type) %>% pull()
  }else{
    variable_list <- process_output %>% filter(value_type %in% value_type_filter) %>%
      distinct(value_type) %>% pull()

    if(length(variable_list) > 5){
      cli::cli_alert_warning(paste0('We recommend using 5 or less variables as input to maintain',
                                    ' visibility on the graph.'))
    }
  }

  grob_list <- list()

  squba_build_patch <- colorRampPalette(unname(squba_colors_standard))

  colors <- squba_build_patch(length(variable_list))

  k <- 1

  for(i in variable_list){

    g2 <- ggplot(freq_dist %>% filter(value_type %in% c(i)),
                 aes(x = value_col, y = value_type, fill = value_type)) +
      stat_halfeye(
        # adjust bandwidth
        adjust = 10,
        # move to the right
        justification = -0.2,
        # remove the slub interval
        .width = 0,
        point_colour = NA
      ) +
      geom_boxplot_interactive(aes(tooltip = tooltip),
                               width = 0.2,
                               outliers = display_outliers,
                               outlier.color = outlier_color,
                               alpha = 0.5) +
      geom_point(data = mean_input %>% filter(value_type %in% i),
                 aes(y = value_type, x = mean_val, fill = value_type),
                 shape = 23, color = 'blue') +
      scale_fill_manual(values = colors[k]) +
      theme_minimal() +
      theme(legend.position = 'none',
            axis.text.y = element_blank(),
            axis.title = element_blank()) +
      facet_wrap(~value_type, scales = 'free')

    g2[["metadata"]] <- tibble('pkg_backend' = 'ggiraph',
                               'tooltip' = TRUE)

    grob_list[[i]] <- g2

    k <- k + 1

  }

  grob <- patchwork::wrap_plots(grob_list, ncol = 1)

  return(grob)

}

#' *Single Site, Exploratory, Longitudinal*
#'
#' @param process_output the output of the `qvd_process` function
#' @param value_type_filter a string or vector of strings to filter the graph to specific variables of interest
#' @param summary_stat a string indicating the summary statistic that should be displayed on the plot; required for
#'                     exploratory, longitudinal results; defaults to `mean`, but `median`, `q1`, `q3`, or `sd` are also accepted
#'
#' @returns a line plot displaying the summary_stat of choice across the time span
#'
qvd_ss_exp_la <- function(process_output,
                          value_type_filter = NULL,
                          summary_stat = 'mean'){

  if(is.null(value_type_filter)){
    process_output <- process_output
  }else{
    process_output <- process_output %>% filter(value_type %in% value_type_filter)

    variable_list <- process_output %>%
      distinct(value_type) %>% pull()

    if(length(variable_list) > 5){
      cli::cli_alert_warning(paste0('We recommend using 5 or less variables as input to maintain',
                                    ' visibility on the graph.'))
    }
  }

  grph <- process_output %>%
    distinct(site, time_start, value_type, mean_val, median_val,
             q1_val, q3_val, sd_val) %>%
    mutate(tooltip = paste0('Variable: ', value_type,
                            '\nMean: ', round(mean_val, 3),
                            '\nMedian: ', round(median_val, 3),
                            '\nQ1,Q3: ', round(q1_val, 3), ', ', round(q3_val,3),
                            '\nSD: ', round(sd_val, 3))) %>%
    ggplot(aes(x = time_start, y = !!sym(paste0(summary_stat, '_val')),
               color = value_type, group = value_type, text = tooltip)) +
    geom_line() +
    geom_point() +
    facet_grid(rows = 'value_type', scales = 'free', switch = 'y') +
    theme_bw() +
    theme(legend.position = 'none') +
    scale_color_squba() +
    labs(x = 'Time',
         y = stringr::str_to_sentence(summary_stat))

  grph[['metadata']] <- tibble('pkg_backend' = 'plotly',
                               'tooltip' = TRUE)

  return(grph)

}

#' *Single Site, Anomaly Detection, Cross-Sectional*
#'
#' @param process_output the output of the `qvd_process` function
#'
#' @returns a bar plot displaying the proportion of values falling the user-selected number of SD away from the mean
#'          lower and upper outliers are separated by the color of the bar
#'
qvd_ss_anom_cs <- function(process_output){

  grph <- process_output %>%
    mutate(tooltip = paste0('SD Threshold: ', sd_threshold,
                            '\nNo. Outliers: ', formatC(n_outlier, big.mark = ','),
                            '\nTotal Values: ', formatC(total_vals, big.mark = ','))) %>%
    mutate(prop_outlier = ifelse(outlier_type == 'lower', prop_outlier * -1, prop_outlier)) %>%
    ggplot(aes(y = value_type, x = prop_outlier, fill = outlier_type,
               tooltip = tooltip)) +
    geom_col_interactive() +
    theme_minimal() +
    theme(axis.title.y = element_blank()) +
    scale_fill_manual(values = c("#DF7713FF", "#579EA4FF")) +
    scale_x_continuous(labels = abs) +
    labs(x = 'Proportion of Outlier Values',
         fill = 'Outlier Direction')

  grph[['metadata']] <- tibble('pkg_backend' = 'ggiraph',
                               'tooltip' = TRUE)

  return(grph)

}

#' *Single Site, Anomaly Detection, Longitudinal*
#'
#' @param process_output the output of the `qvd_process` function
#' @param value_type_filter a string or vector of strings to filter the graph to specific variables of interest
#'
#' @returns a line plot showing the proportion of values falling the user-selected number of SD away from the mean at
#'          each time point; lower and upper outliers are displayed as separate lines
#'
qvd_ss_anom_la <- function(process_output,
                           value_type_filter = NULL){

  if(is.null(value_type_filter)){
    process_output <- process_output
  }else{
    process_output <- process_output %>% filter(value_type %in% value_type_filter)

    variable_list <- process_output %>%
      distinct(value_type) %>% pull()

    if(length(variable_list) > 5){
      cli::cli_alert_warning(paste0('We recommend using 5 or less variables as input to maintain',
                                    ' visibility on the graph.'))
    }
  }

  grph <- process_output %>%
    mutate(tooltip = paste0('SD Threshold: ', sd_threshold,
                            '\nNo. Outliers: ', formatC(n_outlier, big.mark = ','),
                            '\nTotal Values: ', formatC(total_vals, big.mark = ','))) %>%
    ggplot(aes(x = time_start, y = prop_outlier, color = outlier_type,
               group = outlier_type, tooltip = tooltip)) +
    geom_line() +
    geom_point_interactive() +
    scale_color_manual(values = c("#DF7713FF", "#579EA4FF")) +
    facet_grid(rows = 'value_type', switch = 'y') +
    theme_bw() +
    labs(x = 'Time',
         y = 'Proportion of Outlier Values \n(Based on All-Time Mean + SD)',
         color = 'Outlier Direction')

  grph[['metadata']] <- tibble('pkg_backend' = 'ggiraph',
                               'tooltip' = TRUE)

  return(grph)

}

#' *Multi Site, Exploratory, Cross-Sectional*
#'
#' @param process_output the output of the `qvd_process` function
#' @param display_outliers for boxplot output, a boolean to indicate whether outliers should be displayed; defaults to FALSE
#' @param frequency_min an integer to establish a minimum amount of times a value should occur to be included in the output;
#'                      aimed at trimming infrequently occurring outliers for a cleaner plot; defaults to 5
#' @param value_type_filter a string or vector of strings to filter the graph to specific variables of interest
#'
#' @returns a plot with boxplots for the distributions at each site, stratified by variable
#'
qvd_ms_exp_cs <- function(process_output,
                          value_type_filter = NULL,
                          frequency_min = 5,
                          display_outliers = FALSE){

  if(display_outliers){
    outlier_color = 'lightgray'
  }else{outlier_color = NA}

  if(is.null(value_type_filter)){
    process_output <- process_output
  }else{
    process_output <- process_output %>% filter(value_type %in% value_type_filter)

    variable_list <- process_output %>%
      ungroup() %>%
      distinct(value_type) %>% pull()

    if(length(variable_list) > 5){
      cli::cli_alert_warning(paste0('We recommend using 5 or less variables as input to maintain',
                                    ' visibility on the graph.'))
    }
  }

  freq_dist <- process_output %>%
    mutate(tooltip = paste0('Variable: ', value_type,
                            '\nMean: ', round(mean_val, 3),
                            '\nMedian: ', round(median_val, 3),
                            '\nQ1,Q3: ', round(q1_val, 3), ', ', round(q3_val,3),
                            '\nSD: ', round(sd_val, 3))) %>%
    select(site, value_type, value_col, tooltip, value_freq) %>%
    filter(!is.na(value_col), value_freq > frequency_min) %>%
    uncount(value_freq)

  mean_input <- process_output %>%
    distinct(site, value_type, mean_val)

  grph <- freq_dist %>%
    ggplot(aes(x = site, y = value_col, fill = site)) +
    geom_boxplot_interactive(aes(tooltip = tooltip),
                             outliers = display_outliers,
                             outlier.colour = outlier_color,
                             alpha = 0.5) +
    geom_point(data = mean_input, aes(x = site, y = mean_val, fill = site),
               shape = 23, color = 'blue') +
    facet_wrap(~value_type, scales = 'free', ncol = 2) +
    scale_fill_squba() +
    theme_minimal() +
    theme(legend.position = 'none',
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          axis.title.x = element_blank()) +
    labs(y = 'Value')

  grph[['metadata']] <- tibble('pkg_backend' = 'ggiraph',
                               'tooltip' = TRUE)

  return(grph)
}

#' *Multi Site, Exploratory, Longitudinal*
#'
#' @param process_output the output of the `qvd_process` function
#' @param value_type_filter a string or vector of strings to filter the graph to specific variables of interest
#' @param summary_stat a string indicating the summary statistic that should be displayed on the plot; required for
#'                     exploratory, longitudinal results; defaults to `mean`, but `median`, `q1`, `q3`, or `sd` are also accepted
#'
#' @returns a line plot displaying the summary_stat of interest across the time period for each site
#'
qvd_ms_exp_la <- function(process_output,
                          value_type_filter = NULL,
                          summary_stat = 'mean'){

  if(is.null(value_type_filter)){
    process_output <- process_output
  }else{
    process_output <- process_output %>% filter(value_type %in% value_type_filter)

    variable_list <- process_output %>%
      distinct(value_type) %>% pull()

    if(length(variable_list) > 5){
      cli::cli_alert_warning(paste0('We recommend using 5 or less variables as input to maintain',
                                    ' visibility on the graph.'))
    }
  }

  grph <- process_output %>%
    distinct(site, time_start, value_type, mean_val, median_val,
             q1_val, q3_val, sd_val) %>%
    mutate(tooltip = paste0('Variable: ', value_type,
                            '\nMean: ', round(mean_val, 3),
                            '\nMedian: ', round(median_val, 3),
                            '\nQ1,Q3: ', round(q1_val, 3), ', ', round(q3_val,3),
                            '\nSD: ', round(sd_val, 3))) %>%
    ggplot(aes(x = time_start, y = !!sym(paste0(summary_stat, '_val')),
               color = site, group = site, text = tooltip)) +
    geom_line() +
    geom_point() +
    facet_grid(rows = 'value_type', scales = 'free', switch = 'y') +
    theme_bw() +
    scale_color_squba() +
    labs(x = 'Time',
         y = stringr::str_to_sentence(summary_stat),
         color = 'Site')

  grph[['metadata']] <- tibble('pkg_backend' = 'plotly',
                               'tooltip' = TRUE)

  return(grph)

}

#' *Multi Site, Anomaly Detection, Cross-Sectional*
#'
#' @param process_output the output of the `qvd_process` function
#' @param value_type_filter a string or vector of strings to filter the graph to specific variables of interest
#'
#' @returns a radial lolipop graph displaying the KL divergence value for each site, indicating the divergence from
#'          the all-site frequency distritbution
#'
qvd_ms_anom_cs <- function(process_output,
                           value_type_filter = NULL){

  if(is.null(value_type_filter)){
    process_output <- process_output
  }else{
    process_output <- process_output %>% filter(value_type %in% value_type_filter)

    variable_list <- process_output %>%
      distinct(value_type) %>% pull()

    if(length(variable_list) > 5){
      cli::cli_alert_warning(paste0('We recommend using 5 or less variables as input to maintain',
                                    ' visibility on the graph.'))
    }
  }

  grph <- process_output %>%
    filter(!is.na(kl)) %>%
    mutate(tooltip = paste0('Site: ', site,
                            '\nKullback-Leibler: ', round(kl, 3))) %>%
    ggplot(aes(x = site, y = kl, tooltip = tooltip)) +
    coord_radial(r.axis.inside = FALSE, rotate.angle = TRUE) +
    guides(theta = guide_axis_theta(angle = 0)) +
    geom_segment(aes(x = site, xend = site, y = 0, yend = kl), color = 'navy') +
    geom_point_interactive(aes(color = site), size = 2) +
    scale_color_squba() +
    facet_wrap(~value_type, scales = 'free_y', ncol = 2) +
    theme_minimal() +
    theme(legend.position = 'none') +
    labs(y = '',
         x = 'Kullback-Leibler Divergence')

  grph[['metadata']] <- tibble('pkg_backend' = 'ggiraph',
                               'tooltip' = TRUE)

  return(grph)

}

#' *Multi Site, Anomaly Detection, Longitudinal*
#'
#' @param process_output the output of the `qvd_process` function
#' @param value_type_filter a string or vector of strings to filter the graph to specific variables of interest
#'
#' @returns three graphs:
#'    1) line graph that shows the smoothed euclidean_stat of a
#'    variable across time computation with the Euclidean distance associated with each line
#'    2) line graph that shows the raw euclidean_stat of a
#'    variable across time computation with the Euclidean distance associated with each line
#'    3) a bar graph with the Euclidean distance value for each site, with the average
#'    euclidean_stat as the fill
#'
#' THIS GRAPH SHOWS ONLY ONE VALUE TYPE AT A TIME!
#'
qvd_ms_anom_la <- function(process_output,
                           value_type_filter){

  if(is.null(value_type_filter) | length(value_type_filter) > 1){
    cli::cli_abort('Please choose one value type to visualize for this graph.')
  }


  filt_op <- process_output %>% filter(value_type == value_type_filter)
  stat_col <- colnames(filt_op)[grepl('_val', colnames(filt_op))]
  stat_lab <- process_output %>% ungroup() %>% distinct(euclidean_stat) %>%
    mutate(euclidean_stat = str_to_sentence(euclidean_stat)) %>% pull()

  allsites <-
    filt_op %>%
    select(time_start,value_type,allsite_var) %>% distinct() %>%
    rename(stat_value = allsite_var) %>%
    mutate(site=paste0('all site ', tolower(stat_lab))) %>%
    mutate(text_smooth=paste0("Site: ", site,
                              "\n", stat_lab, ": ",stat_value),
           text_raw=paste0("Site: ", site,
                           "\n", stat_lab, ": ",stat_value))

  dat_to_plot <-
    filt_op %>%
    rename('stat_value' := !!sym(stat_col)) %>%
    mutate(text_smooth=paste0("Site: ", site,
                              "\n","Euclidean Distance from All-Site ", stat_lab, ': ', dist_eucl_mean),
           text_raw=paste0("Site: ", site,
                           "\n","Site ", stat_lab, ": ",stat_value,
                           "\n","Site Smoothed ", stat_lab, ": ",site_loess,
                           "\n","Euclidean Distance from All-Site ", stat_lab, ': ', dist_eucl_mean))

  p <- dat_to_plot %>%
    ggplot(aes(y = stat_value, x = time_start, color = site, group = site, text = text_smooth)) +
    geom_line(data=allsites, linewidth=1.1) +
    geom_smooth(se=TRUE,alpha=0.1,linewidth=0.5, formula = y ~ x) +
    scale_color_squba() +
    theme_minimal() +
    #theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
    labs(y = paste0(stat_lab, ' (Loess)'),
         x = 'Time',
         title = paste0('Smoothed ', stat_lab, ' of ', value_type_filter, ' Across Time'))

  q <- dat_to_plot %>%
    ggplot(aes(y = stat_value, x = time_start, color = site,
               group=site, text=text_raw)) +
    scale_color_squba() +
    geom_line(data=allsites,linewidth=1.1) +
    geom_line(linewidth=0.2) +
    theme_minimal() +
    #theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
    labs(x = 'Time',
         y = stat_lab,
         title = paste0(stat_lab, ' of ', value_type_filter, ' Across Time'))

  t <- dat_to_plot %>%
    distinct(site, dist_eucl_mean, site_loess) %>%
    group_by(site, dist_eucl_mean) %>%
    summarise(mean_site_loess = mean(site_loess)) %>%
    mutate(tooltip = paste0('Site: ', site,
                            '\nEuclidean Distance: ', dist_eucl_mean,
                            '\nAverage Loess ', stat_lab, ': ', mean_site_loess)) %>%
    ggplot(aes(x = site, y = dist_eucl_mean, fill = mean_site_loess, tooltip = tooltip)) +
    geom_col_interactive() +
    # geom_text(aes(label = dist_eucl_mean), vjust = 2, size = 3,
    #           show.legend = FALSE) +
    coord_radial(r.axis.inside = FALSE, rotate.angle = TRUE) +
    guides(theta = guide_axis_theta(angle = 0)) +
    theme_minimal() +
    scale_fill_squba(palette = 'diverging', discrete = FALSE) +
    # theme(legend.position = 'bottom',
    #       legend.text = element_text(angle = 45, vjust = 0.9, hjust = 1),
    #       axis.text.x = element_text(face = 'bold'))
    labs(fill = paste0(stat_lab,  '\n(Loess)'),
         y ='Euclidean Distance',
         x = '',
         title = paste0('Euclidean Distance for ', value_type_filter))

  p[['metadata']] <- tibble('pkg_backend' = 'plotly',
                            'tooltip' = TRUE)

  q[['metadata']] <- tibble('pkg_backend' = 'plotly',
                            'tooltip' = TRUE)

  t[['metadata']] <- tibble('pkg_backend' = 'ggiraph',
                            'tooltip' = TRUE)

  output <- list(p,
                 q,
                 t)

  return(output)
}

