#' @importFrom magrittr %>%


extract_MAGNET_d1 <- function(scenarios, periods, indicators, filetype, filepath, fileformat) {
  d1_scenarios_all <- NULL
  for (k in 1:length(scenarios)) {
    d1_periods_all <- NULL
    for (l in 1:length(periods)) {
      d1a <- HARr::read_har(file.path(filepath,paste0("",scenarios[k],"_",periods[l],"_",filetype,".",fileformat,"")))

      d1_indicators_all <- NULL
      for (m in 1:length(indicators)) {
        d1 <- d1a[[indicators[m]]]
        d1_indicators <- d1 %>%
          data.frame() %>%
          dplyr::rename("value" = ".") %>%
          dplyr::mutate(region = row.names(.data)) %>%
          dplyr::mutate(indicator = indicators[m]) %>%
          dplyr::mutate(scenario = scenarios[k]) %>%
          dplyr::mutate(year = as.numeric(substr(periods[l], 6, 9)))


        d1_indicators_all <- rbind(d1_indicators_all, d1_indicators)
      }
      d1_periods_all <- rbind(d1_periods_all, d1_indicators_all)
    }
    d1_scenarios_all <- rbind(d1_scenarios_all, d1_periods_all)
  }
  print(d1_scenarios_all)
}
