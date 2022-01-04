#' @importFrom magrittr %>%

#' @export
extract_MAGNET_d1 <- function(scenarios, periods, indicators, region_select, filetype, filepath, fileformat) {
  d1_scenarios_all <- NULL
  for (k in 1:length(scenarios)) {
    d1_periods_all <- NULL
    for (l in 1:length(periods)) {
      d1a <- HARr::read_har(file.path(filepath,paste0("",scenarios[k],"_",periods[l],"_",filetype,".",fileformat,"")))

      d1_indicators_all <- NULL
      for (m in 1:length(indicators)) {
        d1 <- d1a[[indicators[m]]]
        dim <- length(dimnames(d1))

        # indicator with 1 dimensions
        if(dim == "1"){
          d1_indicators <- d1 %>%
            data.frame() %>%
            dplyr::rename("value" = ".") %>%
            dplyr::mutate(region = row.names(.)) %>%
            dplyr::mutate(indicator = indicators[m]) %>%
            dplyr::mutate(scenario = scenarios[k]) %>%
            dplyr::mutate(year = as.numeric(substr(periods[l], 6, 9)))


          d1_indicators_all <- rbind(d1_indicators_all, d1_indicators)

          # indicator with 2 dimensions
        } else if (dim == "2") {
          names_col2 <- names(attributes(d1)$dim[2])

          d1_indicators <- d1 %>%
            data.frame() %>%
            dplyr::mutate(commodity = row.names(.)) %>%
            tidyr::pivot_longer(!commodity, names_to = paste0("",names_col2,""), values_to = "value") %>%
            dplyr::rename(region = paste0("",names_col2,"")) %>%
            dplyr::mutate(indicator = indicators[m]) %>%
            dplyr::mutate(scenario = scenarios[k]) %>%
            dplyr::mutate(year = as.numeric(substr(periods[l], 6, 9))) %>%
            dplyr::mutate(variable_name =  names(attributes(d1)$dim[1]))

          d1_indicators_all <- rbind(d1_indicators_all, d1_indicators)

          # indicator with 3 dimensions
        } else if (dim == "3")  {
          names_col2 <- names(attributes(d1)$dim[2])
          regions <-  dimnames(d1)[[3]]

          atr_2 <- attributes(d1)$dim[[2]]
          if (atr_2 == "1")  {
            for (n in 1:length(regions)) {

              d1_sel <- d1[, , regions[n]]

              d1_regions <- d1_sel %>%
                data.frame() %>%
                dplyr::rename("value" = ".") %>%
                dplyr::mutate(commodity = row.names(.)) %>%
                dplyr::mutate(indicator = indicators[m]) %>%
                dplyr::mutate(region = regions[n]) %>%
                dplyr::mutate(scenario = scenarios[k]) %>%
                dplyr::mutate(year = as.numeric(substr(periods[l], 6, 9))) %>%
                dplyr::mutate(variable_name =  names(attributes(d1)$dim[1]))


              d1_regions_all <- rbind(d1_regions_all, d1_regions)
            }
            d1_indicators_all <- rbind(d1_indicators_all, d1_regions_all)
          } else {
            d1_regions_all <- NULL
            for (n in 1:length(regions)) {

              d1_sel <- d1[, , regions[n]]

              d1_regions <- d1_sel %>%
                data.frame() %>%
                dplyr::mutate(variable1 = row.names(.)) %>%
                tidyr::pivot_longer(!variable1, names_to = paste0("",names_col2,""), values_to = "value") %>%
                dplyr::rename(commodity = paste0("",names_col2,"")) %>%
                dplyr::mutate(indicator = indicators[m]) %>%
                dplyr::mutate(region = regions[n]) %>%
                dplyr::mutate(scenario = scenarios[k]) %>%
                dplyr::mutate(year = as.numeric(substr(periods[l], 6, 9))) %>%
                dplyr::mutate(variable_name =  names(attributes(d1)$dim[1]))

              d1_regions_all <- rbind(d1_regions_all, d1_regions)
            }
            d1_indicators_all <- rbind(d1_indicators_all, d1_regions_all)
          }

          # indicator with 4 dimensions
        } else {
          names_col2 <- names(attributes(d1)$dim[2])
          variables <-  dimnames(d1)[[3]]
          regions <-  dimnames(d1)[[4]]

          d1_regions_all <- NULL
          for (n in 1:length(regions)) {

            d1_variables_all <- NULL
            for (o in 1:length(variables)) {

              d1_sel <- d1[, , variables[o], regions[n]]

              d1_variables <- d1_sel %>%
                data.frame() %>%
                dplyr::mutate(variable1 = row.names(.)) %>%
                tidyr::pivot_longer(!variable1, names_to = paste0("",names_col2,""), values_to = "value") %>%
                dplyr::rename(variable3 = paste0("",names_col2,"")) %>%
                dplyr::mutate(indicator = indicators[m]) %>%
                dplyr::mutate(region = regions[n]) %>%
                dplyr::mutate(commodity = variables[o]) %>%
                dplyr::mutate(scenario = scenarios[k]) %>%
                dplyr::mutate(year = as.numeric(substr(periods[l], 6, 9))) %>%
                dplyr::mutate(variable_name =  names(attributes(d1)$dim[1]))

              d1_variables_all <- rbind(d1_variables_all, d1_variables)
            }
            d1_regions_all <- rbind(d1_regions_all, d1_variables_all)
          }
          d1_indicators_all <- rbind(d1_indicators_all, d1_regions_all)
        }


      }
      d1_periods_all <- rbind(d1_periods_all, d1_indicators_all)
    }
    d1_scenarios_all <- rbind(d1_scenarios_all, d1_periods_all) %>%
      dplyr::filter(region %in% region_select)
  }
  print(d1_scenarios_all)
}


#' @export
extract_MAGNET_base <- function(scenarios, indicators, region_select, base_year, filetype, filepath) { #add year
  d1_scenarios_all <- NULL
  for (k in 1:length(scenarios)) {
      d1a <- HARr::read_har(file.path(filepath,paste0("BaseData_b_",filetype,".har")))

      d1_indicators_all <- NULL
      for (m in 1:length(indicators)) {
        d1 <- d1a[[indicators[m]]]
        dim <- length(dimnames(d1))

        # indicator with 1 dimensions
        if(dim == "1"){
          d1_indicators <- d1 %>%
            data.frame() %>%
            dplyr::rename("value" = ".") %>%
            dplyr::mutate(region = row.names(.)) %>%
            dplyr::mutate(indicator = indicators[m]) %>%
            dplyr::mutate(scenario = scenarios[k]) %>%
            dplyr::mutate(year = base_year) #variable maken!!


          d1_indicators_all <- rbind(d1_indicators_all, d1_indicators)

          # indicator with 2 dimensions
        } else if (dim == "2") {
          names_col2 <- names(attributes(d1)$dim[2])

          d1_indicators <- d1 %>%
            data.frame() %>%
            dplyr::mutate(commodity = row.names(.)) %>%
            tidyr::pivot_longer(!commodity, names_to = paste0("",names_col2,""), values_to = "value") %>%
            dplyr::rename(region = paste0("",names_col2,"")) %>%
            dplyr::mutate(indicator = indicators[m]) %>%
            dplyr::mutate(scenario = scenarios[k]) %>%
            mutate(year = base_year) %>%
            dplyr::mutate(variable_name =  names(attributes(d1)$dim[1]))

          d1_indicators_all <- rbind(d1_indicators_all, d1_indicators)

          # indicator with 3 dimensions
        } else if (dim == "3")  {
          names_col2 <- names(attributes(d1)$dim[2])
          regions <-  dimnames(d1)[[3]]

          atr_2 <- attributes(d1)$dim[[2]]
          if (atr_2 == "1")  {
            for (n in 1:length(regions)) {

              d1_sel <- d1[, , regions[n]]

              d1_regions <- d1_sel %>%
                data.frame() %>%
                dplyr::rename("value" = ".") %>%
                dplyr::mutate(commodity = row.names(.)) %>%
                dplyr::mutate(indicator = indicators[m]) %>%
                dplyr::mutate(region = regions[n]) %>%
                dplyr::mutate(scenario = scenarios[k]) %>%
                dplyr::mutate(year = base_year) %>%
                dplyr::mutate(variable_name =  names(attributes(d1)$dim[1]))


              d1_regions_all <- rbind(d1_regions_all, d1_regions)
            }
            d1_indicators_all <- rbind(d1_indicators_all, d1_regions_all)
          } else {
            d1_regions_all <- NULL
            for (n in 1:length(regions)) {

              d1_sel <- d1[, , regions[n]]

              d1_regions <- d1_sel %>%
                data.frame() %>%
                dplyr::mutate(variable1 = row.names(.)) %>%
                tidyr::pivot_longer(!variable1, names_to = paste0("",names_col2,""), values_to = "value") %>%
                dplyr::rename(commodity = paste0("",names_col2,"")) %>%
                dplyr::mutate(indicator = indicators[m]) %>%
                dplyr::mutate(region = regions[n]) %>%
                dplyr::mutate(scenario = scenarios[k]) %>%
                dplyr::mutate(year = base_year) %>%
                dplyr::mutate(variable_name =  names(attributes(d1)$dim[1]))

              d1_regions_all <- rbind(d1_regions_all, d1_regions)
            }
            d1_indicators_all <- rbind(d1_indicators_all, d1_regions_all)
          }

          # indicator with 4 dimensions
        } else {
          names_col2 <- names(attributes(d1)$dim[2])
          variables <-  dimnames(d1)[[3]]
          regions <-  dimnames(d1)[[4]]

          d1_regions_all <- NULL
          for (n in 1:length(regions)) {

            d1_variables_all <- NULL
            for (o in 1:length(variables)) {

              d1_sel <- d1[, , variables[o], regions[n]]

              d1_variables <- d1_sel %>%
                data.frame() %>%
                dplyr::mutate(variable1 = row.names(.)) %>%
                tidyr::pivot_longer(!variable1, names_to = paste0("",names_col2,""), values_to = "value") %>%
                dplyr::rename(variable3 = paste0("",names_col2,"")) %>%
                dplyr::mutate(indicator = indicators[m]) %>%
                dplyr::mutate(region = regions[n]) %>%
                dplyr::mutate(commodity = variables[o]) %>%
                dplyr::mutate(scenario = scenarios[k]) %>%
                dplyr::mutate(year = base_year) %>%
                dplyr::mutate(variable_name =  names(attributes(d1)$dim[1]))

              d1_variables_all <- rbind(d1_variables_all, d1_variables)
            }
            d1_regions_all <- rbind(d1_regions_all, d1_variables_all)
          }
          d1_indicators_all <- rbind(d1_indicators_all, d1_regions_all)
        }


    }
    d1_scenarios_all <- rbind(d1_scenarios_all, d1_indicators_all) %>%
      dplyr::filter(region %in% region_select)
  }
  print(d1_scenarios_all)
}


