#'@title Load MAGNET results in long format in R
#'
#'
#'@param scenarios The scenario name in quotation marks or a vector of scenario names
#'@param periods The period name in quotation marks or a vector of the period names
#'@param indicators The indicator name quotation marks or a vector of the indicator names, indicator name(s) need to correspond with the Header names in the .har file
#'@param region_select The region name quotation marks or a vector of the region names, region name(s) corresponds with abbreviations used in MAGNET
#'@param filetype This corresponds with from where the results are extracted, like from a .har or .sol file with the name "Update", "Solution" or "update_view"
#'@param file_path The file path where the MAGNET results in e.g. .har or .sol files are storred
#'@param file_suffix The suffix of the data file e.g. har or sol
#'@examples
#'
#'\dontrun{
#'
#'raw_path <- "W:/WECR/Magnet_data9/PCSL/4_MAGNET"/"Updates"
#'scenarios <-c("Base_PCSL_ProdPath_YouthBranch" , "Base_PCSL_ProdPath", "Base_PCSL_Extpath")
#'periods <- c("2014-2020", "2020-2025", "2025-2030", "2030-2035", "2035-2040", "2040-2045")
#'country <- c("Ken")

#'extract_MAGNET_d1(scenarios, periods, "POP", country,"update", dataUpdatesPath, "har")
#'}
#'
#' @importFrom magrittr %>%
#' @export
extract_MAGNET_d1 <- function(scenarios, periods, indicators, region_select, filetype, file_path, file_suffix) {
  d1_scenarios_all <- NULL
  for (k in 1:length(scenarios)) {
    d1_periods_all <- NULL
    for (l in 1:length(periods)) {
      d1a <- HARr::read_har(file.path(file_path,paste0("",scenarios[k],"_",periods[l],"_",filetype,".",file_suffix,"")))

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
extract_MAGNET_base <- function(scenarios, indicators, region_select, base_year, filetype, file_path) { #add year
  d1_scenarios_all <- NULL
  for (k in 1:length(scenarios)) {
      d1a <- HARr::read_har(file.path(file_path,paste0("BaseData_b_",filetype,".har")))

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
            dplyr::mutate(year = base_year) %>%
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



#' @export
magnet_indicator <- function(indicator, scenarios, periods, base_year, region_select, file_path_base, file_path_scenario){


  if(indicator == "population" | indicator == "gdp_pc") {
    population <- rbind(extract_MAGNET_base(scenarios, "POP", region_select, base_year, "all", file_path_base),
                 extract_MAGNET_d1(scenarios, periods, "POP", region_select, "Update", file_path_scenario, "har"))

    if(indicator == "population") {
      print(population) }

  } else if (indicator == "labour") {
    labour <- rbind(extract_MAGNET_base(scenarios, "QLAB", region_select, base_year, "all", file_path_base),
                 extract_MAGNET_d1(scenarios, periods, "QLAB", region_select, "Update", file_path_scenario, "har"))

    print(labour)

  } else if (indicator == "nutrient_cons_pc") {
    nutrient_cons_pc <- rbind(extract_MAGNET_base(scenarios, "NSPC", region_select, base_year, "all", file_path_base),
                    extract_MAGNET_d1(scenarios, periods, "NSPC", region_select, "update_view", file_path_scenario, "har"))

    print(nutrient_cons_pc)

  } else if (indicator == "gdp" | indicator == "gdp_pc") {
    gdp_base <- extract_MAGNET_base(scenarios, "AG02", region_select, base_year, "all", file_path_base) %>%
      dplyr::rename(commodity1 = region,
             region = commodity) %>%
      dplyr::rename(commodity = commodity1) %>%
      dplyr::group_by(region, indicator, scenario, year) %>%
      dplyr::summarize(dplyr::across(c(value),
                       sum,
                       na.rm = TRUE),
                .groups = "drop"
      ) %>%
      dplyr::rename(value_base = value)

    qgdp <- extract_MAGNET_d1(scenarios, periods, "qgdp", region_select, "Solution", file_path_scenario, "sol") %>%
      dplyr::rename(q = value) %>%
      dplyr::select(-indicator) %>%
      dplyr::mutate(year = as.character(year))

    gdp <- dplyr::left_join(gdp_base %>%
                              dplyr::select(-year)
                     , qgdp) %>%
      rbind(., gdp_base %>%
              dplyr::mutate(q = 0)
      ) %>%
      dplyr::group_by(indicator, region, scenario) %>%
      dplyr::arrange(year) %>%
      dplyr::mutate(percent_cumulative = cumprod(1 + (q/100))) %>%
      dplyr::mutate(value = value_base * percent_cumulative) %>%
      dplyr::ungroup()

#add code to derive gdp_pc
    gdp_pc <- "has to be added"

    if(indicator == "gdp") {
      print(gdp)
    } else if(indicator == "gdp_pc"){
        print(gdp_pc)
      }
  }
}


utils::globalVariables(c(".", "commodity", "commodity1", "variable1", "region", "year", "scenario", "value", "value_base", "percent_cumulative"))
