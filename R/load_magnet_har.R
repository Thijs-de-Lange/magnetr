#'@export
magnet_scenario_support <- function(indicators, scenarios, periods, file_path, file_type, file_suffix) {
  d1_scenarios_all <- NULL
  for (k in 1:length(scenarios)) {
    d1_periods_all <- NULL
    for (l in 1:length(periods)) {

      if(file_suffix != "sol") {
      d1a <- HARr::read_har(file.path(file_path,paste0("",scenarios[k],"_",periods[l],"",file_type,".",file_suffix,"")))

    } else if(file_suffix == "sol") { # stupid fix for error in read_har, only happening in v6 version .sol file it seems
        system(paste("har2gdx",file.path(file_path,paste0("",scenarios[k],"_",periods[l],"",file_type,".",file_suffix,"")), "./tmp.gdx"))
        system("gdx2har ./tmp.gdx ./tmp.har")
        d1a <- HARr::read_har("./tmp.har")
        file.remove("./tmp.har")
        file.remove("./tmp.gdx")
      }


      if (all(indicators %in% names(d1a)) == FALSE){
        stop("indicator name(s) are incorrect") } else {

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
} #end else statement after if statement wrong indicator name(s)

      }
      d1_periods_all <- rbind(d1_periods_all, d1_indicators_all)
    }
    d1_scenarios_all <- rbind(d1_scenarios_all, d1_periods_all) #%>%
      #dplyr::filter(region %in% regions)
  }
  return(d1_scenarios_all)
}


magnet_base_support <- function(indicators, scenarios, base_year, file_path, file_type) { #add year
  d1_scenarios_all <- NULL
  for (k in 1:length(scenarios)) {
    d1a <- HARr::read_har(file.path(file_path,paste0("BaseData_b",file_type,".har")))

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
    d1_scenarios_all <- rbind(d1_scenarios_all, d1_indicators_all) #%>%
      #dplyr::filter(region %in% regions)

  }
  return(d1_scenarios_all)
}


#'@title Load magnet results in long format in R
#'
#'\code{magnet_scenario} load results from the general computable general equilibrium model 'MAGNET' in long format
#'@param scenarios The scenario name in quotation marks or a vector of scenario names
#'@param periods The period name in quotation marks or a vector of the period names
#'@param indicators The indicator name quotation marks or a vector of the indicator names, indicator name(s) need to correspond with the Header names in the .har file
#'@param regions The region name quotation marks or a vector of the region names, region name(s) corresponds with abbreviations used in magnet
#'@param file_type This corresponds with from where the results are extracted, like from a .har or .sol file with the name "Update", "Solution" or "update_view"
#'@param file_path The file path where the 'MAGNET' results in e.g. .har or .sol files are stored
#'@param file_suffix The suffix of the data file e.g. har or sol
#'@examples
#'
#'\dontrun{
#'
#'raw_path <- "W:/WECR/magnet_data9/PCSL/4_magnet"/"Updates"
#'scenarios <-c("Base_PCSL_ProdPath_YouthBranch" , "Base_PCSL_ProdPath", "Base_PCSL_Extpath")
#'periods <- c("2014-2020", "2020-2025", "2025-2030", "2030-2035", "2035-2040", "2040-2045")
#'country <- c("Ken")

#'magnet_scenario(scenarios, periods, "POP", country,"update", dataUpdatesPath, "har")
#'}
#'
#' @importFrom magrittr %>%
#' @export
magnet_scenario <- function(indicators, scenarios, periods, file_path, file_type, file_suffix) {
  output <- magnet_scenario_support(indicators, scenarios, periods, file_path, file_type, file_suffix)

  #shows warning when regions used as input does not correspond with regions in dataset
  #if (all(regions %in% output$region) == FALSE){
  #  return("country/region name(s) are incorrect") } else {

    return(output)
#}
}

#'@title Load magnet base year data in long format in R
#'
#' @export
magnet_base <- function(indicators, scenarios, base_year, file_path, file_type) {
  output <- magnet_base_support(indicators, scenarios, base_year, file_path, file_type)

  #shows warning when regions used as input does not correspond with regions in dataset
 # if (all(regions %in% output$region) == FALSE){
#    return("country/region name(s) are incorrect") } else {

  return(output)
#    }
}


magnet_indicators = c("pop", "qlab", "gross_wage", "net_wage", "nutrient_cons_pc", "gdp", "price_cons_good_market_price", "endow_market_price", "price_cons_good_agent_price")





#'@title Load specific indicators from MAGNET in long format in R
#' @export
magnet_indicator <- function(indicator, scenarios, periods, base_year, path_project){

  path_basedata <- file.path(path_project,'Basedata')
  path_update <- file.path(path_project,'Updates')
  path_solutions <- file.path(path_project,'Solutions')



if(all(indicator %in% magnet_indicators) == FALSE){
  stop("typo in indicator: function support the indicators 'population', 'labour', 'nutrient_cons_pc' 'gdp' and 'endow_market_price'")
  } else {

  if(indicator == "pop") {
    output <- rbind(magnet_base_support("POP", scenarios, base_year, path_basedata, ""),
                 magnet_scenario_support("POP", scenarios, periods, path_update, "_Update", "har")) %>%
      dplyr::select(-indicator)



  } else if (indicator == "qlab") {
    output <- rbind(magnet_base_support("QLAB", scenarios, base_year, path_basedata, ""),
                 magnet_scenario_support("QLAB", scenarios, periods, path_update, "_Update", "har"))

    } else if (indicator == "gross_wage") {
    qlab <- rbind(magnet_base_support("QLAB", scenarios, base_year, path_basedata, ""),
                    magnet_scenario_support("QLAB", scenarios, periods, path_update, "_Update", "har")) %>%
      dplyr::select(-indicator, - variable_name) %>%
      dplyr::rename(qlab = value)

    evfb <- rbind(magnet_base_support("EVFB", scenarios, base_year, path_basedata, ""),
                  magnet_scenario_support("EVFB", scenarios, periods, path_update, "_Update", "har")) %>%
      dplyr::select(-indicator, - variable_name) %>%
      dplyr::rename(evfb = value)

    output <- dplyr::left_join(qlab, evfb) %>%
      dplyr::mutate(value = ifelse(qlab == 0, 0, evfb/qlab))

    } else if (indicator == "net_wage") {
      qlab <- rbind(magnet_base_support("QLAB", scenarios, base_year, path_basedata, ""),
                    magnet_scenario_support("QLAB", scenarios, periods, path_update, "_Update", "har")) %>%
        dplyr::select(-indicator, - variable_name) %>%
        dplyr::rename(qlab = value)

      evos <- rbind(magnet_base_support("EVOS", scenarios, base_year, path_basedata, ""),
                    magnet_scenario_support("EVOS", scenarios, periods, path_update, "_Update", "har")) %>%
        dplyr::select(-indicator, - variable_name) %>%
        dplyr::rename(evos = value)

      output <- dplyr::left_join(qlab, evos) %>%
        dplyr::mutate(value = ifelse(qlab == 0, 0, evos/qlab))


  } else if (indicator == "nutrient_cons_pc") {
    output <- rbind(magnet_base_support("NSPC", scenarios, base_year, path_basedata, "_view"),
                    magnet_scenario_support("NSPC", scenarios, periods, path_update, "_update_view", "har"))

  } else if (indicator == "gdp") {
    gdp_base <- magnet_base_support("AG02", scenarios, base_year,  path_basedata, "_view") %>%
      dplyr::rename(commodity1 = region,
             region = commodity) %>%
      dplyr::rename(commodity = commodity1) %>%
      dplyr::group_by(region, indicator, scenario, year) %>%
      dplyr::summarize(dplyr::across(c(value),
                       sum,
                       na.rm = TRUE),
                .groups = "drop"
      ) %>%
      dplyr::rename(value_base = value) %>%
      dplyr::select(-indicator)

    qgdp <- magnet_scenario_support("QGDP", scenarios, periods, path_solutions, "_Solution", "sol") %>%
      dplyr::rename(q = value) %>%
      dplyr::select(-indicator) %>%
      dplyr::mutate(year = as.character(year))

    output <- dplyr::left_join(gdp_base %>%
                              dplyr::select(-year)
                     , qgdp) %>%
      rbind(., gdp_base %>%
              dplyr::mutate(q = 0)
      ) %>%
      dplyr::group_by(region, scenario) %>%
      dplyr::arrange(year) %>%
      dplyr::mutate(percent_cumulative = cumprod(1 + (q/100))) %>%
      dplyr::mutate(value = value_base * percent_cumulative) %>%
      dplyr::ungroup()

    } else if (indicator == "price_cons_good_market_price") {
      vdpb_value <- rbind(magnet_base_support("VDPB", scenarios, base_year, path_basedata, ""),
                          magnet_scenario_support("VDPB", scenarios, periods, path_update, "_Update", "har")) %>%
        dplyr::rename(vdpb_value = value) %>%
        dplyr::select(-variable_name, -indicator)

      vmpb_value <- rbind(magnet_base_support("VMPB", scenarios, base_year, path_basedata, ""),
                          magnet_scenario_support("VMPB",scenarios, periods, path_update, "_Update", "har")) %>%
        dplyr::rename(vmpb_value = value) %>%
        dplyr::select(-variable_name, -indicator)

      vpb_value <- dplyr::left_join(vdpb_value, vmpb_value) %>%
        dplyr::mutate(vpb_value = vdpb_value + vmpb_value)

      vdpb_base <- magnet_base_support("VDPB", scenarios, base_year,  path_basedata, "") %>%
        dplyr::rename(value_base = value) %>%
        dplyr::select(-indicator, - variable_name)


      vmpb_base <- magnet_base_support("VMPB", scenarios, base_year,  path_basedata, "") %>%
        dplyr::rename(value_base = value) %>%
        dplyr::select(-indicator, - variable_name)



      qpd <- magnet_scenario_support("QPD",  scenarios, periods, path_solutions, "_Solution", "sol") %>%
        dplyr::rename(qpd = value) %>%
        dplyr::select(-indicator, - variable_name) %>%
        dplyr::mutate(year = as.character(year))


      qpm <- magnet_scenario_support("QPM", scenarios, periods, path_solutions, "_Solution", "sol") %>%
        dplyr::rename(qpm = value) %>%
        dplyr::select(-indicator, - variable_name) %>%
        dplyr::mutate(year = as.character(year))


      vdpb_volume <- dplyr::left_join(vdpb_base %>%
                                        dplyr::select(-year),
                                      qpd) %>%
        rbind(., vdpb_base %>%
                dplyr::mutate(qpd = 0)
        ) %>%
        dplyr::group_by(region, scenario, commodity) %>%
        dplyr::arrange(year) %>%
        dplyr::mutate(percent_cumulative = cumprod(1 + (qpd/100))) %>%
        dplyr::mutate(vdpb_volume = value_base * percent_cumulative) %>%
        dplyr::ungroup() %>%
        dplyr::rename(percent_cumulative_vdpb = percent_cumulative) %>%
        dplyr::select(-value_base)


      vmpb_volume <- dplyr::left_join(vmpb_base %>%
                                        dplyr::select(-year),
                                      qpm) %>%
        rbind(., vmpb_base %>%
                dplyr::mutate(qpm = 0)
        ) %>%
        dplyr::group_by(region, scenario, commodity) %>%
        dplyr::arrange(year) %>%
        dplyr::mutate(percent_cumulative = cumprod(1 + (qpm/100))) %>%
        dplyr::mutate(vmpb_volume = value_base * percent_cumulative) %>%
        dplyr::ungroup() %>%
        dplyr::rename(percent_cumulative_vmpb = percent_cumulative) %>%
        dplyr::select(-value_base)

      vpb_volume <- dplyr::left_join(vdpb_volume, vmpb_volume) %>%
        dplyr::mutate(vpb_volume = vdpb_volume + vmpb_volume)

      output <- dplyr::left_join(vpb_value, vpb_volume) %>%
        dplyr::mutate(price_cons_good_market_price = vpb_value / vpb_volume)
  } else if (indicator == "endow_market_price") {
    vfm_value <- rbind(magnet_base_support("VFM", scenarios, base_year, path_basedata, ""),
                      magnet_scenario_support("VFM", scenarios, periods, path_update, "_Update", "har")) %>%
      dplyr::rename(vfm_value = value,
                    variable = commodity) %>%
      dplyr::rename(commodity = variable1)


    vfm_base <- magnet_base_support("VFM", scenarios, base_year,  path_basedata, "") %>%
      dplyr::rename(value_base = value,
                    variable = commodity) %>%
      dplyr::rename(commodity = variable1) %>%
      dplyr::select(- variable_name) %>%
      dplyr::mutate(year = as.character(year))


    qo <- magnet_scenario_support("QO", scenarios, periods, path_solutions, "_Solution", "sol") %>%
      dplyr::rename(q = value) %>%
      dplyr::select(-indicator, - variable_name) %>%
      dplyr::mutate(year = as.character(year))


    vfm_volume <- dplyr::left_join(vfm_base %>%
                                 dplyr::select(-year),
                               qo) %>%
      rbind(., vfm_base %>%
              dplyr::mutate(q = 0)
      ) %>%
      dplyr::group_by(indicator, region, scenario, commodity, variable) %>%
      dplyr::arrange(year) %>%
      dplyr::mutate(percent_cumulative = cumprod(1 + (q/100))) %>%
      dplyr::mutate(vfm_volume = value_base * percent_cumulative) %>%
      dplyr::ungroup()

     output <- dplyr::left_join(vfm_volume , vfm_value) %>%
      dplyr::mutate(value = vfm_value/vfm_volume)

  } else if (indicator == "price_cons_good_agent_price") {
    vdpa_value <- rbind(magnet_base_support("VDPA", scenarios, base_year, path_basedata, ""),
                       magnet_scenario_support("VDPA", scenarios, periods, path_update, "_Update", "har")) %>%
      dplyr::rename(vdpm_value = value) %>%
      dplyr::select(-variable_name, -indicator)

    vipa_value <- rbind(magnet_base_support("VIPA", scenarios, base_year, path_basedata, ""),
                        magnet_scenario_support("VIPA",scenarios, periods, path_update, "_Update", "har")) %>%
      dplyr::rename(vipa_value = value) %>%
      dplyr::select(-variable_name, -indicator)

    vpa_value <- dplyr::left_join(vdpa_value, vipa_value) %>%
      dplyr::mutate(vpa_value = vdpm_value + vipa_value)

    vdpm_base <- magnet_base_support("VDPM", scenarios, base_year,  path_basedata, "") %>%
      dplyr::rename(value_base = value) %>%
      dplyr::select(-indicator, - variable_name)


    vipm_base <- magnet_base_support("VIPM", scenarios, base_year,  path_basedata, "") %>%
      dplyr::rename(value_base = value) %>%
      dplyr::select(-indicator, - variable_name)



    qpd <- magnet_scenario_support("QPD",  scenarios, periods, path_solutions, "_Solution", "sol") %>%
      dplyr::rename(qpd = value) %>%
      dplyr::select(-indicator, - variable_name) %>%
      dplyr::mutate(year = as.character(year))


    qpm <- magnet_scenario_support("QPM", scenarios, periods, path_solutions, "_Solution", "sol") %>%
      dplyr::rename(qpm = value) %>%
      dplyr::select(-indicator, - variable_name) %>%
      dplyr::mutate(year = as.character(year))


    vdpm_volume <- dplyr::left_join(vdpm_base %>%
                                 dplyr::select(-year),
                               qpd) %>%
      rbind(., vdpm_base %>%
              dplyr::mutate(qpd = 0)
      ) %>%
      dplyr::group_by(region, scenario, commodity) %>%
      dplyr::arrange(year) %>%
      dplyr::mutate(percent_cumulative = cumprod(1 + (qpd/100))) %>%
      dplyr::mutate(vdpm_volume = value_base * percent_cumulative) %>%
      dplyr::ungroup() %>%
      dplyr::rename(percent_cumulative_vdpm = percent_cumulative) %>%
      dplyr::select(-value_base)


    vipm_volume <- dplyr::left_join(vipm_base %>%
                                      dplyr::select(-year),
                                    qpm) %>%
      rbind(., vipm_base %>%
              dplyr::mutate(qpm = 0)
      ) %>%
      dplyr::group_by(region, scenario, commodity) %>%
      dplyr::arrange(year) %>%
      dplyr::mutate(percent_cumulative = cumprod(1 + (qpm/100))) %>%
      dplyr::mutate(vipm_volume = value_base * percent_cumulative) %>%
      dplyr::ungroup() %>%
      dplyr::rename(percent_cumulative_vipm = percent_cumulative) %>%
      dplyr::select(-value_base)

    vpm_volume <- dplyr::left_join(vdpm_volume, vipm_volume) %>%
      dplyr::mutate(vpm_volume = vdpm_volume + vipm_volume)

    output <- dplyr::left_join(vpa_value, vpm_volume) %>%
      dplyr::mutate(price_consumer_good = vpa_value / vpm_volume)



  }
    #shows warning when regions used as input does not correspond with regions in dataset
  #  if (all(regions %in% output$region) == FALSE){
 #     return("country/region name(s) are incorrect") } else {


  #    }

  }

  return(output)
}

utils::globalVariables(c(".", "commodity", "commodity1", "variable1", "region", "year", "scenario", "value", "value_base", "percent_cumulative", "magnet_indicators"))
