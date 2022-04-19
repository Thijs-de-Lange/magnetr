#test wrong country/region warning 1
test_that("wrong country/region warning", {

  indicator <- "POP"
  country <- c("Ken", "wrong_country")
  scenario <-c("Base_PCSL_ProdPath_YouthBranch" , "Base_PCSL_ProdPath")
  period <- c("2035-2040", "2040-2045")


  raw_path <- "W:/WECR/Magnet_data9/PCSL/4_MAGNET"

  dataUpdatesPath <- file.path(raw_path,'Updates')
  file_type <- "update"
  file_suffix <- "har"

  expect_equal(magnet_scenario(indicator, country, scenario, period,  dataUpdatesPath, file_type, file_suffix),
               "country/region name(s) are incorrect")

})


#test wrong country/region warning 2
test_that("wrong country/region warning 2", {

  indicator <- "nutrient_cons_pc"
  country <- c("Ken", "wrong_country")
  scenario <-c("Base_PCSL_ProdPath", "Base_PCSL_Extpath")
  period <- c("2030-2035", "2035-2040", "2040-2045")
  base_year = "2014"

  raw_path <- "W:/WECR/Magnet_data9/PCSL/4_MAGNET"
  dataBasePath <- file.path(raw_path,'Basedata')
  dataUpdatesPath <- file.path(raw_path,'Updates')

  expect_equal(magnet_indicator(indicator, country, scenario, period, base_year, dataBasePath, dataUpdatesPath),
               "country/region name(s) are incorrect")

})


####default functie voor datapath toevoegen!!!!

#test wrong indicator warning 1
test_that("wrong indicator warning", {

  indicator <- "wrong_indicator"
  country <- c("Ken")
  scenario <-c("Base_PCSL_ProdPath_YouthBranch")
  period <- c("2014-2020", "2020-2025")
  base_year = "2014"

  raw_path <- "W:/WECR/Magnet_data9/PCSL/4_MAGNET"
  dataBasePath <- file.path(raw_path,'Basedata')
  dataUpdatesPath <- file.path(raw_path,'Updates')

  expect_error(magnet_indicator(indicator, country, scenario, period, base_year, dataBasePath, dataUpdatesPath),
               "typo in indicator: function support the indicators 'population', 'labour', 'nutrient_cons_pc' 'gdp' and 'gdp_pc'", fixed = TRUE)

})



#test wrong indicator warning 2, is not working well
test_that("wrong indicator warning 2", {

  indicator <- "wrong_indicator"
  country <- c("Ken")
  scenario <-c("Base_PCSL_ProdPath_YouthBranch" , "Base_PCSL_ProdPath", "Base_PCSL_Extpath")
  period <- c("2014-2020", "2020-2025", "2025-2030", "2030-2035", "2035-2040", "2040-2045")


  raw_path <- "W:/WECR/Magnet_data9/PCSL/4_MAGNET"

  dataUpdatesPath <- file.path(raw_path,'Updates')
  file_type <- "update"
  file_suffix <- "har"

  expect_error(magnet_scenario(indicator, country, scenario, period,  dataUpdatesPath, file_type, file_suffix),
               "indicator name(s) are incorrect", fixed = TRUE)
})

indicator <- "endow_market_price"
country <- c("Eth")
scenario <-c("Base_PCSL_ProdPath")
period <- c("2014-2020", "2020-2025")
base_year = "2014"


raw_path <- "W:/WECR/Magnet_data9/PCSL/4_MAGNET"
dataBasePath <- file.path(raw_path,'Basedata')
dataUpdatesPath <- file.path(raw_path,'Updates')

path_solutions <- file.path(raw_path,'Solutions')
path_basedata <- file.path(raw_path,'Basedata')
qpm <- magnet_scenario_support("TF", scenario, period, path_solutions, "Solution", "sol")

price_cons_good_agent_price <- magnet_indicator("price_cons_good_agent_price", scenario, period, base_year, raw_path)

endow_market_price <- magnet_indicator("endow_market_price",  scenario, period, base_year, raw_path)

gdp <- magnet_indicator("gdp",  scenario, period, base_year, raw_path)

vdpm_base <- magnet_base_support("VDPM", scenario, base_year,  path_basedata, "all")
qpd <- magnet_scenario_support("qpd", scenarios, periods, path_solutions, "Solution", "sol")


qo <- magnet_scenario_support("QO", scenario, period, path_solutions, "Solution", "sol")


#####

magnet_path <- "W:/WECR/Magnet_data4/Thijs/Dhaka_uganda_5lab/4_MAGNET"
dataBasePath <- file.path(magnet_path,'Basedata')
dataUpdatesPath <- file.path(magnet_path,'Updates')
dataSolutionPath <- file.path(magnet_path,'Solutions')

country <- c("PAK")
scenario <-c("BaseGDPExo_msx_SSP2_5lab_labshock")
period <- c("2014-2018")#, "2018-2020", "2020-2025", "2025-2030","2035-2040", "2040-2045", "2045-2050")
base_year <- "2014"


gross_wage <- magnet_indicator("gross_wage", scenario, period, base_year, magnet_path)

pop <- magnet_indicator("pop", scenario, period, base_year, magnet_path)

gdp <- magnet_indicator("gdp", scenario, period, base_year, magnet_path)

nutrient_cons_pc <- magnet_indicator("nutrient_cons_pc", scenario, period, base_year, magnet_path)

price_cons_good_market_price <- magnet_indicator("price_cons_good_market_price", scenario, period, base_year, magnet_path)


gdp_base <- magnet_base_support("AG02", scenario, base_year,  dataBasePath, "_view") %>%
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

qgdp <- magnet_scenario_support("QGDP", scenario, period, dataSolutionPath, "Solution", "sol") %>%
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
