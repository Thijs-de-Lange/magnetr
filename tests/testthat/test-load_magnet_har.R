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


