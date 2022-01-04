# Create new folder for relevant files
raw_path <- "W:/WECR/Magnet_data9/PCSL/4_MAGNET"

dataBasePath <- file.path(raw_path,'Basedata')
dataUpdatesPath <- file.path(raw_path,'Updates')
dataSolPath <- file.path(raw_path,'Solutions')
dataAddPath <- file.path(raw_path,'Additional')
#dataShocksPath <- "./4_MAGNET/Shocks"
dataResultPath <- file.path(raw_path,'Results')

scenarios <-c("Base_PCSL_ProdPath_YouthBranch" , "Base_PCSL_ProdPath", "Base_PCSL_Extpath")
periods <- c("2014-2020", "2020-2025", "2025-2030", "2030-2035", "2035-2040", "2040-2045")
country <- c("Ken")

extract_MAGNET_d1(scenarios, periods, "POP", country,"update", dataUpdatesPath, "har")

extract_MAGNET_base(scenarios, "BI02", country, "2014", "all", dataBasePath)

magnet_indicator("population", scenarios, periods, "2014", country, dataBasePath, dataUpdatesPath)

magnet_indicator("nutrient_cons_pc", scenarios, periods, "2014", country, dataBasePath, dataUpdatesPath)
