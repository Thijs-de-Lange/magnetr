# Create new folder for relevant files
raw_path <- "W:/WECR/Magnet_data9/PCSL/4_MAGNET"

dataBasePath <- file.path(raw_path,'Basedata')
dataUpdatesPath <- file.path(raw_path,'Updates')
dataSolPath <- file.path(raw_path,'Solutions')
dataAddPath <- file.path(raw_path,'Additional')
#dataShocksPath <- "./4_MAGNET/Shocks"
dataResultPath <- file.path(raw_path,'Results')

scenarios<-c("Base_PCSL_ProdPath_YouthBranch", "Base_PCSL_ProdPath", "Base_PCSL_Extpath")
periods <- c("2014-2020", "2020-2025", "2025-2030", "2030-2035", "2035-2040", "2040-2045")


extract_MAGNET_d1(scenarios, periods, "POP", "update", dataUpdatesPath, "har")
