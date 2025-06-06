
### Main har read and write functions ------
#' @export
magnet_read_all_headers <- function(fullfilepath, whitelist = c(), blacklist = c(), useCoefficientsAsNames = FALSE) {
  #Function returns all headers in a list of tidy data frames, where the names of the list are the headers (or coefficients).
  #It converts all the header names and column names (except Value) to upper case. This is our default but the HARr package spits out lower case

  dflist <- HARr::read_har(fullfilepath, useCoefficientsAsNames = useCoefficientsAsNames, toLowerCase = FALSE)

  # Sometimes coefficient names can be duplicate, very unhandy so fixing here by forcing names to be unique.
  # CONVERTING TO UPPERCASE as sometimes the headers are note consistent
  headers <- make.names(toupper(names(dflist)), unique = TRUE)
  names(dflist) <- headers

  if(length(whitelist)>0){ # this simple filters to keep anything in the whitelist.
    headers <- intersect(headers, whitelist)
  }

  #by default blacklist the "XXCD","XXCR","XXCP","XXHS"
  #headers which pop up with readhar and contain some meta data
  blacklist = c(blacklist,"XXCD","XXCR","XXCP","XXHS")
  headers <- setdiff(headers, blacklist)

  dflist_out <- list()
  for (h in headers) {
    d1_header <- reshape2::melt(dflist[[h]])

    if(setequal(colnames(d1_header), c("Var1","Var2","Var3","Var4","Var5","Var6","Var7","value"))){
      # this catches some odd behaviour when the header contains a single vector. Bug in HARr package.
      d1_header <- select(d1_header, -c(Var1,Var2,Var3,Var4,Var5,Var6,Var7))
    }
    if(setequal(colnames(d1_header), c("Var1","Var2","value")) & is.integer(d1_header$value)){
      # this catches some odd behaviour when the header is integer lists, like mappings or  Bug in HARr package.
      d1_header <- select(d1_header, -c(Var1,Var2))
    }
    # CONVERTING TO UPPERCASE just in case, probably is ok everywhere but HARr setting can mess it up.
    colnames(d1_header) <- toupper(colnames(d1_header))

    #Sometines, like with REG,REG, colnames can be double, should be avoided
    cnamesnew <-  make.names(colnames(d1_header), unique = TRUE)
      #changeing some commendouble names names to _2 instead of .1
    cnamesnew <- gsub("REG\\.1","REG_2",cnamesnew) # this is the most typical case, and I change to REG and REG_2 here.
    cnamesnew <- gsub("SAMAC\\.1","SAMAC_2",cnamesnew)
    cnamesnew <- gsub("CTRY\\.1","CTRY_2",cnamesnew)
    cnamesnew <- gsub("TRAD_COMM\\.1","TRAD_COMM_2",cnamesnew)
    cnamesnew <- gsub("COMM\\.1","COMM_2",cnamesnew)

    #Some headers use ENDWL_COMM instead of ENDWL and it drives me crazy
    cnamesnew <- gsub("ENDWL_COMM","ENDWL",cnamesnew)

    colnames(d1_header) <- cnamesnew

    #using Value with capital V by default
    d1_header <- rename(d1_header, Value = VALUE)

    dflist_out[[h]] <- d1_header
  }

  return(dflist_out)
}

magnet_prepdf_for_write_har <- function(df, dimlist) {
  # prepares the array for writing in order of the dimlist
  # dimlist should be a named list with the entries of the dimensions in the right order and the same names as df
  # This is to make sure that the dimension is correct (in case of missing values adds 0 with left_join0)
  outputdf <- expand.grid(dimlist) %>% left_join0(df)

  return(outputdf)
}

#' @export
magnet_write_har <- function(dflist, outfilename) {
  # Takes a list of named dataframes where the names should be the
  # final HEADER names that will be in the output hr file. The values should be in a column named 'Value'
  # for example
  # exampledf <- data.frame (REG  = c("NL", "ROW", "NL", "ROW"), COMM = c("pdr","pdr","Wht","wht"), Value = c(1,2,3,4))
  # dflist <- list(PROD = exampledf, PRD2 = exampledf)
  # Where "PROD" will be the header name and REG en COMM the sets.
  # Make sure header names are not longer than 4 and set names not longer than 12 characters
  # magnet_write_har(dflist ,"test.har")
  # The write har is sensitive to the order of the values related to the order of the dimensions and the code  tries to do that.
  # Use the magnet_prepdf_for_write_har to changes dimensions order manually
  # If you set descriptoin attribute it will be used in the name column of the har files.
  ar <- list()

  if(is.data.frame(dflist)){ # If someone juts passing a single dataframe, make it work.
    dflist <- list(NH01 = dflist)
    warning("Seem you provided a single dataframe. Writing as header NHO1")
  }

  for (h in names(dflist)) {
    df <- dflist[[h]]

    if ("value" %in% colnames(df)){df <- rename(df, Value = value)}

    if(nchar(h) > 4) {
      if(is.null(attr(df,"description"))){
        attributes(df)$description <- h
      }

      h <- substr(h,1,4)
      print("warning, headers cannot be longer than 4 characters, shortening")


    }


    if(ncol(df) == 1 & is.character(df$Value)){
      #This means it's a set, and writing just simple means putting the list of strings in there.
      ar[[h]] <- df$Value
      next
    }

    dimlist = list()
    dimsizelist = list()

    for (c in colnames(select(df, -Value))){
      dimlist[[c]] <- unique(df[[c]])
      dimsizelist[[c]] <- length(unique(df[[c]]))
    }

    # This is to make sure that the dimension is correct (in case of missing values adds 0 with left_join0)
    outputdf <- expand.grid(dimlist) %>% left_join0(df)

    # By default we use "REG_2" for duplicate dimensions as our datafram cant handle that.
    # For har file bring back to regular REG.
    names(dimlist) <- replace(names(dimlist), names(dimlist) == "REG_2", "REG")
    names(dimlist) <- replace(names(dimlist), names(dimlist) == "REG_3", "REG")
    names(dimlist) <- replace(names(dimlist), names(dimlist) == "CTRY_2", "CTRY")
    names(dimlist) <- replace(names(dimlist), names(dimlist) == "SAMAC_2", "SAMAC")
    names(dimlist) <- replace(names(dimlist), names(dimlist) == "COMM_2", "COMM")
    names(dimlist) <- replace(names(dimlist), names(dimlist) == "TRAD_COMM_2", "TRAD_COMM")

    ar[[h]] <- array(
      outputdf$Value,
      dim = dimsizelist,
      dimnames = dimlist
    )

    attributes(ar[[h]])$description <- attr(df,"description")
  }

  HARr::write_har(ar, outfilename)
}

### Dealing with MAGNET scenario names -----

#' @export
magnet_get_scenarioinfo <- function(maindir) {
  #Creates a dataframe with usefull info of all scenarios with a log file present.
  # Used as basis for other read functions

  getinfobasedata <- function(x){
    answertxt <- read.delim(x,header=FALSE,quote="")
    m = match(c("Base data file"),answertxt$V1) + 1
    BaseData_b <- str_trim(answertxt[m,])
  }
  getinfosets <- function(x){
    answertxt <- read.delim(x,header=FALSE,quote="")
    m = match(c("Base data file"),answertxt$V1) + 4
    BaseData_b <- file.path("4_MAGNET","BaseData","Sets",gsub("- ","",str_trim(answertxt[m,])))
  }

  getinfoperiods <- function(x){
    answertxt <- read.delim(x,header=FALSE,quote="")
    answertxt <- subset(answertxt, grepl("Period", V1)) %>%
      mutate(Periods = gsub(" ","",str_extract(V1,"\\d{4} - \\d{4}")))
    return(paste(answertxt$Periods, collapse = ";"))
  }

  scen <- file.info(list.files(file.path(maindir,"4_MAGNET","Scenarios"),recursive = TRUE, pattern = "GTAPLog.*\\.log", full.names = TRUE))
  scen = scen[with(scen, order(as.POSIXct(mtime), decreasing = TRUE)),]

  scen$files <- rownames(scen)
  scen$Scenario <- dirname(gsub(".*/4_MAGNET/Scenarios/","",rownames(scen)))
  scen$Maindir <- gsub("/4_MAGNET/Scenarios/.*","",rownames(scen))
  scen <- select(scen, c(Scenario,Maindir)) %>% unique()
  rownames(scen) <- NULL
  scen <- scen %>% mutate(scentextfile = file.path(Maindir, "4_MAGNET","Scenarios", Scenario, paste(Scenario,".txt", sep = ""))) %>%
    subset(file.exists(scentextfile))

  scenariosinfo <- scen %>%
    mutate(answerfile = file.path(Maindir, "4_MAGNET","Scenarios", Scenario, paste(Scenario,".txt", sep = ""))) %>%
    subset(file.exists(answerfile)) %>%
    mutate(BaseData_b = file.path(maindir,unlist(lapply(answerfile, getinfobasedata)))) %>%
    mutate(BaseData_b_view = gsub("\\.har$","_view.har",BaseData_b), BaseData_b_tax = gsub("\\.har$","_tax.har",BaseData_b)) %>%
    mutate(BaseData_b_solution = ifelse(grepl("update_view",BaseData_b_view),
                                        gsub("_update.har$","_solution.sol",BaseData_b,ignore.case = TRUE),"")) %>%
    mutate(BaseData_b_solution = gsub("Updates","/Solutions/",BaseData_b_solution,ignore.case = TRUE)) %>%
    mutate(Periods = unlist(lapply(answerfile, getinfoperiods))) %>%
    mutate(Sets = file.path(maindir,unlist(lapply(answerfile, getinfosets)))) %>%

  return(scenariosinfo)
}

magnet_get_scenarioinfo_long <- function(maindir) {
  # Creates a dataframe, long list, with usefull info of all scenarios with a log file present.
  # Used as basis for other read functions

  scen <- file.info(list.files(file.path(maindir,"4_MAGNET","Scenarios"), recursive = TRUE, pattern = "GTAPLog.*\\.log", full.names = TRUE))
  scen = scen[with(scen, order(as.POSIXct(mtime), decreasing = TRUE)),]

  scen$files <- rownames(scen)
  scen$Scenario <- dirname(gsub(".*/4_MAGNET/Scenarios/","",rownames(scen)))
  scen$Maindir <- gsub("/4_MAGNET/Scenarios/.*","",rownames(scen))
  scen <- select(scen, c(Scenario,Maindir)) %>% unique()
  rownames(scen) <- NULL
  scen <- scen %>% mutate(scentextfile = file.path(Maindir, "4_MAGNET","Scenarios", Scenario, paste(Scenario,".txt", sep = ""))) %>%
    subset(file.exists(scentextfile))


  sceninfoall <- data.frame()
  for(i in 1:nrow(scen)) {
    scenname = scen$Scenario[i]
    scentextfile <- scen$scentextfile[i]
    scentxt <- read.delim(scentextfile,  header = FALSE,quote="")
    colnames(scentxt) <- "Settings"
    scentxt$Section <- scentxt$Settings
    for (i in 1:length(scentxt$Settings)) {
      t <- scentxt$Settings[i]
      hdr <- TRUE
      if (substr(t, 1, 3) == "   ") {
        hdr <- FALSE
      }
      if(hdr){
        hdrtxt = t
      }
      scentxt$Section[i] <- hdrtxt
    }
    basedata <- file.path(str_trim(subset(scentxt, Section == "Base data file" & Settings != "Base data file")$Settings))
    baseinfo <- subset(scentxt, !grepl("Period",Section)) %>% rename(Period = Section)
    sceninfo <- subset(scentxt, grepl("Period",Section)) %>% rename(Period = Section)
    sceninfo <- scentxt %>% rename(Period = Section)

    sceninfo$Question <- ""
    sceninfo$Answer <- ""
    hdrtxt = ""
    answertxt = ""
    for (i in 1:length(sceninfo$Settings)) {
      t <- sceninfo$Settings[i]
      answertxt = ""
      hdr <- TRUE
      if (grepl(" - ",t)) {
        hdr <- FALSE
        answertxt = t
      }
      if(hdr){
        hdrtxt = t
      }
      sceninfo$Question[i] <- str_trim(hdrtxt)
      sceninfo$Answer[i] <- gsub("- ","",str_trim(answertxt))
    }
    sceninfo <- sceninfo %>% mutate(Answer = ifelse(Period == "Base data file" & Question != 'Base data file',Question,Answer))
    sceninfo <- sceninfo %>% mutate(Question = ifelse(Period == "Base data file" & Question != 'Base data file',"Base data file",Question))
    # sceninfo <- sceninfo %>% mutate(Answer = ifelse(Period == "Scenario description" & Question != 'Scenario description',Question,Answer))
    # sceninfo <- sceninfo %>% mutate(Answer = ifelse(Period == "Scenario name" & Question != 'Scenario name',Question,Answer))
    #
    sceninfo2 <- subset(sceninfo,Answer != "") %>% select(-Settings) %>%
      mutate(fileext = tolower(tools::file_ext(Answer))) %>% subset(fileext != "") %>%
      mutate(folder = case_when(Question == "Closure file" ~ "CommandFiles/Closures",
                                Question == "Shocks file" ~ "CommandFiles/Shocks",
                                Question == "Solution method" ~ "CommandFiles/SolutionMethods",
                                Question == "Sets for shocks" ~ "CommandFiles/PolicySets",
                                Question == "Program sets file" ~ "BaseData/Sets",
                                Question == "Parameter file" ~ "BaseData/Par",
                                Question == "Model parameter files" ~ "BaseData/ModPar",
                                Question == "Shock data file" ~ "Shocks",
                                Question == "Shock program" ~ "CodeShock",
                                Question == "GTAP executable" ~ "CodeMainProgram")) %>%
    #  mutate(folder = ifelse(Question == "Base data file",gsub("4_MAGNET/","",gsub("\\\\","/",Answer)),as.character(folder))) %>%
      mutate(File = file.path("4_MAGNET", folder,Answer), Scenario = scenname) %>%
      mutate(File = ifelse(Question == "Base data file",gsub("\\\\","/",Answer),as.character(File))) %>%
      select(Scenario, Period, Question, File)

    sceninfoall <- bind_rows(sceninfoall, sceninfo2)
  }
  return(sceninfoall)
}


magnet_get_years <- function(scenariosinfo){
  years <- c()
  for (s in 1:nrow(scenariosinfo)){
    periods <- scenariosinfo$Periods[s]
    ys <- unique(unlist(str_split(periods, "-|;")))
    years <- unique(c(years, ys))
  }
  years <- sort(years)

  return(years)
}

### Scenario reader functions -----
#' @export
readscenariofile <- function(fullfilepath, scenname, whitelist = c(), readcoef = TRUE, year = "") {
  #Reads a single scenario file (.har or .sol) and adds year and scenario name info to each dataframes in the list.

  df <-  tryCatch(
    {
      message(paste("Starting to read",scenname,"from", fullfilepath))
      magnet_read_all_headers(fullfilepath, whitelist = whitelist,useCoefficientsAsNames = readcoef)
    },
    error=function(cond) {
      message("Error reading HArr file, quiting loop over files")
      return(NULL)
    },
    finally={
      message("read scenario succesfully")
    }
  )

  period <- str_extract(fullfilepath,"\\d{4}-\\d{4}")
  years <- unlist(str_split(period, "-"))

  # The below is added to take year as present on the datafile, which helps in cases with incorrect filenames
  # for updateview and solution files the correct year is added based on this as well.
  if(year == ""){year = as.character(df$YEAR$Value)} # works if it's an update file
  if(length(year)==0){year = years[2]} # final guess is from the file name

  df <- addyearandscen(df, year, scenname)

  return(df)
}

readscenariofile_gvc <- function(fullfilepath, year, scenname, sets,NCMF = NULL,threshold = 1E-6) {

  df <-  tryCatch(
    {
      message(paste("Starting to read",scenname,"from", fullfilepath))
      magnet_read_all_headers(fullfilepath,useCoefficientsAsNames = FALSE)
    },
    error=function(cond) {
      message("Error reading HArr file, here's the error:")
      message(cond)
      return(NULL)
    },
    finally={
      message("read scenario succesfully")
    }
  )

  df <- MBL_fixbdata(df) # fixing old header to new namess if applicable
  ACTDAT <- MBL_MakeACTDAT(sets,df)

  MBL_FOOTP_FD <- MBL_Footprints(sets, ACTDAT, df, threshold = threshold)

  dfout <- list(MBL_FOOTP = MBL_FOOTP_FD[[1]], MBL_IO_q = MBL_FOOTP_FD[[2]],
                MBL_F_q = MBL_FOOTP_FD[[3]],MBL_Q_q = MBL_FOOTP_FD[[4]],MBL_FD_shr =MBL_FOOTP_FD[[5]], MBL_COMM_SHR = MBL_FOOTP_FD[[6]],
                ACTDAT_AFP = ACTDAT$A_FP, MBL_ACTDAT_out = MBL_FOOTP_FD[[7]])
  dfout <- addyearandscen(dfout, year, scenname)

  return(dfout)
}


addyearandscen <- function(df, year, scenname){
  for (n in names(df)){
    if(nrow(df[[n]])>0){ # this is to catch empty headers, simply removing now
      df[[n]]$Year <- year
      df[[n]]$Scenario <- scenname
      if("Value" %in% colnames(df[[n]])) {
        df[[n]] <- df[[n]] %>% relocate(Value, .after = last_col())  # this moves Value to the final column
      }
    } else {
      df[[n]] <- NULL
    }
  }
  return(df)
}

readscenario <- function(scenname, maindir, whitelist = c(), readcoef = TRUE, addgvcinfo = FALSE, NCMF = NULL, sets = NULL, threshold = 1E-6, years_sel = NULL, stopatyear = NULL, overwrite_scenname = FALSE) {
  # Reads all files in a scenario for a given scenario name.
  # Produce a list of lists: on list with Update, Updatview, update_tax, and solution headers.

  updateviewfiles <- list.files(file.path(maindir,"4_MAGNET","Updates"),
                                pattern = paste("^",scenname,"_\\d{4}-\\d{4}_update_view.har$",sep=""), full.names = TRUE, ignore.case = TRUE)
  updatefiles <- gsub("_view", "", updateviewfiles)
  updatetaxfiles <- gsub("_view", "_tax", updateviewfiles)
  solfiles <- list.files(file.path(maindir,"4_MAGNET","Solutions"),
                         pattern = paste("^",scenname,"_\\d{4}-\\d{4}_Solution.sol$",sep=""), full.names = TRUE, ignore.case = TRUE)

  if(!is.null(whitelist)){whitelist = c(whitelist,"YEAR")}

  #Using this in case the basedata is another update file and we want to go recursive
  if(overwrite_scenname != FALSE){scenname = overwrite_scenname}

  df_update <- list()
  yearlist <- data.frame()
  for (f in updatefiles) {
    year <- unlist(str_split(str_extract(f,"\\d{4}-\\d{4}"), "-"))[2]
    if(!is.null(years_sel)){if(!(year %in% years_sel)){next}}
    dftmp <- readscenariofile(f,scenname,whitelist,readcoef)
    if(is.null(dftmp) | length(dftmp) == 0){warning(paste(f,"has no data, stopping reading scenario"));break}
    yearfromfile <- as.character(as.integer(dftmp$YEAR$Value))
    yearlist <- bind_rows(yearlist,data.frame(Year = year,Yearfromfile=yearfromfile))
    for (n in names(dftmp)){
      df_update[[n]][[yearfromfile]] <- dftmp[[n]]
    }
    rm(dftmp)
    if(!is.null(stopatyear)){if(year == stopatyear){break}} # this is to stop reading at a certain year, useful for adding basedata from scenario
  }
  df_update <- lapply(df_update, bind_rows)


  df_update_view <- list()
  for (f in updateviewfiles) {
    year <- unlist(str_split(str_extract(f,"\\d{4}-\\d{4}"), "-"))[2]
    if(!is.null(years_sel)){if(!(year %in% years_sel)){next}}
    if(!(year %in% yearlist$Year)){next} # this means this year is not found in the update files, so we skip it.
    yearfromfile <- subset(yearlist, Year == year)$Yearfromfile
    dftmp <- readscenariofile(f,scenname,whitelist,readcoef, year = yearfromfile)
    if(is.null(dftmp) | length(dftmp) == 0){warning(paste(f,"has no data, stopping reading scenario"));break}
    for (n in names(dftmp)){
      df_update_view[[n]][[yearfromfile]] <-dftmp[[n]]
    }
    rm(dftmp)
    if(!is.null(stopatyear)){if(year == stopatyear){break}} # this is to stop reading at a certain year, useful for adding basedata from scenario
  }
  df_update_view <- lapply(df_update_view, bind_rows)

  df_update_tax <- list()
  for (f in updatetaxfiles) {
    year <- unlist(str_split(str_extract(f,"\\d{4}-\\d{4}"), "-"))[2]
    if(!is.null(years_sel)){if(!(year %in% years_sel)){next}}
    if(!(year %in% yearlist$Year)){next} # this means this year is not found in the update files, so we skip it.
    yearfromfile <- subset(yearlist, Year == year)$Yearfromfile
    dftmp <- readscenariofile(f,scenname,whitelist,readcoef, year = yearfromfile)
    if(is.null(dftmp) | length(dftmp) == 0){break} # This happens quite often so surpressing warning.
    for (n in names(dftmp)){
      df_update_tax[[n]][[yearfromfile]] <- dftmp[[n]]
    }
    rm(dftmp)
    if(!is.null(stopatyear)){if(year == stopatyear){break}} # this is to stop reading at a certain year, useful for adding basedata from scenario
  }
  df_update_tax <- lapply(df_update_tax, bind_rows)

  df_solution <- list()
  for (f in solfiles) {
    #for soluation file we always keep all years
    year <- unlist(str_split(str_extract(f,"\\d{4}-\\d{4}"), "-"))[2]
    if(!is.null(years_sel)){if(year > max(years_sel)){next}}
    if(!(year %in% yearlist$Year)){next} # this means this year is not found in the update files, so we skip it.
    yearfromfile <- subset(yearlist, Year == year)$Yearfromfile
    # In solution need to add this step, because intermediate years may be skipped for update reading, but not here, so things can be missing in the yearlist.
    # also means this won't catch all weird year setups, but I try.
    if(length(yearfromfile)==0){yearfromfile = year}
    dftmp <- readscenariofile(f,scenname,whitelist,readcoef, year = yearfromfile)
    if(is.null(dftmp) | length(dftmp) == 0){warning(paste(f,"has no data, stopping reading scenario"));break}
    for (n in names(dftmp)){
      df_solution[[n]][[yearfromfile]] <- dftmp[[n]]
    }
    rm(dftmp)
    if(!is.null(stopatyear)){if(year == stopatyear){break}} # this is to stop reading at a certain year, useful for adding basedata from scenario
  }
  df_solution <- lapply(df_solution, bind_rows)

  df_gvc <- list()
  if(addgvcinfo){
    for (f in updatefiles) {
      year <- unlist(str_split(str_extract(f,"\\d{4}-\\d{4}"), "-"))[2]
      if(!is.null(years_sel)){if(!(year %in% years_sel)){next}}
      if(!(year %in% yearlist$Year)){next} # this means this year is not found in the update files, so we skip it.
      yearfromfile <- subset(yearlist, Year == year)$Yearfromfile
      dftmp <- readscenariofile_gvc(f,year=yearfromfile,scenname=scenname,sets,NCMF,threshold = threshold)
      if(is.null(dftmp) | length(dftmp) == 0){warning(paste(f,"has no data, stopping reading scenario"));break}
      for (n in names(dftmp)){
        df_gvc[[n]][[yearfromfile]] <- dftmp[[n]]
      }
      rm(dftmp)
      if(!is.null(stopatyear)){if(year == stopatyear){break}} # this is to stop reading at a certain year, useful for adding basedata from scenario
    }
  }
  df_gvc <- lapply(df_gvc, bind_rows)

  df_scendata <- list(Update = df_update, Update_view = df_update_view,
                      Update_tax = df_update_tax, Solution = df_solution, GVC = df_gvc)
  return(df_scendata)
}

readbasedata <- function(scenname, scenariosinfo, whitelist = c(),
                         recursive = FALSE, overwrite_scenname = FALSE, readcoef = TRUE,
                         addgvcinfo = FALSE, sets = NULL, threshold = threshold, years_sel = years_sel){
  #Reads basedata.
  # Uses the scenario info which can possible have a normal run as input, so it tries to read solution file if it is ther
  # Recursively then will also read the original basedata. I think it works ;).

  sceninfo = subset(scenariosinfo, tolower(Scenario) == tolower(scenname))
  # need the year data if it's not in the whitelist
  if(!is.null(whitelist)){whitelist = c(whitelist,"YEAR")}
  BaseData_b <- magnet_read_all_headers(sceninfo$BaseData_b, whitelist = whitelist, useCoefficientsAsNames = readcoef)

  BaseData_b_view <- magnet_read_all_headers(sceninfo$BaseData_b_view, whitelist = whitelist,useCoefficientsAsNames = readcoef)
  BaseData_b_tax <- magnet_read_all_headers(sceninfo$BaseData_b_tax, whitelist = whitelist,useCoefficientsAsNames = readcoef)

  year <- as.character(BaseData_b$YEAR$Value)

  #Using this in case the basedata is another update file and we want to go recursive
  if(overwrite_scenname != FALSE){scenname = overwrite_scenname}

  BaseData_b <- addyearandscen(BaseData_b, year, scenname)
  BaseData_b_view <- addyearandscen(BaseData_b_view, year, scenname)
  BaseData_b_tax <- addyearandscen(BaseData_b_tax, year, scenname)

  # if(sceninfo$BaseData_b_solution != ""){
  #   print("reading baesdata solution")
  #   print(sceninfo$BaseData_b_solution)
  #   BaseData_b_solution <- magnet_read_all_headers(sceninfo$BaseData_b_solution, whitelist = whitelist,useCoefficientsAsNames = readcoef)
  #   BaseData_b_solution <- addyearandscen(BaseData_b_solution, year, scenname)
  # } else {BaseData_b_solution <- NULL}

  df_basedata <- list(Update = BaseData_b, Update_view = BaseData_b_view,
                      Update_tax = BaseData_b_tax) #, Solution = BaseData_b_solution)

  if(addgvcinfo & year %in% years_sel){
    # The other things want to keep for likely but skip this if base year is not selected
    df_basedata$GVC <- readscenariofile_gvc(sceninfo$BaseData_b, year = year, scenname = scenname, sets = sets, threshold = threshold)
  }

  return(df_basedata)
}

#' @export
readscenarioandbase <- function(scenname, scenariosinfo, whitelist = c(), recursive = FALSE, readcoef = TRUE, addgvcinfo = FALSE, threshold = 1E-6, years_sel = NULL){

  sceninfo = subset(scenariosinfo, tolower(Scenario) == tolower(scenname))
  maindir <- sceninfo$Maindir

  sets <- suppressWarnings(magnet_read_all_headers(sceninfo$Sets))

  df_basedata <- readbasedata(scenname, scenariosinfo, whitelist = whitelist, recursive = recursive,
                              readcoef = readcoef, addgvcinfo = addgvcinfo, sets = sets, threshold = threshold, years_sel = years_sel)
  NCMF = df_basedata$GVC$NCMF
  df_scendata <- readscenario(scenname, maindir, whitelist = whitelist, readcoef = readcoef,
                              addgvcinfo = addgvcinfo, NCMF = NCMF, sets = sets, threshold = threshold, years_sel = years_sel)


  if(grepl("_update.har$",sceninfo$BaseData_b)){
    # If it is an update file, we need to read the basedata from the original scenario.
    base_run <- gsub("_\\d{4}-\\d{4}_update.har$","", split_path(sceninfo$BaseData_b)[1])
    print(paste0("reading basedate from scenario ", base_run))
    df_basedata_deeper <- readscenario(base_run, maindir, whitelist = whitelist, readcoef = readcoef,
                                       addgvcinfo = addgvcinfo, NCMF = NCMF, sets = sets, threshold = threshold,
                                       years_sel = years_sel,
                                       stopatyear = df_basedata$Update$YEAR$Value,
                                       overwrite_scenname = scenname)
    df_basedata <- readbasedata(base_run, scenariosinfo, whitelist = whitelist, recursive = recursive,
                                readcoef = readcoef, addgvcinfo = addgvcinfo, sets = sets, threshold = threshold, years_sel = years_sel,
                                overwrite_scenname = scenname)

    df_basedata <- mergescendata(df_basedata, df_basedata_deeper)
  }

  df_scendata <- mergescendata(df_basedata, df_scendata)
  baseyear <- min(df_scendata$Update$YEAR$Value)

  df_scendata$Solution_index <- list()
  for (h in names(df_scendata$Solution)) {
    df_scendata$Solution_index[[h]] <- makesolindex(df_scendata$Solution[[h]],as.character(baseyear))
  }

  return(df_scendata)

}

makesolindex <- function(df, fy) {
  #creates index version of the sol file. fy is the baseyear, has to be supplied as e.g. 2014
  if(nrow(df) == 0){return(NULL)}
  if(class(df$Value) %in% c("character")){return(NULL)}

  # years should be in order, this should do the trick.
  df <- df[order(df$Year),]

  years <- c(fy,unique(df$Year))
  df <-  tryCatch(
    {
      df <- spread(df, Year, Value)
      df[[fy]] = 1 #add base is 1 for index fy is
      for (p in 2:length(years)){ # probably smarter ways to do this, but seems to work!
        df[[years[p]]] <- df[[years[p-1]]] * (1+df[[years[p]]]/100)
      }
      df <- gather(df, Year, Value, all_of(years))
    },
    error=function(cond) {
      message("Error making index of sol variable, something must be wrong")
      message(cond)
      return(NULL)
    }
  )

  return(df)
}

#' @export
mergescendata <- function(df1, df2) {
  # merges list of lists
  #in usal magnet data set up this should be Update, Update_view, Update_tax, and Solution as names
  mainnames <- unique(c(names(df1),names(df2)))

  #probably a smarter way to this
  for (m in mainnames){
    headers <- unique(c(names(df1[[m]]), names(df2[[m]])))
    for(h in headers){
      df1_h  <- df1[[m]][[h]]
      df2_h  <- df2[[m]][[h]]
      if(is.null(df1_h)){
        df1[[m]][[h]] <- df2_h
      } else if(is.null(df2_h)){
        df1[[m]][[h]] <- df1_h
      } else {
        if(!identical(names(df1_h), names(df2_h)) & (ncol(df1_h) == ncol(df2_h))) {
          # if names are not the same assume that names of df1 are valid
          df2_h <- setNames(df2_h, names(df1_h))
        }
        if(!identical(names(df1_h), names(df2_h)) & (ncol(df1_h) != ncol(df2_h))) {
          # add ENDWL in case it's missing, this is for my new AEZ version where some headers have ENDWL added as header.
          if("ENDWL" %in% names(df1_h) & !("ENDWL" %in% names(df2_h))){
            df2_h$ENDWL <- "Land"
          } else if ("ENDWL" %in% names(df2_h) & !("ENDWL" %in% names(df1_h))) {
            df1_h$ENDWL <- "Land"
          }
        }
        if(identical(names(df1_h), names(df2_h))) {
          df1[[m]][[h]] <- bind_rows(df1_h,df2_h)
        } else {
          warning(paste("Headers of",h,"in",m,"do not match"))
        }
      }
    }
  }

  return(df1)
}

removescendata <- function(dflist, scen ) {
  # removes scenarios from all dfs in list, can be usefull sometimes
  mainnames <- names(dflist)
  for (m in mainnames){
    headers <- names(dflist[[m]])
    for(h in headers){
      dflist[[m]][[h]]  <- dflist[[m]][[h]] %>% subset(Scenario != scen)
    }
  }
  return(dflist)
}

### Scenario data cleaning and writing functions ----

remove_zero_entries <- function(df){
  #removes zeroes of "Value" if all entries are zero of given subset
  mainnames <- names(df)
  for (m in mainnames){
    headers <- names(df[[m]])
    for(h in headers){
      if(!("Value" %in% colnames(df[[m]][[h]]))){next}
      if("REG_2" %in% colnames(df[[m]][[h]]) & "REG" %in% colnames(df[[m]][[h]])) {
        dftmp <- df[[m]][[h]] %>%
          group_by(across(c(-Value,-Year,-Scenario,-REG,-REG_2))) %>% mutate(Valuetest = sum(Value)) %>% ungroup()  %>%
          subset(Valuetest != 0) %>% select(-Valuetest)
        df[[m]][[h]] <- dftmp
      } else if ("REG" %in% colnames(df[[m]][[h]])) {
        dftmp <- df[[m]][[h]] %>%
          group_by(across(c(-Value,-Year,-Scenario,-REG))) %>% mutate(Valuetest = sum(Value)) %>% ungroup()  %>%
          subset(Valuetest != 0) %>% select(-Valuetest)
        df[[m]][[h]] <- dftmp
      }
    }
  }
  return(df)
}

filter_scendata <- function(df, whitelist){
  #removes headers if not in list
  mainnames <- names(df)
  for (m in mainnames){
    headers <- names(df[[m]])
    for(h in headers){
      if(!(h %in% whitelist)){df[[m]][[h]] <- NULL}
    }
  }
  return(df)
}

write_scendata_csv <- function(df, dir = ".", writemore = FALSE){

  #writes scendata in csvs with the same headers, in folders based on the filename
  #the writemore just writes individual headers out into specific csv files in folders for Update, Update_view, etcetera.
  mainnames <- names(df)
  for (m in mainnames){
    allouts <- list()
    headers <- names(df[[m]])
    fp <- file.path(dir, m)
    if (!dir.exists(fp)) dir.create(fp, recursive = TRUE)
    for(h in headers){
      outcsv <- df[[m]][[h]]
      if(writemore){write.csv(outcsv, file.path(fp, paste0(h,".csv")), row.names = FALSE)}
      dim = toupper(colnames(select(outcsv,-Value)))
      dim <- dim[!(dim %in% c("YEAR","SCENARIO"))]
      dim = paste(str_sort(dim),collapse = "_")
      outcsv$Header <- h
      allouts[[dim]][[h]] <- outcsv
    }

    for (dm in names(allouts)){
      headers <- names(allouts[[dm]])
      outcsv <- data.frame()
      for(h in headers){
        outcsv <- bind_rows(outcsv, allouts[[dm]][[h]])
      }
      if("Header" %in% colnames(outcsv)){
        outcsv <- spread(outcsv, Header, Value, fill = 0)
        write.csv(outcsv, file.path(fp, paste0("x_",m,"_",dm,".csv")), row.names = FALSE)
      }
    }
  }
}

makeaggs <- function(dflist, mapping) {

  mapfrom <- colnames(mapping)[1]
  mapto <- colnames(mapping)[2]

  for (m in names(dflist)){
    for(h in names(dflist[[m]])){
      df1 <- dflist[[m]][[h]]
      if(mapfrom %in% colnames(df1) & "Value" %in% colnames(df1)) {
        if(!(class(df1$Value) %in% c("character"))){ # just excluding ppotential funny stuff
          df1[[mapfrom]] <- plyr::mapvalues(df1[[mapfrom]],from=mapping[[mapfrom]],to=mapping[[mapto]],warn_missing = FALSE)
          df1 <- df1 %>% group_by(across(-Value)) %>% summarise(Value = sum(Value))
          dflist[[m]][[h]] <- unique(df1)
        }
      }
    }
  }
  return(dflist)
}

makeagg_singledf <- function(df1, mapping) {

  mapfrom <- colnames(mapping)[1]
  mapto <- colnames(mapping)[2]

  if(mapfrom %in% colnames(df1) & "Value" %in% colnames(df1)) {
    if(!(class(df1$Value) %in% c("character"))){ # just excluding potential funny stuff
      df1[[mapfrom]] <- plyr::mapvalues(df1[[mapfrom]],from=mapping[[mapfrom]],to=mapping[[mapto]],warn_missing = FALSE)
      df1 <- df1 %>% group_by(across(-Value)) %>% summarise(Value = sum(Value))
      df1 <- unique(df1)
    }
  }

  return(df1)
}

addworld <- function(dflist) {

  for (m in names(dflist)){
    for(h in names(dflist[[m]])){
      df1 <- dflist[[m]][[h]]
      # to be smartified, both for situations with REG + REG_2 to and whether to include internal trade
      if ("REG" %in% colnames(df1)){
        dfwld <- select(df1, -REG) %>% group_by(across(c(-Value))) %>% summarise(Value = sum(Value)) %>% ungroup() %>%
          mutate(REG = "World")
        df1 <- bind_rows(df1, dfwld)
      }
      if ("REG_2" %in% colnames(df1)){
        dfwld <- select(df1, -REG_2) %>% group_by(across(c(-Value))) %>% summarise(Value = sum(Value)) %>% ungroup() %>%
          mutate(REG_2 = "World")
        df1 <- bind_rows(df1, dfwld)
      }
    }
  }
  return(dfLIST)
}

add_total_singledf <- function(df, colname = "REG", total = "Total", ignoreerror = TRUE) {
  #Quietly returns unaltered df if the colname isn't present.
  df1 <- ungroup(df)
  # to be smartified, both for situations with REG + REG_2 to and whether to include internal trade

  if(ignoreerror){
    if(!colname %in% colnames(df1)){return(df)}
  }
  dftot <- select(df1, -one_of(colname)) %>% group_by(across(c(-Value))) %>% summarise(Value = sum(Value)) %>% ungroup()
  dftot[[colname]] <- total
  df1 <- bind_rows(df1, dftot)
  return(df1)
}

add_manualagg_singledf <- function(df, filterlist, colname, total,ignoreerror = TRUE) {
  # filters list of colname and makes and agg, handy add eu in a single step for example
  #Quietly returns unaltered df if the colname isn't present.
  df1 <- ungroup(df)
  # to be smartified, both for situations with REG + REG_2 to and whether to include internal trade

  if(ignoreerror){
    if(!colname %in% colnames(df1)){return(df)}
  }
  df1 <- df1[df1[[colname]] %in% filterlist, ]
  dftot <- select(df1, -one_of(colname)) %>% group_by(across(c(-Value))) %>% summarise(Value = sum(Value)) %>% ungroup()
  dftot[[colname]] <- total
  df1 <- bind_rows(df1, dftot)
  return(df1)
}

### Some simple helper functions ------
split_path <- function(path) {
  if (dirname(path) %in% c(".", path)) return(basename(path))
  return(c(basename(path), split_path(dirname(path))))
}

read_excel_allsheets <- function(filename, tibble = FALSE) {
  library(readxl)
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

left_join0 <- function(x, y, fillwith = 0){
  #This does left_join and fillls missing values with zeros.
  z <- left_join(x, y)
  new_cols <- c(setdiff(names(z), names(x)))
  for (n in new_cols){
    if(is.numeric(z[[n]])){
      z[[n]] <- ifelse(is.na(z[[n]]), fillwith, z[[n]])
    }
  }
  z
}

full_join0 <- function(x, y, fillwith = 0){
  #This does left_join and fillls missing values with zeros.
  z <- full_join(x, y)
  for (n in colnames(z)){
    if(is.numeric(z[[n]])){
      z[[n]] <- ifelse(is.na(z[[n]]), fillwith, z[[n]])
    }
  }
  z
}

set_header_as_valcolname <- function(dflist){

 # This just uses the header as the Value col name, sometimes useful.
  for (header in names(dflist)) {
    d1_header <- dflist[[header]]
    #very likely a smarter way to do this
    d1_header[[header]] <- d1_header$Value
    d1_header <- select(d1_header, -Value)

    dflist[[header]] <- d1_header
  }
  return(dflist)
}

find_any_header <- function(df_list, coef, indexsol = TRUE) {
  # this is just to get a coefficient regardless of where it is in the database
  # returns the first one it encountes
  if(indexsol){
    excl = "Solution"
  } else {
    excl = "Solution_index"
  }
  for (m in setdiff(names(df_list), excl)){ #Excluding solution by default so that "Solution_index" will be used
    headers <- names(df_list[[m]])
    for(h in headers){
      if (h == coef){
        return(df_list[[m]][[h]])
      }
    }
  }
}

getusefulsets <- function(modelsetfile) {
  msets <- magnet_read_all_headers(modelsetfile)

  setsmap <- bind_rows(#msets$CRPS %>% mutate(Header = "CRPS", Description = "Crops"),
                   #msets$LVSK %>% mutate(Header = "LVSK", Description = "Livestock"),
                   msets$ANI2 %>% mutate(Header = "ANI2", Description = "Livestock using land"),
                   msets$AGRI %>% mutate(Header = "AGRI", Description = "Agriculture"),
                   # msets$LAND %>% mutate(Header = "LAND", Description = "Land endowments"),
                   # msets$UNSK %>% mutate(Header = "UNSK", Description = "Unskilled labour"),
                   # msets$SKLB %>% mutate(Header = "SKLB", Description = "Skilled labour"),
                   # msets$ENDC %>% mutate(Header = "ENDC", Description = "Capital endowments"),
                   msets$SERV %>% mutate(Header = "SERV", Description = "Services"),
                   msets$INDS %>% mutate(Header = "INDS", Description = "Industry"),
                   msets$FOOD %>% mutate(Header = "FOOD", Description = "Food commodities"),
                   msets$PRAG %>% mutate(Header = "PRAG", Description = "Primary agri food"),
                   msets$PRFD %>% mutate(Header = "PRFD", Description = "Processed food"),
                   msets$FDSV %>% mutate(Header = "FDSV", Description = "Food services"),
                   msets$NONF %>% mutate(Header = "NONF", Description = "Non food commodities"))

  return(setsmap)
}

getcommapping <- function(sets) {

  modelsetdata <- sets %>% set_header_as_valcolname()
  if(!("COMM" %in% names(modelsetdata))){modelsetdata[["COMM"]] <-modelsetdata[["H2"]] %>% rename(COMM = H2)}
  if(!("COMO" %in% names(modelsetdata))){modelsetdata[["COMO"]] <-modelsetdata[["H2O"]]  %>% rename(COMO = H2O)}
  comm2dcomm <- cbind(modelsetdata[["MAPT"]],modelsetdata[["COMO"]])
  dcomm2gcomm <- cbind(modelsetdata[["COMO"]],modelsetdata[["MTDG"]])

  names <- cbind(modelsetdata[["COMM"]],modelsetdata[["H2L"]]) %>%
    rename(MAPT = COMM, Descr = H2L) %>% mutate(nr = row_number())

  allmap <- right_join(comm2dcomm,dcomm2gcomm) %>%
    left_join(names) %>%
    select(nr, COMM = MAPT, DCOMM = COMO, GCOMM = MTDG, Descr) %>%
    arrange(nr) %>% select(-nr)

  return(allmap)
}

getsectormapping <- function(sets) {

  modelsetdata <- sets %>% set_header_as_valcolname()

  acts2dacts <- cbind(modelsetdata[["MAPS"]],modelsetdata[["ACTO"]])
  dacts2gacts <- cbind(modelsetdata[["ACTO"]],modelsetdata[["MTRS"]])

  allmap <- right_join(acts2dacts,dacts2gacts)

  select(ACTS = MAPS, DACTS = ACTO, GACTS = MTRS)

  return(allmap)
}

getmendwmapping <- function(sets) {

  modelsetdata <- sets %>% set_header_as_valcolname()
  if(!("ENDO" %in% names(modelsetdata))){modelsetdata[["ENDO"]] <-modelsetdata[["H6"]]  %>% rename(ENDO = H6)}
  endw2dendw <- cbind(modelsetdata$MAPF,modelsetdata$ENDO) %>% na.omit()

  allmap <- endw2dendw%>%
    select(ENDW = MAPF, DENDW = ENDO)

  return(allmap)
}

getregmapping <- function(sets) {

  modelsetdata <- sets %>% set_header_as_valcolname()

  if(!("REGO" %in% names(modelsetdata))){modelsetdata[["REGO"]] <-modelsetdata[["H1O"]]  %>% rename(REGO = H1O)}
  if(!("REG" %in% names(modelsetdata))){modelsetdata[["REG"]] <-modelsetdata[["H1"]]  %>% rename(REG = H1)}

  reg2dreg <- cbind(modelsetdata$MAPR,modelsetdata$REGO)

  names <-  cbind(modelsetdata$REG, modelsetdata$H1L) %>% na.omit() %>%
    rename(MAPR = REG, Descr = H1L)

  allmap <- reg2dreg %>%
    left_join(names) %>%
    select(REG = MAPR, DREG = REGO, Descr) %>% unique() %>% na.omit()

  return(allmap)
}



collapse_df_list <- function(dflist, newcol = "Variable"){
  #Collapses a single level df list.
  # it wil try to find all the columnnames, so be carefull
  hdrs <- c()
  for (header in names(dflist)) {
    hdrs <- unique(c(hdrs,colnames(dflist[[header]])))
  }

  fncols <- function(df, cnames) {
    for (cname in cnames){
      add <-cname[!cname%in%names(df)]

      if(length(add)!=0) df[add] <- ""
    }

    return(df)
  }

  newdf <- data.frame()
  # This just uses the header as the Value col name, sometimes useful.
  for (header in names(dflist)) {
    d1_header <- dflist[[header]]
    d1_header[[newcol]] <- header #adding new column
    d1_header <- fncols(d1_header, hdrs) #adding missing columns with empty values
    newdf <- bind_rows(newdf, d1_header)
  }

  newdf <-newdf %>% select(-Value,Value) #This moves value to last column, just in case

  return(newdf)
}

removescendata <- function(dflist, scen) {
  # removes scenarios from all dfs in list
  #in usal magnet data set up this should be Update, Update_view, Update_tax, and Solution as names
  mainnames <- names(dflist)

  #probably a smarter way to this
  for (m in mainnames){
    headers <- names(dflist[[m]])
    for(h in headers){
      dflist[[m]][[h]]  <- dflist[[m]][[h]] %>% subset(Scenario != scen)
    }
  }
  return(dflist)
}

