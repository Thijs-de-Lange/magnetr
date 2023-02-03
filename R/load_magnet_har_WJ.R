
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

    colnames(d1_header) <- cnamesnew

    #using Value with capital V by default
    d1_header <- rename(d1_header, Value = VALUE)

    dflist_out[[h]] <- d1_header
  }

  return(dflist_out)
}

#' @export
magnet_write_har <- function(dflist, outfilename) {
  #Needs some testing, but takes a list of named dataframes where the names should be the
  #final HEADER names that will be in the output hr file
  #The write har is sensitive to the order of the values related to the order of the dimensions and the code below tries to do that

  ar <- list()

  for (h in names(dflist)) {
    df <- dflist[[h]]

    if ("value" %in% colnames(df)){df <- rename(df, Value = value)}

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

    dimnames <- names(dimlist)

    # This is to make sure that the dimension is correct (in case of missing values adds 0 with left_join0)
    outputdf <- expand.grid(dimlist) %>% left_join0(df)

    # By default we use "REG_2" for duplicate dimensions. For har file bring back to regular REG.
    names(dimlist) <- replace(dimnames, dimnames == "REG_2", "REG")
    names(dimlist) <- replace(dimnames, dimnames == "REG_3", "REG")
    names(dimlist) <- replace(dimnames, dimnames == "CTRY_2", "CTRY")
    names(dimlist) <- replace(dimnames, dimnames == "SAMAC_2", "SAMAC")
    names(dimlist) <- replace(dimnames, dimnames == "COMM_2", "COMM")
    names(dimlist) <- replace(dimnames, dimnames == "TRAD_COMM_2", "TRAD_COMM")

    ar[[h]] <- array(
      outputdf$Value,
      dim = dimsizelist,
      dimnames = dimlist
    )
  }

  HARr::write_har(ar, outfilename)
}

### Dealing with MAGNET scenario names -----

#' @export
magnet_get_scenarioinfo <- function(maindir) {
  #Creates a dataframe with usefull info of all scenarios with a log file present.
  # Used as basis for other read functions
  getinfobasedata <- function(x){
    answertxt <- read.delim(x,header=FALSE)
    BaseData_b <- str_trim(answertxt[6,])
  }

  getinfoperiods <- function(x){
    answertxt <- read.delim(x,header=FALSE)
    answertxt <- subset(answertxt, grepl("Period", V1)) %>%
      mutate(Periods = gsub(" ","",str_extract(V1,"\\d{4} - \\d{4}")))
    return(paste(answertxt$Periods, collapse = ";"))
  }

  getinfomodelsets <- function(x){
    answertxt <- read.delim(x,header=FALSE)
    modelsets <- gsub("- ","",str_trim(answertxt[9,]))
  }

  getinfomodelpar <- function(x){
    answertxt <- read.delim(x,header=FALSE)
    modelpar <- gsub("- ","",str_trim(answertxt[11,]))
  }

  getinfomodelsettings <- function(x){
    answertxt <- read.delim(x,header=FALSE)
    modelsettings <- gsub("- ","",str_trim(answertxt[13,]))
  }

  scennamesall = data.frame()

  scen <- file.info(list.files(file.path(maindir,"4_MAGNET","Scenarios"), recursive = TRUE, pattern = "GTAPLog.*\\.log", full.names = TRUE))
  scen = scen[with(scen, order(as.POSIXct(mtime), decreasing = TRUE)),]

  scen$files <- rownames(scen)
  scen$Scenario <- dirname(gsub(".*/4_MAGNET/Scenarios/","",rownames(scen)))
  scen$Maindir <- gsub("/4_MAGNET/Scenarios/.*","",rownames(scen))

  scen <- select(scen, c(Scenario,Maindir)) %>% unique()
  rownames(scen) <- NULL

  scennamesall <- rbind(scennamesall,scen)

  # scentxt <- read.delim(scenfile,  header = FALSE)
  # colnames(scentxt) <- "Settings"
  # scentxt$Section <- scentxt$Settings
  # for (i in 1:length(scentxt$Settings)) {
  #   t <- scentxt$Settings[i]
  #   hdr <- TRUE
  #   if (substr(t, 1, 3) == "   ") {
  #     hdr <- FALSE
  #   }
  #   if(hdr){
  #     hdrtxt = t
  #   }
  #   scentxt$Section[i] <- hdrtxt
  # }
  # basedata <- normalizePath(file.path(maindir,
  #             subset(scentxt, Section == "Base data file" & Settings != "Base data file")$Settings %>% str_trim()))
  # periods <- subset(scentxt, grepl("Period",Section)) %>% rename(Period = Section)
  #
  # periods$Question <- ""
  # periods$Answer <- ""
  # for (i in 1:length(periods$Settings)) {
  #   t <- periods$Settings[i]
  #   answertxt = ""
  #   hdr <- TRUE
  #   if (grepl(" - ",t)) {
  #     hdr <- FALSE
  #     answertxt = t
  #   }
  #   if(hdr){
  #     hdrtxt = t
  #   }
  #   periods$Question[i] <- str_trim(hdrtxt)
  #   periods$Answer[i] <- gsub("- ","",str_trim(answertxt))
  # }
  # periods <- subset(periods,Answer != "")

  scenariosinfo <- scennamesall %>%
    mutate(answerfile = file.path(Maindir, "4_MAGNET","Scenarios", Scenario, paste(Scenario,".txt", sep = ""))) %>%
    subset(file.exists(answerfile)) %>%
    mutate(BaseData_b = file.path(maindir,unlist(lapply(answerfile, getinfobasedata)))) %>%
    mutate(BaseData_b_view = gsub("\\.har$","_view.har",BaseData_b), BaseData_b_tax = gsub("\\.har$","_tax.har",BaseData_b)) %>%
    mutate(BaseData_b_solution = ifelse(grepl("update_view",BaseData_b_view),
                                        gsub("_update.har$","_solution.sol",BaseData_b,ignore.case = TRUE),"")) %>%
    mutate(BaseData_b_solution = gsub("Updates","/Solutions/",BaseData_b_solution,ignore.case = TRUE)) %>%
    #  mutate(Sets = file.path(maindir,"BaseData","Sets",unlist(lapply(answerfile, getinfomodelsets))))%>%
    #  mutate(Par = file.path(maindir,"BaseData","Par",unlist(lapply(answerfile, getinfomodelpar))))%>%
    #  mutate(ModPar = file.path(maindir,"BaseData","ModPar",unlist(lapply(answerfile, getinfomodelsettings)))) %>%
    mutate(Periods = unlist(lapply(answerfile, getinfoperiods)))

  return(scenariosinfo)
}

### Scenario reader functions -----
#' @export
readscenariofile <- function(fullfilepath, scenname, whitelist = c()) {
  #Reads a single scenario file (.har or .sol) and adds year and scenario name info to each dataframes in the list.

  df <-  tryCatch(
    {
      message(paste("Starting to read",scenname,"from", fullfilepath))
      magnet_read_all_headers(fullfilepath, whitelist = whitelist,useCoefficientsAsNames = TRUE)
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

  period <- str_extract(fullfilepath,"\\d{4}-\\d{4}")
  years <- unlist(str_split(period, "-"))

  df <- addyearandscen(df, years[2], scenname)
  return(df)
}

addyearandscen <- function(df, year, scenario){
  for (n in names(df)){
    if(nrow(df[[n]]>0)){ # this is to catch empty headers, simpmly removing now
      df[[n]]$Year <- year
      df[[n]]$Scenario <- scenario
      df[[n]] <- df[[n]] %>% relocate(Value, .after = last_col())  # this moves Value to the final column
    } else {
      df[[n]] <- NULL
    }
  }
  return(df)
}


readscenario <- function(scenname, maindir, whitelist = c()) {
  # Reads all files in a scenario for a given scenario name.
  # Produce a list of lists: on list with Update, Updatview, update_tax, and solution headers.

  updateviewfiles <- list.files(file.path(maindir,"4_MAGNET","Updates"), recursive = TRUE,
                                pattern = paste(scenname,"_\\d{4}-\\d{4}_update_view.har$",sep=""), full.names = TRUE, ignore.case = TRUE)
  updatefiles <- gsub("_view", "", updateviewfiles)
  updatetaxfiles <- gsub("_view", "_tax", updateviewfiles)
  solfiles <- list.files(file.path(maindir,"4_MAGNET","Solutions"), recursive = TRUE,
                         pattern = paste(scenname,"_\\d{4}-\\d{4}_Solution.sol$",sep=""), full.names = TRUE, ignore.case = TRUE)

  df_update <- list()
  for (f in updatefiles) {
    dftmp <- readscenariofile(f,scenname,whitelist)
    for (n in names(dftmp)){
      df_update[[n]] <- rbind(df_update[[n]], dftmp[[n]])
    }
  }

  df_update_view <- list()
  for (f in updateviewfiles) {
    dftmp <- readscenariofile(f,scenname,whitelist)
    for (n in names(dftmp)){
      df_update_view[[n]] <- rbind(df_update_view[[n]], dftmp[[n]])
    }
  }
  df_update_tax <- list()
  for (f in updatetaxfiles) {
    dftmp <- readscenariofile(f,scenname,whitelist)
    for (n in names(dftmp)){
      df_update_tax[[n]] <- rbind(df_update_tax[[n]], dftmp[[n]])
    }
  }
  df_solution <- list()
  for (f in solfiles) {
    dftmp <- readscenariofile(f,scenname,whitelist)
    for (n in names(dftmp)){
      df_solution[[n]] <- rbind(df_solution[[n]], dftmp[[n]])
    }
  }

  df_scendata <- list(Update = df_update, Update_view = df_update_view, Update_tax = df_update_tax, Solution = df_solution)
  return(df_scendata)
}

readbasedata <- function(scenname, scenariosinfo, whitelist = c(), recursive = FALSE, overwrite_scenname = FALSE){
  #Reads basedata.
  # Uses the scenario info which can possible have a normal run as input, so it tries to read solution file if it is ther
  # Recursively then will also read the original basedata. I think it works ;).

  sceninfo = subset(scenariosinfo, tolower(Scenario) == tolower(scenname))
  BaseData_b <- magnet_read_all_headers(sceninfo$BaseData_b, whitelist = whitelist,useCoefficientsAsNames = TRUE)
  # need the year data if it's not in the whitelist
  if(!("YEAR" %in% colnames(BaseData_b))){
    BaseData_b$YEAR <- magnet_read_all_headers(sceninfo$BaseData_b, whitelist = c("YEAR"),useCoefficientsAsNames = TRUE)$YEAR
  }


  BaseData_b_view <- magnet_read_all_headers(sceninfo$BaseData_b_view, whitelist = whitelist,useCoefficientsAsNames = TRUE)
  BaseData_b_tax <- magnet_read_all_headers(sceninfo$BaseData_b_tax, whitelist = whitelist,useCoefficientsAsNames = TRUE)

  year = as.character(BaseData_b$YEAR$Value)

  #Using this in case the basedata is another update file and we want to go recursive
  if(overwrite_scenname != FALSE){scenname = overwrite_scenname}
  BaseData_b <- addyearandscen(BaseData_b, year, scenname)
  BaseData_b_view <- addyearandscen(BaseData_b_view, year, scenname)
  BaseData_b_tax <- addyearandscen(BaseData_b_tax, year, scenname)

  if(sceninfo$BaseData_b_solution != "" & recursive == TRUE){
    BaseData_b_solution <- magnet_read_all_headers(sceninfo$BaseData_b_solution, whitelist = whitelist,useCoefficientsAsNames = TRUE)
    BaseData_b_solution <- addyearandscen(BaseData_b_solution, year, scenname)
  } else {BaseData_b_solution <- NULL}

  df_basedata <- list(Update = BaseData_b, Update_view = BaseData_b_view,
                      Update_tax = BaseData_b_tax, Solution = BaseData_b_solution)

  if(grepl("_update.har$",sceninfo$BaseData_b) & recursive == TRUE){
    # If it is an update file, go deeper but keep same scenario name.
    base_run <- gsub("_\\d{4}-\\d{4}_update.har$","", split_path(sceninfo$BaseData_b)[1])
    df_basedata_deeper <- readbasedata(base_run, scenariosinfo, overwrite_scenname = scenname)
    df_basedata <- mergescendata(df_basedata, df_basedata_deeper)
  }

  return(df_basedata)
}

#' @export
readscenarioandbase <- function(scenname, scenariosinfo, whitelist = c(), recursive = FALSE){

  sceninfo = subset(scenariosinfo, tolower(Scenario) == tolower(scenname))
  maindir <- sceninfo$Maindir

  df_scendata <- readscenario(scenname, maindir, whitelist = whitelist)
  df_basedata <- readbasedata(scenname, scenariosinfo, whitelist = whitelist, recursive = FALSE)

  df_scendata <- mergescendata(df_scendata, df_basedata)
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
      gather(df, Year, Value, all_of(years))
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
        df1[[m]][[h]] <- rbind(df1_h,df2_h)
      }
    }
  }

  return(df1)
}

### Scenario data cleaning and writing functions ----

remove_zero_entries <- function(df){
  #removes zeroes if all entries are zero of given subset
  mainnames <- names(df)
  for (m in mainnames){
    headers <- names(df[[m]])
    for(h in headers){
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

write_scendata_csv <- function(df, dir = "."){
  #this just writes individual headers out into specific csv files in folders for Update, Update_view, etcetera.
  mainnames <- names(df)
  for (m in mainnames){
    headers <- names(df[[m]])
    fp <- file.path(dir, m)
    if (!dir.exists(fp)) dir.create(fp, recursive = TRUE)
    for(h in headers){
      outcsv <- df[[m]][[h]]
      write.csv(outcsv, file.path(fp, paste0(h,".csv")), row.names = FALSE)
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

addworld <- function(dflist) {

  for (m in names(dflist)){
    for(h in names(dflist[[m]])){
      df1 <- dflist[[m]][[h]]
      # to be smartified, both for situations with REG + REG_2 to and whether to include internal trade
      if ("REG" %in% colnames(df1)){
        dfwld <- select(df1, -REG) %>% group_by(across(c(-Value))) %>% summarise(Value = sum(Value)) %>% ungroup() %>%
          mutate(REG = "World")
        df1 <- bind_rows(df1, dfwld,dfeur)
      }
      if ("REG_2" %in% colnames(df1)){
        dfwld <- select(df1, -REG_2) %>% group_by(across(c(-Value))) %>% summarise(Value = sum(Value)) %>% ungroup() %>%
          mutate(REG_2 = "World")
        df1 <- bind_rows(df1, dfwld,dfeur)
      }
    }
  }
  return(dfLIST)
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
    z[[n]] <- ifelse(is.na(z[[n]]), fillwith, z[[n]])
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

