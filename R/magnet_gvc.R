gvc_fixbdata <- function(bdata) {
  #this is to fix the names to accomodate for the new names by Marijke. To be used just after reading basedata. Does not delete new headers
  if(!("TRAD" %in% names(bdata))){bdata$TRAD <- bdata$TR_Q}
  if(!("PRDQ" %in% names(bdata))){bdata$PRDQ <- bdata$PQ_Q}
  if(!("DINQ" %in% names(bdata))){bdata$DINQ <- bdata$DA_Q}
  if(!("MINQ" %in% names(bdata))){bdata$MINQ <- bdata$MA_Q}
  if(!("DFNH" %in% names(bdata))){bdata$DFNH <- bdata$DP_Q}
  if(!("MFNH" %in% names(bdata))){bdata$MFNH <- bdata$MP_Q}
  if(!("DFNG" %in% names(bdata))){bdata$DFNG <- bdata$DG_Q}
  if(!("MFNG" %in% names(bdata))){bdata$MFNG <- bdata$MG_Q}
  if(!("DFNI" %in% names(bdata))){bdata$DFNI <- bdata$DI_Q}
  if(!("MFNI" %in% names(bdata))){bdata$MFNI <- bdata$MI_Q}
  if(!("DTRN" %in% names(bdata))){bdata$DTRN <- bdata$TD_Q}
  if(!("STRN" %in% names(bdata))){bdata$STRN <- bdata$TS_Q}

  if(("TRADE_Q" %in% names(bdata))){bdata$TRAD <- bdata$TRADE_Q}
  if(("PROD_Q" %in% names(bdata))){bdata$PRDQ <- bdata$PROD_Q}
  if(("D_INT_Q" %in% names(bdata))){bdata$DINQ <- bdata$D_INT_Q}
  if(("M_INT_Q" %in% names(bdata))){bdata$MINQ <- bdata$M_INT_Q}
  if(("D_FINP_Q" %in% names(bdata))){bdata$DFNH <- bdata$D_FINP_Q}
  if(("M_FINP_Q" %in% names(bdata))){bdata$MFNH <- bdata$M_FINP_Q}
  if(("D_FING_Q" %in% names(bdata))){bdata$DFNG <- bdata$D_FING_Q}
  if(("M_FING_Q" %in% names(bdata))){bdata$MFNG <- bdata$M_FING_Q}
  if(("D_FINI_Q" %in% names(bdata))){bdata$DFNI <- bdata$D_FINI_Q}
  if(("M_FINI_Q" %in% names(bdata))){bdata$MFNI <- bdata$M_FINI_Q}
  if(("TRANSD_Q" %in% names(bdata))){bdata$DTRN <- bdata$TRANSD_Q}
  if(("TRANSS_Q" %in% names(bdata))){bdata$STRN <- bdata$TRANSS_Q}
  if(("MAKEB" %in% names(bdata))){bdata$MAKB <- bdata$MAKEB}
  if(("NQ_VOM" %in% names(bdata))){bdata$NVOM <- bdata$NQ_VOM}



  return(bdata)
}

gvc_prepmatbal <- function(bdata, threshold = 0, useloop = FALSE){

  regs <- unique(bdata$PRDQ$REG)
  margs <- unique(bdata$DTRN$MARG)
  agents <- c("hh","govt","CGDS")

  comms <- unique(bdata$MAKB$COMM)

  # this is the code copied from PostSimCalc/Materialbalances.gmp, which collects the data form basedata
#  chk_0ITRADE <- sum(subset(bdata$VFOB, REG == REG_2)$Value)
#  if(chk_0ITRADE >0){stop("Check that your data hase no internal trade #")}

  # COMM_SHR(c,a,r) # Commodity share in activity input use #;
  COMM_SHR <- bdata$MAKB %>% group_by(ACTS,REG) %>% mutate(Value = Value/sum(Value)) %>% ungroup() %>%
    mutate(Value = ifelse(is.nan(Value), 0, Value))

  # Prodiction
  Q_q <- bdata$PRDQ %>% group_by(COMM,REG) %>% summarize(Value = sum(Value))

  # I_DQ(c,p,r) # Intermediate demand for domestic commodities (mil USD) #;
  I_DQ <-  full_join0(rename(COMM_SHR, p = COMM), rename(bdata$DINQ, V1 = Value, c = COMM)) %>% mutate(Value = V1 * Value) %>%
    group_by(c,p,REG) %>% summarize(Value = sum(Value)) %>%
    rename(COMM = c, COMM_2 = p)

  # F_DQ(c,a,r) # Final demand for domestic commodities by agent (mil USD) #;
  F_DQ <- bind_rows(bdata$DFNH %>% mutate(AGENT = "hh"),
                    bdata$DFNG %>% mutate(AGENT = "govt"),
                    bdata$DFNI %>% mutate(AGENT = "CGDS"))

  # Coefficient (all,c,COMM)(all,a,MDEMCAT)(all,r,REG)
  # MDEMAQ(c,a,r) # Quantity of agent's demand for imports by region r #;
  MDEMAQ <- bind_rows(full_join0(COMM_SHR %>% rename(p = COMM, V1 = Value),
                                 bdata$MINQ %>% rename(c = COMM, V2 = Value)) %>%
                        mutate(Value = V1 * V2) %>% select(-V2,-V1) %>%
                        rename(COMM = c, MDEMCAT = p) %>%
                        group_by(COMM, MDEMCAT, REG) %>% summarize(Value = sum(Value)),
                      bdata$MFNH %>% mutate(MDEMCAT = "hh"),
                      bdata$MFNG %>% mutate(MDEMCAT = "govt"),
                      bdata$MFNI %>% mutate(MDEMCAT = "CGDS")) %>%
    mutate(Value = ifelse(Value <0, 0, Value))

  # MDEMQT(c,r) # Quantity total demand for imports by region r #;
  MDEMQT <- MDEMAQ %>% group_by(COMM,REG) %>% summarize(Value = sum(Value))

  # MAQSHR(c,a,r) # Agent's quantity shares in total imports by region r #;
  MAQSHR <- full_join0(MDEMAQ, rename(MDEMQT, V1 = Value)) %>%
    mutate(Value = ifelse(V1 > 0, Value/V1,0)) %>% select(-V1)

  # MAGNTQ(c,a,s,d) # Quantity of agent's imports by region r (mil USD) #;
  MAGNTQ <- full_join0(rename(MAQSHR, d = REG),
                       rename(bdata$TRAD, s = REG, d = REG_2, V1 = Value)) %>%
    mutate(Value = Value * V1) %>%
    select(COMM,MDEMCAT,REG = s, REG_2 = d, Value) %>%
    subset(Value > threshold)

  # #Quantity of intermediate demand for imported commodities s in d (mil USD) #;
  I_MQ <- MAGNTQ %>% subset(MDEMCAT %in% comms) %>% rename(COMM_2 = MDEMCAT)%>%
    subset(Value > threshold)

  # # Quantity final demand for imported commodities from s in d (mil USD) #;
  F_MQ <- MAGNTQ %>% subset(MDEMCAT %in% agents) %>% rename(AGENT = MDEMCAT)

  # TRMMAGNTQ(m,c,a,s,d) # Agent's transport margins by region s #;
  TRMMAGNTQ <- full_join0(rename(MAQSHR, REG_2 = REG), rename(bdata$DTRN, V1 = Value)) %>%
    mutate(Value = V1 * Value) %>%
    select(MARG, COMM, MDEMCAT, REG, REG_2, Value) %>%
    subset(Value > threshold)

  # !Compute bilateral exports of transport services based on regional contributions to global pool!
  # # Region share in quantity global pool of transport services #;
  shr_TRANSS_q <- bdata$STRN %>% group_by(MARG) %>% mutate(Value = Value/sum(Value))

  # # Linking transporter country (t) to demand for transport by destination d #;
  if(useloop){
    TRANSPRTQ <- data.frame()
    looplist <- as.character(unique(select(ungroup(shr_TRANSS_q), REG))$REG)
    print("starting loop over REG column to make TRANSPRTQ matrix, this can take some time.")
    for (n in 1:length(looplist)) {
      print(paste("now at region",n,"of",length(looplist)))
      reg <- looplist[n]
      shr_TRANSS_q_part <- subset(shr_TRANSS_q, REG == reg)
      TRANSPRTQ_part <- left_join(rename(shr_TRANSS_q_part, t = REG), rename(TRMMAGNTQ, s = REG, REG = REG_2, V1 = Value)) %>%
        mutate(Value = Value * V1) %>% group_by(MARG,MDEMCAT,REG,t) %>% summarize(Value = sum(Value)) %>%
        select(MARG,MDEMCAT,REG,REG_2 = t,Value) %>%
        subset(Value > threshold)
      TRANSPRTQ <- bind_rows(TRANSPRTQ, TRANSPRTQ_part)
    }

  } else {
    TRANSPRTQ <- left_join(rename(shr_TRANSS_q, t = REG), rename(TRMMAGNTQ, s = REG, REG = REG_2, V1 = Value)) %>%
      mutate(Value = Value * V1) %>% group_by(MARG,MDEMCAT,REG,t) %>% summarize(Value = sum(Value)) %>%
      select(MARG,MDEMCAT,REG,REG_2 = t,Value) %>%
      subset(Value > threshold)
  }

  # # Qunatity domestic transport services for trade from regions s (own trade 0)#;
  dTRANSPRTQ <- TRANSPRTQ %>% mutate(Value = ifelse(REG == REG_2, Value, 0)) %>% subset(REG == REG_2) %>% select(-REG_2)
  TRANSPRTQ <- TRANSPRTQ %>% mutate(Value = ifelse(REG == REG_2, 0, Value))

  # ! Add the transport services (margins) from other services to imports by region!
  # I_MQ(m,p,t,d) = I_MQ(m,p,t,d) + TRANSPRTQ(m,p,d,t);
  I_MQ2 <- left_join0(rename(I_MQ, m = COMM, p = COMM_2, t = REG, d = REG_2),
                      rename(TRANSPRTQ, m = MARG, p = MDEMCAT,  d = REG, t = REG_2, V1 = Value)) %>%
    mutate(V2 = V1 + Value) %>% #since we did left_join0 with zeroes this only does something for what is in TRANSPRT
    select(COMM = m, COMM_2 = p, REG = t, REG_2 = d, Value = V2) %>%
    subset(Value > threshold)

  F_MQ2 <- left_join0(rename(F_MQ, m = COMM, a = AGENT, t = REG, d = REG_2),
                      rename(TRANSPRTQ, m = MARG, a = MDEMCAT,  d = REG, t = REG_2, V1 = Value)) %>%
    mutate(V2 = V1 + Value) %>% #since we did left_join0 with zeroes this only does something for what is in TRANSPRT
    select(COMM = m, AGENT = a, REG = t, REG_2 = d, Value = V2) %>%
    subset(Value > threshold)

  # !Margins provided to self are added to domestic demand for transport services!
  I_DQ2 <- left_join0(rename(I_DQ, m = COMM, p = COMM_2, d = REG),
                      rename(dTRANSPRTQ, m = MARG, p = MDEMCAT, d = REG, V1 = Value)) %>%
    mutate(V2 = V1 + Value) %>% #since we did left_join0 with zeroes this only does something for what is in TRANSPRT
    select(COMM = m, COMM_2 = p, REG = d, Value = V2)%>%
    subset(Value > threshold)

  F_DQ2 <- left_join0(rename(F_DQ, m = COMM, a = AGENT, d = REG),
                      rename(dTRANSPRTQ, m = MARG, a = MDEMCAT, d = REG, V1 = Value)) %>%
    mutate(V2 = V1 + Value) %>% #since we did left_join0 with zeroes this only does something for what is in TRANSPRT
    select(COMM = m, AGENT = a, REG = d, Value = V2)%>%
    subset(Value > threshold)

  # ! Intermediates!
  I_q <- bind_rows(subset(I_MQ2, REG != REG_2), mutate(I_DQ2, REG_2 = REG)) %>%
    subset(Value > threshold)

  # ! Final demand!
  F_q <- bind_rows(subset(F_MQ2, REG != REG_2), mutate(F_DQ2, REG_2 = REG))%>%
    subset(Value > threshold)

  #IO coeff.(qtity): use of i from region s when producing comm. p in region d#;
  IO_q <- full_join0(I_q, rename(Q_q, REG_2 = REG, COMM_2 = COMM, V2 = Value)) %>%
    mutate(Value = ifelse(V2 > 0, Value/V2, 0)) %>%
    select(COMM, REG, COMM_2, REG_2, Value) %>% ungroup()

  return(list(Q_q,F_q,IO_q))
}

gvc_prepiomatrix <- function(IO_q,Q_q){
  #make commreg, commbination of comm and reg to make matrix with.:
  comregmap <- Q_q %>% select(-Value) %>% mutate(COMREG = paste(COMM, REG, sep="_"))
  comregmap2 <- rename(comregmap, COMREG_2 = COMREG, REG_2 = REG, COMM_2 = COMM)
  comreg <- comregmap$COMREG

  aIO_q <- left_join(IO_q, comregmap) %>% left_join(comregmap2) %>% select(-COMM,-COMM_2,-REG,-REG_2)

  iomatrix_q <- expand_grid(COMREG = comreg, COMREG_2 = comreg)
  iomatrix_q <- left_join0(iomatrix_q, aIO_q) %>% mutate(COMREG = factor(COMREG, levels = comreg)) # to ensure order
  iomatrix_q <- spread(iomatrix_q, COMREG_2, Value) %>% select(all_of(comreg)) # the select ensure order of columns

  return(iomatrix_q)
}

gvc_leontiefinverse <- function(iomatrix_q,Q_q){

  comregmap <- Q_q %>% select(-Value) %>% mutate(COMREG = paste(COMM, REG, sep="_"))
  comregmap2 <- rename(comregmap, COMREG_2 = COMREG, REG_2 = REG, COMM_2 = COMM)
  comreg <- comregmap$COMREG

  idmatrix <- diag(length(comreg)) #identity matrix with same dimension

  A_q <- idmatrix - iomatrix_q # Making A matrix
  leontiefinverse_q <- solve(A_q) # this inverts the matrix
  X_q <- array(as.vector(leontiefinverse_q), #put the right names in again after solve
               dim = c(length(comreg),length(comreg)),
               dimnames = list(COMREG = comreg,COMREG_2 = comreg))
  return(X_q)
}

gvc_melt_matrix <- function(X_q, Q_q) {
  #make commreg, commbination of comm and reg to make matrix with.:
  comregmap <- Q_q %>% select(-Value) %>% mutate(COMREG = paste(COMM, REG, sep="_"))
  comregmap2 <- rename(comregmap, COMREG_2 = COMREG, REG_2 = REG, COMM_2 = COMM)
  comreg <- comregmap$COMREG

  LI_q <- reshape2::melt(X_q) %>% left_join(comregmap) %>% left_join(comregmap2) %>% select(-COMREG,-COMREG_2)
  return(LI_q)
}

gvc_prep_LI_q <- function(bdata, threshold = 0, useloop = TRUE){
  # might be usefull if you only want the LI
  mbal = gvc_prepmatbal(bdata,threshold,useloop)
  Q_q <- mbal[[1]]
  F_q <- mbal[[2]]
  IO_q <- mbal[[3]]

  iomatrix_q <- gvc_prepiomatrix(IO_q,Q_q)
  X_q <- gvc_leontiefinverse(iomatrix_q, Q_q)

  LI_q <- gvc_melt_matrix(X_q, Q_q)

  return(LI_q)
}

getMBALflows <- function(bdata, aggsets = FALSE, threshold = 0, useloop = TRUE) {
  #threshold applied to a couple of bigger matrices in the code that are not shares or coefficients
  # useloop will try to break some joins down into smaller loops, for memory use efficiency.
  # For bigger things this is helps prevent crashes and is faster, for smaller things this is slower

  mbal = gvc_prepmatbal(bdata,threshold,useloop)
  Q_q <- mbal[[1]]
  F_q <- mbal[[2]]
  IO_q <- mbal[[3]]

  ## Invert Matrix -----
  iomatrix_q <- gvc_prepiomatrix(IO_q,Q_q)
  X_q <- gvc_leontiefinverse(iomatrix_q, Q_q)

  # converse converted to 'long' format'
  LI_q <- gvc_melt_matrix(X_q, Q_q)
  ## Material Flow indicators ---------
  colnames(F_q) <- c("COMM_2","AGENT","REG_2","REG_3","Value_F")
  F_q <- subset(F_q, Value_F > threshold)
  LI_q <- subset(LI_q, value > 0)

  # Production required for final demand by agent !
  # ! Mulitply inverted Leontief matrix with demand to  determine amount of production included (direct and indirect)
  # - Leontief inverse specifies the amounnt of production i from region p needed for final demand of c in s
  # - the final demand from the material balances specifies for each agent in
  # region d how much final product c they demand from region s!
  # # Production i in p for final demand c in d from s by a, q-based (mil USD)#;

  if(useloop){
    QFD_q <- data.frame()

    looplist <- unique(select(ungroup(F_q), COMM_2, REG_2))
    looplist <- unique(select(ungroup(F_q), REG_2))
    print("starting loop over REG_2 column to make QFD_q matrix, this can take some time.")
    for (n in 1:nrow(looplist)) {
      print(paste("now at row",n,"of",nrow(looplist)))
      reg2 <- looplist$REG_2[n]
      F_Q_part <- subset(F_q, REG_2 == reg2)
      LI_q_part <- subset(LI_q, REG_2 == reg2)
      QFD_q_part <- full_join(F_Q_part,LI_q_part) %>% mutate(Value = value * Value_F) %>% select(-value, -Value_F) %>%
        subset(Value > threshold) %>%
        select(COMM, REG, COMM_2, REG_2, AGENT, REG_3, Value)
      QFD_q <- bind_rows(QFD_q,QFD_q_part)
    }

  } else {
    QFD_q <- full_join(F_q,LI_q) %>% mutate(Value = value * Value_F) %>% select(-value, -Value_F) %>%
      subset(Value > threshold) %>%
      select(COMM, REG, COMM_2, REG_2, AGENT, REG_3, Value)
  }




  #This provides the full matrix of how production of i in region p flows to the
  # agent a in d. Based on the Leontief inverse it captures all direct and
  # indirect flows through the global economy. Of these flows only the producer
  # location (p) and final commodity (c) source region (s) are made explicit in
  # tracing how commodity i from region p arrives in final consumption of c from s
  # by agent a located in region d.

  # This coefficient can serve to derive more manageable subset of flows like
  # direct and indirect flows of primary products etc.
  #
  # NB summing over all c,s,d and a should equal the production i in p
  # (requirement of the material balance that the demand for each commodity
  #   (direct + indirect) has to equal production!

  # Check if sum over all flows to all final demand = production - These check numbers should be small.
  # This only works properly if you don't filter out all the small numbers in the step above
  QFD_ch <- group_by(QFD_q,COMM,REG) %>% summarize(Value = sum(Value)) %>% left_join(rename(Q_q, Value_Q = Value)) %>% mutate(valuecheck = Value - Value_Q, valuecheckperc = 100* valuecheck/Value_Q)

  if(typeof(aggsets) == "list") {
    # adding margs back into aggregation,
   # aggsets <- subset(aggsets, Value %in% margs) # doing
    QFD_q$COMM <- plyr::mapvalues(QFD_q$COMM, aggsets$Value, aggsets$Header)
    QFD_q$COMM_2 <- plyr::mapvalues(QFD_q$COMM_2, aggsets$Value, aggsets$Header)
    QFD_q <- QFD_q %>% group_by(across(c(-Value))) %>% summarize(Value = sum(Value))
  }

  #Adding production side shares
  QFD_q <- QFD_q %>% group_by(COMM,REG) %>% mutate(ProdShare = Value/sum(Value)) %>% ungroup()

  return(QFD_q)

}


getcommregindicators <- function(sets, bdata, aggsets = FALSE) {

  qprod <- bdata$PROD %>% rename(COMM = ACTS) %>% mutate(Indicator = "Quantity", Unit = "1000 ton",
                                                         Value = Value/1000)  #converting to 1000 ton)

  qprod$COMM <- ifelse(qprod$COMM == "othcmt","othctl",as.character(qprod$COMM))
  qprod$COMM <- ifelse(qprod$COMM == "bfmt","cattle",as.character(qprod$COMM))
  qprod$COMM <- ifelse(qprod$COMM == "othmt","pigpls",as.character(qprod$COMM))
  qprod$COMM <- ifelse(qprod$COMM == "pulmt","pltry",as.character(qprod$COMM))
  qprod$COMM <- ifelse(qprod$COMM == "vol","oils",as.character(qprod$COMM))
  qprod$COMM <- ifelse(qprod$COMM == "dairy","milk",as.character(qprod$COMM))
  qprod$COMM <- ifelse(qprod$COMM == "sugar","sug",as.character(qprod$COMM))
  qprod$COMM <- ifelse(qprod$COMM == "mola","sug",as.character(qprod$COMM))
  qprod$COMM <- ifelse(qprod$COMM == "b_t","crops",as.character(qprod$COMM))
  qprod$COMM <- ifelse(qprod$COMM == "fishp","wfish",as.character(qprod$COMM))
  qprod$COMM <- ifelse(qprod$COMM == "fishm","wfish",as.character(qprod$COMM))
  qprod$COMM <- ifelse(qprod$COMM == "pcr","pdr",as.character(qprod$COMM))

  vprod <- bdata$PRDQ %>% group_by(COMM, REG) %>% summarize(Value = sum(Value)) %>%
           mutate(Indicator = "Dollar Quantity", Unit = "mil USD")

  if("ENDWL_COMM" %in% colnames(bdata$LDEM)){
    bdata$LDEM <- rename(bdata$LDEM, ENDWL = ENDWL_COMM)
  }
  ldem <- bdata$LDEM %>%ungroup()%>% select(-ENDWL) %>% rename(COMM = ACTS) %>% mutate(Indicator = "Land", Unit = "1000 km2",
                                                                                       Value = Value/1000)  #converting to 1000 km2)
  wtvl <- bdata$WTVL %>%rename(COMM = ACTS) %>% mutate(Indicator = "Water", Unit = "million m3",
                                                       Value = Value/1000000)  #converting to Million m3)

  if("GAS" %in% colnames(bdata$QEMI)) { #name is different in updates.
    bdata$QEMI <- rename(bdata$QEMI, GHG = GAS)
  }

  co2eq <- bdata$QEMI %>% rename(COMM = FUELUSER) %>% group_by(COMM,REG) %>% summarize(Value = sum(Value)) %>%
    mutate(Indicator = "CO2eq", Unit = "Mton CO2eq")

  ch4 <- bdata$QEMI %>% subset(GHG == "CH4") %>% rename(COMM = FUELUSER) %>% group_by(COMM,REG) %>% summarize(Value = sum(Value)) %>%
    mutate(Indicator = "CH4", Unit = "Mton CO2eq")

  n2o <- bdata$QEMI %>% subset(GHG == "N2O") %>% rename(COMM = FUELUSER) %>% group_by(COMM,REG) %>% summarize(Value = sum(Value)) %>%
    mutate(Indicator = "N2O", Unit = "Mton CO2eq")

  cal <- bdata$NVOM %>% rename(COMM = PRIM_AGRI) %>% subset(NUTRIENTS == "CAL") %>% select(-NUTRIENTS) %>%
    mutate(Indicator = "Calories", Unit = "kcal")

  quant <- bdata$NVOM %>% rename(COMM = PRIM_AGRI) %>% subset(NUTRIENTS == "QUANT") %>% select(-NUTRIENTS) %>%
    mutate(Indicator = "Quant", Unit = "g")

  transco2eq <- bdata$QEMI %>% subset(FUELUSER == "trans") %>% group_by(REG) %>% summarize(TransCO2eq = sum(Value)) %>% ungroup()
  transshr <- bdata$VDFB %>% subset(COMM == "trans") %>% group_by(REG) %>% mutate(Value = Value/sum(Value))
  transco2eq <- left_join(transshr, transco2eq) %>% mutate(TransCO2eq = TransCO2eq * Value) %>% select(-Value) %>%
    select(COMM = ACTS, REG, Value = TransCO2eq) %>%
    mutate(Indicator = "TransCO2eq", Unit = "Mton CO2eq")

  MBALIndicators <- bind_rows(qprod,vprod,ldem, wtvl, co2eq, ch4,n2o,cal,quant,transco2eq)

  fertfile <- system.file("extdata", "FERTDEM.HAR", package="magnetr")
  fertdem <- magnet_read_all_headers(fertfile)$FCTN #fertilzer use in tonnes, but by gtap agg.
  fertdem <- rename(fertdem, DCOMM = AGRI_COMM)
  commmap <-  getcommapping(sets)
  regmap <-  getregmapping(sets)

  fertdem2 <- makeagg_singledf(fertdem, select(commmap, DCOMM,COMM)) %>% makeagg_singledf(select(regmap, DREG,REG)) %>%
    rename(COMM = DCOMM, REG = DREG) %>% ungroup()

  fert_p <- subset(fertdem2, FERTT == "fert_p") %>% select(-FERTT) %>% mutate(Indicator = "Fert_P", Unit = "ton P2O5")
  fert_n <- subset(fertdem2, FERTT == "fert_n") %>% select(-FERTT) %>% mutate(Indicator = "Fert_N", Unit = "ton N")

  # landcomm <- unique(subset(ldem, Value > 0)$COMM)
  # pest <- rbind(bdata$DINQ, bdata$MINQ) %>%
  #   subset(COMM %in% c("chm", "chem", "chmbphplas") & ACTS %in% landcomm) %>% select(-COMM) %>% rename(COMM = ACTS) %>%
  #   group_by(COMM,REG) %>% summarize(Value = sum(Value)) %>% mutate(Indicator = "Pest_$", Unit = "dollar")


  pestfile <- system.file("extdata", "PESTDEM.HAR", package="magnetr")
  pestdem <- magnet_read_all_headers(pestfile)$PCOQ #fertilzer use in tonnes, but by gtap agg on 160 regions.
  pestdem <- rename(pestdem, DCOMM = A_PACT) %>% select(-PESTT) %>%
    mutate(DCOMM = gsub("^a_","",DCOMM))

  pestdem2 <- makeagg_singledf(pestdem, select(commmap, DCOMM,COMM)) %>% makeagg_singledf(select(regmap, DREG,REG)) %>%
    rename(COMM = DCOMM, REG = DREG) %>% ungroup() %>% subset(REG %in% regmap$REG)

  pest <- pestdem2 %>% mutate(Indicator = "Pesticides", Unit = "1000 ton")


  MBALIndicators <- bind_rows(MBALIndicators, fert_n,fert_p,pest)

  if(typeof(aggsets) == "list") {
    MBALIndicators$COMM <- plyr::mapvalues(MBALIndicators$COMM, aggsets$Value, aggsets$Header)
  }
  MBALIndicators <- group_by(MBALIndicators, across(c(-Value))) %>% summarize(Value = sum(Value)) %>% unique()

  MBALIndicators <- rename(MBALIndicators, IndicatorValue = Value)

  MBALIndicators <- ungroup(MBALIndicators)

  return(MBALIndicators)
}

addgvcindicators <- function(gvcdata, indicators){

  gvcdataout <- data.frame()

  looplist <- as.character(unique(select(ungroup(indicators), Indicator))$Indicator)
  print("starting loop over indicators (to avoid gigantic data) to make indicators long gvc list.")
  for (n in 1:length(looplist)) {
    print(paste("now at indicator",n,"of",length(looplist)))
    ind <- looplist[n]
    indicator_part <- subset(indicators, Indicator == ind)
   # gvcdataout_part <- merge(gvcdata,indicator_part) %>%
  #    mutate(VirtualFlow = ProdShare * IndicatorValue) %>% select(-ProdShare, -Value) %>% subset(VirtualFlow > 0)
    gvcdataout_part <- left_join(gvcdata,indicator_part) %>%
      mutate(VirtualFlow = ProdShare * IndicatorValue) %>% select(-ProdShare, -Value) %>% subset(VirtualFlow > 0)

    gvcdataout <- bind_rows(gvcdataout, gvcdataout_part)
  }

  return(gvcdataout)

}


generate_ncmf <- function(gvcdata, bdata){

  #This calcualtes the correction factor from primary produciton quantities (NVOM) to supply values (FSNU) according to the food balance sheets
  FSNU <- bdata$FSNU %>% rename(REG_3 = REG, COMM = PRIM_AGRI, FSNUval = Value) #%>% subset(NUTRIENTS != "lanU")
  NVOM <- bdata$NVOM %>% rename(COMM = PRIM_AGRI, NVOMval = Value) #%>% subset(NUTRIENTS != "lanU")

  ncmf <-  gvcdata %>%
    left_join(NVOM) %>% mutate(VirtualFlow = NVOMval * ProdShare) %>% subset(VirtualFlow > 0) %>%
    left_join(FSNU) %>% group_by(COMM, REG_3, NUTRIENTS) %>% mutate(FSNU_LI = sum(VirtualFlow)) %>% ungroup() %>% mutate(NCMFVal = FSNU_LI/FSNUval) %>%
    select(COMM,REG_3,NUTRIENTS,NCMFVal) %>% unique()

  return(ncmf)
}

make_food_gvc <- function(gvcdata, sets){
  #typical way to subset the gvcdata for food analysies, ignores all nonfood flows, and focuses on primary --> processed flows into households only.
  nonfoodset <- sets$NONF$Value
  comm <- sets$COMM$Value
  hfood <- setdiff(comm, nonfoodset)
  primagri <- sets$PRAG$Value
  procsevfood <- setdiff(hfood, primagri)

  gvc_food <- gvcdata %>% subset(COMM %in% primagri & COMM_2 %in% procsevfood | (COMM == COMM_2 & COMM %in% primagri)) %>%
    subset(AGENT == "hh") %>% select(-AGENT) %>%
    group_by(COMM,REG) %>% mutate(ProdShare = Value/sum(Value)) %>% ungroup()

  return(gvc_food)
}

make_nutrients_gvc <- function(gvcdata,bdata,NCMF){

  if("PRIM_AGRI" %in% colnames(NCMF)){NCMF <- rename(NCMF, COMM = PRIM_AGRI)}
  NVOM <- bdata$NVOM
  if("NUTRIENTS" %in% colnames(NVOM)){NVOM <- rename(NVOM, NUTRIENTS0 = NUTRIENTS)}
  NVOM <- NVOM %>% rename(COMM = PRIM_AGRI, NVOMval = Value) %>% subset(NUTRIENTS0 != "lanU")

  population <- bdata$POP %>% rename(REG_3 = REG, POP = Value)
  gvcdata_nutrients <- gvcdata %>%
    left_join(NVOM) %>% left_join(NCMF)  %>%
    mutate(VirtFlow = (NVOMval * ProdShare / NCMFVal)) %>% subset(VirtFlow > 0) %>%
    left_join(population) %>%
    mutate(VirtFlowPerCapDay = VirtFlow / POP / 365) %>%
    select(-NVOMval, -ProdShare,-POP,-NCMFVal)
  return(gvcdata_nutrients)
}

make_pefood <- function(gvcdata_nutrients){

  if("NUTRIENTS" %in% colnames(gvcdata_nutrients)){gvcdata_nutrients <- rename(gvcdata_nutrients, NUTRIENTS0 = NUTRIENTS)}

  PEFOOD <- select(gvcdata_nutrients, PRIM_AGRI = COMM, HFOOD = COMM_2,REG, REG_2 = REG_3,NUTRIENTS0,Value = VirtFlowPerCapDay) %>%
    group_by(PRIM_AGRI, HFOOD, REG, REG_2, NUTRIENTS0) %>% summarize(Value = sum(Value))
  PEFOOD <- with(PEFOOD, PEFOOD[order(HFOOD,PRIM_AGRI),])

  PEFOODTOT <- select(gvcdata_nutrients, PRIM_AGRI = COMM, HFOOD = COMM_2,REG, REG_2 = REG_3,NUTRIENTS0,Value = VirtFlow) %>%
    group_by(PRIM_AGRI, HFOOD, REG, REG_2, NUTRIENTS0) %>% summarize(Value = sum(Value))
  PEFOODTOT <- with(PEFOODTOT, PEFOODTOT[order(HFOOD,PRIM_AGRI),])

  pefoodout <- list()
  pefoodout$PEFO <- PEFOOD
  pefoodout$PEFT <- PEFOODTOT
  return(pefoodout)
}
