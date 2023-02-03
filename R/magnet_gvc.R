getMBALflows <- function(sets, bdata, threshold = 0.1) {
  regs <- sets$REG$Value
  margs <- sets$MARG$Value
  agents <- c("hh","govt","CGDS")
  comms<- unique(bdata$MAKB$COMM)

  ## start basedata read ----------------

  # this is the code copied from PostSimCalc/Materialbalances.gmp, which collectes the data form basedatabe.

  chk_0ITRADE <- sum(subset(bdata$VFOB, REG == REG_2)$Value)
  if(chk_0ITRADE >0){stop("Check that your data hase no internal trade #")}

  # COMM_SHR(c,a,r) # Commodity share in activity input use #;
  COMM_SHR <- bdata$MAKB %>% group_by(ACTS,REG) %>% mutate(Value = Value/sum(Value)) %>% ungroup() %>%
    mutate(Value = ifelse(is.nan(Value), 0, Value))

  # ! Total value of domestic production = sum of activity accounts (MAKE matrix) +
  #   export subsidies (taxes) on exports included in VFOB values of exports (tee_entry in GTAP SAM)!
  # TEE(c,s,d) # Export subsiides/taxes included in VFOB #;
  TEE <- left_join(bdata$VFOB, rename(bdata$VXSB, V2 = Value)) %>% mutate(Value = Value - V2) %>% select(-V2)

  Q_v <- left_join(bdata$MAKB %>% group_by(COMM,REG) %>% summarize(V1 = sum(Value)),
                   TEE %>% group_by(COMM,REG) %>% summarize(V2 = sum(Value))) %>%
    mutate(Value = V1 + V2) %>% select(-V1,-V2)

  # I_D(c,p,r) # Intermediate demand for domestic commodities (mil USD) #;
  I_D <- left_join(COMM_SHR %>% rename(COMM_2 = COMM, V1 = Value),
                   bdata$VDFB %>% rename(V2 = Value)) %>%
    mutate(Value = V1 * V2) %>% select(-V2,-V1) %>%
    group_by(COMM, COMM_2, REG) %>% summarize(Value = sum(Value))

  # F_D(c,a,r) # Final demand for domestic commodities by agent (mil USD) #;
  F_D <- rbind(bdata$VDPB %>% mutate(AGENT = "hh"),
               bdata$VDGB %>% mutate(AGENT = "govt"),
               bdata$VDIB %>% mutate(AGENT = "CGDS"))
  #
  # MDEMA(c,a,r) # Agent's demand for imports by region r #;
  #"MDEMCAT set is comms + agents"
  MDEMA <- bind_rows(left_join(COMM_SHR %>% rename(p = COMM, V1 = Value),
                               bdata$VMFB %>% rename(c = COMM, V2 = Value)) %>%
                       mutate(Value = V1 * V2) %>% select(-V2,-V1) %>%
                       rename(COMM = c, MDEMCAT = p) %>%
                       group_by(COMM, MDEMCAT, REG) %>% summarize(Value = sum(Value)),
                     bdata$VMPB %>% mutate(MDEMCAT = "hh"),
                     bdata$VMGB %>% mutate(MDEMCAT = "govt"),
                     bdata$VMIB %>% mutate(MDEMCAT = "CGDS")) %>%
    mutate(Value = ifelse(Value <0, 0, Value))  # !get rid of tiny negatives throwing zero divide errors!

  # MDEMT(c,r) # Total demand for imports by region r #;
  MDEMT <- MDEMA %>% group_by(COMM,REG) %>% summarize(Value = sum(Value))

  # MASHR(c,a,r) # Agent's shares in total imports by region r #;
  MASHR <- left_join(MDEMA, rename(MDEMT, V1 = Value)) %>%
    mutate(Value = ifelse(V1 > 0, Value/V1,0)) %>% select(-V1)

  # MAGNT(c,a,s,d) # Agent's imports by region r (FOB) #;
  MAGNT <- left_join(rename(MASHR, REG_2 = REG),
                     rename(bdata$VFOB, V1 = Value)) %>%
    mutate(Value = Value * V1) %>%
    select(COMM,MDEMCAT,REG, REG_2, Value)

  # ! Assign to intermediate and final demand categories!
  # I_M(c,p,s,d) # Intermediate demand for imported commodities s in d (mil USD) #;
  I_M <- MAGNT %>% subset(MDEMCAT %in% comms) %>% rename(COMM_2 = MDEMCAT)

  # F_M(c,a,s,d) # Final demand for imported commodities from s in d (mil USD) #;
  F_M <- MAGNT %>% subset(MDEMCAT %in% agents) %>% rename(AGENT = MDEMCAT)

  # !Compute transport margins by region and agent based on shares in imports!
  TRMMAGNT <- left_join(rename(MASHR, REG_2 = REG), rename(bdata$VTWR, V1 = Value)) %>%
    mutate(Value = V1 * Value) %>%
    select(MARG, COMM, MDEMCAT, REG, REG_2, Value)

  # !Compute bilateral exports of transport services based on regional
  # contributions to global pool!
  # VSTSHR(i,r) # Regions's share in global pool of transport services #;
  VSTSHR <- bdata$VST %>% group_by(MARG) %>% mutate(Value = Value/sum(Value))

  # # Linking transporter country (t) to demand for transport by destination d #;
  TRANSPRT <- left_join(rename(VSTSHR, t = REG), rename(TRMMAGNT, s = REG, REG = REG_2, V1 = Value)) %>%
    mutate(Value = Value * V1) %>% group_by(MARG,MDEMCAT,REG,t) %>% summarize(Value = sum(Value)) %>%
    select(MARG,MDEMCAT,REG,REG_2 = t,Value)

  # ! Remove deliveries to self from transport coefficient  - to be added to domestic use of margins!
  # dTRANSPRT(m,a,d,t)  Domestic transport services for trade from regions s (own trade 0) #;
  dTRANSPRT <- TRANSPRT %>% mutate(Value = ifelse(REG == REG_2, Value, 0)) %>% subset(REG == REG_2) %>% select(-REG_2)
  TRANSPRT <- TRANSPRT %>% mutate(Value = ifelse(REG == REG_2, 0, Value))

  # ! Add the transport services (margins) from other services to imports by region!
  I_M <- left_join0(rename(I_M, m = COMM, p = COMM_2, t = REG, d = REG_2),
                    rename(TRANSPRT, m = MARG, p = MDEMCAT,  d = REG, t = REG_2, V1 = Value)) %>%
    mutate(V2 = V1 + Value) %>% #since we did left_join0 with zeroes this only does something for what is in TRANSPRT
    select(COMM = m, COMM_2 = p, REG = t, REG_2 = d, Value = V2)

  F_M <- left_join0(rename(F_M, m = COMM, a = AGENT, t = REG, d = REG_2),
                    rename(TRANSPRT, m = MARG, a = MDEMCAT,  d = REG, t = REG_2, V1 = Value)) %>%
    mutate(V2 = V1 + Value) %>% #since we did left_join0 with zeroes this only does something for what is in TRANSPRT
    select(COMM = m, AGENT = a, REG = t, REG_2 = d, Value = V2)

  # !Margins provided to self are added to domestic demand for transport services!
  I_D <- left_join0(rename(I_D, m = COMM, p = COMM_2, d = REG),
                    rename(dTRANSPRT, m = MARG, p = MDEMCAT, d = REG, V1 = Value)) %>%
    mutate(V2 = V1 + Value) %>% #since we did left_join0 with zeroes this only does something for what is in TRANSPRT
    select(COMM = m, COMM_2 = p, REG = d, Value = V2)

  F_D <- left_join0(rename(F_D, m = COMM, a = AGENT, d = REG),
                    rename(dTRANSPRT, m = MARG, a = MDEMCAT, d = REG, V1 = Value)) %>%
    mutate(V2 = V1 + Value) %>% #since we did left_join0 with zeroes this only does something for what is in TRANSPRT
    select(COMM = m, AGENT = a, REG = d, Value = V2)

  # ! Intermediates!
  I_v <- left_join(I_M, rename(I_D, V1 = Value)) %>%
    mutate(Value = ifelse(REG == REG_2, V1, Value)) %>% select(-V1)

  # ! Final demand!
  F_v <- left_join(F_M, rename(F_D, V1 = Value)) %>%
    mutate(Value = ifelse(REG == REG_2, V1, Value)) %>% select(-V1)

  # Prodiction
  Q_q <- bdata$PRDQ %>% group_by(COMM,REG) %>% summarize(Value = sum(Value))

  # I_DQ(c,p,r) # Intermediate demand for domestic commodities (mil USD) #;
  I_DQ <- left_join(rename(COMM_SHR, p = COMM), rename(bdata$DINQ, V1 = Value, c = COMM)) %>% mutate(Value = V1 * Value) %>%
    group_by(c,p,REG) %>% summarize(Value = sum(Value)) %>%
    rename(COMM = c, COMM_2 = p)

  # F_DQ(c,a,r) # Final demand for domestic commodities by agent (mil USD) #;
  F_DQ <- rbind(bdata$DFNH %>% mutate(AGENT = "hh"),
                bdata$DFNG %>% mutate(AGENT = "govt"),
                bdata$DFNI %>% mutate(AGENT = "CGDS"))

  # Coefficient (all,c,COMM)(all,a,MDEMCAT)(all,r,REG)
  # MDEMAQ(c,a,r) # Quantity of agent's demand for imports by region r #;
  MDEMAQ <- bind_rows(left_join(COMM_SHR %>% rename(p = COMM, V1 = Value),
                                bdata$MINQ %>% rename(c = COMM, V2 = Value)) %>%
                        mutate(Value = V1 * V2) %>% select(-V2,-V1) %>%
                        rename(COMM = c, MDEMCAT = p) %>%
                        group_by(COMM, MDEMCAT, REG) %>% summarize(Value = sum(Value)),
                      bdata$MFNH %>% mutate(MDEMCAT = "hh"),
                      bdata$MFNG %>% mutate(MDEMCAT = "govt"),
                      bdata$MFNI %>% mutate(MDEMCAT = "CGDS")) %>%
    mutate(Value = ifelse(Value <0, 0, Value))   # !get rid of tiny negatives throwing zero divide errors!

  # MDEMQT(c,r) # Quantity total demand for imports by region r #;
  MDEMQT <- MDEMAQ %>% group_by(COMM,REG) %>% summarize(Value = sum(Value))

  # MAQSHR(c,a,r) # Agent's quantity shares in total imports by region r #;
  MAQSHR <- left_join(MDEMAQ, rename(MDEMQT, V1 = Value)) %>%
    mutate(Value = ifelse(V1 > 0, Value/V1,0)) %>% select(-V1)

  # MAGNTQ(c,a,s,d) # Quantity of agent's imports by region r (mil USD) #;
  MAGNTQ <- left_join(rename(MAQSHR, d = REG),
                      rename(bdata$TRAD, s = REG, d = REG_2, V1 = Value)) %>%
    mutate(Value = Value * V1) %>%
    select(COMM,MDEMCAT,REG = s, REG_2 = d, Value)

  # #Quantity of intermediate demand for imported commodities s in d (mil USD) #;
  I_MQ <- MAGNTQ %>% subset(MDEMCAT %in% comms) %>% rename(COMM_2 = MDEMCAT)

  # # Quantity final demand for imported commodities from s in d (mil USD) #;
  F_MQ <- MAGNTQ %>% subset(MDEMCAT %in% agents) %>% rename(AGENT = MDEMCAT)
  #
  # TRMMAGNTQ(m,c,a,s,d) # Agent's transport margins by region s #;
  TRMMAGNTQ <- left_join(rename(MAQSHR, REG_2 = REG), rename(bdata$DTRN, V1 = Value)) %>%
    mutate(Value = V1 * Value) %>%
    select(MARG, COMM, MDEMCAT, REG, REG_2, Value)

  # !Compute bilateral exports of transport services based on regional contributions to global pool!
  # # Region share in quantity global pool of transport services #;
  shr_TRANSS_q <- bdata$STRN %>% group_by(MARG) %>% mutate(Value = Value/sum(Value))

  # # Linking transporter country (t) to demand for transport by destination d #;
  TRANSPRTQ <- left_join(rename(shr_TRANSS_q, t = REG), rename(TRMMAGNTQ, s = REG, REG = REG_2, V1 = Value)) %>%
    mutate(Value = Value * V1) %>% group_by(MARG,MDEMCAT,REG,t) %>% summarize(Value = sum(Value)) %>%
    select(MARG,MDEMCAT,REG,REG_2 = t,Value)

  # # Qunatity domestic transport services for trade from regions s (own trade 0)#;

  dTRANSPRTQ <- TRANSPRTQ %>% mutate(Value = ifelse(REG == REG_2, Value, 0)) %>% subset(REG == REG_2) %>% select(-REG_2)
  TRANSPRTQ <- TRANSPRTQ %>% mutate(Value = ifelse(REG == REG_2, 0, Value))

  # ! Add the transport services (margins) from other services to imports by region!
  # I_MQ(m,p,t,d) = I_MQ(m,p,t,d) + TRANSPRTQ(m,p,d,t);
  I_MQ <- left_join0(rename(I_MQ, m = COMM, p = COMM_2, t = REG, d = REG_2),
                     rename(TRANSPRTQ, m = MARG, p = MDEMCAT,  d = REG, t = REG_2, V1 = Value)) %>%
    mutate(V2 = V1 + Value) %>% #since we did left_join0 with zeroes this only does something for what is in TRANSPRT
    select(COMM = m, COMM_2 = p, REG = t, REG_2 = d, Value = V2)

  F_MQ <- left_join0(rename(F_MQ, m = COMM, a = AGENT, t = REG, d = REG_2),
                     rename(TRANSPRTQ, m = MARG, a = MDEMCAT,  d = REG, t = REG_2, V1 = Value)) %>%
    mutate(V2 = V1 + Value) %>% #since we did left_join0 with zeroes this only does something for what is in TRANSPRT
    select(COMM = m, AGENT = a, REG = t, REG_2 = d, Value = V2)

  # !Margins provided to self are added to domestic demand for transport services!
  I_DQ <- left_join0(rename(I_DQ, m = COMM, p = COMM_2, d = REG),
                     rename(dTRANSPRTQ, m = MARG, p = MDEMCAT, d = REG, V1 = Value)) %>%
    mutate(V2 = V1 + Value) %>% #since we did left_join0 with zeroes this only does something for what is in TRANSPRT
    select(COMM = m, COMM_2 = p, REG = d, Value = V2)

  F_DQ <- left_join0(rename(F_DQ, m = COMM, a = AGENT, d = REG),
                     rename(dTRANSPRTQ, m = MARG, a = MDEMCAT, d = REG, V1 = Value)) %>%
    mutate(V2 = V1 + Value) %>% #since we did left_join0 with zeroes this only does something for what is in TRANSPRT
    select(COMM = m, AGENT = a, REG = d, Value = V2)

  # ! Intermediates!
  I_q <- left_join(I_MQ, rename(I_DQ, V1 = Value)) %>%
    mutate(Value = ifelse(REG == REG_2, V1, Value)) %>% select(-V1)

  # ! Final demand!
  F_q <- left_join(F_MQ, rename(F_DQ, V1 = Value)) %>%
    mutate(Value = ifelse(REG == REG_2, V1, Value)) %>% select(-V1)

  #IO coeff.(value): use of i from region s when producing comm. p in region d#;
  IO_v <- left_join(I_v, rename(Q_v, REG_2 = REG, COMM_2 = COMM, V2 = Value)) %>%
    mutate(Value = ifelse(V2 > 0, Value/V2, 0)) %>%
    select(COMM, REG, COMM_2, REG_2, Value) %>% ungroup()
  #IO coeff.(qtity): use of i from region s when producing comm. p in region d#;
  IO_q <- left_join(I_q, rename(Q_q, REG_2 = REG, COMM_2 = COMM, V2 = Value)) %>%
    mutate(Value = ifelse(V2 > 0, Value/V2, 0)) %>%
    select(COMM, REG, COMM_2, REG_2, Value) %>% ungroup()

  ## Invert Matrix -----
  #make commreg, commbination of comm and reg to make matrix with.:
  comregmap <- Q_q %>% select(-Value) %>% mutate(COMREG = paste(COMM, REG, sep="_"))
  comregmap2 <- rename(comregmap, COMREG_2 = COMREG, REG_2 = REG, COMM_2 = COMM)
  comreg <- comregmap$COMREG

  aIO_q <- left_join(IO_q, comregmap) %>% left_join(comregmap2) %>% select(-COMM,-COMM_2,-REG,-REG_2)

  idmatrix <- diag(length(comreg)) #identity matrix with same dimension

  iomatrix_q <- expand_grid(COMREG = comreg, COMREG_2 = comreg)
  iomatrix_q <- left_join(iomatrix_q, aIO_q) %>% mutate(COMREG = factor(COMREG, levels = comreg)) # to ensure order
  iomatrix_q <- spread(iomatrix_q, COMREG_2, Value) %>% select(comreg) # the select ensure order of columns

  A_q <- idmatrix - iomatrix_q # Making A matrix
  leontiefinverse_q <- solve(A_q) # this inverts the matrix
  X_q <- array(as.vector(leontiefinverse_q), #put the right names in again after solve
               dim = c(length(comreg),length(comreg)),
               dimnames = list(COMREG = comreg,COMREG_2 = comreg))

  # converse converted to 'long' format'
  LI_q <- melt(X_q) %>% left_join(comregmap) %>% left_join(comregmap2) %>% select(-COMREG,-COMREG_2)


  ## can also be done for value based, but commmenting out for speed of code and memmory
  # aIO_v <- left_join(IO_v, comregmap) %>% left_join(comregmap2) %>% select(-COMM,-COMM_2,-REG,-REG_2)
  #
  # idmatrix <- diag(length(comreg)) #identity matrix with same dimension
  #
  # iomatrix_v <- expand_grid(COMREG = comreg, COMREG_2 = comreg)
  # iomatrix_v <- left_join(iomatrix_v, aIO_v) %>% mutate(COMREG = factor(COMREG, levels = comreg)) # to ensure order
  #
  # iomatrix_v <- spread(iomatrix_v, COMREG_2, Value) %>% select(comreg) # the select ensure order of columns
  #
  # A_v <- idmatrix - iomatrix_v
  #
  # leontiefinverse_v <- solve(A_v)
  # X_v <- array(as.vector(leontiefinverse_v), #put the right names in again after solve
  #              dim = c(length(comreg),length(comreg)),
  #              dimnames = list(COMREG = comreg,COMREG_2 = comreg))
  #
  #
  # # converse converted to 'long' format'
  # LI_v <- melt(X_v) %>% left_join(comregmap)  %>% left_join(comregmap2) %>% select(-COMREG,-COMREG_2)

  # colnames(F_v) <- c("COMM_2","AGENT","REG_2","REG_3","Value_F")

  ## Material Flow indicators ---------
  colnames(F_q) <- c("COMM_2","AGENT","REG_2","REG_3","Value_F")
  F_q <- subset(F_q, Value_F > 0)
  LI_q <- subset(LI_q, value > 0)
 # filter out zeros for compactness, removing small numbers if model is large.
  if(length(unique(F_q$REG_2))>20) {
    F_q <- subset(F_q, Value_F > threshold)
    LI_q <- subset(LI_q, value > threshold)
  }

  # Production required for final demand by agent !
  # ! Mulitply inverted Leontief matrix with demand to  determine amount of production included (direct and indirect)
  # - Leontief inverse specifies the amounnt of production i from region p needed for final demand of c in s
  # - the final demand from the material balances specifies for each agent in
  # region d how much final product c they demand from region s!
  # # Production i in p for final demand c in d from s by a, q-based (mil USD)#;
  QFD_q <- merge(F_q,LI_q) %>% mutate(Value = value * Value_F) %>% select(-value, -Value_F) %>%
    subset(Value > threshold) %>%
    select(COMM, REG, COMM_2, REG_2, AGENT, REG_3, Value)

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

  #FBDG header PEQ_FBDG geeft mapping.

  MBALIndicators <- rbind(qprod,ldem, wtvl, co2eq, ch4,n2o,cal,quant)

  if(typeof(aggsets) == "list") {
    MBALIndicators$COMM <- plyr::mapvalues(MBALIndicators$COMM, aggsets$Value, aggsets$Header)
  }
  MBALIndicators <- group_by(MBALIndicators, across(c(-Value))) %>% summarize(Value = sum(Value)) %>% unique()

  MBALIndicators <- rename(MBALIndicators, IndicatorValue = Value)

  return(MBALIndicators)
}
