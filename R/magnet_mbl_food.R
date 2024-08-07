MBL_InvertLeontief_food <- function(GTAPSETS, ACTDAT, GTAPDATA, Check_inv = FALSE) {

  # EXTRA sets to trace food flows -------------------------------------------------

  # Set COMM # Traded commodities (including split sectors) #
  COMM <- GTAPSETS$COMM$Value

  # Set PRAG # Primary agriculture in the model
  PRIM_AGRI <- GTAPSETS$PRAG$Value

  # Set NONF # Non-food commodities in the model
  NONF <- GTAPSETS$NONF$Value

  # Set HFOOD # Commodities consumed as food
  HFOOD <- setdiff(COMM, NONF)

  # Set PFOOD # Commodities consumed as processed food
  PFOOD <- setdiff(HFOOD, PRIM_AGRI)

  # Load results from MBL_ConstructBalances -----------------------------------------
  print("start routine MBL_ConstructBalances")
  ConstructBalances <- MBL_ConstructBalances(GTAPSETS, ACTDAT, GTAPDATA)
  print("finished routine MBL_ConstructBalances")

  MBL_COMM_SHR <- ConstructBalances[[1]]
  MBL_Q_q <- ConstructBalances[[2]]
  MBL_s_FP_q <- ConstructBalances[[3]]
  MBL_s_FG_q <- ConstructBalances[[4]]
  MBL_s_FI_q <- ConstructBalances[[5]]
  MBL_s_I_q <- ConstructBalances[[6]]

  # Remove all flows from primary agriculture which is not consumed by humans

  MBL_s_I_q_food <-  MBL_s_I_q %>%
    rename(i = COMM,
           s = REG,
           c = COMM_2,
           d = REG_2) %>%
    #remove all primary agriculture inputs in activities, except for processed food;
    mutate(Value = ifelse(i %in% PRIM_AGRI & !c %in% PFOOD, 0, Value)) %>%
    #remove all inputs in the activity primary agriculture;
    mutate(Value = ifelse(c %in% PRIM_AGRI, 0, Value)) %>%
    select(COMM = i,
           REG = s,
           COMM_2 = c,
           REG_2 = d,
           Value)

  # Derive production difference take sum of the use of i from region s;
  MBL_Q_q_diff <- MBL_s_I_q %>%
    rename(i = COMM,
           s = REG,
           c = COMM_2,
           d = REG_2,
           s_I_q_original = Value) %>%
    left_join(.,              MBL_s_I_q_food %>%
                rename(i = COMM,
                       s = REG,
                       c = COMM_2,
                       d = REG_2,
                       s_I_q_corrected = Value)) %>%
    mutate(s_I_q_diff = s_I_q_original - s_I_q_corrected ) %>%
    group_by(i,s) %>%
    summarise(Value = sum(s_I_q_diff)) %>%
    ungroup %>%
    select(COMM = i,
           REG = s,
           Value)

  # Correct total production for removed flows in intermediates maintain material balance;

  MBL_s_Q_q <- MBL_Q_q %>%
    rename(i = COMM,
           s = REG,
           Q_q = Value) %>%
    left_join(. , MBL_Q_q_diff %>%
                rename(i = COMM,
                       s = REG,
                       Q_q_diff = Value)
    ) %>%
    mutate(Value = Q_q - Q_q_diff) %>%
    select(COMM = i,
           REG = s,
           Value)

  # Region Create matrix of technical coefficients

  # MBL_s_IO_q(i,s,c,d) #IO coeff.(qtity): use of i from region s when producing comm. c in region d#;
  MBL_s_IO_q <- MBL_s_I_q_food %>%
    rename(i = COMM,
           s = REG,
           c = COMM_2,
           d = REG_2,
           s_I_q = Value) %>%
    left_join(.,
              MBL_s_Q_q %>%
                rename(c = COMM,
                       d = REG,
                       Q_q = Value)) %>%
    mutate(Value = ifelse(Q_q > 0, s_I_q / Q_q, 0)) %>%
    select(COMM = i,
           REG = s,
           COMM_2 = c,
           REG_2 = d,
           Value)

  #make commreg, combination of comm and reg to make matrix with.:
  comregmap <-
    MBL_s_Q_q %>%
    select(-Value) %>%
    mutate(COMREG = paste(COMM, REG, sep = "_"))

  comregmap2 <- rename(comregmap,
                       COMREG_2 = COMREG,
                       REG_2 = REG,
                       COMM_2 = COMM)

  comreg <- comregmap$COMREG

  #Map technical coefficients into 2-dimensopnal matrix for inversion
  MBL_s_IO_q_2 <- MBL_s_IO_q %>%
    left_join(.,
              comregmap) %>%
    left_join(.,
              comregmap2) %>%
    select(-COMM,
           -COMM_2,
           -REG,
           -REG_2)

  MBL_IO <- expand_grid(COMREG = comreg,
                        COMREG_2 = comreg)

  MBL_IO <- MBL_IO %>%
    left_join(.,
              MBL_s_IO_q_2) %>%
    mutate(Value = ifelse(is.nan(Value), 0, Value)) %>%
    mutate(COMREG = factor(COMREG,
                           levels = comreg)) # to ensure order

  MBL_IO <-  spread(MBL_IO,
                    COMREG_2,
                    Value) %>%
    select(all_of(comreg)) # the select ensure order of columns


  # Initialize maximum deviation from identity matrix as check on inversion -----------

  #MBL_IMDIFmax # Maximum (absolute) deviation from identity matrix #;
  MBL_IMDIFmax <- 0.001

  # MBL_IM(i,k) # Identity matrix #;
  MBL_IM <-
    diag(length(comreg))

  #MBL_I_IO(i,j) # Identity - IO matrix that needs to be inverted #;
  MBL_I_IO <- MBL_IM - MBL_IO


  # Variables & equation ----------------------------------------------------------------

  # MBL_L(j,k) # Inverse Leontief matrix #;
  MBL_L <- solve(MBL_I_IO)
  # Identical to Equation E_MBL_L in GEMPACK code

  MBL_L <- array(
    as.vector(MBL_L),
    #put the right names in again after solve
    dim = c(length(comreg), length(comreg)),
    dimnames = list(COMREG = comreg, COMREG_2 = comreg)
  )

  # converse converted to 'long' format'
  MBL_L_long_comreg <- melt(MBL_L) %>%
    rename(Value = value)

  rownames(MBL_I_IO) <- comreg

  MBL_I_IO <- MBL_I_IO %>%
    tibble::rownames_to_column(., "COMREG") %>%
    pivot_longer(cols = comreg, names_to = "COMREG_2", values_to = "Value")

  # Post-simulation checks

  # This is very slow and crashes with larger models
#  if(Check_inv){
#    #MBL_CHK_INV(i,k) # Check on Leontief inversion - should be close to identity matrix#;
#    MBL_CHK_INV <-  MBL_I_IO %>%
#      rename(i = COMREG,
#             j = COMREG_2,
#             I_IO = Value) %>%
#      left_join(.,
#                MBL_L_long_comreg %>%
#                  rename(j = COMREG,
#                         k = COMREG_2,
#                         L = Value)) %>%
#      mutate(Value = I_IO * L) %>%
#      group_by(i, k) %>%
#      summarize(Value = sum(Value)) %>%
#      ungroup() %>%
#      select(COMREG = i,
#             COMREG_2 = k,
#             Value)
#
#    # Define assertion to generate error in case of faulty inversion#
#
#    rownames(MBL_IM) <- comreg
#    colnames(MBL_IM) <- comreg
#
#
#    #MBL_IM_DIF # Difference from identity matrix #;
#    MBL_IM_DIF <- MBL_CHK_INV  %>%
#      rename(i = COMREG,
#             k = COMREG_2,
#             CHK_INV = Value) %>%
#      left_join(.,
#                MBL_IM %>%
#                  melt() %>%
#                  rename(i = Var1,
#                         k = Var2,
#                         L = value)) %>%
#      mutate(Value = CHK_INV - L) %>%
#      summarize(Value = sum(Value))
#
#    if (MBL_IM_DIF$Value < MBL_IMDIFmax){
#      print("Good job!") } else { print("Arbitrary boundary - meant as warning to carefully check results")}
#  }

  return(list(MBL_COMM_SHR, MBL_s_Q_q, MBL_s_FP_q, MBL_s_FG_q, MBL_s_FI_q, MBL_L, comregmap, comregmap2, MBL_s_IO_q))

}

MBL_ProductionShares_food <- function(GTAPSETS, ACTDAT, GTAPDATA){

  # Load MBL_INvertLeontief
  print("start routine MBL_InvertLeontief_food")
  InvertLeontief_food <- MBL_InvertLeontief_food(GTAPSETS, ACTDAT, GTAPDATA)
  print("finished routine MBL_InvertLeontief_food")

  MBL_COMM_SHR <- InvertLeontief_food[[1]]
  MBL_s_Q_q <- InvertLeontief_food[[2]]
  MBL_s_FP_q <- InvertLeontief_food[[3]]
  MBL_s_FG_q <- InvertLeontief_food[[4]]
  MBL_s_FI_q <- InvertLeontief_food[[5]]
  MBL_L <-  InvertLeontief_food[[6]]
  comregmap <- InvertLeontief_food[[7]]
  comregmap2 <- InvertLeontief_food[[8]]
  MBL_s_IO_q <- InvertLeontief_food[[9]] # keeping this to return in main output

  rm(InvertLeontief_food)

  # Standard GTAP sets ----------------------------------------------------------

  # Set REG # Regions in the model
  REG <- GTAPSETS$REG

  # Set COMM # Traded commodities (including split sectors) #
  COMM <- GTAPSETS$COMM

  # Set MARG # margin commodities #
  MARG <- GTAPSETS$MARG
  # Subset MARG is subset of COMM;

  # Additional sets and mappings for footprints -------------------------------

  # Set FDEM # Final demand categories # (phh,gvt,inv);
  Value <- c("phh", "gvt", "inv")
  FDEM <- data.frame(Value)

  # Data preparation -----------------------------------------------------------

  # Split combined indices in Leontief invers
  #MBL_LI(i,s,c,d) # Quantity-based Leontief inverse  i from s in final demand c in d #;
  MBL_LI <- MBL_L %>%
    melt(.) %>%
    left_join(comregmap) %>%
    left_join(comregmap2) %>%
    select(-COMREG, -COMREG_2) %>%
    rename(Value = value)

  # Production required for final demand by agent  ---------------------------------

  # Leontief inverse specifies the amount of production i from region p needed
  # for final demand of c in s. Multiplying this with the final demand for c from s
  # by final demand categories from d provides full description of how products
  # flow from p via s to final demand in d. We combine the final demand categories
  # in a single set to create a single matrix

  #MBL_Q2FD(i,p,c,s,f,d) # Production i in p for final demand in d via cons. of c from s (mil USD)#;
  MBL_Q2FD <- data.frame()

  print("start creating MBL_Q2FD output")

  MBL_s_Fall_q <- bind_rows(
                  mutate(MBL_s_FP_q,f="phh"),
                  mutate(MBL_s_FG_q,f="gvt"),
                  mutate(MBL_s_FI_q,f="inv"))

  for (reg in REG$Value) {
    print(paste("adding MBL_Q2FD output for region", reg))
    MBL_Q2FDpart <-
      full_join(MBL_LI %>% subset(REG_2 == reg) %>%
                  rename(i = COMM,
                         p = REG,
                         c = COMM_2,
                         s = REG_2,
                         LI = Value),
      MBL_s_Fall_q %>% subset(REG == reg) %>%
                  rename(c = COMM,
                         s = REG,
                         d = REG_2,
                         s_Fall_q = Value)) %>%
      mutate(Value = s_Fall_q * LI) %>%
      select(COMM = i,
             REG = p,
             COMM_2 = c,
             REG_2 = s,
             FDEM = f,
             REG_3 = d,
             Value) %>%

      subset(Value > 0)

    MBL_Q2FD <- bind_rows(MBL_Q2FD, MBL_Q2FDpart)
  }
  # This provides the full matrix of how production of i in region p flows to
  # final demand categories in d. Based on the Leontief inverse it captures all direct
  # and indirect flows through the global economy. Of these flows only the producer
  # location (p) and final commodity (c) source region (s) are made explicit in
  # tracing how commodity i from region p arrives in final consumption of c from s
  # by agent a located in region d. It can serve to derive more manageable subset of
  # flows like  direct and indirect flows of primary products etc.

  # NB summing over all c,s,d and f should equal the production i in p
  # (requirement of the material balance that the demand for each commodity (direct
  # + indirect) has to equal production.

  # As matrix inversion is less numerically accurate than solving system of
  # equations there will be deviations. Given larg differences in size of sectors
  # we express this check in percentage terms


  # # MBL_chk_FDq(i,p) # Difference beteen sum of demand and production (%) #;
  # MBL_chk_FDq <- MBL_Q2FD %>%
  #   rename(i = COMM,
  #          p = REG,
  #          c = COMM_2,
  #          s = REG_2,
  #          f = FDEM,
  #          d = REG_3,
  #          Q2FD = Value) %>%
  #   group_by(i, p) %>%
  #   summarize(Q2FD = sum(Q2FD)) %>%
  #   ungroup() %>%
  #   left_join(. , MBL_Q_q %>%
  #               rename(i = COMM,
  #                      p = REG,
  #                      Q_q = Value)) %>%
  #   mutate(Value = 100 * (Q2FD/Q_q -1)) %>%
  #   select(COMM = i,
  #          REG = p,
  #          Value)


  # Express final demand as shares ----------------------------------------------------

  # To ease the introduction of for example biophysical quantities we express
  # material flows in shares. This also takes care of any slacks due to
  # imprecision in the matrix inversion.

  #MBL_FD_shr(i,p,c,s,f,d)
  #Shares final demand in d for produced i from p via consumption of c from d #;
  MBL_FD_shr <- MBL_Q2FD %>%
    rename(i = COMM,
           p = REG,
           c = COMM_2,
           s = REG_2,
           f = FDEM,
           d = REG_3,
           Q2FD = Value) %>%
    group_by(i, p) %>%
    mutate(Q2FD = ifelse(is.nan(Q2FD), 0, Q2FD)) %>%
    mutate(Value = Q2FD / sum(Q2FD)) %>%
    ungroup() %>%
    select(COMM = i,
           REG = p,
           COMM_2 = c,
           REG_2 = s,
           FDEM = f,
           REG_3 = d,
           Value, Q2FD)

  MBL_s_Fall_q <- rename(MBL_s_Fall_q, FDEM = f)

  return(list(MBL_COMM_SHR, MBL_FD_shr,MBL_s_IO_q, MBL_s_Fall_q,MBL_s_Q_q))

}


make_nutrients_gvc <- function(bdata, NCMF){

  ProductionShares_food <- MBL_ProductionShares_food(GTAPSETS, ACTDAT, GTAPDATA)

  MBL_FD_shr <- ProductionShares_food[[2]]

  NVOM <- bdata$NVOM

  if("PRIM_AGRI" %in% colnames(NCMF)){NCMF <-
    rename(NCMF, COMM = PRIM_AGRI)}

  if("NUTRIENTS" %in% colnames(NVOM)){NVOM <-
    rename(NVOM, NUTRIENTS0 = NUTRIENTS)}

  if("PRIM_AGRI" %in% colnames(NVOM)){NVOM <-
    rename(NVOM, COMM = PRIM_AGRI)}

  NVOM <- NVOM %>%
    rename(NVOMval = Value) %>%
    subset(NUTRIENTS0 != "lanU")


  population <- bdata$POP %>%
    rename(REG_3 = REG, POP = Value)

  gvcdata_nutrients <- MBL_FD_shr %>%
    rename(ProdShare = Value) %>%
    left_join(NVOM) %>%
    left_join(NCMF)  %>%
    mutate(VirtFlow = (NVOMval * ProdShare / NCMFVal)) %>%
    subset(VirtFlow > 0) %>%
    left_join(population) %>%
    mutate(VirtFlowPerCapDay = VirtFlow / POP / 365) %>%
    select(-NVOMval, -ProdShare,-POP,-NCMFVal)

  return(gvcdata_nutrients)
}

make_pefood <- function(gvcdata_nutrients){

  gvcdata_nutrients <- magnetr:::make_nutrients_gvc(gvcdata_food, bdata, rename(ncmp, NCMFVal = Value))

  if("NUTRIENTS" %in% colnames(gvcdata_nutrients)){gvcdata_nutrients <-
    rename(gvcdata_nutrients, NUTRIENTS0 = NUTRIENTS)}

  PEFOOD <- gvcdata_nutrients %>%
    select(PRIM_AGRI = COMM,
           HFOOD = COMM_2,REG,
           REG_2 = REG_3,
           NUTRIENTS0,
           Value = VirtFlowPerCapDay) %>%
    group_by(PRIM_AGRI, HFOOD, REG, REG_2, NUTRIENTS0) %>%
    summarize(Value = sum(Value))

  PEFOOD <- with(PEFOOD, PEFOOD[order(HFOOD,PRIM_AGRI),])

  PEFOODTOT <- gvcdata_nutrients %>%
    select(PRIM_AGRI = COMM,
           HFOOD = COMM_2,
           REG,
           REG_2 = REG_3,
           NUTRIENTS0,
           Value = VirtFlow) %>%
    group_by(PRIM_AGRI, HFOOD, REG, REG_2, NUTRIENTS0) %>%
    summarize(Value = sum(Value))

  PEFOODTOT <- with(PEFOODTOT, PEFOODTOT[order(HFOOD,PRIM_AGRI),])

  pefoodout <- list()
  pefoodout$PEFO <- PEFOOD
  pefoodout$PEFT <- PEFOODTOT
  return(pefoodout)
}




