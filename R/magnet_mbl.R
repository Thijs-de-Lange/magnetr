MBL_fixbdata <- function(bdata) {
  #this is to fix the names to accomodate for the new names by Marijke. To be used just after reading basedata. Does not delete new headers
  if(!("TR_Q" %in% names(bdata))){bdata$TR_Q <- bdata$TRAD}
  if(!("PQ_Q" %in% names(bdata))){bdata$PQ_Q <- bdata$PRDQ}
  if(!("DA_Q" %in% names(bdata))){bdata$DA_Q <- bdata$DINQ}
  if(!("MA_Q" %in% names(bdata))){bdata$MA_Q <- bdata$MINQ}
  if(!("DP_Q" %in% names(bdata))){bdata$DP_Q <- bdata$DFNH}
  if(!("MP_Q" %in% names(bdata))){bdata$MP_Q <- bdata$MFNH}
  if(!("DG_Q" %in% names(bdata))){bdata$DG_Q <- bdata$DFNG}
  if(!("MG_Q" %in% names(bdata))){bdata$MG_Q <- bdata$MFNG}
  if(!("DI_Q" %in% names(bdata))){bdata$DI_Q <- bdata$DFNI}
  if(!("MI_Q" %in% names(bdata))){bdata$MI_Q <- bdata$MFNI}
  if(!("TD_Q" %in% names(bdata))){bdata$TD_Q <- bdata$DTRN}
  if(!("TS_Q" %in% names(bdata))){bdata$TS_Q <- bdata$STRN}

  return(bdata)
}

MBL_ConstructBalances <-  function(GTAPSETS, ACTDAT, GTAPDATA, MANUAL_CSHR = NULL) {

  # Standard GTAP sets ----------------------------------------------------------

  # Set REG # Regions in the model
  REG <- GTAPSETS$REG

  # Set COMM # Traded commodities (including split sectors) #
  COMM <- GTAPSETS$COMM

  # Set MARG # margin commodities #
  MARG <- GTAPSETS$MARG
  # Subset MARG is subset of COMM;

  # Set ACTS # Sectors producing traded commodities #
  ACTS <- GTAPSETS$ACTS
  # Subset ACTS is subset of COMM;


  # Make matrix (for value shares) ---------------------------------------------

  # 'make' matrix valued at basic prices #;
  MAKEB <- GTAPDATA$MAKB

  # Production -----------------------------------------------------------------

  # Quantity produced of c by activity a in r (mil USD) #;
  MBL_PROD_q <- GTAPDATA$PQ_Q

  # Domestic demand ------------------------------------------------------------

  # Intermediates
  # Quantity domestic intermediate demand for c by a in s (mil USD)#;
  MBL_d_INT_q  <- GTAPDATA$DA_Q

  # Final demand, private household
  # Quantity domestic final private household demand for c in r (mil USD)#;
  MBL_d_FINP_q <- GTAPDATA$DP_Q

  # Final demand, government
  # Quantity domestic final government demand for c in r (mil USD)#;
  MBL_d_FING_q <- GTAPDATA$DG_Q

  # Final demand, investments
  # Quantity domestic final investment demand for c in r (mil USD)#;
  MBL_d_FINI_q <- GTAPDATA$DI_Q

  # Trade -----------------------------------------------------------------------

  # Quantity of c exported from r to d (mil USD) #;
  MBL_TRADE_q <- GTAPDATA$TR_Q

  # Import demand ----------------------------------------------------------------

  # Intermediates
  # Quantity imported intermediate demand for c by a in d (mil USD)#;
  MBL_m_INT_q <- GTAPDATA$MA_Q

  # Final demand, private household
  # Quantity imported private household demand for c in d (mil USD)#;
  MBL_m_FINP_q <- GTAPDATA$MP_Q

  # Final demand, government
  # Quantity imported government demand for c in d (mil USD)#;
  MBL_m_FING_q <- GTAPDATA$MG_Q

  # Final demand, investments
  # Quantity imported investment demand for c in d (mil USD)#;
  MBL_m_FINI_q <- GTAPDATA$MI_Q

  # Transport margin supply and demand -------------------------------------------

  # Supply of margin commodities to global transport pool
  # Quantity int'l margin m supplied to global pool by r (mil USD)#;
  MBL_TRANSS_q <- GTAPDATA$TS_Q

  # Demand for margin commodities from global transport pool
  # Quantity int'l margin demanded for imports of c in d from r (mil USD) #;
  MBL_TRANSD_q <- GTAPDATA$TD_Q

  # Allocate activity intermediate demand to commodities--------------------

  #  Activity accounts need to be split in case of by-products. Default split is
  #  value-based (computed from baseedata). Can be replaced by user-defined split
  #  read from file if provided.

  # Value-shares of commodities in intermediate input use - no differentiation by inputs

  # Commodity share in activity a input use  in r #;

  # MBL_VAL_SHR(c,a,r) # Commodity share in activity a input use  in r #;
  MBL_VAL_SHR <- MAKEB %>%
    rename(c = COMM,
           a = ACTS,
           r = REG) %>%
    group_by(a, r) %>%
    mutate(Value = Value / sum(Value)) %>%
    ungroup() %>%
    rename(COMM = c,
           ACTS = a,
           REG = r)


  COMM_2 <- data.frame(MBL_VAL_SHR$COMM) %>%
    rename(COMM_2 = MBL_VAL_SHR.COMM) %>%
    unique()

  # MBL_COMM_SHR(c,i,a,r) # Commodity share c in inputs i used by activity a in r #;
  MBL_COMM_SHR <- merge(MBL_VAL_SHR,
                        COMM_2)


  #Write to check file so can serve as template for custom shares!
  #CHEK$VBCS <- magnetr:::magnet_prepdf_for_write_har(
  #  MBL_COMM_SHR, list(COMM = COMM$Value,
  #                     ACTS = ACTS$Value,
  #                     COMM_2 = COMM$Value,
  #                     REG = REG$Value))

  # Allow user-defined shares to change allocation of inputs #
  MBL_COMM_SHR <- if (is.null(MANUAL_CSHR)) {
    MBL_COMM_SHR
  } else {
    MANUAL_CSHR
  }

  #  Define components of material balance equations ------------------------------------------

  #Define coefficients for material balance equations in quantity terms
  # production (Q) = intermediate demand (I) + final demand (F)

  # Total demand for imports from demand - assures that shares sum to 1 ----------------------
  # MBL_m_TOT_q(i,d)  # Total demand for imports i in destination region d (mil. USD)#;
  MBL_m_TOT_q <- MBL_m_INT_q %>%
    rename(i = COMM,
           a = ACTS,
           d = REG,
           m_INT_q = Value) %>%
    group_by(i, d) %>%
    summarize(sum_m_INT_q = sum(m_INT_q)) %>%
    ungroup() %>%
    left_join(.,
              MBL_m_FINP_q %>%
                rename(i = COMM,
                       d = REG,
                       m_FINP_q = Value)) %>%
    left_join(.,
              MBL_m_FING_q %>%
                rename(i = COMM,
                       d = REG,
                       m_FING_q = Value)) %>%
    left_join(., MBL_m_FINI_q  %>%
                rename(i = COMM,
                       d = REG,
                       m_FINI_q = Value)) %>%
    mutate(Value = sum_m_INT_q + m_FINP_q + m_FING_q + m_FINI_q) %>%
    select(COMM = i,
           REG = d,
           Value)

  # Fill Production -------------------------------------------------------------------------------
  # MBL_Q_q(i,s) # Quantity of production by commodity i and region s (mil USD)#;
  MBL_Q_q <-  MBL_PROD_q %>%
    group_by(COMM, REG) %>%
    summarize(Value = sum(Value)) %>%
    ungroup()

  #  Fill for non-margin demand --------------------------------------------------------------------

  # Fill intermediate demand -----------------------------------------------------------------------

  # Imports
  # MBL_I_q(i,s,c,d) # Intermediate demand for i from s by commodity c in region d (mil USD) #;
  MBL_I_q <- merge(MBL_TRADE_q %>%
                         rename(i = COMM,
                                s = REG,
                                d = REG_2,
                                TRADE_q = Value),
                       left_join(MBL_COMM_SHR %>%
                                   rename(c = COMM,
                                          i = COMM_2,
                                          a = ACTS,
                                          d = REG,
                                          COMM_SHR = Value),
                                 MBL_m_INT_q %>%
                                   rename(i = COMM,
                                          a = ACTS,
                                          d = REG,
                                          m_INT_q = Value)) %>%
                         mutate(V2 = COMM_SHR * m_INT_q) %>%
                         group_by(c, i, d) %>%
                         summarize(V2 = sum(V2)) %>%
                         ungroup()
  ) %>%
    left_join(MBL_m_TOT_q %>%
                rename(i = COMM,
                       d = REG,
                       m_TOT_q = Value)) %>%
    mutate(Value = ifelse(m_TOT_q > 0, TRADE_q * V2 / m_TOT_q, 0)) %>%
    select(i, s, c, d, Value)
  # domestic (allowing for self-trade)
  MBL_I_q <- MBL_I_q %>%
    rename(I_q = Value) %>%
    left_join(.,
              left_join(MBL_COMM_SHR %>%
                          rename(c = COMM,
                                 i = COMM_2,
                                 a = ACTS,
                                 d = REG,
                                 COMM_SHR = Value),
                        MBL_d_INT_q %>%
                          rename(i = COMM,
                                 a = ACTS,
                                 d = REG,
                                 d_INT_q = Value)) %>%
                mutate(V2 = COMM_SHR * d_INT_q) %>%
                group_by(c, i, d) %>%
                summarize(V2 = sum(V2)) %>%
                ungroup()
    ) %>%
    mutate(Value =  ifelse(s == d, I_q + V2, I_q)) %>%
    select(COMM = i,
           REG = s,
           COMM_2 = c,
           REG_2 = d,
           Value)


  # MBL_FP_q(i,s,d) # Final demand for i from s by private household in region d (mil USD) #;
  MBL_FP_q <- MBL_TRADE_q %>%
    rename(i = COMM,
           s = REG,
           d = REG_2,
           TRADE_q = Value) %>%
    left_join(. ,
              MBL_m_FINP_q %>%
                rename(i = COMM,
                       d = REG,
                       m_FINP_q = Value)) %>%
    left_join(.,
              MBL_m_TOT_q %>%
                rename(i = COMM,
                       d = REG,
                       m_TOT_q = Value)) %>%
    mutate(Value = ifelse(m_TOT_q > 0,TRADE_q * m_FINP_q / m_TOT_q, 0)) %>%
    select(i, s, d, Value)
  # domestic (allowing for self-trade)
  MBL_FP_q <- MBL_FP_q %>%
    rename(FP_q = Value) %>%
    left_join(.,
              MBL_d_FINP_q %>%
                rename(i = COMM,
                       d = REG,
                       d_FINP_q = Value)) %>%
    mutate(Value =  FP_q + d_FINP_q) %>%
    mutate(Value =  ifelse(s == d, FP_q + d_FINP_q, FP_q)) %>%
    select(COMM = i,
           REG = s,
           REG_2 = d,
           Value)

  # MBL_FG_q(i,s,d) # Final demand for i from s by government in region d (mil USD) #;
  MBL_FG_q <- MBL_TRADE_q %>%
    rename(i = COMM,
           s = REG,
           d = REG_2,
           TRADE_q = Value) %>%
    left_join(. ,
              MBL_m_FING_q %>%
                rename(i = COMM,
                       d = REG,
                       m_FING_q = Value)) %>%
    left_join(.,
              MBL_m_TOT_q %>%
                rename(i = COMM,
                       d = REG,
                       m_TOT_q = Value)) %>%
    mutate(Value = ifelse(m_TOT_q > 0, TRADE_q * m_FING_q / m_TOT_q, 0)) %>%
    select(i, s, d, Value)
  # domestic (allowing for self-trade)
  MBL_FG_q <- MBL_FG_q %>%
    rename(FG_q = Value) %>%
    left_join(.,
              MBL_d_FING_q %>%
                rename(i = COMM,
                       d = REG,
                       d_FING_q = Value)) %>%
    mutate(Value =  ifelse(s == d, FG_q + d_FING_q, FG_q)) %>%
    select(COMM = i,
           REG = s,
           REG_2 = d,
           Value)


  # MBL_FI_q(i,s,d) # Final demand for i from s by government in region d (mil USD) #;
  MBL_FI_q <- MBL_TRADE_q %>%
    rename(i = COMM,
           s = REG,
           d = REG_2,
           TRADE_q = Value) %>%
    left_join(. ,
              MBL_m_FINI_q %>%
                rename(i = COMM,
                       d = REG,
                       m_FINI_q = Value)) %>%
    left_join(.,
              MBL_m_TOT_q %>%
                rename(i = COMM,
                       d = REG,
                       m_TOT_q = Value)) %>%
    mutate(Value = ifelse(m_TOT_q > 0, TRADE_q * m_FINI_q / m_TOT_q, 0)) %>%
    select(i, s, d, Value)
  # domestic (allowing for self-trade)
  MBL_FI_q <- MBL_FI_q %>%
    rename(FI_q = Value) %>%
    left_join(.,
              MBL_d_FINI_q %>%
                rename(i = COMM,
                       d = REG,
                       d_FINI_q = Value)) %>%
    mutate(Value =  ifelse(s == d, FI_q + d_FINI_q, FI_q)) %>%
    select(COMM = i,
           REG = s,
           REG_2 = d,
           Value)



  #  Allocate international margins & add to demand  ----------------------------------------

  #  Allocate international margins supplied to the global transpot pool by using
  #  the share of supplying rgeions t in total transport and allocating the demand
  #  for international margins linked to imports of c from from s based on the
  #  share of each demand category in total imports of c from s. Result is then
  #  summed over c and set to get the (indiretc) imports of margins m from
  #  transporter region t by the demand categories in d.

  #regional share of transporter t in supply of margin m to global pool#
  MBL_SHR_TRANSS <- MBL_TRANSS_q %>%
    rename(m = MARG,
           t = REG,
           Value_MARG = Value) %>%
    group_by(m) %>%
    mutate(Value = Value_MARG/ sum(Value_MARG )) %>%
    ungroup() %>%
    select(MARG = m,
           REG = t,
           Value)

  #MBL_m_I_TRNS(m,t,c,d) # Int'l transport margins from s for intermediate use by c in d (mil. USD) #;
  MBL_m_I_TRNS <- MBL_COMM_SHR %>%
    rename(c = COMM,
           i = COMM_2,
           a = ACTS,
           d = REG,
           COMM_SHR = Value) %>%
    left_join(.,
              MBL_m_INT_q %>%
                rename(i = COMM,
                       a = ACTS,
                       d = REG,
                       m_INT_q = Value)) %>%
    # demand category share in total imports of commodity
    mutate(Value = COMM_SHR * m_INT_q) %>%
    group_by(c, i, d) %>%
    summarize(Value = sum(Value)) %>%
    ungroup() %>%
    left_join(., MBL_m_TOT_q %>%
                rename(i = COMM,
                       d = REG,
                       m_TOT_q = Value)) %>%
    mutate(Value = ifelse(m_TOT_q > 0, Value / m_TOT_q, 0)) %>%
    # international margins demanded for imports of commodity i from s #
    left_join(.,
              MBL_TRANSD_q %>%
                rename(m = MARG,
                       i = COMM,
                       s = REG,
                       d = REG_2,
                       TRANSD_q = Value)) %>%
    #regional share of transporter t in supply of margin m to global pool#
    left_join(., MBL_SHR_TRANSS %>%
                rename(m = MARG,
                       t = REG,
                       SHR_TRANSS = Value)
    ) %>%
    mutate(Value = SHR_TRANSS * Value * TRANSD_q) %>%
    group_by(m,t,c,d) %>%
    summarize(Value = sum(Value)) %>%
    ungroup() %>%
    rename(MARG = m,
           REG = t,
           COMM = c,
           REG_2 = d)

#MBL_m_FP_TRS(m,t,d) # Int'l transport margins from t for private hh imports in d (mil. USD) #;
  MBL_m_FP_TRS <-  MBL_m_FINP_q %>%
    rename(i = COMM,
           d = REG,
           m_FINP_q = Value) %>%
    left_join(. ,
              MBL_m_TOT_q %>%
                rename(i = COMM,
                       d = REG,
                       m_TOT_q = Value)) %>%
    #demand category share in total imports of commodity i#
    mutate(Value = ifelse(m_TOT_q > 0, m_FINP_q/m_TOT_q,0)) %>%
    # international margins demanded for imports of commodity i from s #
    left_join(.,
              MBL_TRANSD_q %>%
                rename(m = MARG,
                       i = COMM,
                       s = REG,
                       d = REG_2,
                       TRANSD_q = Value)) %>%
    #regional share of transporter t in supply of margin m to global pool#
    left_join(., MBL_SHR_TRANSS %>%
                rename(m = MARG,
                       t = REG,
                       SHR_TRANSS = Value)
    ) %>%
    mutate(Value = SHR_TRANSS * Value *  TRANSD_q) %>%
    group_by(m, t, d) %>%
    summarize(Value = sum(Value)) %>%
    ungroup() %>%
    rename(MARG = m,
           REG = t,
           REG_2 = d)

  #MBL_m_FG_TRS(m,t,d)  # Int'l transport margins from t for government imports in d (mil. USD) #;
  MBL_m_FG_TRS <-  MBL_m_FING_q %>%
    rename(i = COMM,
           d = REG,
           m_FING_q = Value) %>%
    left_join(. ,
              MBL_m_TOT_q %>%
                rename(i = COMM,
                       d = REG,
                       m_TOT_q = Value)) %>%
    #demand category share in total imports of commodity i#
    mutate(Value = ifelse(m_TOT_q > 0, m_FING_q/m_TOT_q,0)) %>%
    # international margins demanded for imports of commodity i from s #
    left_join(.,
              MBL_TRANSD_q %>%
                rename(m = MARG,
                       i = COMM,
                       s = REG,
                       d = REG_2,
                       TRANSD_q = Value)) %>%
    #regional share of transporter t in supply of margin m to global pool#
    left_join(., MBL_SHR_TRANSS %>%
                rename(m = MARG,
                       t = REG,
                       SHR_TRANSS = Value)
    ) %>%
    mutate(Value = SHR_TRANSS * Value *  TRANSD_q) %>%
    group_by(m, t, d) %>%
    summarize(Value = sum(Value)) %>%
    ungroup() %>%
    rename(MARG = m,
           REG = t,
           REG_2 = d)

  #MBL_m_FI_TRS(m,t,d)  # Int'l transport margins from t for investment imports in d (mil. USD) #;
  MBL_m_FI_TRS <-  MBL_m_FINI_q %>%
    rename(i = COMM,
           d = REG,
           m_FINI_q = Value) %>%
    left_join(. ,
              MBL_m_TOT_q %>%
                rename(i = COMM,
                       d = REG,
                       m_TOT_q = Value)) %>%
    #demand category share in total imports of commodity i#
    mutate(Value = ifelse(m_TOT_q > 0, m_FINI_q/m_TOT_q,0)) %>%
    # international margins demanded for imports of commodity i from s #
    left_join(.,
              MBL_TRANSD_q %>%
                rename(m = MARG,
                       i = COMM,
                       s = REG,
                       d = REG_2,
                       TRANSD_q = Value)) %>%
    #regional share of transporter t in supply of margin m to global pool#
    left_join(., MBL_SHR_TRANSS %>%
                rename(m = MARG,
                       t = REG,
                       SHR_TRANSS = Value)
    ) %>%
    mutate(Value = SHR_TRANSS * Value *  TRANSD_q) %>%
    group_by(m, t, d) %>%
    summarize(Value = sum(Value)) %>%
    ungroup() %>%
    rename(MARG = m,
           REG = t,
           REG_2 = d)

  # Add this indirect demand for transport services to the direct demand already
  # computed above

  MBL_I_q <- MBL_I_q %>%
    #filter(COMM == MARG$Value) %>%
    rename(i = COMM,
           s = REG,
           c = COMM_2,
           d = REG_2,
           I_q = Value) %>%
    left_join(. ,
              MBL_m_I_TRNS %>%
                rename(m = MARG,
                       s = REG,
                       c = COMM,
                       d = REG_2,
                       m_I_TRNS = Value)) %>%
    mutate(Value = ifelse(i %in% MARG$Value, I_q + m_I_TRNS, I_q)) %>%
    select(COMM = i,
           REG = s,
           COMM_2 = c,
           REG_2 = d,
           Value)

  MBL_FP_q <- MBL_FP_q %>%
    rename(i = COMM,
           s = REG,
           d = REG_2,
           FP_q = Value) %>%
    left_join(., MBL_m_FP_TRS %>%
                rename(m = MARG,
                       s = REG,
                       d = REG_2,
                       m_FP_TRS = Value)) %>%
    mutate(Value = ifelse(i %in% MARG$Value, FP_q + m_FP_TRS, FP_q)) %>%
    select(COMM = i,
           REG = s,
           REG_2 = d,
           Value)

  MBL_FG_q <- MBL_FG_q %>%
    rename(i = COMM,
           s = REG,
           d = REG_2,
           FG_q = Value) %>%
    left_join(., MBL_m_FG_TRS %>%
                rename(m = MARG,
                       s = REG,
                       d = REG_2,
                       m_FG_TRS = Value)) %>%
    replace(is.na(.), 0) %>%
    mutate(Value = ifelse(i %in% MARG$Value, FG_q + m_FG_TRS, FG_q)) %>%
    select(COMM= i,
           REG = s,
           REG_2 = d,
           Value)

  MBL_FI_q <- MBL_FI_q %>%
    rename(i = COMM,
           s = REG,
           d = REG_2,
           FI_q = Value) %>%
    left_join(., MBL_m_FI_TRS %>%
                rename(m = MARG,
                       s = REG,
                       d = REG_2,
                       m_FI_TRS = Value)) %>%
    replace(is.na(.), 0) %>%
    mutate(Value = ifelse(i %in% MARG$Value, FI_q + m_FI_TRS, FI_q)) %>%
    select(COMM = i,
           REG = s,
           REG_2 = d,
           Value)


  # Check if material balances hold-------------------------------


  #MBL_TOTDEM_q(i,s) # Total demand for commodity i from s (mil USD) #;
  MBL_TOTDEM_q <-  MBL_I_q %>%
    rename(i = COMM,
           s = REG,
           c = COMM_2,
           d = REG_2,
           I_q = Value) %>%
    group_by(i,s,d) %>%
    summarize(I_q = sum(I_q)) %>%
    ungroup() %>%
    left_join(.,
              MBL_FP_q %>%
                rename(i = COMM,
                       s = REG,
                       d = REG_2,
                       FP_q = Value)
    ) %>%
    left_join(.,
              MBL_FG_q %>%
                rename(i = COMM,
                       s = REG,
                       d = REG_2,
                       FG_q = Value)
    ) %>%
    left_join(., MBL_FI_q %>%
                rename(i = COMM,
                       s = REG,
                       d = REG_2,
                       FI_q = Value)
    ) %>%
    mutate(Value = I_q + FP_q + FG_q + FI_q) %>%
    group_by(i, s) %>%
    summarize(Value = sum(Value)) %>%
    ungroup() %>%
    select(COMM = i,
           REG = s,
           Value)

  # Compute slack on material balances in percentages as quantities produced vary
  # widely

  # MBL_SLACK_p(i,s) # Slack or imbalance in material flows for commodity i from s (%) #;
  MBL_SLACK_p <- MBL_TOTDEM_q %>%
    rename(i = COMM,
           s = REG,
           TOTDEM_q = Value) %>%
    left_join(. ,
              MBL_Q_q %>%
                rename(i = COMM,
                       s = REG,
                       Q_q = Value)
    ) %>%
    mutate(Value = ifelse(Q_q > 0, 100 * (TOTDEM_q / Q_q - 1),0)) %>%
    select(COMM = i,
           REG = s,
           Value)


  #  Scale demand categories to quantity produced -----------------------------------------

  # We trust changes in production most as here maximum one CET in case of joint
  #  production. Redistribute slack over demand categories to not bias relative
  #  amounts

  # MBL_s_I_q(i,s,c,d) # Scaled interm. demand for i from s by commodity c in region d (mil USD) #;
  MBL_s_I_q <-  MBL_Q_q %>%
    rename(i = COMM,
           s = REG,
           Q_q = Value) %>%
    left_join(.,
              MBL_TOTDEM_q %>%
                rename(i = COMM,
                       s = REG,
                       TOTDEM_q = Value)
    ) %>%
    left_join(MBL_I_q %>%
                rename(i = COMM,
                       s = REG,
                       c = COMM_2,
                       d = REG_2,
                       I_q = Value)
    ) %>%
    mutate(Value = ifelse(TOTDEM_q > 0, (Q_q/TOTDEM_q) * I_q, 0)) %>%
    select(COMM = i,
           REG = s,
           COMM_2 = c,
           REG_2 = d,
           Value)

  #MBL_s_FP_q(i,s,d) # Scaled final demand for i from s by private hh in region d (mil USD) #;
  MBL_s_FP_q <-  MBL_Q_q %>%
    rename(i = COMM,
           s = REG,
           Q_q = Value) %>%
    left_join(.,
              MBL_TOTDEM_q %>%
                rename(i = COMM,
                       s = REG,
                       TOTDEM_q = Value)
    ) %>%
    left_join(MBL_FP_q %>%
                rename(i = COMM,
                       s = REG,
                       d = REG_2,
                       FP_q = Value)
    ) %>%
    mutate(Value = ifelse(TOTDEM_q > 0, (Q_q/TOTDEM_q) * FP_q,0)) %>%
    select(COMM = i,
           REG = s,
           REG_2 = d,
           Value)

  # MBL_s_FG_q(i,s,d) # Scaled final demand for i from s by government in region d (mil USD) #;
  MBL_s_FG_q <-  MBL_Q_q %>%
    rename(i = COMM,
           s = REG,
           Q_q = Value) %>%
    left_join(.,
              MBL_TOTDEM_q %>%
                rename(i = COMM,
                       s = REG,
                       TOTDEM_q = Value)
    ) %>%
    left_join(MBL_FG_q %>%
                rename(i = COMM,
                       s = REG,
                       d = REG_2,
                       FG_q = Value)
    ) %>%
    mutate(Value = ifelse(TOTDEM_q > 0, (Q_q/TOTDEM_q) * FG_q,0)) %>%
    select(COMM = i,
           REG = s,
           REG_2 = d,
           Value)

  # MBL_s_FI_q(i,s,d) # Scaled final demand for i from s by investment in region d (mil USD) #;
  MBL_s_FI_q <-  MBL_Q_q %>%
    rename(i = COMM,
           s = REG,
           Q_q = Value) %>%
    left_join(.,
              MBL_TOTDEM_q %>%
                rename(i = COMM,
                       s = REG,
                       TOTDEM_q = Value)
    ) %>%
    left_join(MBL_FI_q %>%
                rename(i = COMM,
                       s = REG,
                       d = REG_2,
                       FI_q = Value)
    ) %>%
    mutate(Value = ifelse(TOTDEM_q > 0, (Q_q/TOTDEM_q) * FI_q,0)) %>%
    select(COMM = i,
           REG = s,
           REG_2 = d,
           Value)

  return(list(MBL_COMM_SHR, MBL_Q_q, MBL_s_FP_q, MBL_s_FG_q, MBL_s_FI_q, MBL_s_I_q))
}

MBL_InvertLeontief <- function(GTAPSETS, ACTDAT, GTAPDATA) {

  # Load results from MBL_ConstructBalances -----------------------------------------
  ConstructBalances <- MBL_ConstructBalances(GTAPSETS, ACTDAT, GTAPDATA)

  MBL_COMM_SHR <- ConstructBalances[[1]]
  MBL_Q_q <- ConstructBalances[[2]]
  MBL_s_FP_q <- ConstructBalances[[3]]
  MBL_s_FG_q <- ConstructBalances[[4]]
  MBL_s_FI_q <- ConstructBalances[[5]]
  MBL_s_I_q <- ConstructBalances[[6]]

  # Region Create matrix of technical coefficients

  # MBL_s_IO_q(i,s,c,d) #IO coeff.(qtity): use of i from region s when producing comm. c in region d#;
  MBL_s_IO_q <- MBL_s_I_q %>%
    rename(i = COMM,
           s = REG,
           c = COMM_2,
           d = REG_2,
           s_I_q = Value) %>%
    left_join(.,
              MBL_Q_q %>%
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
    MBL_Q_q %>%
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

  # Commented out since it's very slow
  # MBL_CHK_INV(i,k) # Check on Leontief inversion - should be close to identity matrix#;
  # MBL_CHK_INV <-  MBL_I_IO %>%
  #   rename(i = COMREG,
  #          j = COMREG_2,
  #          I_IO = Value) %>%
  #   left_join(.,
  #             MBL_L_long_comreg %>%
  #               rename(j = COMREG,
  #                      k = COMREG_2,
  #                      L = Value)) %>%
  #   mutate(Value = I_IO * L) %>%
  #   group_by(i, k) %>%
  #   summarize(Value = sum(Value)) %>%
  #   ungroup() %>%
  #   select(COMREG = i,
  #          COMREG_2 = k,
  #          Value)

  # Define assertion to generate error in case of faulty inversion

  # rownames(MBL_IM) <- comreg
  # colnames(MBL_IM) <- comreg


  #MBL_IM_DIF # Difference from identity matrix #;
  # MBL_IM_DIF <- MBL_CHK_INV  %>%
  #   rename(i = COMREG,
  #          k = COMREG_2,
  #          CHK_INV = Value) %>%
  #   left_join(.,
  #             MBL_IM %>%
  #               melt() %>%
  #               rename(i = Var1,
  #                      k = Var2,
  #                      L = value)) %>%
  #   mutate(Value = CHK_INV - L) %>%
  #   summarize(Value = sum(Value))
  #
  # if (MBL_IM_DIF$Value < MBL_IMDIFmax){
  #   print("Good job!") } else { print("Arbitrary boundary - meant as warning to carefully check results")}

  return(list(MBL_COMM_SHR, MBL_Q_q, MBL_s_FP_q, MBL_s_FG_q, MBL_s_FI_q, MBL_L, comregmap, comregmap2))

}

MBL_ProductionShares <- function(GTAPSETS, ACTDAT, GTAPDATA){

  # Load MBL_INvertLeontief
  MBL_InvertLeontief <- MBL_InvertLeontief(GTAPSETS, ACTDAT, GTAPDATA)

  MBL_COMM_SHR <- MBL_InvertLeontief[[1]]
  MBL_Q_q <- MBL_InvertLeontief[[2]]
  MBL_s_FP_q <- MBL_InvertLeontief[[3]]
  MBL_s_FG_q <- MBL_InvertLeontief[[4]]
  MBL_s_FI_q <- MBL_InvertLeontief[[5]]
  MBL_L <-  MBL_InvertLeontief[[6]]
  comregmap <- MBL_InvertLeontief[[7]]
  comregmap2 <- MBL_InvertLeontief[[8]]

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

  # note that + enforces sets to be disjoint - so footprints are eiher defined at
  # activity or at intermediate input level

  # Set CHNL # Channels through which products flow to final demand #
  CHNL <- ACTDAT$MC2C
  # Mapping COMM2CHNL from COMM to CHNL
  COMM2CHNL <- data.frame(ACTDAT$MC2C %>%
                            rename(CHNL = Value),
                          COMM %>%
                            rename(COMM = Value))

  # Set FDCAT # Grouping of final demand categories #
  FDCAT <- ACTDAT$FDCT
  # Mapping FDEM2FDCAT from FDEM to FDCAT
  FDEM2FDCAT <- data.frame(ACTDAT$MD2F %>%
                             rename(FDCAT = Value),
                           FDEM %>%
                             rename(FDEM = Value))


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

  # making an output format, region s is added in the loop below.
  # This is much faster than merging all at once for bigger models
  outputscaffold <- COMM %>% rename(i = Value) %>%
    merge(., REG %>% rename(p = Value)) %>%
    merge(., COMM %>% rename(c = Value)) %>%
    merge(., FDEM %>% rename(f = Value)) %>%
    merge(., REG %>% rename(d = Value))

  for (reg in REG$Value) {
    MBL_Q2FDpart <- outputscaffold %>%
              mutate(s = reg) %>% # looping over s region
      left_join(., MBL_LI %>% subset(REG_2 == reg) %>%
                  rename(i = COMM,
                         p = REG,
                         c = COMM_2,
                         s = REG_2,
                         LI = Value)) %>%
      left_join(., MBL_s_FP_q %>% subset(REG == reg) %>%
                  rename(c = COMM,
                         s = REG,
                         d = REG_2,
                         s_FP_q = Value) %>%
                  mutate(f = "phh")) %>%
      left_join(., MBL_s_FG_q %>% subset(REG == reg) %>%
                  rename(c = COMM,
                         s = REG,
                         d = REG_2,
                         s_FG_q = Value) %>%
                  mutate(f = "gvt")) %>%
      left_join(., MBL_s_FI_q %>% subset(REG == reg) %>%
                  rename(c = COMM,
                         s = REG,
                         d = REG_2,
                         s_FI_q = Value) %>%
                  mutate(f = "inv")) %>%
      mutate(Value = ifelse(f == "phh", s_FP_q * LI,
                            ifelse(f == "gvt", s_FG_q * LI,
                                   ifelse(f == "inv", s_FI_q * LI, 0)))) %>%
      select(COMM = i,
             REG = p,
             COMM_2 = c,
             REG_2 = s,
             FDEM = f,
             REG_3 = d,
             Value)

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

  # commented out because it's very slow to do by default.
  # MBL_chk_FDq(i,p) # Difference beteen sum of demand and production (%) #;
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
           Value)

  return(list(MBL_COMM_SHR, MBL_FD_shr))

}


MBL_Footprints <- function(GTAPSETS, ACTDAT, GTAPDATA){

  # Standard GTAP sets ----------------------------------------------------------

  # Set COMM # Traded commodities (including split sectors) #
  COMM <- GTAPSETS$COMM

  # Additional sets and mappings for footprints -------------------------------

  # Set FDEM # Final demand categories # (phh,gvt,inv);
  Value <- c("phh", "gvt", "inv")
  FDEM <- data.frame(Value)

  # note that + enforces sets to be disjoint - so footprints are either defined at
  # activity or at intermediate input level

  # Set CHNL # Channels through which products flow to final demand #
  CHNL <- ACTDAT$MC2C
  # Mapping COMM2CHNL from COMM to CHNL
  COMM2CHNL <- data.frame(ACTDAT$MC2C %>%
                            rename(CHNL = Value),
                          COMM %>%
                            rename(COMM = Value))

  # Set FDCAT # Grouping of final demand categories #
  FDCAT <- ACTDAT$FDCT
  # Mapping FDEM2FDCAT from FDEM to FDCAT
  FDEM2FDCAT <- data.frame(ACTDAT$MD2F %>%
                             rename(FDCAT = Value),
                           FDEM %>%
                             rename(FDEM = Value))

  # Load MBL_ProductionShares ----------------------------------------------------
  ProductionShares <- MBL_ProductionShares(GTAPSETS, ACTDAT, GTAPDATA)

  MBL_COMM_SHR <- ProductionShares[[1]]
  MBL_FD_shr <- ProductionShares[[2]]

  # Read data on foot print indicators -------------------------------------------

  # Cannot read from file in post-sim after initialization so short intermezzo

  # MBL_A_FPRINT(n,a,p)  # Activity level footprints (various units) #;
  MBL_A_FPRINT <- ACTDAT$A_FP %>%
    data.frame()

  # MBL_I_FPRINT(n,k,a,p) #Intermediate input level footprints (various units) #;
  MBL_I_FPRINT <- ACTDAT$I_FP %>%
    data.frame()

  # Compute footprint indicator ----------------------------------------------------

  # Combine footprint data in single header

  #MBL_FOOTP_RW(n,i,p,g,s,t,d)
  #Footprint n for i produced in p by channel in s and final demand cat t in d#;
  if (dim(MBL_I_FPRINT)[1] == 0) {
    # Assign activity footprints to produced commodities
    MBL_FOOTP_RW <-  MBL_COMM_SHR %>%
      rename(i = COMM,
             k = COMM_2,
             a = ACTS,
             p = REG,
             COMM_SHR = Value) %>%
      left_join(., MBL_A_FPRINT %>%
                  rename(n = FPRNT_A,
                         a = ACTS,
                         p = REG,
                         FPRINT_A_Value = Value)) %>%
      mutate(Value = COMM_SHR * FPRINT_A_Value) %>%
      select(-k) %>%
      unique() %>%
      group_by(n,i,p) %>%
      summarize(Value = sum(Value)) %>%
      ungroup() %>%
      left_join(. ,
                MBL_FD_shr %>%
                  rename(
                    i = COMM,
                    p = REG,
                    c = COMM_2,
                    s = REG_2,
                    f = FDEM,
                    d = REG_3,
                    FD_shr = Value) ) %>%
      mutate(Value = FD_shr * Value) %>%
      left_join(., COMM2CHNL %>%
                  rename(c = COMM,
                         g = CHNL)) %>%
      left_join(., FDEM2FDCAT %>%
                  rename(f = FDEM,
                         t = FDCAT)) %>%
      group_by(n, i, p, g, s, t, d) %>%
      summarize(Value = sum(Value)) %>%
      ungroup() %>%
      select(FPRNT_A = n,
             COMM = i,
             REG = p,
             CHNL = g,
             REG_2 = s,
             FDCAT = t,
             REG_3 = d,
             Value)
  } else {
    # Assign intermediate input footprints to produced commodities
    # Note WJ: I think this doesn't work because MBL_FOOTP_RW doesn't exist yet
    MBL_FOOTP_RW <- MBL_FOOTP_RW %>%
      bind_rows(., MBL_COMM_SHR %>%
              rename(i = COMM,
                     k = COMM_2,
                     a = ACTS,
                     p = REG,
                     COMM_SHR = Value) %>%
              left_join(., MBL_I_FPRINT %>%
                          rename(n = FPRNT_I,
                                 a = ACTS,
                                 p = REG,
                                 FPRINT_I_Value = Value)) %>%
              mutate(Value = COMM_SHR * FPRINT_I_Value) %>%
              group_by(n,i,p) %>%
              select(-k) %>%
              unique() %>%
              summarize(Value = sum(Value)) %>%
              ungroup() %>%
              left_join(. ,
                        MBL_FD_shr %>%
                          rename(
                            i = COMM,
                            p = REG,
                            c = COMM_2,
                            s = REG_2,
                            f = FDEM,
                            d = REG_3,
                            FD_shr = Value) ) %>%
              mutate(Value = FD_shr * Value) %>%
              left_join(., COMM2CHNL %>%
                          rename(c = COMM,
                                 g = CHNL)) %>%
              left_join(., FDEM2FDCAT %>%
                          rename(f = FDEM,
                                 t = FDCAT)) %>%
              group_by(n, i, p, g, s, t, d) %>%
              summarize(Value = sum(Value)) %>%
              ungroup() %>%
              select(FPRNT_I = n,
                     COMM = i,
                     REG = p,
                     CHNL = g,
                     REG_2 = s,
                     FDCAT = t,
                     REG_3 = d,
                     Value))
  }


  #MBL_FOOTP_FD(n,i,p,g,s,t,d) #Footprint n of i produced in p by channel g in s and final demand cat t in d#
  MBL_FOOTP_FD <- MBL_FOOTP_RW %>%
    rename(n = FPRNT_A,
           i = COMM,
           p = REG,
           g = CHNL,
           s = REG_2,
           t = FDCAT,
           d = REG_3) %>%
    # Apply filter to remove commodities in i with no footprint data
    group_by(i) %>%
    mutate(sum_Value = sum(Value)) %>%
    ungroup() %>%
    filter(sum_Value != 0)

  # Define produced commodities with footprint data

  #MBL_FPCOMM # Produced commodities with footprint data #
  MBL_FPCOMM <- MBL_FOOTP_FD$i %>%
    unique()

  # Apply filter to remove commodities in i with no footprint data

  # MBL_FOOTP_FD(n,i,p,g,s,t,d) #Footprint n of i produced in p by channel g in s and final demand cat t in d#
  MBL_FOOTP_FD <- MBL_FOOTP_FD %>%
    select(FPRNT = n,
           COMM = i,
           REG = p,
           CHNL = g,
           REG_2 = s,
           FDCAT = t,
           REG_3 = d,
           Value)

  return(MBL_FOOTP_FD)

}

MBL_MakeACTDAT <- function(GTAPSETS, GTAPDATA) {

  ACTDAT <- list()

  #Initiating footprints data with quantity production, converting to 1000 ton
  A_FP <- GTAPDATA$PROD %>% mutate(FPRNT_A = "Quantity",
                            Value = Value/1000) %>% select(FPRNT_A,ACTS,REG,Value)

  #converting to 1000 km2)
  ldem <- GTAPDATA$LDEM %>% group_by(ACTS,REG) %>% summarize(Value = sum(Value)/1000) %>% ungroup() %>%
          mutate(FPRNT_A = "Land")
  A_FP <- bind_rows(A_FP, ldem)

  if("WTVL" %in% names(GTAPDATA)){
    wtvl <- GTAPDATA$WTVL %>% mutate(FPRNT_A = "Water", Value = Value/1000000)  #converting to Million m3
    A_FP <- bind_rows(A_FP, wtvl)
  }

  ACTDAT$A_FP <- A_FP

  # Set CHNL # Channels through which products flow to final demand #
  # By default setting highest level of detail, some other options are commented out
  ACTDAT$CHNL <- data.frame(Value = c("Food","NonFood"))
  # ACTDAT$FDCT <- data.frame(Value = c("TotFindDem"))
  ACTDAT$FDCT <- data.frame(Value = c("phh", "gvt", "inv"))
  ACTDAT$FDEM <- data.frame(Value = c("phh", "gvt", "inv"))
  # ACTDAT$MD2F <- data.frame(Value = c("TotFindDem", "TotFindDem", "TotFindDem"))
  ACTDAT$MD2F <- data.frame(Value = c("phh", "gvt", "inv"))

  # MC2C <- data.frame(GTAPSETS$COMM) %>% mutate(MC2C = ifelse(Value %in% GTAPSETS$NONF$Value, "NonFood","Food"))
  # This below gives highest level of detail.
  MC2C <- data.frame(GTAPSETS$COMM) %>% mutate(MC2C = Value)
  ACTDAT$MC2C <- data.frame(Value = MC2C$MC2C)

  return(ACTDAT)
}

