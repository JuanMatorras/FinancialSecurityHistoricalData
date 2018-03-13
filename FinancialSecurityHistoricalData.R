##########################################################################################################
###################################### A S3 Class used to download hist price ############################
##########################################################################################################
###################################### S3 Class reference: ###############################################
###################################### http://www.cyclismo.org/tutorial/R/s3Classes.html #################
##########################################################################################################

#
# install.packages("devtools")
# devtools::install_github("joshuaulrich/quantmod", ref="157_yahoo_502")
#
#
# Load libraries
#
libraries <- c("quantmod","PerformanceAnalytics")
lapply(libraries, function(lib){ library(lib, character.only=TRUE) })

#
# Define default values 
#
def_channel <- "quantmod"       # Alternative: expressfeed
def_source <- "yahoo"
def_div_adj_switch <- TRUE
def_splt_adj_switch <- TRUE
def_freq <- "d"    # daily
def_watchlist_colnames <- c("LocalTicker","Currency","SecurityType","Comments")
def_hist_prc_colnames <- c("LocalTicker","Currency","SecurityType","Frequency",
                           "SpltAdj","DivAdj","AdjMethod",
                           "HistBegDate","HistEndDate","RunResult")
def_manual_adj_switch <- FALSE

#
# Define FinancialInstrumentHistoricalData
#
FinancialSecurityHistoricalData <- function(id = 1,
                                            hist_startdate = NULL,
                                            hist_enddate = NULL,
                                            watchlist = NULL){
  InitWatchlist <- function(){
    return(data.frame(LocalTicker = character(0),
                      Currency = character(0),
                      SecurityType = character(0),
                      Comments = character(0),
                      stringsAsFactors = FALSE))
  }
  
  InitResultSummary <- function(){
    return(data.frame(LocalTicker = character(0),
                      Currency = character(0),
                      SecurityType = character(0),
                      Frequency = character(0),
                      SpltAdj = character(0),
                      DivAdj = character(0),
                      AdjMethod = character(0),
                      RunResult = character(0),
                      stringsAsFactors = FALSE))
  }
  
  res <- list(FSHD_instance_id = id,
              FSHD_channel = def_channel,
              FSHD_source = def_source,
              FSHD_div_adj_switch = def_div_adj_switch,
              FSHD_splt_adj_switch = def_splt_adj_switch,
              FSHD_freq = def_freq,
              FSHD_watchlist = InitWatchlist(),
              FSHD_hist_startdate = hist_startdate,
              FSHD_hist_enddate = hist_enddate,
              FSHD_hist_prc = NULL,
              FSHD_hist_ret = NULL,
              FSHD_hist_cumret = NULL,
              FSHD_result_summary = InitResultSummary(),
              FSHD_manual_adj_switch = def_manual_adj_switch)
  class(res) <- append(class(res), "FinancialSecurityHistoricalData")
  return(res)
}

#
# Set attributes
#
FSHDSetAttributes <- function(fshd,st_date,end_date,channel,div_adj_switch,splt_adj_switch,freq,manual_adj_switch) UseMethod("FSHDSetAttributes")
FSHDSetAttributes.default <- function(fshd,st_date,end_date,channel,div_adj_switch,splt_adj_switch,freq,manual_adj_switch){ return(fshd) }
FSHDSetAttributes.FinancialSecurityHistoricalData <- function(fshd,   # Required field
                                                              st_date,    # Required field
                                                              end_date,   # Required field
                                                              channel = def_channel,
                                                              src = def_source,
                                                              div_adj_switch = def_div_adj_switch,
                                                              splt_adj_switch = def_splt_adj_switch,
                                                              freq = def_freq,
                                                              manual_adj_switch = def_manual_adj_switch){
  
  ifelse(class(st_date) == "Date", fshd$FSHD_hist_startdate <- st_date, print("Error setting start date!"))
  ifelse(class(end_date) == "Date", fshd$FSHD_hist_enddate <- end_date, print("Error setting end date!"))
  ifelse(class(channel) == "character", fshd$FSHD_channel <- channel, print("Error setting channel!"))
  ifelse(class(div_adj_switch) == "logical", fshd$FSHD_div_adj_switch <- div_adj_switch, print("Error setting channel!"))
  ifelse(class(splt_adj_switch) == "logical", fshd$FSHD_splt_adj_switch <- splt_adj_switch, print("Error setting channel!"))
  ifelse(class(freq) == "character", fshd$FSHD_freq <- freq, print("Error setting frequency!"))
  
  if(src == "yahoo"){
    fshd$FSHD_source <- src
  } else if (src == "google") {
    fshd$FSHD_source <- src
    fshd$FSHD_div_adj_switch <- FALSE
    fshd$FSHD_splt_adj_switch <- TRUE
  } else {
    print("Error setting source. Only google and yahoo is allowed!")
  }
  
  if(class(manual_adj_switch) == "logical"){
    fshd$FSHD_manual_adj_switch <- manual_adj_switch
    if(manual_adj_switch == TRUE){
      fshd$FSHD_div_adj_switch <- TRUE
      fshd$FSHD_splt_adj_switch <- TRUE
    }
  } else {
    print("Error setting manual diviend switch!")
  }
  # ifelse(class(reserved) == "character", fshd$FSHD_reserved <- reserved, print("Error setting reserved!"))
  
  return(fshd)
}

#
# Set watchlist
#
FSHDSetWatchlist <- function(fshd, ws) UseMethod("FSHDSetWatchlist")
FSHDSetWatchlist.default <- function(fshd, ws){ return(fshd) }
FSHDSetWatchlist.FinancialSecurityHistoricalData <- function(fshd, ws){
  ifelse(identical(colnames(ws), def_watchlist_colnames),
         fshd$FSHD_watchlist <- ws,
         print("Error loading watchlist. Make sure the fields are correct!"))
  return(fshd)
}

#
# Load historical price data for one security
#
FSHDObtainHistPrc <- function(fshd, single_security) UseMethod("FSHDObtainHistPrc")
FSHDObtainHistPrc.default <- function(fshd, single_security){ return(fshd) }
FSHDObtainHistPrc.FinancialSecurityHistoricalData <- function(fshd, single_security){

  if(identical(colnames(single_security), def_watchlist_colnames)){
    # Field: c("LocalTicker","Currency","SecurityType","Comments")
    tik <- single_security$LocalTicker
    c <- single_security$Currency
    yahoo.tik <- ifelse(c=="CAD",paste(tik,".TO",sep=""),tik)
    st <- single_security$SecurityType
    bd <- fshd$FSHD_hist_startdate
    ed <- fshd$FSHD_hist_enddate
    src <- fshd$FSHD_source
    
    prc <- data.frame(price = numeric(0), stringsAsFactors = FALSE)
    if(tolower(fshd$FSHD_channel) == "quantmod"){
      if(tolower(st) == "stk"){
        if(tolower(src) == "google"){
          prc <- DownloadPrice(tik, "google", "Close", bd-10, ed+3)
        } else{
          if(fshd$FSHD_manual_adj_switch == TRUE){
            #
            # If manual adjustment switch is on, everything is adjusted
            #
            prc <- DownloadPrice_AdjustForSpltAndDiv(yahoo.tik, bd-10, ed+3)
          } else {
            #
            # If manual adjustment switch is off, then user can use different combination
            #
            if(fshd$FSHD_splt_adj_switch == TRUE){
              if(fshd$FSHD_div_adj_switch == TRUE){
                prc <- DownloadPrice(yahoo.tik,"yahoo", "Adjusted", bd-10, ed+3)
              } else {
                prc <- DownloadPrice(yahoo.tik,"yahoo", "Close", bd-10, ed+3)
              }
            } else {
              prc <- DownloadPrice(yahoo.tik,"yahoo", "Close", bd-10, ed+3)
              print("Warning currently only split adjusted stock price is available. Therefore, split adjusted stock price will be used.")
            }
          }
        }
        
        return(prc)
      } else {
        print("Error currently only equity can be handled!")
      }
    } else {
      print("Error currently only quantmod method can be handled!")
    }
    
  } else {
    print("Error security dataframe is in unacceptable format!")
  }
  return(fshd)
}

#
# Load all historical price data
#
FSHDObtainAllHistPrcs <- function(fshd) UseMethod("FSHDObtainAllHistPrcs")
FSHDObtainAllHistPrcs.default <- function(fshd){ return(fshd) }
FSHDObtainAllHistPrcs.FinancialSecurityHistoricalData <- function(fshd){
  if(nrow(fshd$FSHD_watchlist) != 0){
    result_summary <- fshd$FSHD_watchlist[,c("LocalTicker","Currency","SecurityType")]
    
    # "Frequency","SpltAdj","DivAdj","RunResult"
    result_summary$Frequency <- rep(fshd$FSHD_freq, nrow(result_summary))
    result_summary$SpltAdj <- rep(fshd$FSHD_splt_adj_switch, nrow(result_summary))
    result_summary$DivAdj <- rep(fshd$FSHD_div_adj_switch, nrow(result_summary))
    result_summary$AdjMethod <- rep(ifelse(fshd$FSHD_manual_adj_switch, "Manual", "Automatic"), nrow(result_summary))
    result_summary$HistBegDate <- rep(as.Date("1900-01-01"), nrow(result_summary))
    result_summary$HistEndDate <- rep(as.Date("1900-01-01"), nrow(result_summary))
    result_summary$RunResult <- rep("NA", nrow(result_summary))
    
    prcs <- NULL
    for(i in 1:nrow(fshd$FSHD_watchlist)){
      prc <- FSHDObtainHistPrc(fshd, fshd$FSHD_watchlist[i,])
      if(nrow(prc) != 0 & class(prc)[1] == "xts"){
        #if(i == 1){
        #  prcs <- prc
        #} else {
          prcs <- merge.xts(prcs, prc, all = TRUE)
        #}
        result_summary[i,"RunResult"] <- "Success"
        result_summary[i,"HistBegDate"] <- max(min(index(prc)), fshd$FSHD_hist_startdate)
        result_summary[i,"HistEndDate"] <- min(max(index(prc)), fshd$FSHD_hist_enddate)
      } else {
        prcs
        result_summary[i,"RunResult"] <- "Failed"
        print(paste("Error price data is not successfully downloaded for ticker ",
                    result_summary[i, "LocalTicker"], " (", result_summary[i, "Currency"], ")",sep=""))
      }
    }
    
    #
    # Calculate returns
    #
    prcs <- na.locf(prcs, fromLast = TRUE) 
    log.rets <- Return.calculate(prcs, method="log")[-1,]
    cumrets <- exp(cumsum(log.rets)) - 1
    prcs <- prcs[index(cumrets)]
    
    #
    # Update results
    #
    start_date <- max(min(index(log.rets)), fshd$FSHD_hist_startdate)
    end_date <- min(max(index(log.rets)), fshd$FSHD_hist_enddate)
    date_duration <- paste(start_date, end_date, sep="::")
    
    fshd$FSHD_hist_prc <- prcs[date_duration]
    fshd$FSHD_hist_ret <- log.rets[date_duration]
    fshd$FSHD_hist_cumret <- cumrets[date_duration]
    
    fshd$FSHD_hist_startdate <- min(index(fshd$FSHD_hist_prc))
    fshd$FSHD_hist_enddate <- max(index(fshd$FSHD_hist_prc))
    fshd$FSHD_result_summary <- result_summary
  } else {
    print("Warning watchlist is empty!")
  }
  return(fshd)
}

#
# Local function DownloadPrice
#
DownloadPrice <- function(tik, srcc, fld, rev_sd, ed){
  # Default rec to fallback
  prc <- data.frame(tik = numeric(0), stringsAsFactors = FALSE)
  
  tryCatch({
    # Download and get data from quantmod
    raw_rec <- getSymbols(tik, src = srcc, from = rev_sd, to = ed, auto.assign = FALSE)
    prc <- raw_rec[,paste(tik, ".", fld , sep="")]
    colnames(prc) <- tik
  }, warning = function(w) {
    # nothing
  }, error = function(e) {
    print(paste("Error retrieve price for ticker - ", tik, sep=""))
  }, finally = {
    return(prc)
  })
}

#
# Local function DownloadPrice_AdjustForSpltAndDiv
#
DownloadPrice_AdjustForSpltAndDiv <- function(tik, rev_sd, ed){
  
  # Default rec to fallback
  prc <- data.frame(tik = numeric(0), stringsAsFactors = FALSE)
  
  tryCatch({
    # Download and get data from quantmod
    uaj_prc <- getSymbols(tik, src = "yahoo", from = rev_sd, to = ed, auto.assign = FALSE)
    if(class(uaj_prc)[1] == "xts"){
      splt_exdiv_prc <- uaj_prc[,paste(tik, ".Close" , sep="")]
      colnames(splt_exdiv_prc) <- "splt_exdiv_prc"
    } else{
      splt_exdiv_prc <- data.frame(splt_exdiv_prc=numeric(0), stringsAsFactors = FALSE)
    }
    
    splt <- getSplits(tik, src = "yahoo", from = rev_sd, to = ed, auto.assign = FALSE)
    ifelse(class(splt)[1] == "xts", 
           colnames(splt) <- "splt", 
           splt <- data.frame(splt=numeric(0), stringsAsFactors = FALSE))
    
    usplt_div <- getDividends(tik, src = "yahoo", from = rev_sd, to = ed, 
                              split.adjust = FALSE, auto.assign = FALSE)
    ifelse(class(usplt_div)[1] == "xts" & nrow(usplt_div) > 0, 
           colnames(usplt_div) <- "usplt_div", 
           usplt_div <- data.frame(usplt_div=numeric(0), stringsAsFactors = FALSE))
    
    # Adjust raw price with split and dividend
    prc <- AdjustPriceWithSplitAndDividend(splt_exdiv_prc, splt, usplt_div)
    colnames(prc) <- tik
  }, warning = function(w) {
    # nothing
  }, error = function(e) {
    print(paste("Error retrieving price for ticker - ", tik, sep=""))
  }, finally = {
    return(prc)
  })
  
}

#
# Adjust price with split and dividend
#
AdjustPriceWithSplitAndDividend <- function(splt_exdiv_prc, splt, usplt_div){
  
  #
  # Adjust dividend price
  #
  AdjustPriceWithDiv <- function(price){
    
    tryCatch({
      prc.with.div <- price[,c("splt_div","splt_exdiv_prc")]
      div.fac <- (prc.with.div$splt_exdiv_prc+prc.with.div$splt_div)/prc.with.div$splt_exdiv_prc
      adj.pnts <- which(prc.with.div$splt_div!=0)
      
      # Container for final adjusted close price
      prc.with.div.vec <- as.vector(prc.with.div$splt_exdiv_prc)
      
      #
      # Retrospectly adjust close price
      #
      for(j in adj.pnts){
        prc.with.div.vec[1:(j-1)] <- prc.with.div.vec[1:(j-1)]/div.fac[j]
      }
      prc.with.div$splt_div_prc <- prc.with.div
    }, warning = function(w) {
      # nothing
    }, error = function(e) {
      prc.with.div <- data.frame(splt_div_prc = numeric(0), stringsAsFactors = FALSE)
      print(paste("Error adjusting dividend.", sep=""))
    }, finally = {
    })
    
    return(prc.with.div)
  }
  
  if(nrow(splt_exdiv_prc) != 0){
    res_prelim <- splt_exdiv_prc
    
    #
    # Test for split
    #
    if(nrow(splt) != 0){
      #
      # Step 1. Obtain split factor
      #
      splt$cumSpltFac <- sapply(c(1:nrow(splt)), function(x) { prod(splt[x:nrow(splt),"splt"]) })
      
      #
      # Step 2. Merge cumulative factors with price
      #
      res_prelim <- merge.xts(res_prelim, splt$cumSpltFac, all = TRUE)
      res_prelim <- res_prelim[!is.na(res_prelim$splt_exdiv_prc),]   # Remove period with no price
      res_prelim$cumSpltFac <- na.locf(res_prelim$cumSpltFac, fromLast = TRUE)   # Fill na with latest factors
      res_prelim[is.na(res_prelim$cumSpltFac),"cumSpltFac"] <- 1   # Fill most recent nas with 1
    } else {
      #
      # No split -> cumulative split factor is 1
      #
      res_prelim$cumSpltFac <- rep(1, nrow(res_prelim))
    }
    
    #
    # Test for dividend
    #
    if(nrow(usplt_div) != 0){
      #
      # Scenario 1: with div
      #
      res_prelim2 <- merge.xts(res_prelim, usplt_div$usplt_div, all = TRUE)
      res_prelim2 <- res_prelim2[!is.na(res_prelim2$splt_exdiv_prc),]   # Remove period with no price
      res_prelim2[is.na(res_prelim2$usplt_div),"usplt_div"] <- 0   # Fill all nas with 0
      res_prelim2$splt_div <- res_prelim2$usplt_div * res_prelim2$cumSpltFac
      
      apwd <- AdjustPriceWithDiv(res_prelim2)
      res_final <- apwd$splt_div_prc
    } else {
      #
      # Scenario 2: no div
      #
      res_prelim2 <- res_prelim
      res_prelim2$splt_div_prc <- res_prelim2$splt_exdiv_prc
      
      res_final <- res_prelim2$splt_div_prc
    }
    return(res_final)
  } else {
    return(splt_exdiv_prc)
    print("Error no price data is given!")
  }
}

#
# ADDNOTE
#
FSHDXXXX <- function(fshd) UseMethod("FSHDXXXX")
FSHDXXXX.default <- function(fshd){ return(fshd) }
FSHDXXXX.FinancialSecurityHistoricalData <- function(fshd){
  
}