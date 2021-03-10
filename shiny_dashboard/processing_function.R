processing_function <- function(data){
  #list of all unique departures
  data_0 <- data[which(data$BEOBACHTUNGSZEITPUNKT==0),] 
  n <- nrow(data_0)
  #make a data frame in correct format
  dat <- data.frame(matrix(NA, ncol=25, nrow=n))
  names <- c("Dep. Date", "Dep. Hour", "Dep. Day", "Train No.", "Leg No.", "Line Type", "450", "91", "63", "42", "28", "21", "17", "14", "12", "10", "8", "7", "6", "5", "4", "3", "2", "1", "0")
  colnames(dat) <- names
  dat$`Dep. Date` <- data_0$z_datum[order(as.Date(data_0$z_datum, tryFormats = c("%Y-%m-%d")), data_0$STUNDE)]
  dat$`Dep. Hour` <- data_0$STUNDE[order(as.Date(data_0$z_datum, tryFormats = c("%Y-%m-%d")), data_0$STUNDE)]
  dat$`Dep. Day` <- data_0$day_of_week[order(as.Date(data_0$z_datum, tryFormats = c("%Y-%m-%d")), data_0$STUNDE)]
  dat$`Train No.` <- data_0$z_nr[order(as.Date(data_0$z_datum, tryFormats = c("%Y-%m-%d")), data_0$STUNDE)]
  dat$`Leg No.` <- data_0$leg_nr[order(as.Date(data_0$z_datum, tryFormats = c("%Y-%m-%d")), data_0$STUNDE)]
  dat$`Line Type`<- data_0$ZUGGATTUNG[order(as.Date(data_0$z_datum, tryFormats = c("%Y-%m-%d")), data_0$STUNDE)]
  #add in departure numbers
  data_sorted <- data[order(-data$BEOBACHTUNGSZEITPUNKT, as.Date(data$z_datum, tryFormats = c("%Y-%m-%d")), data$STUNDE),]
  for (i in 1:n){
    dat[i,7:25] <- pmax(0, data_sorted$wbs_cum[seq(i, nrow(data), n)])
  }
  return(dat)
}

