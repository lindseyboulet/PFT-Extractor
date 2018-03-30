meanPFT <- function(pftData){
  mean <- summarise_all(pftData,  funs(round(mean(., na.rm = TRUE), 1)))
  sem <- summarise_all(pftData,  funs(round(std.error(., na.rm = TRUE), 1)))
  meanPFTData <-as.data.frame(t(data.frame(paste(mean, " ± ", sem))))
  colnames(meanPFTData) <- colnames(mean)
  rownames(meanPFTData) <- ""
  meanPFTData[1,1:2] <- ""
  meanPFTData <- meanPFTData[,-c(1:2, 8, 18)]
  meanPFTData
}