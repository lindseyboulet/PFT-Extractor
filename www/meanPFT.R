meanPFT <- function(pftData){
  dl <- lapply(pftData, `[[`, 5)
  dl <- lapply(dl, '[', 3:5)
  means <- round(aaply(laply(dl, as.matrix), c(2, 3), mean, na.rm = TRUE),1)
  sem <- round(aaply(laply(dl, as.matrix), c(2, 3), std.error, na.rm = TRUE),1)
  meanPFTData<- data.frame(matrix(paste(means, " \u00B1 ", sem), ncol = 3))
  meanPFTData <- cbind(pftData[[1]][[5]][,1:2], meanPFTData)
  colnames(meanPFTData) <- c(" ", " ", "Reference", "Measured", "Percent_Predicted")
  meanPFTData
}
