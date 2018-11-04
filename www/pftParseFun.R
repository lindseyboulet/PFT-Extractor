pftParse <- function(fileDir){
  files <- list.files(path = fileDir,
                      pattern = "pdf$", full.names = TRUE)
  fileId <- list.files(path = fileDir,
                       pattern = "pdf$")
  read <- readPDF(engine = "xpdf", control = list(text = "-layout"))
  document <- Corpus(URISource(files), readerControl = list(reader = read))
  pftList <- list()
  for(i in 1:length(document)){
    doc <- content(document[[i]])
    report <- list()
    report$date <- strapplyc(doc[grep("Date", doc)], "\\d\\d[[:punct:]]\\d\\d[[:punct:]]\\d\\d", simplify = TRUE)
    report$id <- substr(doc[grep("Id", doc)],gregexpr("Id", doc[grep("Id", doc)])[[1]]+4, nchar(doc[grep("Id", doc)]))
    org <- unlist(strsplit(doc[grep("Name", doc)], " "))
    name <- org[c(grep("name", org, ignore.case = TRUE)[1]+1, grep("name", org, ignore.case = TRUE)[1]+2)]
    report$name <- paste(name[1], name[2])
    report$age <- substr(doc[grep("Age", doc)],gregexpr("Age",
                            doc[grep("Age", doc)])[[1]]+5, gregexpr("Age", doc[grep("Age", doc)])[[1]]+6)
    report$baro <- substr(doc[grep("PBar", doc)],gregexpr("PBar",
                                               doc[grep("PBar", doc)])[[1]]+6, gregexpr("PBar", doc[grep("PBar", doc)])[[1]]+8)
    doc2 <- doc[grep("Spirometry", doc):(grep("PEF", doc)[1])] # extract spirometry data
    scData <- scan(text = doc2, what = "")
    report$data <- data.frame(matrix(
    c(scData[which(scData == "FVC"):(which(scData == "FVC")+4)],
    scData[which(scData == "FEV1"):(which(scData == "FEV1")+4)],
    c(scData[which(scData == "FEV1/FVC"):(which(scData == "FEV1/FVC")+3)], NA),
    scData[which(scData == "FEF25-75%"):(which(scData == "FEF25-75%")+4)],
    scData[which(scData == "PEF"):(which(scData == "PEF")+4)]),
    ncol = 5, byrow = TRUE, dimnames = list(NULL, c("", "", "Ref", "Meas", "percent" ))))
    
    doc2 <- doc[grep("Lung Volumes", doc)[1]:(grep("Diffusion", doc)-1)] # extract spirometry data
    scData <- scan(text = doc2, what = "")
    switch <- 1
   if(length(which(scData == "TLC"))>1){ switch <- 2}
    report$data <- rbind(report$data, data.frame(matrix(
      c(scData[which(scData == "TLC")[switch]:(which(scData == "TLC")[switch]+4)],
        scData[which(scData == "VC"):(which(scData == "VC")+4)],
        scData[which(scData == "RV")[1]:(which(scData == "RV")[1]+4)]),
      ncol = 5, byrow = TRUE, dimnames = list(NULL, c("", "", "Ref", "Meas", "percent" )))))
    
    doc2 <- doc[grep("Diffusion", doc):length(doc)] # extract spirometry data
    scData <- scan(text = doc2, what = "")
    report$data <- rbind(report$data, data.frame(matrix(
      c(scData[which(scData == "DLCO")[1]:(which(scData == "DLCO")[1]+4)],
        scData[which(scData == "DL"):(which(scData == "DL")+5)][-2],
        scData[which(scData == "VA"):(which(scData == "VA")+4)],
        scData[which(scData == "DLCO/VA"):(which(scData == "DLCO/VA")+4)]),
      ncol = 5, byrow = TRUE, dimnames = list(NULL, c("", "", "Ref", "Meas", "percent" )))))
    
    report$data[,1] <- as.character(report$data[,1])
    report$data[,2] <- as.character(report$data[,2])
    report$data[,3] <- as.numeric(as.character(report$data[,3]))
    report$data[,4] <- as.numeric(as.character(report$data[,4]))
    report$data[,5] <- as.numeric(as.character(report$data[,5]))
    report$data[12,1] <- "VA_adj"
    colnames(report$data) <- c(" ", " ", "Reference", "Measured", "Percent_Predicted")
    pftList[[fileId[i]]] <- report
  }
pftList
}

addHead <- function(listItem){
  d <- listItem
  df <- d[[6]] ; df2 <- df[1:6,]
  df2[1,] <- c("Id:", d[[2]], rep(" ", 3))
  df2[2,] <- c("Name:", d[[3]], rep(" ", 3))
  df2[3,] <- c("Age:", d[[4]], rep(" ", 3))
  df2[4,] <- c("Date:", d[[1]], rep(" ", 3))
  df2[5,] <- rep(" ", 5)
  df2[6,] <- c(" ", " ", colnames(df)[3:5])
  df <- rbind(df2, df)
  colnames(df) <- rep(" ",5)
  df
}
