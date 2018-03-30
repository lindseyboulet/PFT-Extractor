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
    
    rows <- data.frame(substr(doc[12:43], 1,18))
    rows <- as.character(rows[c(3:5, 16:17, 23, 26:29),])
    
    ref <- data.frame(substr(doc[12:43], 35,45))
    ref <- as.character(ref[c(3:5, 16:17, 23, 26:29),])
    
    rg <- data.frame(substr(doc[12:43], 46,60))
    rg <- as.character(rg[c(3:5, 16:17, 23, 26:29),])
    
    pre <- data.frame(substr(doc[12:43], 61,72))
    pre <- as.character(pre[c(3:5, 16:17, 23, 26:29),])
    
    pref <- data.frame(substr(doc[12:43], 73,87))
    pref <- as.character(pref[c(3:5, 16:17, 23, 26:29),])
    pref <- gsub(" ", "", pref)
    
    pftSing <- as.data.frame(cbind(rows, pre, pref))
    pftSing <- japply(pftSing, which(sapply(pftSing, class)=="factor"), as.character )
    
    # pftSing$`%RefPost`[which(nchar(pftSing$`%RefPost`)>3)] <-
    #  substr(pftSing$`%RefPost`[which(nchar(pftSing$`%RefPost`)>3)],
    #   3, nchar(pftSing$`%RefPost`[which(nchar(pftSing$`%RefPost`)>3)]))
    pftSing2 <- as.data.frame(rbind(cbind(pftSing[,1], as.character(as.numeric(pftSing[,2]))),
                                    cbind(paste(pftSing[,1], "_ref", sep = ""), as.character(as.numeric(pftSing[,3])))))
    pftSing2 <- as.data.frame(t(pftSing2))
    pftSing2 <- japply(pftSing2, which(sapply(pftSing2, class)=="factor"), as.character )
    colnames(pftSing2) <- as.character(pftSing2[1,])
    pftSing2 <- pftSing2[-1,]
    pftSing2 <- japply(pftSing2, which(sapply(pftSing2, class)=="character"), as.numeric )
    pftSing2 <- as.data.frame(cbind("date" = substr(doc[1], 93,102), "id" = substr(fileId[i], 6,7),
                                    pftSing2))
    pftSing2 <- pftSing2[,-c(8,18)]
    pftList[[i]] <- pftSing2
  }
  pftData <- ldply( pftList, rbind)  
  pftData <-  data.table(pftData)
  pftData$id <- paste("SLHS",pftData$id, sep = "")
  pftData[,1:20]
}