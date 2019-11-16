##functions for processing the PP
preparePP <- function(RR, annotations = c()){
  RRi <- RR[1:(length(RR)-1)]; RRii <- RR[2:length(RR)];
  annotationsi <- annotations[1:(length(annotations)-1)]; annotationsii <- annotations[2:length(annotations)];
  removei <- which(annotationsi!=0)
  removeii <- which(annotationsii!=0)
  if (length(c(removei, removeii))!=0){
    RRi <- RRi[-c(removei,removeii)]
    RRii <- RRii[-c(removei,removeii)]
  }
  return(cbind(RRi,RRii))
}

drawPP <- function(PP, ...){
  additional = list(...)
  xlab = parse(text = paste(additional$variableName, "[i]"))
  ylab = parse(text = paste(additional$variableName, "[i+1]"))
  listOfParameters <- list(PP[,2]~PP[,1], xlab = xlab, ylab = ylab, pch = 21, col = "black", bg = additional$color)
  do.call(what = "plot", listOfParameters, quote = TRUE)
  abline(0,1, lty = 2, lwd = 2)
}

HRAdescriptors <- function(PP){
  RRi <- PP[,"RRi"]; RRii <- PP[,"RRii"]
  correct <- (length(RRi)-1)/length(RRi)
  SD1 <- sqrt(var((RRi-RRii)/sqrt(2))*correct)

  correct <- (length(RRi)-1)/length(RRi)
  SD2 <- sqrt(var((RRi+RRii)/sqrt(2))*correct)

  correct <- (length(RRi)-1)/length(RRi)
  SDNN <- sqrt(1/2*(SD2^2+SD1^2))

  n <- length(RRi)
  SD1I <- sqrt((1/n)*sum((RRi-RRii)^2)/2)
  
  xy <- (RRii-RRi)/sqrt(2)
  n <- length(RRi)
  decelerations <- which(xy>0)
  accelerations <- which(xy<0)
  SD1d <- sqrt(sum(xy[decelerations]^2)/n)
  SD1a <- sqrt(sum(xy[accelerations]^2)/n)
  C1d <- (SD1d/SD1)^2

  xy <- (RRii-RRi)/sqrt(2)
  nochange <- which(xy==0);
  n <- length(RRi)
  decelerations <- which(xy>0)
  accelerations <- which(xy<0)
  XY <- (RRi-mean(RRi)+RRii-mean(RRii))/sqrt(2);
  SD2a <- sqrt(1/n*(sum(XY[accelerations]^2)+1/2*sum(XY[nochange]^2)))
  SD2d <- sqrt(1/n*(sum(XY[decelerations]^2)+1/2*sum(XY[nochange]^2)))
  C2d <- (SD2d/SD2)^2

  SDNNa <- sqrt(1/2*(SD1a^2+SD2a^2))
  SDNNd <- sqrt(1/2*(SD1d^2+SD2d^2))
  Cd=(SDNNd/SDNN)^2
  return(c(SD1, SD2, SDNN, SD1d, SD1a, C1d, SD2d, SD2a, C2d, SDNNd, SDNNa, Cd))
}

