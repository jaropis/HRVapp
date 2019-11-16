### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.

### You should have received a copy of the GNU General Public License
### along with PP.  If not, see <http://www.gnu.org/licenses/>.
### PP is free software: you can redistribute it and/or modify
### it under the terms of the GNU General Public License as published by
### the Free Software Foundation, either version 3 of the License, or
### (at your option) any later version.

### PP is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.

### You should have received a copy of the GNU General Public License
### along with PP.  If not, see <http://www.gnu.org/licenses/>.

options( java.parameters = c("-Xss2560k", "-Xmx2g")) 
library(shiny)
library(XLConnect)
### switching off the scientific notation

options(scipen=999)
### so that the rnorm function is called only once during the session
### these are the starting values of the application
source("HRA.R")

### plot for download
readNumbersFromField <- function(listArgument){
  listOfNamesAndNumbers <- paste(strsplit(listArgument, ",")[[1]], collapse=".")
  listOfNamesAndNumbers <- strsplit(listOfNamesAndNumbers," ")[[1]]
  nazwa <- ""
  myLengthosc <- length(listOfNamesAndNumbers)
  for (element in 1:myLengthosc){
    if (is.na(as.numeric(listOfNamesAndNumbers[element]))) nazwa <- paste(nazwa, listOfNamesAndNumbers[element])
    else break
  }
  listOfNumbers <- listOfNamesAndNumbers[element:myLengthosc]
  myData <- as.vector(sapply(listOfNumbers, as.numeric))
  return(myData)
}

plotInput <- function(data1, data2, col, variableName){
  tempPP <- preparePP(data1, data2)
  xlab = parse(text = paste(variableName, "[i]"))
  ylab = parse(text = paste(variableName, "[i+1]"))
  plot(tempPP[,2]~tempPP[,1], xlab = xlab, ylab = ylab, pch = 21, col = "black", bg = col)
  abline(0,1, lty = 2, lwd = 2)
}

read_and_filter_one_file <- function(fileAddresses, lineNumber, separator, column_data, minmax, usingExcel){
  dataFile <- fileAddresses$datapath[lineNumber]
  javaerror <- FALSE; csverror <- FALSE # these show whether the function should return "some_problem" and exit
  if (usingExcel){
    if (dataFile=="./RR.csv") dataFile="./RR.xlsx" # just making sure that the XLConnect does not crash on text
    tryCatch(
    wb <- loadWorkbook(dataFile),
    error = function(e) javaerror <<- TRUE # if java fails, "some_problem" will be returned and it should be handled in reactive plot
    )
    if (javaerror) return(data.frame("some_problem"))
    data <- readWorksheet(wb, sheet = 1) # this will never happen if java fails
  } else {
    data <- read.csv(dataFile, sep = separator, header = T, row.names=NULL)
  }
  column_idx <- readNumbersFromField(column_data)
  RR_idx <- column_idx[1]
  flag_idx <- ifelse(length(column_idx)>1, column_idx[2], 0)
  tryCatch( # this will go wrong if the wrong type of file is selected
    RR <- data[[RR_idx]],
    error = function(e) csverror <<- TRUE  # i just return some_problem and it should be handled in the reactive plot
  )
  if (csverror) return(data.frame("some_problem"))
  if (flag_idx>0){
    tryCatch( # now, an error can also happen if there is a flags column, but it does not correspond to an actual column flag
    flags <- data[[flag_idx]],
    error = function(e) csverror <<- TRUE
    )
    if (csverror) return(data.frame("some_problem")) # if this happens, return some_problem
  }
  else
    flags <- RR*0
  # time based filtering here
  minmax <- readNumbersFromField(minmax)
  which_min <- RR <= minmax[1]
  which_max <- RR >= minmax[2]
  flags[which_min] <- 2
  flags[which_max] <- 3
  return(list(RR=RR, flags=flags))
}
getPpResults <- function(fileAddresses, separator = "\t", column_data, minmax, usingExcel){
  results <- c()
  withProgress(message = 'Calculating results', value = 0,{
  for (lineNumber in  1:length(fileAddresses[[1]])){
    rr_and_flags <- read_and_filter_one_file(fileAddresses, lineNumber, separator, column_data, minmax, usingExcel)
    tempPP <- preparePP(rr_and_flags[[1]], rr_and_flags[[2]])
    results <- rbind(results, HRAdescriptors(tempPP))
    incProgress(1/length(fileAddresses[[1]]), paste("Processing file", fileAddresses[[1]][lineNumber]))
  }
})
  results <- round(results,3)
  results <- cbind(fileAddresses$name, results)
  colnames(results) <- c("file", "SD1", "SD2", "SDNN", "SD1d", "SD1a", "C1d", "SD2d", "SD2a", "C2d", "SDNNd", "SDNNa", "Cd")
  return(results)
}

getSep <- function(separator){
  sep = separator
  if (separator == "tabulator"){
    sep = "\t"
  }
  if (separator == "space"){
    sep = " "
  }
  return(sep)
}

shinyServer(function(input, output){
  dataAddress <- reactive({
    dataPaths <- data.frame(name = c("RR.csv"), size = 0, type = c("text/plain"), datapath = c("./RR.csv"), stringsAsFactors = FALSE)
    if (!is.null(input$files)){
      dataPaths <- input$files
    }
    return(dataPaths)
  })
  
  output$plot <- renderPlot({
    errorOnRead <- FALSE
    rr_and_flags <- read_and_filter_one_file(dataAddress(), 1, separator=getSep(input$separator), input$data_columns, input$minmax, input$usingExcel)
    tryCatch(
      tempPP <- preparePP(rr_and_flags[[1]], rr_and_flags[[2]]),
      error = function(e) errorOnRead <<-  TRUE
     )
    tryCatch(
      drawPP(tempPP, variableName = ifelse(input$variableName=="", "RR", input$variableName), color = input$color),
      error = function(e)   errorOnRead <<- TRUE
    )
    if (errorOnRead){
     plot(1:10, 1:10, col = "white")
      text(6,7, "Fail - incorrect format", cex = 1.5)
      text(6,5, "try another file type,", cex = 1.5)      
      text(6,3, "column selection, or separator", cex = 1.5)      
    } else {
    drawPP(tempPP, variableName = ifelse(input$variableName=="", "RR", input$variableName), color = input$color)
    }
  })
  
  # now reactive conductor holding the results of Poincare plot calculations
  
  currentPPvalues <- reactive({
    tryCatch(
    returnTable <- getPpResults(dataAddress(), sep = getSep(input$separator), input$data_columns, input$minmax, input$usingExcel),
    error = function(e) returnTable <<- NA
  )
    if (is.na(returnTable[1])) return(data.frame(Info = "FAIL - incorrect format - try choosing another file type, column selection or separator"))
    else return(returnTable)})
  
  output$filesView <- renderTable({
  return(currentPPvalues())
  }, include.rownames = FALSE)
  
  output$myDataView <- renderTable({
    X <- input$variableName
    myTable <- data.frame(myData()[[1]], transformData()$data)
    colnames(myTable) <- c(input$variableName, "transformation")
    myTable
  })
  
  output$downloadPlot <- downloadHandler(
    filename = "PoincarePlot.png",
    content = function(file) {
    rr_and_flags <- read_and_filter_one_file(dataAddress(), 1, separator=getSep(input$separator), input$data_columns, input$minmax, input$usingExcel)
    png(file, width=1800, height = 1900, res=300)
    plotInput(rr_and_flags[[1]], rr_and_flags[[2]], input$color, input$variableName)
    dev.off()
  })

  output$downloadResults <- downloadHandler(
    filename = "PPResults.xlsx",
    content = function(file) {
      writeWorksheetToFile( file = file, data=currentPPvalues(), sheet="Poincare plot")
    })

  ### end of server below
}
)

