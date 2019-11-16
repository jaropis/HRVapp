### runs is free software: you can redistribute it and/or modify
### it under the terms of the GNU General Public License as published by
### the Free Software Foundation, either version 3 of the License, or
### (at your option) any later version.

### runs is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.

### You should have received a copy of the GNU General Public License
### along with Time Series.  If not, see <http://www.gnu.org/licenses/>.
### Time Series is free software: you can redistribute it and/or modify
### it under the terms of the GNU General Public License as published by
### the Free Software Foundation, either version 3 of the License, or
### (at your option) any later version.

### Time Series is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.

### You should have received a copy of the GNU General Public License
### along with Time Series.  If not, see <http://www.gnu.org/licenses/>.

library(shiny)
library(XLConnect)
### switching off the scientific notation
options(scipen=999)
### so that the rnorm function is called only once during the session
### these are the starting values of the application
readNumbersFromField <- function(listArgument){
  listOfNamesAndNumbers <- paste(strsplit(listArgument, ",")[[1]], collapse=".")
  listOfNamesAndNumbers <- strsplit(listOfNamesAndNumbers," ")[[1]]
  nazwa <- ""
  myLengthosc <- length(listOfNamesAndNumbers)
  for (element in 1:myLengthosc){
    ## jezeli element nie jest liczba,to dolacz go do nNames i szukaj dalej
    if (is.na(as.numeric(listOfNamesAndNumbers[element]))) nazwa <- paste(nazwa, listOfNamesAndNumbers[element])
    else break
  }
  listOfNumbers <- listOfNamesAndNumbers[element:myLengthosc]
  ## indeksy.przecinkow <- agrep(input$numbers, ",")
  ##input$variable <- nazwa
  myData <- as.vector(sapply(listOfNumbers, as.numeric))
  return(myData)
}

getRunsResults <- function(fileAddresses, separator = "\t", data_columns, minmax, usingExcel){
  javaerror <- FALSE; csverror <- FALSE
  source("runs.R")
  results <- list()
  column_idx <- readNumbersFromField(data_columns)
  RR_idx <- column_idx[1]
  flag_idx <- ifelse(length(column_idx)>1, column_idx[2], 0)
  minmax <- readNumbersFromField(minmax)
  for (lineNumber in  1:length(fileAddresses[[1]])){
    dataFile <- fileAddresses$datapath[lineNumber]
    if (usingExcel){
      if (dataFile=="./RR.csv") dataFile="./RR.xlsx" # just making sure that the XLConnect does not crash on text
      tryCatch(
        wb <- loadWorkbook(dataFile),
        error = function(e) javaerror <<- TRUE # if java fails, "bullshit" will be returned and it should be handled in reactive plot
      )
      if (javaerror) return(data.frame(Info = "FAIL - incorrect format - try choosing another file format, column selection or separator"))
      data <- readWorksheet(wb, sheet = 1) # this will never happen if java fails
    } else {
      tryCatch(
        data <- read.csv(dataFile, sep = separator, header = T),
        warning = function(e) csverror <<- TRUE # because here we do get a warning, rather than an error
      )
    }
    if (csverror) return(data.frame(Info = "FAIL - incorrect format - try choosing another file format, column selection or separator"))
    RR <- data[[RR_idx]]
    if (flag_idx>0)
      flags <- data[[flag_idx]]
    else
      flags <- RR*0
    # time based filtering here
    which_min <- RR <= minmax[1]
    which_max <- RR >= minmax[2]
    flags[which_min] <- 2
    flags[which_max] <- 3
    tryCatch(
    tryCountingRuns <- countForAll(RR, flags),
    error = function(e) tryCountingRuns <<- NA
    )
    if (is.na(tryCountingRuns[1])) return(data.frame(Info = "FAIL - incorrect format - try choosing another file format, column selection or separator"))
    else results <- c(results, list(tryCountingRuns))
  }
  lenUp <- 0; lenDown <- 0; lenNoChange <- 0
  ## in this loop I am getting the maximum run length of a scpecific type for the analysed group of recordings
  for (result in results){
    if (length(result$directionup) > lenUp) lenUp <- length(result$directionup)
    if (length(result$directiondown) > lenDown) lenDown <- length(result$directiondown)
    if (length(result$noChange) > lenNoChange) lenNoChange <- length(result$noChange)
  }
  
  finalResults <- data.frame()
  for (result in results){
    finalResults <- rbind(finalResults, c(result$directionup[1:lenUp], result$directiondown[1:lenDown], result$noChange[1:lenNoChange]))
  }
  finalResults <- cbind(fileAddresses$name,finalResults)
  computedNames <- c("file", 
                     paste("up", 1:lenUp, sep = ""),
                     paste("down", 1:lenDown, sep = ""))
  if(lenNoChange > 0){
                     computedNames <- c(computedNames,paste("no change", seq_len(lenNoChange), sep = ""))
  }
  ### computing column names (i.e. how many "Up"'s, "Down"'s etc.)
  #print(computedNames)
  #print(finalResults)
  colnames(finalResults) <- computedNames
  ### and finally replacing NA's by zeros, so that it is easier to process (in fact, count 0 is obviously no NA, as 0 is a valid number of runs)
  finalResults[is.na(finalResults)] <- as.integer(0)
  return(finalResults)
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
  
  # reactive conductor for runs
  currentRuns <- reactive({
    errorOnRead <- FALSE
    tryCatch(
      runsResults <- getRunsResults(dataAddress(), sep = getSep(input$separator), input$data_columns,  input$minmax, input$usingExcel),
      error = function(e) errorOnRead <<-  TRUE
    )
    if (errorOnRead){
      runsResults <- data.frame(Info = "FAIL - incorrect format - try choosing another file format, column selection or separator")
    }
    return(runsResults) 
    })
  
  output$filesRunsView <- renderTable({
  currentRuns()    
  }, include.rownames = FALSE)
  
  output$downloadResults <- downloadHandler(
    filename = "RunsResults.xlsx",
    content = function(file) {
      writeWorksheetToFile( file = file, data=currentRuns(), sheet="Runs distribution")
    })
  ### end of server below
}
)
