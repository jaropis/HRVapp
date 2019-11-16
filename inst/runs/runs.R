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

splitOnAnnot <- function(signal, annotations){
  ### this function splits the signal time series into disjoint subseries, breaking the signal on annotations
  ### which are not 0
  badIdx <- which(annotations != 0)
  if (length(badIdx) == 0)
    return(list(signal))
  start <- 1
  signalSegments <- list()
  for (idx in badIdx){
    end <- idx
    if (start<=end-1)
      signalSegments <- c(signalSegments, list(signal[start:end-1]))
    start <- idx+1
  }
  if (annotations[length(annotations)]==0)
    signalSegments <- c(signalSegments, list(signal[start:length(signal)]))
  return(signalSegments)
}

countRuns <- function(signal, allRuns = list(), directions = c()){
  ## allRuns - this list keeps all the runs in order
  ## directions - this vector keeps the designation - whether the run in the allRuns list is a deceleration or aceleration or noChange
  # 1 is for deceleration, 0 for no change, -1 for acceleration
  #cusignalent <- ifelse(signal[1]==signal[2],0, ifelse(signal[1]<signal[2], 1, -1)) 
  if (length(signal)<2)
    stop("the lenght of the time series must be greater than 1")## signal must be longer than 1, or runs make no sense
  last <- ifelse(signal[1]==signal[2],0, ifelse(signal[1]<signal[2], 1, -1)) 
  begin <- 1
  for (index in seq(length = (length(signal)-2))+1){
    cusignalent <- ifelse(signal[index]==signal[index+1],0, ifelse(signal[index]<signal[index+1], 1, -1)) 
    if (cusignalent != last){
      if (last == -1){
        allRuns <- c(allRuns, list(signal[(begin):index]))
        directions <- c(directions, "Down")
      }
      if (last == 0){
        allRuns <- c(allRuns, list(signal[(begin):index]))
        directions <- c(directions, "noChange")
      }
      if (last == 1){
        allRuns <- c(allRuns, list(signal[(begin):index]))
        directions<- c(directions, "Up")
      } 
      begin <- index+1
      last <- cusignalent
    }
  }
  #now check the last run
  
  if (last == -1){
    allRuns <- c(allRuns, list(signal[(begin):length(signal)]))
    directions <- c(directions, "Down")
  }
  if (last == 0){
    allRuns <- c(allRuns, list(signal[(begin):length(signal)]))
    directions <- c(directions, "noChange")
  }
  if (last == 1){
    allRuns <- c(allRuns, list(signal[(begin):length(signal)]))
    directions <- c(directions, "Up")
  } 
  return(list(allRuns = allRuns, directions = directions))
}

splitAllIntoRuns <- function(signal, annotations){
  listOfSeparateSegments <- splitOnAnnot(signal, annotations)
  ### initialize the list keeping separate runs in consecutive segments
  separateRunsAndDirections <- list(allRuns = list(), directions = c())
  for (segment in listOfSeparateSegments){
    if (length(segment)>1){
      temp <- countRuns(segment)
      separateRunsAndDirections$allRuns <- c(separateRunsAndDirections$allRuns, temp$allRuns)
      separateRunsAndDirections$directions <- c(separateRunsAndDirections$directions, temp$directions)
    }
  }
  return(separateRunsAndDirections)
}

countForAll <- function(signal, annotations){
  ### THIS IS THE MAIN FUNCTION OF THIS SOURCEFILE
  ## this functon counts all the runs of a specific type (decelerations, accelerations, no change)
  ## up to the maximum values
  ## e.g. if there is only one deceleration run of the type 1 2 3 4 5, the result will be
  ## directionup = c(0,0,0,0,1), directiondown = NULL, noChange = NULL
  splitsignal <- splitAllIntoRuns(signal, annotations)
  directions <- splitsignal$directions
  UpRuns <- splitsignal$allRuns[directions == "Up"]
  DownRuns <- splitsignal$allRuns[directions == "Down"]
  noChangeRuns <- splitsignal$allRuns[directions == "noChange"]
  UpRunsCounts <- unlist(lapply(UpRuns, length))
  DownRunsCounts <- unlist(lapply(DownRuns, length))
  noChangeRunsCounts <- unlist(lapply(noChangeRuns, length))
  
  ## getting maximum lengths of the respective runs
  maxUp <- ifelse(length(UpRunsCounts>0), max(UpRunsCounts), 0)
  maxDown <- ifelse(length(DownRunsCounts>0), max(DownRunsCounts), 0)
  maxNoChange <- ifelse(length(noChangeRunsCounts)>0, max(noChangeRunsCounts), 0)
  
  ## now counting
  upRunsCounts <- c()
  downRunsCounts <- c()
  zeroChangeRunsCounts <- c()
  
  #directionup
  for (idxUp in seq(length = maxUp)){
    upRunsCounts <- c(upRunsCounts, sum(UpRunsCounts == idxUp))
  }
  for (idxDown in seq(length = maxDown)){
    downRunsCounts <- c(downRunsCounts, sum(DownRunsCounts == idxDown))
  }
  for (idxNoChange in seq(length = maxNoChange)){
    zeroChangeRunsCounts <- c(zeroChangeRunsCounts, sum(noChangeRunsCounts == idxNoChange))
  }
  return(list(directionup = upRunsCounts, directiondown = downRunsCounts, noChange = zeroChangeRunsCounts))
}

