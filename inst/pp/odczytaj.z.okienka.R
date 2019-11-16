### This file is part of PCSS's Run Test suite.

### Run Test is free software: you can redistribute it and/or modify
### it under the terms of the GNU General Public License as published by
### the Free Software Foundation, either version 3 of the License, or
### (at your option) any later version.

### Run Test is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.

### You should have received a copy of the GNU General Public License
### along with Run Test.  If not, see <http://www.gnu.org/licenses/>.
### Run Test is free software: you can redistribute it and/or modify
### it under the terms of the GNU General Public License as published by
### the Free Software Foundation, either version 3 of the License, or
### (at your option) any later version.

### Run Test is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.

### You should have received a copy of the GNU General Public License
### along with Run Test.  If not, see <http://www.gnu.org/licenses/>.

#' odczytuje numbers wklejone lub wpisane do okienka, numbers moga byc z przecinkiem lub kropka
#'
#' @param listArgument zawiera liste zwracana przez shiny'ego, zawierajaca to co uzytkownik wkleil/wpisal do okienka
#' @return zwraca wektor zawierajacy numbers z okienka
#' @export
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
      }

#' odczytuje faktory wklejone lub wpisane do okienka
#'
#' @param listArgument zawiera liste zwracana przez shiny'ego, zawierajaca to co uzytkownik wkleil/wpisal do okienka
#' @return zwraca wektor zawierajacy faktory z okienka
#' @export
readFactorsFromField <- function(listArgument){
  gsub(",", " ", listArgument)
  return(as.factor(strsplit(listArgument, " ")[[1]]))
}
