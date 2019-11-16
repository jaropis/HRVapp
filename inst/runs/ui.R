### Runs is free software: you can redistribute it and/or modify
### it under the terms of the GNU General Public License as published by
### the Free Software Foundation, either version 3 of the License, or
### (at your option) any later version.

### Runs is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.

### You should have received a copy of the GNU General Public License
### along with Time Series.  If not, see <http://www.gnu.org/licenses/>.
### Time Series is free software: you can redistribute it and/or modify
### it under the terms of the GNU General Public License as published by
### the Free Software Foundation, either version 3 of the License, or
### (at your option) any later version.

### Runs is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.

### You should have received a copy of the GNU General Public License
### along with Time Series.  If not, see <http://www.gnu.org/licenses/>.

runs_description <- function(){
  return("Application for calculating the lengths of rising (up) and falling (down) runs, as well as runs with no change, for one or more files.
<ul>
   <li> <strong>file format</strong> - The application requires either a <strong>text</strong> or <strong>Excel</strong> file (files), with at least the column containing the RR intervals. For text file any extension will be OK, for Excel you must use .xlsx (the latest verion). If you want to use the Excel file format, you must check the <strong>using Excel</strong> checkbox.
  <li> <strong>column selection</strong> - Enter the number of the column containing the RR intervals in the appropriate window. If you have flags, enter the number of the column holding them after a space or a comma. The 0 flag means <em>correct</em>, or of sinus origin. Select the separator (what you have beteen the columns in the text file) from the drop down menu.
<li> <strong>time based filtering</strong> - Enter values to filter the RR intervals based on time. The first value sets the RR intervals that you consider too short to be of sinus origin, the second ones selects the intervals you consider too long to be of sinus origin.
</ul>")
}
library(shiny)
shinyUI(
  fluidPage(
    titlePanel("Mapping of a time-series to rising, falling and neutral runs"),
    fluidRow(
      column(3,
             wellPanel(
               checkboxInput("usingExcel", "using Excel", value = FALSE),
               fileInput('files', label="load files in the correct format - see explanations on the left", multiple=TRUE),
               selectInput("separator", "select separator",
                           list("tabulator", ",", ";", "space")),
               textInput("data_columns", "enter the column for RR intervals and flags - see explanations", "1 2"),
               textInput("minmax","enter minimum and maximum acceptable RR length", "0 3000")
             )
      ),
      column(6,tabsetPanel(
        tabPanel("Numerical results", tags$style(type="text/css", "#filesRunsView { overflow-x: auto; max-width: 100%; }"),
                 tableOutput("filesRunsView"),
                 downloadButton('downloadResults', 'Download results as Excel file'))
      )),
      
      column(3, HTML(paste("<h4>Explanation</h4>",runs_description()))
      )
    )
  )
)

