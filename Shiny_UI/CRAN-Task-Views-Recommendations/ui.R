library(shiny)

library(igraph)
library(bc3net)
library(cranly)
library(RWsearch)
library(lubridate)


########## Data #########################


# Task View Snapshot
load("Data/tvdb.rda")

# CRAN snapshot 

## Data extracted from CRAN package repository
load("Data/CRAN_data.rda")

### all_CRAN_pks is all of the current packages available in CRAN
all_CRAN_pks = CRAN_data$Package


# Define UI for application that draws a histogram
shinyUI(fluidPage(

  navbarPage("CRAN Task View Recommendations",
             
             tabPanel("Package Developer", 
                      
                      # Sidebar with a slider input for number of bins
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "package_query", label = "Select a Package", choices = all_CRAN_pks)
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          print("Hello")
                        )
                      )   
                      
                      ),
             
             
             
             tabPanel("Task View Maintainer")

    
  ))
)