# Assignment-5-Davood
Efren Herrera Lopez:26249493.  Aniket Mrutyunjay Hiremath: .

# Background work #

```{r setup, include=FALSE}
knitr::opts_chunk$set(error = TRUE)
```

```{r, warning=FALSE, message=FALSE, echo=FALSE}
if (!require("tidyverse")) installed.packages("tidyverse")
if (!require("shiny")) installed.packages("shiny")
if (!require("dplyr")) installed.packages("dplyr")
library(tidyverse)
library(shiny)
library(dplyr)
```

# Preparation of Data #

```{r}
DIG <- read.csv("DIG.csv")
dig_df <- data.frame(DIG)
dig_df
```

# Logical Analysis #
```{r}
DATA <- fluidPage(
  selectInput(
  "variable1",
  "Enter the first data to compare:",
  choices = setdiff(names(dig_df), "ID")
),
selectInput(
  "variable2",
  "Enter the second data to compare:",
  choices = setdiff(names(dig_df), "ID")
),
tableOutput("comprison"),
 hr(),

 selectInput(
    "variable",
    "Select variable for analysis:",
    choices = setdiff(names(dig_df), "TRTMT")
  ),

  selectInput(
    "operation",
    "Select operation:",
    choices =c("Mean" = "mean", "Median" = "median", "Standard Deviation" = "sd", "Sum" = "sum", "Minimum" = "min", "Maximum" = "max")
    ),
tableOutput("analysis")
)

ANALYSIS <- function(input, output, session) {
  dig_logical <- reactive({
    
    req(input$variable1, input$variable2)
    
    table(
      dig_df[[input$variable1]],
      dig_df[[input$variable2]]
    )
  })
  output$comprison <- renderTable({
    dig_logical()
  })
    output$analysis <- renderTable({
  req(input$variable, input$operation)



    dig_df %>%
      group_by(TRTMT) %>%
      summarise(
        result = switch(
          input$operation,
          mean = mean(.data[[input$variable]], na.rm = TRUE),
          median = median(.data[[input$variable]], na.rm = TRUE),
          sum = sum(.data[[input$variable]], na.rm = TRUE),
          sd = sd(.data[[input$variable]], na.rm = TRUE),
          min = min(.data[[input$variable]], na.rm = TRUE),
          max = max(.data[[input$variable]], na.rm = TRUE)
    ),
     .groups = "drop"
  )
})
}
shinyApp(DATA, ANALYSIS)
```
