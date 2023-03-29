#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library(dygraphs)
library(rvest)
library(lubridate)
library(splitstackshape)
library(data.table)
library(timetk)
library(janitor)
library(xts)
library(zoo)

load("Data_sets.RData")

funkcje <-c("Suma","Średnia")
zmienne <-c("wyświetlenia pytania"="views",
            "ocena pytania"="vote_count",
            "liczba pytań"="temp_num",
            "liczba odpowiedzi na pytanie"="response_count")
text_of_comment <- "Dane zostały pobrane ze strony pytań StackOverflow. Każde pytanie zostało rozdzielone na wiele rokordów (jeden na tag). 
Część pytań została odfiltrowana. Wśród pytań na stronie znajdują się pytania z zewnątrz strony, które mają treść, ale brakuje takich danych jak data postu, czy dane pytającego."

time_series_tag<-function(stack_tag,in_data,FUN,column){
    result <- in_data %>% 
        filter(tag == stack_tag) %>% 
        mutate(date = as.Date(time),
               Yearmonth = paste(year(date), formatC(month(date), width = 2, flag = "0")),
               formatC(day(date), width = 2, flag = "0"))
    result <- aggregate(result[,column], by = list(result$Yearmonth), FUN )
    result <- ts(result$x,frequency = 12, start = c(2008), end = c(2020))
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel('Popularność tagów w pytaniach na stronie StackOverflow'),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textOutput("comment"),
            br(),
            textInput(inputId = "tags",
                      label = h3("Podaj tagi oddzielone przecinkami"),
                      value = "python, java, r"),
            br(),
            selectInput("funkcja",
                        h3("Wybierz funkcje"),
                        choices = funkcje),
            br(),
            selectInput("zmienna",
                        h3("Wybierz zmienną agregowaną"),
                        choices = zmienne),
            br(),
            submitButton("Aktualizuj")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("napis"),
            tags$head(tags$style("#napis{color: red;
                                 font-size: 25px;
                                 font-style: bold;
                                 }")
            ),
            dygraphOutput("ts_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    tags_check <- reactive(input$tags %>% 
                               str_split(",") %>% 
                               unlist() %>% 
                               str_trim() %>% 
                               str_to_lower())
    bad_tags <- reactive(setdiff(tags_check(),unique(df2$tag)))
    any_ok <- reactive(any(tags_check() %in% df2$tag))
    any_bad <- reactive(length(bad_tags()) > 0)
    output$comment <- renderText({
        text_of_comment
    })
    output$napis <- renderText({
        if(!any_ok()){
            "Proszę wpisać istniejące tagi."
        }
        else if(any_bad()){
            paste("Nie poprawne tagi:",paste(bad_tags()))
        }
    })
    output$ts_plot <-renderDygraph({
        if(any_ok()){
            tags_to_show <- input$tags %>% str_split(",") %>% unlist() %>% str_trim() %>% str_to_lower()
            tags_to_show <- setdiff(tags_to_show,bad_tags())
            FUN <-function(x) mean(x, na.rm = T)
            if (input$funkcja == "Suma") { FUN <-function(x) sum(x, na.rm = T) }
            column <- input$zmienna
            time_ser <- cbind() # empty
            
            for(stack_tag in tags_to_show){
                time_ser <- cbind(time_ser,time_series_tag(stack_tag,df2,FUN,column))
            }
            colnames(time_ser) <- tags_to_show
            dygraph(time_ser,
                    xlab = "Wybierz zakres czasowy") %>% 
                dyOptions() %>%
                dyRangeSelector() %>% 
                dyLegend(labelsSeparateLines = TRUE) %>% 
                dyHighlight(highlightCircleSize = 5, 
                            highlightSeriesBackgroundAlpha = 0.2,
                            hideOnMouseOut = TRUE)
                
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
