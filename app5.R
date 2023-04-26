#
# Yoon Lee
# learning some shiny stuff
#
library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)
options(scipen =99)


server <- function(input, output) {
  
  urlfile="https://raw.githubusercontent.com/oliveingithub/shiny_movieapp/main/movie.csv"
  movie <-read.csv(url(urlfile))
  
  output$studentPlot <- renderPlot({
    movie %>% group_by(main.genre, decades) %>%
      summarise(income = sum(gross)) %>%
      ggplot() + 
      aes(x = decades, y = income, color = main.genre) + 
      geom_line(size=2) + ylim(c(0,100000)) + 
      ggtitle("Gross income by genre") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  output$gross.by.genre <- renderPlot({
    movie.2 <- movie
    genre.name <- "All"
    my.year <- "All"
    
    if (input$genre !="All") {
      movie.2 <- movie.2 %>% filter(main.genre == input$genre)
      genre.name <- input$genre
    }
    if(input$year !="All") {
      movie.2 <- movie.2 %>% filter(decades == input$year)
      my.year <- input$year
    }
    
    movie.2 %>% 
      group_by(main.genre) %>%  
      summarise(gross = sum(gross)) %>%
      ggplot() +
      aes(x = main.genre, y = gross, fill = main.genre) +
      geom_bar(stat="identity", position = "dodge") +
      scale_fill_manual(values = c("#f279de", "#3a5924","#f1a1bb","#2b33bf", "#f1d479",
                                   "#336699","#31a66d","#f2b47d", "#d93914","#f2884d",
                                   "#FF3366", "#990066","#242da6", "#FFFFCC", "#666666",
                                   "#bf5b17", "#7fc97f")) +
      theme_minimal() +
      ggtitle(paste("Gross income by Genre", genre.name, ", year:", my.year))
    
  })
}

ui <- fluidPage( theme = shinytheme("yeti"),
  titlePanel("Movie gross income"),
  
  sidebarLayout(
    sidebarPanel(
      plotOutput("StudentPlot"),
      selectInput("genre", "Select Genre:",
                  choices = c("All","Action", "Adventure", "Animation", "Biography", "Comedy",
                              "Crime", "Documentary", "Drama", "Family", "Fantasy","Horror",
                              "Musical", "Mystery", "Romance", "Sci-fi", "Thriller", "Western" )),
      selectInput("year", "Select Year:",
                  choices = c("All","20s", "30s", "40s", "50s", "60s", "70s", "80s", "90s",
                              "00s","10s","20s"))
    ), # close sidebar panel
    mainPanel(
      plotOutput("gross.by.genre")
    ) # close main panel
  ) # close sidebar layout
) # close fluid page

shinyApp(ui=ui, server=server)