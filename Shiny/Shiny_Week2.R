library(shiny)
library(tidyverse)
library(plotly)
library(DT)

#####Import Data

dat<-read_csv(url("https://www.dropbox.com/s/uhfstf6g36ghxwp/cces_sample_coursera.csv?raw=1"))
dat<- dat %>% select(c("pid7","ideo5","newsint","gender","educ","CC18_308a","region"))
dat<-drop_na(dat)

#####Make your app

ui <- navbarPage("My Application",
                 tabPanel("Page 1",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput(
                                inputId = "fivepoint",
                                label = "Select Five Point Ideology (1=Very liberal, 5=Very conservative",
                                min = 1,
                                max = 5,
                                value = 3
                              )
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Tab 1", plotOutput("plot1tab1")),
                                tabPanel("Tab 2", plotOutput("plot1tab2"))
                              )
                            )
                          )
                 ),
                 tabPanel("Page 2",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput("gender", "Select Gender",
                                                 choices = c("1" = "1",
                                                             "2" = "2"))
                            ),
                            mainPanel(
                              plotlyOutput("plot2")
                            )
                          )
                 ),
                 tabPanel("Page 3",
                          sidebarLayout(
                            sidebarPanel(
                              selectizeInput(
                                "region", "Select Region", choices = c("1", "2", "3", "4"), multiple = TRUE
                              )
                            ),
                            mainPanel(
                              DT::dataTableOutput("table", height = 500)
                            )
                          )
                 )
)


server<-function(input,output, session){
  
  output$plot1tab1 <- renderPlot(
    ggplot(filter(dat, ideo5 == input$fivepoint),
           aes(x = pid7)) + geom_bar() + scale_y_continuous(breaks = seq(0, 100, 25), limits = c(0, 100)) + 
      xlim(0, 8) + xlab("7 Point Party ID, 1=Very D, 7=VeryR") + ylab("Count")
  )
  
  output$plot1tab2 <- renderPlot(
    ggplot(filter(dat, ideo5 == input$fivepoint),
           aes(x = CC18_308a)) + geom_bar() + xlim(0, 5) + xlab("Trump Support") + ylab("Count")
  )
  
  output$plot2 <- renderPlotly(
    ggplot(filter(dat, gender %in% input$gender), aes(x = educ, y = pid7)) + geom_jitter(size = 1.5) + geom_smooth(method = 'lm') + theme(text = element_text(size = 14))
  )
  
  filterData <- reactive({
    dat[which(dat$region %in% input$region),]
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(filterData(), selection = "multiple")
  },
  )
} 

shinyApp(ui,server)
