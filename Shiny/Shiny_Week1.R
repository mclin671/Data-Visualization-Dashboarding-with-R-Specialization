library(shiny)
library(tidyverse)

#####Import Data

dat<-read_csv(url("https://www.dropbox.com/s/uhfstf6g36ghxwp/cces_sample_coursera.csv?raw=1"))
dat<- dat %>% select(c("pid7","ideo5"))
dat<-drop_na(dat)

ui<- fluidPage(
  sidebarLayout(position = "left",
                mainPanel(
                  sliderInput("range", 
                              label = "Select Five Point Ideology (1 =Very liberal, 5=Very conservative",
                              min = 1, max = 5, value = 3
                  )
                ),
                
                mainPanel(
                  plotOutput("plot", width = "800px")
                )
  )
)


server<-function(input,output){
  output$plot <- renderPlot(
    ggplot(
      filter(dat, ideo5 == input$range), 
      aes(x = pid7)) + geom_bar() + xlim(0, 8) + scale_y_continuous(breaks = seq(0, 125, 25), limits =c(0, 125)) + xlab("7 Point Party ID, 1=Very D, 7=Very R") + ylab("Count")
  )
}

shinyApp(ui,server)

