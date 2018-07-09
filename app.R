#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(grid)
library(jpeg)
library(RCurl)
library(Cairo)
options(shiny.usecairo=T)
source('get_data.R')
courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                    width=unit(1,"npc"), height=unit(1,"npc"))


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("NBA Shots Analytics"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("players",
                     "Select Player",
                     players$display_first_last),
         selectInput('season',
                       'Pick Season',
                       c('2014-15', '2015-16', '2016-17'))
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("shotPlot"),
         dataTableOutput('table')
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$shotPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
     player_id <- players$person_id[which(players$display_first_last == input$players)]
     season <- input$season
     shotsdf <- get_shots_data(player_id, season)
     ggplot(shotsdf, aes(x=loc_x, y=loc_y)) +
       annotation_custom(court, -250, 250, -50, 420) +
       geom_point(aes(colour = shot_zone_basic, shape=event_type)) +
       xlim(250, -250) +
       ylim(-50, 420) +
       coord_fixed() +
       ggtitle(paste('Shot Chart\n', unique(shotsdf$player_name), sep = '')) +
       theme(line = element_blank(),
             axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             legend.title = element_blank(),
             plot.title = element_text(size = 15, lineheight = 0.9, face = "bold"))
       
       
     
   })
   
   output$table <- renderDataTable({
     player_id <- players$person_id[which(players$display_first_last == input$players)]
     season <- input$season
     shotsdf <- get_shots_data(player_id, season)
     shotsdf})
}

# Run the application 
shinyApp(ui = ui, server = server)

