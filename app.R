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
source('get_data.R')
# load players data
players_df <- get_players_data()

# generate raster img of court background picture
#courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court <- rasterGrob(readJPEG('nba_court.jpg'),
                    width=unit(1,"npc"), height=unit(1,"npc"))


# Define UI 
ui <- fluidPage(
   
   # Application title
   titlePanel("NBA Shots Analytics"),
   
   
   # Sidebar with season and player inputs
   sidebarLayout(
      sidebarPanel(
         selectInput('season',
                       'Pick Season',
                       c('2014-15', '2015-16', '2016-17')),
         uiOutput("playerSelection")
         
         
      ),
      
      # Show a plot of the shots and data table
      mainPanel(
         plotOutput("shotPlot"),
         dataTableOutput('table')
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$shotPlot <- renderPlot({
      # grab shots data
     player_id <- players_df$person_id[which(players_df$display_first_last == input$players)]
     season <- input$season
    
     shotsdf <- get_shots_data(player_id, season)
       
     # plot shots data
     if (!is.null(shotsdf)) {
     ggplot(shotsdf, aes(x=loc_x, y=loc_y)) +
      annotation_custom(court, -250, 250, -50, 420) +
       geom_point(aes(colour = shot_zone_basic, shape=event_type)) +
       xlim(250, -250) + # flip x coords for proper right/left handling
       ylim(-50, 420) +
       coord_fixed() + # fix chart to stop distortions
       ggtitle(paste('Shot Chart\n', unique(shotsdf$player_name), sep = '')) +
       theme(line = element_blank(),
             axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             legend.title = element_blank(),
             plot.title = element_text(size = 15, lineheight = 0.9, face = "bold"))
     }
     else {
       #render empty plot with no shots data
       ggplot(data.frame()) +
         annotation_custom(court, -250, 250, -50, 420) +
         geom_point() +
         xlim(250, -250) +
         ylim(-50, 420) +
         coord_fixed() +
         ggtitle(paste('No Shot Data this Season for', unique(input$players), sep = ' ')) +
         theme(plot.title = element_text(size = 15, lineheight = 0.9, face = "bold"))
     }
       
     
   })
   
   # render Data Set
   
   output$table <- renderDataTable({
     player_id <- players_df$person_id[which(players_df$display_first_last == input$players)]
     season <- input$season
     shotsdf <- get_shots_data(player_id, season)
     shotsdf})
   
   # Make player input selection dynamic with actual players in season
   
   output$playerSelection <- renderUI({
     selectInput("players", "Select Player", choices = 
      players_df$display_first_last[which(players_df$to_year >= as.numeric(paste('20', unlist(strsplit(input$season,'-'))[2], sep = '')) &
                                            players_df$from_year <= as.numeric(paste('20', unlist(strsplit(input$season,'-'))[2], sep = '')))])
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

