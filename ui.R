#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(RMariaDB)
library(DBI)
library(eeptools) #this is for the age calc
library(zoo)
library(ggplot2)

drv <- dbDriver("MariaDB")
con1 <- dbConnect(drv, username="remote", password="", dbname ="nhl", host="192.168.1.94")
con2 <- dbConnect(drv, username="remote", password="", dbname ="nhl_analytics", host="192.168.1.94")

# Now lets grab a table and create a dataframe
player_info <- dbReadTable(conn = con1, name = 'player_info')
player_info$search <-paste(player_info$firstName,player_info$lastName, player_info$primaryPosition,player_info$nationality)

player_season_stats <- dbReadTable(conn = con2, name = 'season_skater_coach_stats') %>% mutate_if(bit64::is.integer64, as.integer) %>% mutate_if(is.numeric, as.integer)
regular_season_stats <- player_season_stats %>% filter(game_type =='R')

season_skater_coach <- dbReadTable(conn = con2, name = 'season_skater_coach')
reg_season_skater_coach <- season_skater_coach %>% filter(game_type == 'R')

reg_season_skater_coach_col <- head(reg_season_skater_coach %>% select(-"player_id",-"team",-"season",-"game_type",-"head_coach",-"game_id") )

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Player Profile"),
    
    # Read this for more information on selectize inputs
    #https://shiny.rstudio.com/articles/selectize.html
    textOutput("name"),
    textOutput("nation"),
    textOutput("age"),
    textOutput("position"),
    tableOutput("aggStats"),
    sidebarLayout(
        sidebarPanel(
            selectizeInput(inputId="search", label = "Player Search", choices = player_info$search, selected = NULL, multiple = FALSE,
                           options = list(maxOptions = 5)),
            selectInput("seasonInput", "Season: ",
                    c("2010-2011" = 20102011,
                      "2011-2012" = 20112012,
                      "2012-2013" = 20122013,
                      "2013-2014" = 20132014,
                      "2014-2015" = 20142015,
                      "2015-2016" = 20152016,
                      "2016-2017" = 20162017,
                      "2017-2018" = 20172018,
                      "2018-2019" = 20182019
                    )),
            varSelectInput("variable", "Variable:", reg_season_skater_coach_col)
        ),
        mainPanel(
            plotOutput("histogram")
        )
    ),
    
    plotOutput("scatterPlot", click="plot_click"),
    plotOutput("timeSeries_ma"),
    plotOutput("timeSeries_acf", click="plot_click")
    
    ))
