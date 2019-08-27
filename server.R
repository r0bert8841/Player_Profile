#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    df_player_info <- reactive({ player_info %>% filter(search == input$search)})
    df_regular_season_stats <-reactive({regular_season_stats %>% filter(player_id == df_player_info()$player_id)} %>% arrange(season, team, head_coach))
    df_reg_season_skater_coach <-reactive({reg_season_skater_coach %>% filter(player_id == df_player_info()$player_id & season ==input$seasonInput)})
    ts <- reactive({zoo(df_reg_season_skater_coach()[[input$variable]],df_reg_season_skater_coach()[["date_EDT"]])})
    
    # For the Scatter Plot and the Histogram
    
    
    
    output$aggStats <- renderTable(df_regular_season_stats())
    output$name <- renderText(paste("Name: ",df_player_info()$firstName, " ", df_player_info()$lastName))
    output$nation <- renderText(paste("Nationality: ",df_player_info()$nationality))
    output$age <- renderText(paste("Age: ",floor(age_calc(df_player_info()$birthDate, units = "years"))))
    output$position <- renderText(paste("Position: ",df_player_info()$primaryPosition))
    
    output$scatterPlot <- renderPlot({
        stats <- regular_season_stats %>% filter(season == input$seasonInput)
        stats$highlight <- if_else(stats$player_id==df_player_info()$player_id,1,0)
        #http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization
        ggplot(stats,aes_string("games_played",input$variable, col="positionType"))+
            geom_point()+
            geom_point(data = stats[stats$highlight ==1 ,], aes_string("games_played",input$variable, col="positionType"), color = "black", size = 2)+
            geom_smooth(method="lm", se=FALSE)
    })
    
    output$histogram <- renderPlot({
        df_reg_season_skater_coach_filter <- df_reg_season_skater_coach() %>% filter(season == input$seasonInput)
        #var1 <- mean(!!rlang::sym(input$variable))
        # http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
        ggplot(df_reg_season_skater_coach_filter, aes_string(x=input$variable, col="head_coach")) +
            geom_histogram(aes(y=..density..), colour="black", fill="white")+
            geom_density(alpha=.2, fill="#FF6666")#+
        #geom_vline(aes(xintercept=mean(!!rlang::sym(input$variable))),color="blue", linetype="dashed", size=1) # This is the mean line
        
    })
    
    output$timeSeries_ma <- renderPlot({
        ma <- rollmean(ts(),3,align="right") # 3-game moving average, align right means it includes the 4 previous games and the current game
        p <- autoplot(ma, facet = NULL)
        p + geom_hline(yintercept = mean(ma), color="blue")+ geom_hline(yintercept = mean(ma), color="blue")
        #plot(ma)
    })
    output$timeSeries_acf <- renderPlot({
        acf(ts(),na.action=na.pass)
    })

    
    
})
