

# Define server logic for random distribution application
function(input, output) {
    
    
    #print(input$player)
    drive_data <- reactive({
        all.drives[all.drives$player == input$player[1], ]
    })
    
    #print(player_id)
 
    output$driveDistPlot <- renderPlot({
        plotDriveDistAdjustWeatherMultipleTourneys(drive_data())
    })

}
