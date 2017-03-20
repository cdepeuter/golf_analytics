
shinyUI(fluidPage(
    
    titlePanel("Hello Shiny!"),

    sidebarLayout(
        sidebarPanel(
            selectInput("player", "Player:", getPlayerNameMap(safeway.shot_weather), 1810)
        ),
        
        
        mainPanel(
            plotOutput("driveDistPlot")
        )
    )
))