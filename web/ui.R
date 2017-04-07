
shinyUI(fluidPage(
    
    titlePanel("Hello Shiny!"),

    sidebarLayout(
        sidebarPanel(
            selectizeInput("player", "Player:", getPlayerNameMap(all.drives), 1810)
        ),
        
        mainPanel(
            plotOutput("driveDistPlot"),
            plotOutput("adjustedDriveDistPlot")
        )
    )
))