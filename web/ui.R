
shinyUI(fluidPage(
    
    titlePanel("Hello Shiny!"),

    sidebarLayout(
        sidebarPanel(
            selectInput("player", "Player:", getPlayerNameMap(all.drives), 1810)
        ),
        
        mainPanel(
            plotOutput("driveDistPlot")
        )
    )
))