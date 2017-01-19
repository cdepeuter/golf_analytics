library(sp)  
library(rjson)  

shinyServer(function(input, output, session) {  
    
    output$json <- reactive({  
        if(length(input$NE1)>0){  
            
            #From the Google Maps API we have 4 inputs with the coordinates of the NE and SW corners  
            #using these coordinates we can create a polygon  
            pol <- Polygon(coords=matrix(c(input$NE2,input$NE1,input$NE2,input$SW1,input$SW2,input$SW1,input$SW2,input$NE1),ncol=2,byrow=T))  
            polygon <- SpatialPolygons(list(Polygons(list(pol),ID=1)))  
            
            #Then we can use the polygon to create 100 points randomly  
            grid <- spsample(polygon,n=100,type="random")  
            
            #In order to use the function toJSON we first need to create a list  
            lis <- list()
            for(i in 1:100){
                lis[[i]] <- list(i,grid$x[i],grid$y[i])
            }
            
            # lis <- apply(grid, 1, function(xr){
            #     return(list(xr[["x"]],xr[["y"]]) )
            # })
            # 
            
            #This code creates the variable test directly in javascript for export the grid in the Google Maps API  
            #I have taken this part from:http://stackoverflow.com/questions/26719334/passing-json-data-to-a-javascript-object-with-shiny  
            paste('<script>test=',   
                  RJSONIO::toJSON(lis),  
                  ';Cities_Markers();', # print 1 data line to console  
                  '</script>')  
        }  
    })  
})  