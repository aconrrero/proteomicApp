

  getHeatmaply <- function (data=NULL,
                           customColors = NULL,
                           hoverText=NULL,
                           title= NULL){
    
    plot_ly(data = data, 
            x = ~Concentration, 
            y = ~Temperature, 
            z = ~Values,
            type = "heatmap", colorscale = customColors, 
            text = as.list(hoverText),hoverinfo = "text")%>% 
        layout(hoverlabel = list(bgcolor = "white", 
        font = list(size = 12)),
        xaxis = list(title = title))
  
  }
  
  getLineChartly <- function (data=NULL,
                            customColors = NULL,
                            hoverText=NULL,
                            title= NULL){
    
    plot_ly(data = data, 
            x = ~Concentration, 
            y = ~Values,
            color = ~Temperature, 
            colors = customColors, 
            type = "scatter", mode = "lines+markers",
            text = as.list(hoverText),
            hoverinfo = "text")%>% 
        layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))%>% 
        layout(legend = list(title = list(text = "Temperature")),
        xaxis = list(title = title))
    
  }


  getThreeDplotly <- function (data=NULL,
                               customColors = c("darkolivegreen", "tomato4"),
                               hoverText=NULL){
    
    plot_ly(data = data, x = ~`Experiment ID`, y = ~SSM, z = ~`MS1 intensity`,
            color = ~Compound, colors = customColors,
            type = "scatter3d", mode = "lines+markers", marker = list(size = 3.5),
            line = list(width = 2), text = hoverText,
            hoverinfo = "text") %>% 
            layout(title = "SSM and MS1 Intensity",
            scene = list(xaxis = list(title = "Experiment ID"),
                         yaxis = list(title = "SSM"),
                         zaxis = list(title = "MS1 Intensity")))
  
}
