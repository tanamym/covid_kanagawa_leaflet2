
if (!require(htmltools)) {
  install.packages("htmltools")
}
library(htmltools)
if (!require(shiny)) {
  install.packages("shiny")
}
library(shiny)
if (!require(dplyr)) {
  install.packages("dplyr")
}
library(dplyr)
if (!require(data.table)) {
  install.packages("data.table")
}
library(data.table)
if (!require(leaflet)) {
  install.packages("leaflet")
}
library(leaflet)
if (!require(lubridate)) {
  install.packages("lubridate")
}
library(lubridate)
if (!require(readxl)) {
  install.packages("readxl")
}
library(readxl)
if (!require(rsconnect)) {
  install.packages("rsconnect")
}
library(rsconnect)
if (!require(mapview)) {
    install.packages("mapview")
}
library(mapview)

if (!require(magick)) {
    install.packages("magick")
}
library(magick)
if (!require(webshot)) {
    install.packages("webshot")
 
}
library(rgdal)
if (!require(rgdal)) {
  install.packages("rgdal")
  
}
#webshot :: install_phantomjs()


shinyUI(fluidPage(
# Application title
    titlePanel("COVID-MAP"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(h3("入力"),
                     h4("地図の設定"),
                     uiOutput("date"),
                     #dateInput("x",label = h5("累積日数の最後の日付入力"),max=date[1,1],value = date[1,1]),#Sys.Date()),
                     numericInput("y",label = h5("終了日までの累積日数を入力"),value="14"),
                     h6("日付を6/30、日数を14にすると、6/16~6/30の累積感染者マップが出力されます。"),
                     h5("横浜市は市単位での表示になっています。（区単位ではありません。）"),
                     radioButtons("button",label = "グラフの種類を選択してください",
                                  c("累積感染者数"="leaflet1",
                                    "10万人当たりの累積感染者数"="leaflet2")
                                  ),
                     sliderInput("color",
                                 label = "色の調整(一日当たり）",
                                 min = 10,
                                 max = 200,
                                 value = 10,)
                     
                     ),
        
        
        
        # Show a plot of the generated distribution
        mainPanel(h4("出力"),
            leafletOutput("covid_map",height = "500px"),
            
        )
    )


)
)
