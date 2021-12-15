
if (!require(shiny)) {
  install.packages("shiny")
}
library(shiny)


if (!require(leaflet)) {
  install.packages("leaflet")
}
library(leaflet)

shinyUI(fluidPage(
# Application title
    titlePanel("COVID-MAP"),
    

    sidebarLayout(
        sidebarPanel(h3("入力"),
                     h4("地図の設定"),
                     uiOutput("date"),
                     numericInput("y",label = h5("終了日までの累積日数を入力"),value="14"),
                     h6("日付を6/30、日数を14にすると、6/16~6/30の累積感染者マップが出力されます。"),
                     radioButtons("button",label = "グラフの種類を選択してください",
                                  c("累積感染者数"="leaflet1",
                                    "10万人当たりの累積感染者数"="leaflet2")
                                  ),
                     sliderInput("en",label = "円のサイズの指定",
                                 min = 1,
                                 max = 10,
                                 value = 5)
                    
                     
                     ),
        
        
        
        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("covid_map"),
            
        )
    )


)
)
