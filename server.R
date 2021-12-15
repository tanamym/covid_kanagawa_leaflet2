#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
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

if (!require(stringr)) {
  install.packages("stringr")
}
library(stringr)
if (!require(webshot)) {
    install.packages("webshot")

}
webshot :: install_phantomjs()




shinyServer(function(input, output, session) {
  data2020 <-
    fread("https://raw.githubusercontent.com/tanamym/covid19_colopressmap_isehara/main/data2020.csv",encoding="UTF-8")
  
  data202106 <-
    fread("https://raw.githubusercontent.com/tanamym/covid19_colopressmap_isehara/main/data202106.csv",encoding="UTF-8")
  data202109 <-
    fread("https://raw.githubusercontent.com/tanamym/covid19_colopressmap_isehara/main/data202109.csv",encoding="UTF-8")
  
  data2021 <-
    fread("https://raw.githubusercontent.com/tanamym/covid19_colopressmap_isehara/main/data2021.csv",encoding="UTF-8")
  ycd <-
    fread("https://raw.githubusercontent.com/tanamym/covid19_colopressmap_isehara/main/yoko_covid.csv",encoding="UTF-8") %>%
    mutate(Fixed_Date=as.Date(Date),
           Residential_City=City)
  xy<-read.csv("xy.csv",encoding = "SHIFT-JIS")
  list<-read.csv("list.csv",encoding = "SHIFT-JIS")%>%
    rename("X1"="X","Y1"="Y")
  data7 <-
    rbind(data2020,data202106,data202109,data2021) %>%
    mutate(Fixed_Date=as.Date(Fixed_Date)) %>%
    arrange(desc(Fixed_Date),Hos,hos)%>%
    count(Fixed_Date,Residential_City,hos)%>%
    full_join(ycd%>%
                mutate(hos="yokohama"))%>%
    mutate(Residential_City=ifelse(!is.na(City),City,Residential_City)) %>%
    mutate(n=ifelse(!is.na(City),count,n))%>%
    arrange(Fixed_Date)%>%
    left_join(xy,by=c("Residential_City"="City"))%>%
    left_join(list,by=c("Residential_City"="list"))%>%
    mutate(X=ifelse(is.na(X),X1,X),
           Y=ifelse(is.na(Y),Y1,Y))
  date <- 
    data.frame(Date=min(data7$Fixed_Date):max(data7$Fixed_Date)) %>%
    arrange(desc(Date)) %>%
    mutate(Date=as.Date(Date,origin="1970-01-01")) %>%
    filter(Date>="2020-04-20")
  # data7<-
  #   fread("https://raw.githubusercontent.com/tanamym/covid19_colopressmap_isehara/main/coviddata.csv",encoding = "UTF-8")%>%
  #   mutate(Fixed_Date=as.Date(Fixed_Date))%>%
  #   filter(!is.na(X))
  # date<-
  #   data7%>%
  #   data.frame()%>%
  #   arrange(desc(Fixed_Date))%>%
  #   distinct(Fixed_Date)
    output$date<-
      renderUI({
        dateInput("x",
                  label = h5("累積日数の最後の日付入力"),
                  max = date[1,1],
                  value = date[1,1])
      })
    

    jinko<-read.csv("jinko.csv")
    jinko<-data.frame(jinko)

    output$covid_map <- renderLeaflet({
        #ここ書き換える
        switch (input$button,
                leaflet1 = { date<-lubridate::ymd(input$x)-input$y
                #集計
                data7.1<-data7%>%
                    filter(Fixed_Date>=date,Fixed_Date<=lubridate::ymd(input$x))%>%
                    group_by(Residential_City,X,Y)%>%
                    summarise(count=n())%>%
                    filter(X>0,Y>0)

                #leafletの可視化
                leaflet(data7.1) %>% addTiles() %>%
                    addProviderTiles(providers$CartoDB.Positron) %>%
                    #setView(lng=139.4725,lat=35.4478,zoom=10)%>%
                    fitBounds(lng1=139.124343, lat1=35.117843, lng2=139.652899, lat2=35.665052)%>%
                    addCircleMarkers(~X, ~Y, stroke=FALSE,
                                     radius =sqrt(data7.1$count)*input$en,
                                     label = ~htmlEscape(Residential_City),
                                     labelOptions = labelOptions(direction = 'auto',noHide = T, textOnly = TRUE,textsize = "10px"))%>%
                    addCircleMarkers(~X, ~Y, stroke=FALSE,
                                     radius =sqrt(data7.1$count)*input$en,
                                     label = ~htmlEscape(count),
                                     labelOptions = labelOptions(direction = 'bottom',noHide = T, textOnly = TRUE,textsize = "10px"),
                    )%>%addControl(tags$div(HTML(paste(date,lubridate::ymd(input$x),sep = "~")))  , position = "topright") },
                leaflet2={
                    date<-lubridate::ymd(input$x)-input$y
                    #集計
                    data7.1<-data7%>%
                        filter(Fixed_Date>=date,Fixed_Date<=lubridate::ymd(input$x))%>%
                        group_by(Residential_City,X,Y)%>%
                        summarise(count=n())%>%
                        filter(X>0,Y>0)
                    jinko2<-left_join(data7.1,jinko,by=c("Residential_City"="City"))
                    jinko3<-jinko2%>%
                        mutate(count_j=count/jinko*100000)
                    leaflet(jinko3) %>% addTiles() %>%
                        addProviderTiles(providers$CartoDB.Positron) %>%
                        #setView(lng=139.4825,lat=35.4478,zoom=10)%>%
                        fitBounds(lng1=139.124343, lat1=35.117843, lng2=139.652899, lat2=35.665052)%>%
                        addCircleMarkers(~X, ~Y, stroke=FALSE,
                                         radius =sqrt(jinko3$count_j)*input$en,
                                         label = ~htmlEscape(Residential_City),
                                         labelOptions = labelOptions(direction = 'auto',noHide = T, textOnly = TRUE,textsize = "10px"))%>%
                        addCircleMarkers(~X, ~Y, stroke=FALSE,
                                         radius =sqrt(jinko3$count_j)*input$en,
                                         label = ~htmlEscape(round(count_j,digits = 4)),
                                         labelOptions = labelOptions(direction = 'bottom',noHide = T, textOnly = TRUE,textsize = "10px")
                        )%>%addControl(tags$div(HTML(paste(date,lubridate::ymd(input$x),sep = "~")))  , position = "topright")
                    }
                )
    }
    )
    }
    )
      