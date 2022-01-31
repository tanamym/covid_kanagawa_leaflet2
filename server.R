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
library(stringr)
if (!require(stringr)) {
  install.packages("stringr")
}
library(magick)
if (!require(webshot)) {
    install.packages("webshot")
   
}
# library(rgdal)
# if (!require(rgdal)) {
#   install.packages("rgdal")
#   
# }
library(sf)
if (!require(sf)) {
  install.packages("sf")

}
#webshot :: install_phantomjs()



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    #コロナのデータ読み込み
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
  
  data7 <-
    rbind(data2020,data202106,data202109,data2021) %>%
    mutate(Fixed_Date=as.Date(Fixed_Date)) %>%
    arrange(desc(Fixed_Date),Hos,hos)%>%
    count(Fixed_Date,Residential_City,hos,X,Y)%>%
    full_join(ycd%>%
                mutate(hos="yokohama"))%>%
    mutate(Residential_City=ifelse(!is.na(City),City,Residential_City)) %>%
    mutate(n=ifelse(!is.na(City),count,n))%>%
    arrange(Fixed_Date)%>%
    group_by(Fixed_Date,Residential_City,X,Y)%>%
    summarise(n=sum(n))
  date <- 
    data.frame(Date=min(data7$Fixed_Date):max(data7$Fixed_Date)) %>%
    arrange(desc(Date)) %>%
    mutate(Date=as.Date(Date,origin="1970-01-01")) %>%
    filter(Date>="2020-04-20")
  xy<-read.csv("xy.csv",encoding = "SHIFT-JIS")
  output$date<-
    renderUI({
      dateInput("x",
                label = h5("累積日数の最後の日付入力"),
                max = date[1,1],
                value = date[1,1])
    })
  jinko<-read.csv("jinko.csv",fileEncoding = "SJIS")
  jinko<-data.frame(jinko)

  shp<-
    read_sf("N03-190101_14_GML/N03-19_14_190101_2.shp",options = "ENCODING=CP932")

    output$covid_map <- renderLeaflet({
        #ここ書き換える
        switch (input$button,
                leaflet1 = { 
                  x<-input$x
                  y<-input$y
                  if(is.null(x)){
                    x<-max(date$Date)
                    y<-14
                  }
                  date<-lubridate::ymd(x)-y+1
                #集計
                data7.1<-
                  data7%>%
                  filter(Fixed_Date>=date,
                           Fixed_Date<=lubridate::ymd(x))%>%
                  group_by(Residential_City,X,Y)%>%
                  summarise(count=sum(n))%>%
                  ungroup()%>%
                  filter(X>0,Y>0)%>%
                  full_join(xy,by=c("Residential_City"="City"))%>%
                  mutate(count=ifelse(is.na(X),0,count))%>%
                  mutate(N03_004=Residential_City)

                data7.2<-
                  sp::merge(shp, data7.1,
                            by="N03_004", all=F,duplicateGeoms = TRUE)
                head(data7.2)
                #色設定
                pal <- colorNumeric(palette=c("white","red"),domain=c(0,input$color*y), reverse=F)
         
                pal2<-
                  data7.2%>%
                  mutate(col=pal(count),
                         col2=ifelse(count>input$color*y,"red",col),
                         flag=ifelse(count>input$color,paste0(input$color*y,"~"),paste0(count%/%10*10,"~")))
                
                data7.2%>%
                  leaflet() %>%
                  #fitBounds(lng1=139.124343, lat1=35.117843, lng2=139.652899, lat2=35.665052)%>% 
                  setView(lng=139.424343, lat=35.417843,zoom=10)%>%
                  addProviderTiles(providers$CartoDB.Positron) %>% 
                  addPolygons(fillOpacity = 1,
                              weight=1,
                              color = "#666",
                              fillColor = ~pal2$col2,
                              label = paste0(data7.2$N03_004,data7.2$count))%>%
                  addLegend(data=pal2%>%distinct(flag,.keep_all = T)%>%arrange(count),
                            position="bottomright",
                            pal=pal,
                            values = c(0,input$color*y),
                            #color=~col2,labels=~flag,
                            title = "累積感染者数",opacity = 1,
                            #labFormat = labelFormat(transform = function(x)x*x)
                            )%>%
                  addControl(tags$div(HTML(paste(date,lubridate::ymd(x),sep = "~")))  , position = "topright")
                },
                leaflet2={
                  x<-input$x
                  y<-input$y
                  if(is.null(x)){
                    x<-max(date$Date)
                    y<-14
                  }
                    date<-lubridate::ymd(x)-y+1
                    #集計
                    data7.1<-data7%>%
                        filter(Fixed_Date>=date,Fixed_Date<=lubridate::ymd(x))%>%
                        group_by(Residential_City,X,Y)%>%
                        summarise(count=sum(n))%>%
                        filter(X>0,Y>0)%>%
                      full_join(xy,by=c("Residential_City"="City"))%>%
                      mutate(count=ifelse(is.na(X),0,count))
                    jinko2<-left_join(data7.1,jinko,by=c("Residential_City"="City"))
                    jinko3<-jinko2%>%
                        mutate(count_j=count/jinko*100000)%>%
                      mutate(N03_004=Residential_City)
                    
                    data7.2<-
                      sp::merge(shp, jinko3,
                                by="N03_004", all=F,duplicateGeoms = TRUE)
                    #色設定
                    pal <- colorNumeric(palette=c("white","red"),domain=c(0,input$color*y), reverse=F)
                    pal2<-
                      data7.2%>%
                      mutate(col=pal(count_j),
                             col2=ifelse(count_j>input$color*y,"red",col),
                             flag=ifelse(count_j>input$color,paste0(input$color*y,"~"),paste0(count_j%/%10*10,"~")))
                    data7.2%>%
                      leaflet() %>%
                      setView(lng=139.424343, lat=35.417843,zoom=10)%>%
                      #fitBounds(lng1=139.124343, lat1=35.117843, lng2=139.652899, lat2=35.665052)%>% 
                      addProviderTiles(providers$CartoDB.Positron) %>% 
                      addPolygons(fillOpacity = 1,
                                  weight=1,
                                  color = "#666",
                                  #labelOptions = labelOptions(noHide = T, textOnly = TRUE),
                                  fillColor = ~pal2$col2,
                                  label = paste0(data7.2$N03_004,round(data7.2$count_j,2))
                                  )%>%
                      addLegend(data=pal2%>%distinct(flag,.keep_all = T)%>%arrange(count_j)
                                ,position="bottomright",
                                pal=pal,
                                values = c(0,input$color*y),
                                #color=~col2,labels=~flag,
                                title = "10万人当たりの累積感染者数",opacity = 1,
                                #labFormat = labelFormat(transform = function(x)x*x)
                      )%>%
                      addControl(tags$div(HTML(paste(date,lubridate::ymd(x),sep = "~")))  , position = "topright")
                   
                    }
                )
    }
    )
    }
    )
      