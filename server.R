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
#webshot :: install_phantomjs()



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    #コロナのデータ読み込み
    #data<-fread("https://dl.dropboxusercontent.com/s/6mztoeb6xf78g5w/COVID-19.csv", encoding="UTF-8")
    #修正済みデータの読み込み
    data<-fread("kanagawa.csv", encoding="UTF-8") %>%
      mutate(確定日= as.Date(確定日,format = "%m/%d/%Y"))
    patient<-
      read.csv("https://www.pref.kanagawa.jp/osirase/1369/data/csv/patient.csv") %>%
      filter(!str_detect(居住地,"管内")) %>%
      filter(発表日>="2020-12-01") %>%
      rename("確定日"="発表日","居住市区町村"="居住地") %>%
      mutate(年代 = str_replace(年代,"代","")) %>%
      mutate(居住市区町村 = str_replace(居住市区町村,"神奈川県",""))
    kanagawa<-read.csv("kanagawa2.csv") %>%
      select(-X,-備考)%>%
      filter(!居住市区町村%in%c("横浜市","横須賀市","相模原市","川崎市",
                         "藤沢市"))

    kanagawa2<-rbind(kanagawa,patient) %>%
      mutate(受診都道府県 ="神奈川県",
                   居住都道府県="神奈川県"
                   )
    
    xy<-read.csv("xy.csv") %>%
      select(-X.1)
    
    kanagawa2<-
      left_join(kanagawa2,xy,by="居住市区町村") %>%
      mutate(確定日=as.Date(確定日))
    kawasaki<-
      read.csv("kawasaki.csv") %>%
      select(-X,番号,番号2)%>%
      mutate(確定日=as.Date(確定日))
    
    data<-bind_rows(data,kanagawa2,kawasaki)
    date<-
      kawasaki%>%
      data.frame()%>%
      arrange(desc(確定日))%>%
      distinct(確定日)
    
    output$date<-
      renderUI({
        dateInput("x",
                  label = h5("累積日数の最後の日付入力"),
                  max = date[1,1],
                  value = date[1,1])
      })
    #data$確定日 <- lubridate::mdy(data$確定日)
    data$発症日 <- lubridate::mdy(data$発症日)
    data1<-data%>%
        select(年代,性別,確定日,発症日,受診都道府県,
                 居住都道府県,居住管内,居住市区町村,備考,X,Y)%>%
        filter(居住都道府県=="神奈川県")


    data2<-data%>%
        select(年代,性別,確定日,発症日,受診都道府県,
                 居住都道府県,居住市区町村,備考)%>%
        filter(居住都道府県=="神奈川県")
    list1<-read.csv("list.csv")
    #無理やり結合させたいので
    #川崎市備考NAあり
    #居住市区町村NAの場合に結合
    data3<-data2%>%
        filter(居住市区町村=="")
    list1$list<-as.character(list1$list)
    data5.1<-inner_join(data3,list1,by=c("備考"="list"))
    #川崎市はNAがあるため、埋まっている部分だけ結合
    data4<-data2%>%
        filter(居住市区町村=="川崎市",備考!="")
    data5.2<-inner_join(data4,list1,by=c("備考"="list"))
    #それ以外のデータ
    #川崎市
    data5.3<-data1%>%
        filter(居住市区町村=="川崎市",備考=="")
    #居住市区町村NAでの備考のリストに当てはまらない部分
    data5.4<-data1%>%
        filter(居住市区町村=="")%>%
        anti_join(list1,by=c("備考"="list"))
    #居住市区町村が川崎市以外で""ではない市区町村
    data5.5<-data1%>%
        filter(居住市区町村!=""&居住市区町村!="川崎市")
    #川崎市NA
    data5.6<-data1%>%
      filter(居住市区町村=="川崎市",is.na(備考))
    #data5.1~data5.5まで結合
   
    data6<-bind_rows(data5.1,data5.2,data5.3,data5.4,data5.5,data5.6)
    #居住市区町村と備考と管内を一つにまとめたい
    #居住市区町村""
    data6.1<-data6%>%
        filter(居住市区町村=="")%>%
        mutate(居住市区町村及び管内=管内)%>%
        select(年代,性別,確定日,発症日,受診都道府県,
                 居住都道府県,居住市区町村及び管内,X,Y)
    #川崎市
    data6.2<-data6%>%
        filter(居住市区町村=="川崎市")%>%
        tidyr::unite(col=居住市区町村及び管内,居住市区町村,管内,sep="",remove=T)%>%
        select(年代,性別,確定日,発症日,受診都道府県,
                 居住都道府県,居住市区町村及び管内,X,Y)
    #川崎市以外で居住市区町村があるところ

    data6.3<-data6%>%
        filter(居住市区町村!="",居住市区町村!="川崎市")%>%
        mutate(居住市区町村及び管内=居住市区町村)%>%
        select(年代,性別,確定日,発症日,受診都道府県,
                 居住都道府県,居住市区町村及び管内,X,Y)
    data6.1$居住市区町村及び管内<-as.character(data6.1$居住市区町村及び管内)
    #結合
    data7<-dplyr::bind_rows(data6.1, data6.2,data6.3)

    jinko<-read.csv("jinko.csv")
    jinko<-data.frame(jinko)

    output$covid_map <- renderLeaflet({
        #ここ書き換える
        switch (input$button,
                leaflet1 = { date<-lubridate::ymd(input$x)-input$y
                #集計
                data7.1<-data7%>%
                    filter(確定日>=date,確定日<=lubridate::ymd(input$x))%>%
                    group_by(居住市区町村及び管内,X,Y)%>%
                    summarise(count=n())%>%
                    filter(X>0,Y>0)

                #leafletの可視化
                leaflet(data7.1) %>% addTiles() %>%
                    addProviderTiles(providers$CartoDB.Positron) %>%
                    #setView(lng=139.4725,lat=35.4478,zoom=10)%>%
                    fitBounds(lng1=139.124343, lat1=35.117843, lng2=139.652899, lat2=35.665052)%>%
                    addCircleMarkers(~X, ~Y, stroke=FALSE,
                                     radius =sqrt(data7.1$count)*input$en,
                                     label = ~htmlEscape(居住市区町村及び管内),
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
                        filter(確定日>=date,確定日<=lubridate::ymd(input$x))%>%
                        group_by(居住市区町村及び管内,X,Y)%>%
                        summarise(count=n())%>%
                        filter(X>0,Y>0)
                    jinko2<-left_join(data7.1,jinko,by=c("居住市区町村及び管内"="市区町村"))
                    jinko3<-jinko2%>%
                        mutate(count_j=count/人口*100000)
                    leaflet(jinko3) %>% addTiles() %>%
                        addProviderTiles(providers$CartoDB.Positron) %>%
                        #setView(lng=139.4825,lat=35.4478,zoom=10)%>%
                        fitBounds(lng1=139.124343, lat1=35.117843, lng2=139.652899, lat2=35.665052)%>%
                        addCircleMarkers(~X, ~Y, stroke=FALSE,
                                         radius =sqrt(jinko3$count_j)*input$en,
                                         label = ~htmlEscape(居住市区町村及び管内),
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
      