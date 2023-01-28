
#LIBRERIAS--------
rm(list = ls())
library(cleangeo)
library(deldir)
library(dplyr)
library(foreign)
library(ggmap) 
library(ggplot2) 
library(htmltools)
library(htmlwidgets)
library(leaflet)
library(leaflet.extras)
library(mapproj)
library(maptools)
library(mapview)
library(raster)
library(readr)
library(rgdal)
library(spatstat) 
library(sp) 
library(tidyr)
library(webshot)
library(graphics)
library(leafem)

setwd("C:/Users/iosoriod/Desktop/ARCHIVOS_2023/C5_2023/MAPA_HOMICIDIOS/")
#CARGAR BASES-------------------------------------
#ESCUELAS<- read.csv("ESCUELAS_EDUCACION.csv", header = T,stringsAsFactors = F)
base<- read.csv("STVS_2023.csv", fileEncoding = "Latin1", check.names = F)
totem<- read.csv("TOTEMS_2023.csv", fileEncoding = "Latin1", check.names = F)
homi<-read.csv("C5-Homicidios 2022_HOMOLOGADA.csv", fileEncoding = "Latin1", check.names = F)
#COV<-read.csv("COV_ESCUELAS_EDUCACION.csv", header = T,stringsAsFactors = F)

unique(base$TIPO_POSTE)
b9<-base[which((base$TIPO_POSTE)=="9m"),]
b9ir<-base[which((base$TIPO_POSTE)=="9mIR"),]
b9t<-rbind(b9,b9ir)

b20<-base[which((base$TIPO_POSTE)=="20m"),]
b20ir<-base[which((base$TIPO_POSTE)=="20mIR"),]
b20t<-rbind(b20,b20ir)

colonias <- spTransform(shapefile("colonias.shp"), "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
alcaldias <- spTransform(shapefile("ALCALDIAS.shp"), "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

unique(homi$`HOMOLOGACIÓN_TIPO DE EVENTO`)
arma_fuego <-homi[which((homi$`HOMOLOGACIÓN_TIPO DE EVENTO`)=="HOMICIDIO POR ARMA DE FUEGO"),]
arma_blanca <-homi[which((homi$`HOMOLOGACIÓN_TIPO DE EVENTO`)=="HOMICIDIO POR ARMA BLANCA"),]
homi_doloso_golpes <-homi[which((homi$`HOMOLOGACIÓN_TIPO DE EVENTO`)=="HOMICIDIO DOLOSO POR GOLPES"),]
hallazgo_cadaver <-homi[which((homi$`HOMOLOGACIÓN_TIPO DE EVENTO`)=="HALLAZGO DE CADÁVER"),]
homi_dolso_otras <-homi[which((homi$`HOMOLOGACIÓN_TIPO DE EVENTO`)=="HOMICIDIO DOLOSO POR OTRAS CAUSAS"),]


#Alameda, Centro, Congreso y Morelos
# Alameda<-subset(sectores, sectores$SECTOR=="ALAMEDA")
# Centro<-subset(sectores, sectores$SECTOR=="CENTRO")
# Congreso<-subset(sectores, sectores$SECTOR=="CONGRESO")
# Morelos<-subset(sectores, sectores$SECTOR=="MORELOS")

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: TRANSPARENT;
    font-weight: bold;
    font-size: 25px;
    color: #9F2241
  }
"))
#TITULO-------------------------


title <- tags$div(
  tag.map.title, HTML(paste("Homicidios 2022 "))
)


#ICONO
ciudad<-iconList(city=makeIcon(iconUrl = "https://i.ibb.co/7131jWL/logo-reducido.png"))



#para legendas personalizadas

("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B","#358000","#17BECF")
colors <- c("#7F7F7F","#17BECF","red","#FF7F0E","#358000","#9467BD","blue")
labels <- c(
            "ALCALDÍAS","COLONIAS",
            paste("HOMICIDIO POR ARMA BLANCA","(",nrow(arma_blanca),")"),
            paste("HOMICIDIO POR ARMA DE FUEGO","(",nrow(arma_fuego),")"),
            paste("HOMICIDIO DOLOSO POR OTRAS CAUSAS","(",nrow(homi_dolso_otras),")"),
            paste("HOMICIDIO DOLOSO POR GOLPES","(",nrow(homi_doloso_golpes),")"),
            paste("HALLAZGO DE CADÁVER","(",nrow(hallazgo_cadaver),")"))
sizes <- c(10,10,10,10,10,10,10)
shapes <- c("square","square","circle","circle","circle","circle","circle")
borders <- c("#7F7F7F","#17BECF","red","#FF7F0E","#358000","#9467BD","blue")

addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, opacity = 0.5){
  
  make_shapes <- function(colors, sizes, borders, shapes) {
    shapes <- gsub("circle", "50%", shapes)
    shapes <- gsub("square", "0%", shapes)
    paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
  }
  make_labels <- function(sizes, labels) {
    paste0("<div style='display: inline-block;height: ", 
           sizes, "px;margin-top: 4px;line-height: ", 
           sizes, "px;'>", labels, "</div>")
  }
  
  legend_colors <- make_shapes(colors, sizes, borders, shapes)
  legend_labels <- make_labels(sizes, labels)
  
  return(addLegend(map, colors = legend_colors, labels = legend_labels, opacity = opacity,position = "bottomleft"))
}

#rr <- tags$div(
 # HTML('<a href="https://cran.r-project.org/"> <img border="0" alt="ImageTitle" top= "-50px" src="https://i.ibb.co/544jKfq/tabla2.jpg" width="424" height="345"> </a>')
#)  


#GENERAMOS MAPA--------------------------------------------------------------------------------------------------------
leaflet()  %>%addTiles() %>%addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
 # addProviderTiles(providers$OpenStreetMap.Mapnik) %>%

  addFullscreenControl()%>%clearBounds()%>%
  addControl(title,position = "topleft",className="map-title")%>%
#  addControl(rr,position = "bottomleft")%>% #TABLA DE DATOS
  leafem::addMouseCoordinates() %>% addFullscreenControl(position = "topleft", pseudoFullscreen = F) %>%
  leafem::addLogo(img=ciudad$city,src="remote",position="bottomright",width=403,height=127)%>%
  #  addDrawToolbar(editOptions = editToolbarOptions(selectedPathOptions()),polylineOptions = drawPolylineOptions(metric = T),
  #                circleOptions = F,rectangleOptions = F,circleMarkerOptions = F) %>%
  addResetMapButton()%>%
  setView(lng = mean(homi$`LONGITUD X`),lat = mean(homi$`LATITUD Y`),zoom = 12) %>%
  
  addMapPane("polygons",zIndex = 500)%>%
  addMapPane("ce",zIndex = 510)%>%
  addMapPane("li",zIndex=570)%>%
  #addMapPane("col",zIndex = 550)%>%
  addMapPane("lo",zIndex = 580)%>%
  
   addPolygons(data=alcaldias ,color = "#7F7F7F",fillColor = "transparent",fillOpacity =0.01,weight = 3,popup = alcaldias$ALCALDIA,
              highlightOptions = highlightOptions(color = "#7F7F7F", weight = 3) , group="ALCALDÍAS",options = pathOptions(pane="polygons"))%>%

 addPolygons(data=colonias,color = "#17BECF",fillColor = "#F4A460",fillOpacity =0.005,weight = 3,popup = paste("<b>","COLONIA : ","</b>",colonias$NOM_ASENTA) ,
             labelOptions = labelOptions(noHide = T, textOnly = TRUE,direction = "center"),
              highlightOptions = highlightOptions(color = "#17BECF", weight = 3) , group="COLONIAS",options = pathOptions(pane="polygons"))%>%

  
  #STVS-------------------------------  
  addCircles(data = arma_blanca,lng = arma_blanca$`LONGITUD X`,lat = arma_blanca$`LATITUD Y`,color = "red" ,fillColor = "red",radius = 8,fillOpacity = T,
           popup = paste("<b>","FOLIO : ","</b>",as.character(arma_blanca$FOLIO),"<br>",
                         "<b>","TIPO : ","</b>",as.character(arma_blanca$`HOMOLOGACIÓN_TIPO DE EVENTO`)),
           group = paste("HOMICIDIO POR ARMA BLANCA","(",nrow(arma_blanca),")"),options = pathOptions(pane="li"))%>%
  
  addCircles(data = arma_fuego,lng = arma_fuego$`LONGITUD X`,lat = arma_fuego$`LATITUD Y`,color = "#FF7F0E" ,fillColor = "#FF7F0E",radius = 8,fillOpacity = T,
           popup = paste("<b>","FOLIO : ","</b>",as.character(arma_fuego$FOLIO),"<br>",
                         "<b>","TIPO : ","</b>",as.character(arma_fuego$`HOMOLOGACIÓN_TIPO DE EVENTO`)),
           group = paste("HOMICIDIO POR ARMA DE FUEGO","(",nrow(arma_fuego),")"),options = pathOptions(pane="li"))%>%
  
  addCircles(data = homi_dolso_otras,lng = homi_dolso_otras$`LONGITUD X`,lat = homi_dolso_otras$`LATITUD Y`,color = "#358000" ,fillColor = "#358000",radius = 8,fillOpacity = T,
           popup = paste("<b>","FOLIO : ","</b>",as.character(homi_dolso_otras$FOLIO),"<br>",
                         "<b>","TIPO : ","</b>",as.character(homi_dolso_otras$`HOMOLOGACIÓN_TIPO DE EVENTO`)),
           group = paste("HOMICIDIO DOLOSO POR OTRAS CAUSAS","(",nrow(homi_dolso_otras),")"),options = pathOptions(pane="li"))%>%
  
  addCircles(data = homi_doloso_golpes,lng = homi_doloso_golpes$`LONGITUD X`,lat = homi_doloso_golpes$`LATITUD Y`,color = "#9467BD" ,fillColor = "#9467BD",radius = 8,fillOpacity = T,
           popup = paste("<b>","FOLIO : ","</b>",as.character(homi_doloso_golpes$FOLIO),"<br>",
                         "<b>","TIPO : ","</b>",as.character(homi_doloso_golpes$`HOMOLOGACIÓN_TIPO DE EVENTO`)),
           group = paste("HOMICIDIO DOLOSO POR GOLPES","(",nrow(homi_doloso_golpes),")"),options = pathOptions(pane="li"))%>%
  
  addCircles(data = hallazgo_cadaver,lng = hallazgo_cadaver$`LONGITUD X`,lat = hallazgo_cadaver$`LATITUD Y`,color = "blue" ,fillColor = "blue",radius = 8,fillOpacity = T,
           popup = paste("<b>","FOLIO : ","</b>",as.character(hallazgo_cadaver$FOLIO),"<br>",
                         "<b>","TIPO : ","</b>",as.character(hallazgo_cadaver$`HOMOLOGACIÓN_TIPO DE EVENTO`)),
           group = paste("HALLAZGO DE CADÁVER","(",nrow(hallazgo_cadaver),")"),options = pathOptions(pane="li"))%>%


  # addCircles(data = b20t,lng = b20t$LONGITUD,lat = b20t$LATITUD,color = "blue" ,fillColor = "blue",radius = 8,fillOpacity = T,
  #            popup = paste("<b>","ID: ","</b>",as.character(b20t$ID_BCT_O),"<br>",
  #                          "<b>","TIPO DE POSTE : ","</b>",as.character(b20t$TIPO_POSTE)),
  #            group = paste("20m","(",nrow(b20t),")"),options = pathOptions(pane="li"))%>%
  #   
  #   addCircles(data = b9t,lng = b9t$LONGITUD,lat = b9t$LATITUD,color = "red" ,fillColor ="red",radius = 10,fillOpacity = T,
  #              popup = paste("<b>","ID: ","</b>",as.character(b9t$ID_BCT_O),"<br>",
  #                            "<b>","TIPO DE POSTE: ","</b>",as.character(b9t$TIPO_POSTE),"<br>",
  #                            "<b>","BOTON: ","</b>",as.character(b9t$BOTON),"<br>",
  #                            "<b>","ALTAVOZ: ","</b>",as.character(b9t$ALTAVOZ),"<br>"),
  #              group = paste("9m","(",nrow(b9t),")"),options = pathOptions(pane="li"))%>%

  #TOTEMS-------------------
# addCircles(data = totem, lng = totem$LONGITUD, lat = totem$LATITUD,radius = 8,color = "black",fillColor = "black",fillOpacity = T,
#            popup = paste("<b>","ID: ","</b>",(totem$ID_BCT_O)), 
#            group = paste("TÓTEMS","(",nrow(totem),")"),options = pathOptions(pane="li"))%>%


#ETIQUETAS----------------------------------------------------------
  addLayersControl(overlayGroups = c( "&nbsp; <b>CAPAS</b> &nbsp; ",
                                      "ALCALDÍAS","COLONIAS",
                                      "&nbsp; <b>CLASIFICACIÓN</b> &nbsp; ",
                                      paste("HOMICIDIO POR ARMA BLANCA","(",nrow(arma_blanca),")"),
                                      paste("HOMICIDIO POR ARMA DE FUEGO","(",nrow(arma_fuego),")"),
                                      paste("HOMICIDIO DOLOSO POR OTRAS CAUSAS","(",nrow(homi_dolso_otras),")"),
                                      paste("HOMICIDIO DOLOSO POR GOLPES","(",nrow(homi_doloso_golpes),")"),
                                      paste("HALLAZGO DE CADÁVER","(",nrow(hallazgo_cadaver),")")
                                      
                                      
                                      ),
                                     options = layersControlOptions(collapsed = T))%>% 
  
  
  htmlwidgets::onRender(jsCode = htmlwidgets::JS("function(btn,map){ 
                                                
                                                 
                                                 var lc=document.getElementsByClassName('leaflet-control-layers-overlays')[0]
                                                 
                                                 lc.getElementsByTagName('input')[0].style.display='none';
                                                 lc.getElementsByTagName('input')[3].style.display='none';
                                                 
                                                 lc.getElementsByTagName('div')[0].style.fontSize='100%';
                                                 lc.getElementsByTagName('div')[0].style.textAlign='center';
                                                 lc.getElementsByTagName('div')[0].style.color='white';
                                                 lc.getElementsByTagName('div')[0].style.backgroundColor='#9F2241';
                                                 
                                                 lc.getElementsByTagName('div')[3].style.fontSize='100%';
                                                 lc.getElementsByTagName('div')[3].style.textAlign='center';
                                                 lc.getElementsByTagName('div')[3].style.color='white';
                                                 lc.getElementsByTagName('div')[3].style.backgroundColor='#9F2241';
                                                 
                                         
   ;
                                                 }
                                                 ")) %>%
  
  # addMarkers(data = base, lng = as.numeric(base$LONGITUD),lat = as.numeric(base$LATITUD), label = c(base$ID_BCT_O),group = 'CONS')%>%
  # addMarkers(data = totem,lng=totem$LONGITUD,lat = totem$LATITUD,label = totem$ID_TECNOLOGIAS,group = "TOT")%>% 
  # addSearchFeatures(targetGroups = c("CONS","TOT"),
  #                   options = searchFeaturesOptions(zoom=20, openPopup = TRUE, firstTipSubmit = TRUE,
  #                                                   autoCollapse = F, hideMarkerOnCollapse = T,
  #                                                   textPlaceholder="ID STV"))%>% 
   addLegendCustom(colors, labels, sizes, shapes, borders)%>%  
  #addLegend(position = "bottomleft",labels = c("Menos de 250 personas","250 a 599 personas","600 a 1000 personas","Más de 1000 personas"),colors = c("#FFFF00","#FFFF00","#FF0000","#FF0000"),title = "AFLUENCIA CENTRO HISTÓRICO")%>%
  
  hideGroup(c( "&nbsp; <b>CAPAS</b> &nbsp; ",
               "ALCALDÍAS","COLONIAS",
               "&nbsp; <b>CLASIFICACIÓN</b> &nbsp; ",
               paste("HOMICIDIO POR ARMA BLANCA","(",nrow(arma_blanca),")"),
               paste("HOMICIDIO POR ARMA DE FUEGO","(",nrow(arma_fuego),")"),
               paste("HOMICIDIO DOLOSO POR OTRAS CAUSAS","(",nrow(homi_dolso_otras),")"),
               paste("HOMICIDIO DOLOSO POR GOLPES","(",nrow(homi_doloso_golpes),")"),
               paste("HALLAZGO DE CADÁVER","(",nrow(hallazgo_cadaver),")"),
               
               "&nbsp; <b>MI C911E</b> &nbsp; ",
               "TOT","CONS"))

  
  
  


