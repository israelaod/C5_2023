
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

setwd("C:/Users/iosoriod/Desktop/ARCHIVOS_2023/C5_2023/SECTOR_XOTEPINGO/")
#CARGAR BASES-------------------------------------
incidencia<- read.csv("INCIDEN_v1.csv", header = T,stringsAsFactors = F)
base<- read.csv("COL_EL_RELOJ.csv", header = T,stringsAsFactors = F, fileEncoding = "Latin1", check.names = F)
totem<-read.csv("CÁMARAS/TÓTEMS_MAC.csv", header = T,stringsAsFactors = F)
#COV<-read.csv("COV_ESCUELAS_EDUCACION.csv", header = T,stringsAsFactors = F)

# unique(incidencia$tipo_entrada)

# llamada_911 <-incidencia[which((incidencia$tipo_entra)=="LLAMADA DEL 911"),]
# sos_mujeres <-incidencia[which((incidencia$tipo_entra)=="SOS MUJERES *765"),]


unique(base$TIPO_POSTE)
b9<-base[which((base$TIPO_POSTE)=="9m"),]
b9i<-base[which((base$TIPO_POSTE)=="9mIR"),]
b9<-rbind(b9,b9i)
b20<-base[which((base$TIPO_POSTE)=="20m"),]
b20i<-base[which((base$TIPO_POSTE)=="20mIR"),]
b20<-rbind(b20,b20i)
TOTEM<-base[which((base$TIPO_POSTE)=="TOTEM"),]
# SI<-base[which((base$RENV_TCN)=="SI"),]
# NO<-base[which((base$RENV_TCN)=="NO"),]
# CUARENTA<-base[which((base$PRIORIDAD_40stvS)>0),]



# A2<-shapefile("PERÍMETROS DEL CENTRO HISTÓRICO/VERSION_2/HTML_CAMARAS_POLIGONOS/CH_PERIMETRO_A.shp")
# A2<-spTransform(A2, "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
# 
# B2<-shapefile("PERÍMETROS DEL CENTRO HISTÓRICO/VERSION_2/HTML_CAMARAS_POLIGONOS/CH_PERIMETRO_B.shp")
# B2<-spTransform(B2, "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

SAN_BERNABE <- spTransform(shapefile("POLIGONO_LOMAS_SAN BERNABÉ.shp"), "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
COLONIAS <- spTransform(shapefile("colonias.shp"), "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#Alameda, Centro, Congreso y Morelos
# Alameda<-subset(sectores, sectores$SECTOR=="ALAMEDA")
# Centro<-subset(sectores, sectores$SECTOR=="CENTRO")
# Congreso<-subset(sectores, sectores$SECTOR=="CONGRESO")
DEL_RELOJ<-subset(COLONIAS, COLONIAS$NOM_ASENTA=="EL RELOJ")

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
  tag.map.title, HTML(paste("COBERTURA DE CÁMARAS COLONIA EL RELOJ "))
)


#ICONO
ciudad<-iconList(city=makeIcon(iconUrl = "https://i.ibb.co/7131jWL/logo-reducido.png"))



#para legendas personalizadas


colors <- c("GRAY","BLUE","GREEN","RED")
labels <- c(
            "COLONIA EL RELOJ",
            paste("9m","(",nrow(b9),")"),
            paste("20m","(",nrow(b20),")"),
            paste("TÓTEMS","(",nrow(TOTEM),")")
            )
sizes <- c(10,10,10,10)
shapes <- c("square","circle","circle","circle")
borders <- c("GRAY","BLUE","GREEN","RED")

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

# rr <- tags$div(
# HTML('<img border="0" alt="ImageTitle" top= "-50px" src="https://i.ibb.co/5k5cfm3/Whats-App-Image-2023-01-17-at-3-29-22-PM.jpg" width="343" height="130">')
# )


#GENERAMOS MAPA--------------------------------------------------------------------------------------------------------
leaflet()  %>%addTiles() %>%addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
 # addProviderTiles(providers$OpenStreetMap.Mapnik) %>%

  addFullscreenControl()%>%clearBounds()%>%
  addControl(title,position = "topleft",className="map-title")%>%
  # addControl(rr,position = "bottomleft")%>% #TABLA DE DATOS
  leafem::addMouseCoordinates() %>% addFullscreenControl(position = "topleft", pseudoFullscreen = F) %>%
  leafem::addLogo(img=ciudad$city,src="remote",position="bottomright",width=403,height=127)%>%
  #  addDrawToolbar(editOptions = editToolbarOptions(selectedPathOptions()),polylineOptions = drawPolylineOptions(metric = T),
  #                circleOptions = F,rectangleOptions = F,circleMarkerOptions = F) %>%
  addResetMapButton()%>%
  setView(lng = mean(base$LONGITUD),lat = mean(base$LATITUD),zoom = 16) %>%
  
  addMapPane("polygons",zIndex = 500)%>%
  addMapPane("ce",zIndex = 510)%>%
  addMapPane("li",zIndex=570)%>%
  #addMapPane("col",zIndex = 550)%>%
  addMapPane("lo",zIndex = 580)%>%
  
  # addPolygons(data=SAN_BERNAB,color = "ORANGE",fillColor = "transparent",fillOpacity =0.01,weight = 3,popup = paste("LOMAS SAN BERNABE"),
  #             highlightOptions = highlightOptions(color = "ORANGE", weight = 3) , group="LOMAS DE SAN BERNABE",options = pathOptions(pane="polygons"))%>%
  # 
  addPolygons(data=DEL_RELOJ,color = "GRAY",opacity = 0.9,fillColor = "transparent",fillOpacity =0.01,weight = 3,popup = paste("<b>","COLONIA : ","</b>",DEL_RELOJ$NOM_ASENTA),
              label = DEL_RELOJ$NOM_ASENTA,labelOptions = labelOptions(noHide = T, textOnly = TRUE,direction = "center"),
              highlightOptions = highlightOptions(color = "GRAY", weight = 3) , group="COLONIA EL RELOJ",options = pathOptions(pane="polygons"))%>%
  
# 
#  addPolygons(data=alcaldias ,color = "#7F7F7F",fillColor = "transparent",fillOpacity =0.01,weight = 3,popup = alcaldias$ALCALDIA,
#               highlightOptions = highlightOptions(color = "#7F7F7F", weight = 3) , group="ALCALDÍAS",options = pathOptions(pane="polygons"))%>%
#   
#  addPolygons(data=Alameda,color = "#A6A6A6",fillColor = "#F4A460",fillOpacity =0.5,weight = 3,popup = paste("<b>","SECTOR : ","</b>",Alameda$SECTOR) ,
#              label = Alameda$SECTOR,labelOptions = labelOptions(noHide = T, textOnly = TRUE,direction = "center"),
#               highlightOptions = highlightOptions(color = "#A6A6A6", weight = 3) , group="SECTORES",options = pathOptions(pane="polygons"))%>%
# 
#  addPolygons(data=Centro,color = "#A6A6A6",fillColor = "#FAFAD2",fillOpacity =0.5,weight = 3,popup = paste("<b>","SECTOR : ","</b>",Centro$SECTOR) ,
#              label = Centro$SECTOR,labelOptions = labelOptions(noHide = T, textOnly = TRUE,direction = "top"),
#               highlightOptions = highlightOptions(color = "#A6A6A6", weight = 3) , group="SECTORES",options = pathOptions(pane="polygons"))%>%
#  
#  addPolygons(data=Congreso,color = "#A6A6A6",fillColor = "#E6E6FA",fillOpacity =0.5,weight = 3,popup = paste("<b>","SECTOR : ","</b>",Congreso$SECTOR) ,
#              label = Congreso$SECTOR,labelOptions = labelOptions(noHide = T, textOnly = TRUE,direction = "center"),
#               highlightOptions = highlightOptions(color = "#A6A6A6", weight = 3) , group="SECTORES",options = pathOptions(pane="polygons"))%>%
# 
#    addPolygons(data=Morelos,color = "#A6A6A6",fillColor = "#90EE90",fillOpacity =0.5,weight = 3,popup = paste("<b>","SECTOR : ","</b>",Morelos$SECTOR) ,
#                label = Morelos$SECTOR,labelOptions = labelOptions(noHide = T, textOnly = TRUE,direction = "center"),
#               highlightOptions = highlightOptions(color = "#A6A6A6", weight = 3) , group="SECTORES",options = pathOptions(pane="polygons"))%>%
#   
  

  
  #STVS-------------------------------  
# 
#   addCircles(data = llamada_911,lng = llamada_911$longitud,lat = llamada_911$latitud,color = "red" ,fillColor = "red",radius = 8,fillOpacity = T,
#            popup = paste("<b>","FOLIO : ","</b>",as.character(llamada_911$folio)),
#            group = paste("LLAMADA DEL 911","(",nrow(llamada_911),")"),options = pathOptions(pane="li"))%>%
#   

    addCircles(data = b9,lng = b9$LONGITUD,lat = b9$LATITUD,color = "blue" ,fillColor = "blue",radius = 6,fillOpacity = T,
               popup = paste("<b>","ID : ","</b>",as.character(b9$ID_BCT_O),"<br>",
                             "<b>","TIPO DE POSTE : ","</b>",as.character(b9$TIPO_POSTE),"<br>",
                             "<b>","ALTAVOZ : ","</b>",as.character(b9$ALTAVOZ),"<br>",
                             "<b>","BOTÓN : ","</b>",as.character(b9$BOTON)),
               group = paste("9m","(",nrow(b9),")"),options = pathOptions(pane="li"))%>%   
  
  addCircles(data = b20,lng = b20$LONGITUD,lat = b20$LATITUD,color = "green" ,fillColor = "green",radius = 6,fillOpacity = T,
               popup = paste("<b>","ID : ","</b>",as.character(b20$ID_BCT_O),"<br>",
                             "<b>","TIPO DE POSTE : ","</b>",as.character(b20$TIPO_POSTE),"<br>",
                             "<b>","ALTAVOZ : ","</b>",as.character(b20$ALTAVOZ),"<br>",
                             "<b>","BOTÓN : ","</b>",as.character(b20$BOTON)),
               group = paste("20m","(",nrow(b20),")"),options = pathOptions(pane="li"))%>%
  
  #TOTEMS-------------------
addCircles(data = TOTEM, lng = TOTEM$LONGITUD, lat = TOTEM$LATITUD,radius = 8,color = "red",fillColor = "red",fillOpacity = T,
           popup = paste("<b>","ID: ","</b>",(TOTEM$ID_BCT_O)), 
           group = paste("TÓTEMS","(",nrow(TOTEM),")"),options = pathOptions(pane="li"))%>%


#ETIQUETAS----------------------------------------------------------
  addLayersControl(overlayGroups = c( "&nbsp; <b>CAPAS</b> &nbsp; ",
                                      "COLONIA EL RELOJ",
                                      paste("9m","(",nrow(b9),")"),
                                      paste("20m","(",nrow(b20),")"),
                                      paste("TÓTEMS","(",nrow(TOTEM),")")
                                      
                                      
                                      ),
                                     options = layersControlOptions(collapsed = T))%>% 
  
  
  htmlwidgets::onRender(jsCode = htmlwidgets::JS("function(btn,map){ 
                                                
                                                 
                                                 var lc=document.getElementsByClassName('leaflet-control-layers-overlays')[0]
                                                 
                                                 lc.getElementsByTagName('input')[0].style.display='none';
                                                 
                                                 lc.getElementsByTagName('div')[0].style.fontSize='100%';
                                                 lc.getElementsByTagName('div')[0].style.textAlign='center';
                                                 lc.getElementsByTagName('div')[0].style.color='white';
                                                 lc.getElementsByTagName('div')[0].style.backgroundColor='#9F2241';
                                                 
                                         
   ;
                                                 }
                                                 ")) %>%
  
  addMarkers(data = base, lng = as.numeric(base$LONGITUD),lat = as.numeric(base$LATITUD), label = c(base$ID_BCT_O),group = 'CONS')%>%
  # addMarkers(data = TOTEM,lng=TOTEM$LONGITUD,lat = TOTEM$LATITUD,label = TOTEM$ID_TECNOLOGIAS,group = "TOT")%>% 
  addSearchFeatures(targetGroups = c("CONS","TOT"),
                    options = searchFeaturesOptions(zoom=20, openPopup = TRUE, firstTipSubmit = TRUE,
                                                    autoCollapse = F, hideMarkerOnCollapse = T,
                                                    textPlaceholder="ID STV"))%>% 
  addLegendCustom(colors, labels, sizes, shapes, borders)%>%  
  #addLegend(position = "bottomleft",labels = c("Menos de 250 personas","250 a 599 personas","600 a 1000 personas","Más de 1000 personas"),colors = c("#FFFF00","#FFFF00","#FF0000","#FF0000"),title = "AFLUENCIA CENTRO HISTÓRICO")%>%
  
  hideGroup(c( "&nbsp; <b>STVS C5</b> &nbsp; ",
               "COLONIA EL RELOJ",
               paste("9m","(",nrow(b9),")"),
               paste("20m","(",nrow(b20),")"),
               paste("TÓTEMS","(",nrow(TOTEM),")"),
               
               "&nbsp; <b>MI C911E</b> &nbsp; ",
               "TOT","CONS"))
  
  
  
  
  
  
  


