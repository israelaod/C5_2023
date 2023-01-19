
#LIBRERIAS  2023----   
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
library(GWmodel)
library(spdep)
library(maptools)
library(GISTools)
library(plotly)
library(rgeoda)


setwd("C:/Users/iosoriod/Desktop/ARCHIVOS_C5/MORAN_SPEARMAN/")
#CARGAR BASES-------------------------------------
per<-spTransform(shapefile("AGEB_lisa_value.shp"), "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
base<-per@data


#creamos etiquetas PARA REMPLAZAR LOS NUMEROS DEL INDICE BIVARIABLE DE LISA
per$BI_LISA<-ifelse(per$LISA_CL=="0", "NO SIGNIFICATIVO",
                ifelse(per$LISA_CL=="1", "ALTO-ALTO",
                       ifelse(per$LISA_CL=="2","BAJO-BAJO", 
                              ifelse(per$LISA_CL=="3","BAJO-ALTO", 
                                     ifelse(per$LISA_CL=="4","ALTO-BAJO","SIN VECINOS")))))


# creamos una paleta de colores personalizada 
factor(per$BI_LISA)
pal_fun <- colorFactor(c("red", "darkblue", "lightblue", "pink", "grey95","gray"), levels = c(1,2,3,4,0,6))

#inicia creacion del mapa


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
  tag.map.title, HTML(paste("Mapa de cluster análisis de bivariado de Moran  "))
)


#ICONO
ciudad<-iconList(city=makeIcon(iconUrl = "https://i.ibb.co/7131jWL/logo-reducido.png"))



#para legendas personalizadas


colors <- c("transparent","red", "darkblue", "lightblue", "pink", "grey95","gray")
labels <- c("<b>Mapa de cluster análisis de bivariado de Moran</b>",
            paste("ALTO-ALTO","(",sum(per$LISA_CL=="1"),")"),
            paste("BAJO-BAJO","(",sum(per$LISA_CL=="2"),")"),
            paste("BAJO-ALTO","(",sum(per$LISA_CL=="3"),")"),
            paste("ALTO-BAJO","(",sum(per$LISA_CL=="4"),")"),
            paste("NO SIGNIFICATIVO",sum(per$LISA_CL=="0"),")"),
            paste("SIN VECINOS","(",sum(per$LISA_CL=="6"),")")
            
)
sizes <- c(0,10,10,10,10,10,10)
shapes <- c("circle","square","square","square","square","square","square")
borders <- c("transparent","red", "darkblue", "lightblue", "pink", "gray65","gray")

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
  # setView(lng = mean(base$LONGITUD),lat = mean(base$LATITUD),zoom = 13) %>%
  
  addMapPane("polygons",zIndex = 500)%>%
  addMapPane("ce",zIndex = 510)%>%
  addMapPane("li",zIndex=570)%>%
  #addMapPane("col",zIndex = 550)%>%
  addMapPane("lo",zIndex = 580)%>%
  addPolygons(data=per ,color = "#7F7F7F",fillColor = "transparent",fillOpacity =0.01,weight = 0.5,
              highlightOptions = highlightOptions(color = "#7F7F7F", weight = 0.5) , group="AGEB",options = pathOptions(pane="polygons"))%>%
  
  
  addPolygons(data=per,
              stroke = FALSE, 
              fillColor = pal_fun(per$LISA_CL),
              fillOpacity = 0.8, smoothFactor = 0.5,
              popup = paste("<b>","AGEB : ","</b>",per$CVE_AGEB,"<br>",
                            "<b>","valor de correlacion : ","</b>",per$BI_LISA,"<br>"),group="AGEB",options = pathOptions(pane="polygons")) %>%
  
  # addPolygons(data=Alameda,color = "#A6A6A6",fillColor = "#F4A460",fillOpacity =0.5,weight = 3,popup = paste("<b>","SECTOR : ","</b>",Alameda$SECTOR) ,
  #             label = Alameda$SECTOR,labelOptions = labelOptions(noHide = T, textOnly = TRUE,direction = "center"),
  #             highlightOptions = highlightOptions(color = "#A6A6A6", weight = 3) , group="SECTORES",options = pathOptions(pane="polygons"))%>%
  # 
  # addPolygons(data=Centro,color = "#A6A6A6",fillColor = "#FAFAD2",fillOpacity =0.5,weight = 3,popup = paste("<b>","SECTOR : ","</b>",Centro$SECTOR) ,
  #             label = Centro$SECTOR,labelOptions = labelOptions(noHide = T, textOnly = TRUE,direction = "top"),
  #             highlightOptions = highlightOptions(color = "#A6A6A6", weight = 3) , group="SECTORES",options = pathOptions(pane="polygons"))%>%
  # 
  # addPolygons(data=Congreso,color = "#A6A6A6",fillColor = "#E6E6FA",fillOpacity =0.5,weight = 3,popup = paste("<b>","SECTOR : ","</b>",Congreso$SECTOR) ,
  #             label = Congreso$SECTOR,labelOptions = labelOptions(noHide = T, textOnly = TRUE,direction = "center"),
  #             highlightOptions = highlightOptions(color = "#A6A6A6", weight = 3) , group="SECTORES",options = pathOptions(pane="polygons"))%>%
  # 
  # addPolygons(data=Morelos,color = "#A6A6A6",fillColor = "#90EE90",fillOpacity =0.5,weight = 3,popup = paste("<b>","SECTOR : ","</b>",Morelos$SECTOR) ,
  #             label = Morelos$SECTOR,labelOptions = labelOptions(noHide = T, textOnly = TRUE,direction = "center"),
  #             highlightOptions = highlightOptions(color = "#A6A6A6", weight = 3) , group="SECTORES",options = pathOptions(pane="polygons"))%>%
  # 
  # 
  
  
  #STVS-------------------------------  
# 
# addCircles(data = base,lng = base$LONGITUD,lat = base$LATITUD,color = "BLACK" ,fillColor = "BLACK",radius = 8,fillOpacity = T,
#            popup = paste("<b>","ID : ","</b>",as.character(base$ID_BCT_O),"<br>",
#                          "<b>","TIPO DE POSTE : ","</b>",as.character(base$TIPO_POSTE)),
#            group = paste("STV´s","(",nrow(base),")"),options = pathOptions(pane="li"))%>%
#   
  # addCircles(data = b20,lng = b20$LONGITUD,lat = b20$LATITUD,color = "blue" ,fillColor = "blue",radius = 8,fillOpacity = T,
  #            popup = paste("<b>","ID: ","</b>",as.character(b20$ID_BCT_O),"<br>",
  #                          "<b>","TIPO DE POSTE : ","</b>",as.character(b20$TIPO_POSTE),"<br>",
  #                          "<b>","FASE ORIGEN : ","</b>",as.character(b20$FASE_ORIGEN),"<br>",
  #                          "<b>","RENV_TCN: ","</b>",as.character(b20$RENV_TCN),"<br>",
  #                          "<b>","PRIORIDAD: ","</b>",as.character(b20$PRIORIDAD)),
  #            group = paste("20M","(",nrow(b20),")"),options = pathOptions(pane="li"))%>%
  # 
  # addCircles(data = SI,lng = SI$LONGITUD,lat = SI$LATITUD,color = "GREEN" ,fillColor = "transparent",radius = 14,fillOpacity = T,
  #            popup = paste("<b>","ID : ","</b>",as.character(SI$ID_BCT_O),"<br>",
#                          "<b>","TIPO DE POSTE : ","</b>",as.character(SI$TIPO_POSTE),"<br>",
#                          "<b>","FASE ORIGEN : ","</b>",as.character(SI$FASE_ORIGEN),"<br>",
#                          "<b>","RENV_TCN: ","</b>",as.character(SI$RENV_TCN),"<br>",
#                          "<b>","PRIORIDAD: ","</b>",as.character(SI$PRIORIDAD)),
#            group = paste("SI","(",nrow(SI),")"),options = pathOptions(pane="li"))%>%
# 
# addCircles(data = NO,lng = NO$LONGITUD,lat = NO$LATITUD,color = "ORANGE" ,fillColor = "transparent",radius = 14,fillOpacity = T,
#            popup = paste("<b>","ID : ","</b>",as.character(NO$ID_BCT_O),"<br>",
#                          "<b>","TIPO DE POSTE : ","</b>",as.character(NO$TIPO_POSTE),"<br>",
#                          "<b>","FASE ORIGEN : ","</b>",as.character(NO$FASE_ORIGEN),"<br>",
#                          "<b>","RENV_TCN: ","</b>",as.character(NO$RENV_TCN),"<br>",
#                          "<b>","PRIORIDAD: ","</b>",as.character(NO$PRIORIDAD)),
#            group = paste("NO","(",nrow(NO),")"),options = pathOptions(pane="li"))%>%

# addCircles(data = CUARENTA,lng = CUARENTA$LONGITUD,lat = CUARENTA$LATITUD,color = "#CD00CD" ,fillColor = "transparent",radius = 16,fillOpacity = T,
#            popup = paste("<b>","ID : ","</b>",as.character(CUARENTA$ID_BCT_O),"<br>",
#                          "<b>","TIPO DE POSTE : ","</b>",as.character(CUARENTA$TIPO_POSTE),"<br>",
#                          "<b>","FASE ORIGEN : ","</b>",as.character(CUARENTA$FASE_ORIGEN),"<br>",
#                          "<b>","RENV_TCN: ","</b>",as.character(CUARENTA$RENV_TCN),"<br>",
#                          "<b>","PRIORIDAD: ","</b>",as.character(CUARENTA$PRIORIDAD)),
#            group = paste("STVS PRIORITARIOS","(",nrow(CUARENTA),")"),options = pathOptions(pane="li"))%>%

#TOTEMS-------------------
# addCircles(data = totem, lng = totem$LONGITUD, lat = totem$LATITUD,radius = 8,color = "BLUE",fillColor = "BLUE",fillOpacity = T,
#            popup = paste("<b>","ID: ","</b>",(totem$ID_BCT_O)), 
#            group = paste("TÓTEMS","(",nrow(totem),")"),options = pathOptions(pane="li"))%>%
#   
#   
  #ETIQUETAS----------------------------------------------------------
addLayersControl(overlayGroups = c( "&nbsp; <b>CAPAS</b> &nbsp; ",
                                    "AGEB"
                                    
                                    
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
  
  # addMarkers(data = base, lng = as.numeric(base$LONGITUD),lat = as.numeric(base$LATITUD), label = c(base$ID_BCT_O),group = 'CONS')%>%
  # addMarkers(data = totem,lng=totem$LONGITUD,lat = totem$LATITUD,label = totem$ID_TECNOLOGIAS,group = "TOT")%>% 
  # addSearchFeatures(targetGroups = c("CONS","TOT"),
  #                   options = searchFeaturesOptions(zoom=20, openPopup = TRUE, firstTipSubmit = TRUE,
  #                                                   autoCollapse = F, hideMarkerOnCollapse = T,
  #                                                   textPlaceholder="ID STV"))%>% 
  addLegendCustom(colors, labels, sizes, shapes, borders)%>%  
  #addLegend(position = "bottomleft",labels = c("Menos de 250 personas","250 a 599 personas","600 a 1000 personas","Más de 1000 personas"),colors = c("#FFFF00","#FFFF00","#FF0000","#FF0000"),title = "AFLUENCIA CENTRO HISTÓRICO")%>%
  
  hideGroup(c( "&nbsp; <b>STVS C5</b> &nbsp; ",
               "AGEB",
               
               "&nbsp; <b>MI C911E</b> &nbsp; ",
               "TOT","CONS"))

