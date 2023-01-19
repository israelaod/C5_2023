
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


#Mapa de significancia análisis de bivariado de Moran ----
# unique(per$PBI_LISA)
# sum(per$LISA_P>0.05)
#creamos etiquetas PARA REMPLAZAR LOS NUMEROS DEL INDICE BIVARIABLE DE LISA
per$PBI_LISA<-ifelse(per$LISA_P<=0.001 & per$LISA_P>=0.0000001, "P = 0.001",
                    ifelse(per$LISA_P<=0.01 & per$LISA_P>=0.0011, "P = 0.01",
                           ifelse(per$LISA_P<=0.05 & per$LISA_P>=0.011,"P = 0.05", 
                                  ifelse(per$LISA_P>0.05,"NO SIGNIFICATIVO","SIN VECINOS"))))
#sum(per$PBI_LISA=="P = 0.01")
factor(per$PBI_LISA)

# creamos una paleta de colores personalizada 
factor(per$BI_LISA)
pal_sig <- colorFactor(c("grey95", "darkgreen", "#00CD00", "green","gray"), 
                       levels = c("NO SIGNIFICATIVO", "P = 0.001", "P = 0.01", "P = 0.05", "SIN VECINOS"))


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
  tag.map.title, HTML(paste("Mapa de significancia análisis de bivariado de Moran (homicidios vs narcomenudeo) por AGEB "))
)


#ICONO
ciudad<-iconList(city=makeIcon(iconUrl = "https://i.ibb.co/7131jWL/logo-reducido.png"))



#para legendas personalizadas


colors <- c("transparent","#F2F2F2", "darkgreen", "#00CD00", "#7CFC00","gray")
labels <- c("<b>Significancia análisis de bivariado de Moran </b>",
            paste("NO SIGNIFICATIVO","(",sum(per$PBI_LISA=="NO SIGNIFICATIVO"),")"),
            paste("P = 0.001","(",sum(per$PBI_LISA=="P = 0.001"),")"),
            paste("P = 0.01","(",sum(per$PBI_LISA=="P = 0.01"),")"),
            paste("P = 0.05","(",sum(per$PBI_LISA=="P = 0.05"),")"),
            paste("SIN VECINOS","(",sum(per$PBI_LISA=="SIN VECINOS"),")")
            
)
sizes <- c(0,10,10,10,10,10)
shapes <- c("circle","square","square","square","square","square")
borders <- c("transparent","#E5E5E5", "darkgreen", "#00CD00", "#7CFC00","gray")

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
              fillColor = pal_sig(per$PBI_LISA),
              fillOpacity = 0.8, smoothFactor = 0.5,
              popup = paste("<b>","AGEB : ","</b>",per$CVE_AGEB,"<br>",
                            "<b>","VALOR DE SIGNIFICANCIA : ","</b>",per$PBI_LISA,"<br>",
                            "<b>","HOMICIDIOS: ","</b>",per$HOMICIDIOS,"<br>",
                            "<b>","NARCOMENUDEO : ","</b>",per$NARCOMENUD),group="AGEB",options = pathOptions(pane="polygons")) %>%
  
  
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

