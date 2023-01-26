
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
library(reshape2)
library(Hmisc)
library(stats)
library(kableExtra)


setwd("C:/Users/iosoriod/Desktop/ARCHIVOS_2023/C5_2023/SPEARMAN/")
#CARGAR BASES-------------------------------------
per<-spTransform(shapefile("DAI_2022_AGEBS.shp"), "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
base<-per@data
colnames(base)
base[is.na(base)] <- 0
base2<-base %>% dplyr::select(-"POLY_ID",-"CVEGEO",-"CVE_ENT",-"CVE_MUN",-"CVE_LOC",-"CVE_AGEB",-"area",-"HOMICIDIOS", -"NARCOMENUD", -"LISA_I",    
                       -"LISA_CL",-"LISA_P"     )

prueba1<-base %>% dplyr::select("POLY_ID","HOMICIDIOS", "NARCOMENUD")
# d <- mtcars

class(mtcars)
cormatrix = rcorr(as.matrix(prueba1), type='spearman')
cormatrix$r
cormatrix$P

cordata = melt(cormatrix$r)
ggplot(cordata, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + xlab("") + ylab("")

#
homicidio <- prueba1$HOMICIDIOS
narcomenudeo <- prueba1$NARCOMENUD
scatter <- data.frame(homicidio,narcomenudeo)
ggplot(scatter, aes(x=homicidio, y=narcomenudeo)) +
  geom_point() +
  stat_ellipse(geom = "polygon",type = "norm",
               fill = 4, alpha = 0.25)+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color='red')



corranals <- cor.test(homicidio,narcomenudeo, method = "spearman")
corranals$p.value
corranals$method
corranals$estimate
corranals


cormatrix = rcorr(as.matrix(prueba1), type='spearman')
cormatrix$r
cormatrix$P

cordata = melt(cormatrix$r)
ggplot(cordata, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + xlab("") + ylab("")


#creamos etiquetas PARA REMPLAZAR LOS NUMEROS DEL INDICE BIVARIABLE DE LISA
per$BI_LISA<-ifelse(per$LISA_CL=="0", "NO SIGNIFICATIVO",
                ifelse(per$LISA_CL=="1", "ALTO-ALTO",
                       ifelse(per$LISA_CL=="2","BAJO-BAJO", 
                              ifelse(per$LISA_CL=="3","BAJO-ALTO", 
                                     ifelse(per$LISA_CL=="4","ALTO-BAJO","SIN VECINOS")))))
#creamos poligonos separados segun los rangos 
no_signi<-subset(per, per$BI_LISA=="NO SIGNIFICATIVO")
alto_alto<-subset(per, per$BI_LISA=="ALTO-ALTO")
bajo_bajo<-subset(per, per$BI_LISA=="BAJO-BAJO")
bajo_alto<-subset(per, per$BI_LISA=="BAJO-ALTO")
alto_bajo<-subset(per, per$BI_LISA=="ALTO-BAJO")
sin_vecinos<-subset(per, per$BI_LISA=="SIN VECINOS")

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
borders <- c("transparent","red", "darkblue", "lightblue", "pink", "#E5E5E5","gray")

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

rr <- tags$div(
HTML('<a href="https://i.ibb.co/WDhhdYk/AGEB-lisa-value-Lisa-Scatter-Plot-Frame.png" target="_blank"> <img border="0" alt="ImageTitle" top= "-50px" src="https://i.ibb.co/WDhhdYk/AGEB-lisa-value-Lisa-Scatter-Plot-Frame.png" width="300" height="150"> </a>')
)


#GENERAMOS MAPA--------------------------------------------------------------------------------------------------------
leaflet()  %>%addTiles() %>%addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
  # addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  
  addFullscreenControl()%>%clearBounds()%>%
  addControl(title,position = "topleft",className="map-title")%>%
  addControl(rr,position = "bottomleft")%>% #TABLA DE DATOS
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
  # addPolygons(data=per ,color = "#7F7F7F",fillColor = "transparent",fillOpacity =0.01,weight = 0.5,
  #             highlightOptions = highlightOptions(color = "#7F7F7F", weight = 0.5) , group="AGEB",options = pathOptions(pane="polygons"))%>%
  # 
  
 addPolygons(data=alto_alto,
             color =  "gray" ,weight = 0.5 ,highlightOptions = highlightOptions(color = "gray", weight = 0.5) , 
              fillColor = pal_fun(alto_alto$LISA_CL),
              fillOpacity = 0.8, smoothFactor = 0.5,
              popup = paste("<b>","AGEB : ","</b>",alto_alto$CVE_AGEB,"<br>",
                            "<b>","VALOR DE CORRELACION : ","</b>",alto_alto$BI_LISA,"<br>",
                            "<b>","HOMICIDIOS: ","</b>",alto_alto$HOMICIDIOS,"<br>",
                            "<b>","NARCOMENUDEO : ","</b>",alto_alto$NARCOMENUD),group="ALTO-ALTO",options = pathOptions(pane="polygons")) %>%
  addPolygons(data=bajo_bajo,
              color =  "gray" ,weight = 0.5 ,highlightOptions = highlightOptions(color = "gray", weight = 0.5) , 
              fillColor = pal_fun(bajo_bajo$LISA_CL),
              fillOpacity = 0.8, smoothFactor = 0.5,
              popup = paste("<b>","AGEB : ","</b>",bajo_bajo$CVE_AGEB,"<br>",
                            "<b>","valor de correlacion : ","</b>",bajo_bajo$BI_LISA,"<br>",
                            "<b>","HOMICIDIOS: ","</b>",bajo_bajo$HOMICIDIOS,"<br>",
                            "<b>","NARCOMENUDEO : ","</b>",bajo_bajo$NARCOMENUD),group="BAJO-BAJO",options = pathOptions(pane="polygons")) %>%
    addPolygons(data=bajo_alto,
                color =  "gray" ,weight = 0.5 ,highlightOptions = highlightOptions(color = "gray", weight = 0.5) ,
              fillColor = pal_fun(bajo_alto$LISA_CL),
              fillOpacity = 0.8, smoothFactor = 0.5,
              popup = paste("<b>","AGEB : ","</b>",bajo_alto$CVE_AGEB,"<br>",
                            "<b>","valor de correlacion : ","</b>",bajo_alto$BI_LISA,"<br>",
                            "<b>","HOMICIDIOS: ","</b>",bajo_alto$HOMICIDIOS,"<br>",
                            "<b>","NARCOMENUDEO : ","</b>",bajo_alto$NARCOMENUD),group="BAJO-ALTO",options = pathOptions(pane="polygons")) %>%
      addPolygons(data=alto_bajo,
                  color =  "gray" ,weight = 0.5 ,highlightOptions = highlightOptions(color = "gray", weight = 0.5) ,
              fillColor = pal_fun(alto_bajo$LISA_CL),
              fillOpacity = 0.8, smoothFactor = 0.5,
              popup = paste("<b>","AGEB : ","</b>",alto_bajo$CVE_AGEB,"<br>",
                            "<b>","valor de correlacion : ","</b>",alto_bajo$BI_LISA,"<br>",
                            "<b>","HOMICIDIOS: ","</b>",alto_bajo$HOMICIDIOS,"<br>",
                            "<b>","NARCOMENUDEO : ","</b>",alto_bajo$NARCOMENUD),group="ALTO-BAJO",options = pathOptions(pane="polygons")) %>%
        addPolygons(data=no_signi, 
                    color =  "gray" ,weight = 0.5 ,highlightOptions = highlightOptions(color = "gray", weight = 0.5) ,
              fillColor = pal_fun(no_signi$LISA_CL),
              fillOpacity = 0.8, smoothFactor = 0.5,
              popup = paste("<b>","AGEB : ","</b>",no_signi$CVE_AGEB,"<br>",
                            "<b>","valor de correlacion : ","</b>",no_signi$BI_LISA,"<br>",
                            "<b>","HOMICIDIOS: ","</b>",no_signi$HOMICIDIOS,"<br>",
                            "<b>","NARCOMENUDEO : ","</b>",no_signi$NARCOMENUD),group="NO SIGNIFICATIVO",options = pathOptions(pane="polygons")) %>%
          addPolygons(data=sin_vecinos,
                      color =  "gray" ,weight = 0.5 ,highlightOptions = highlightOptions(color = "gray", weight = 0.5) ,
              fillColor = pal_fun(sin_vecinos$LISA_CL),
              fillOpacity = 0.8, smoothFactor = 0.5,
              popup = paste("<b>","AGEB : ","</b>",sin_vecinos$CVE_AGEB,"<br>",
                            "<b>","valor de correlacion : ","</b>",sin_vecinos$BI_LISA,"<br>",
                            "<b>","HOMICIDIOS: ","</b>",sin_vecinos$HOMICIDIOS,"<br>",
                            "<b>","NARCOMENUDEO : ","</b>",sin_vecinos$NARCOMENUD),group="SIN VECINOS",options = pathOptions(pane="polygons")) %>%
  
    
  #ETIQUETAS----------------------------------------------------------
addLayersControl(overlayGroups = c( "&nbsp; <b>CAPAS</b> &nbsp; ",
                                    "ALTO-ALTO","BAJO-BAJO","BAJO-ALTO","ALTO-BAJO","NO SIGNIFICATIVO","SIN VECINOS"
                                    
                                    
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
               "ALTO-ALTO","BAJO-BAJO","BAJO-ALTO","ALTO-BAJO","NO SIGNIFICATIVO","SIN VECINOS",
               
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

