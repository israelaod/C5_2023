
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
#ESCUELAS<- read.csv("ESCUELAS_EDUCACION.csv", header = T,stringsAsFactors = F)
# base<- read.csv("CODELI_presunto_homicidio_robo_auto_colonias_V2.csv", header = T,stringsAsFactors = F)
#totem<-read.csv("TOTEMS_MARATONCDMX_2021.csv", header = T,stringsAsFactors = F)
#STV<-read.csv("STVs_MARATONCDMX_2021.csv", header = T,stringsAsFactors = F)
#COV<-read.csv("TOTSTVS.csv", header = T,stringsAsFactors = F)



#adecuaciones de la base
per<-spTransform(shapefile("AGEB_lisa_value.shp"), "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
base<-per@data
#creamos etiquetas



# Call the color function (colorNumeric) to create a new palette function
factor(per$LISA_CL)
pal_fun <- colorFactor(c("red", "darkblue", "lightblue", "pink", "grey95","gray"), levels = c(1,2,3,4,0,6))
pal_fun(0)

leaflet() %>%
  addPolygons(data=per,
              stroke = FALSE, 
              fillColor = pal_fun(per$LISA_CL),
              fillOpacity = 0.8, smoothFactor = 0.5,
              popup = paste("<b>","AGEB : ","</b>",per$CVE_AGEB,"<br>",
                            "<b>","valor de correlacion : ","</b>",per$LISA_CL,"<br>")) %>%
  addTiles() %>%
  addLegend("bottomleft",  # location
            pal=pal_fun,    # palette function
            values=per$LISA_CL,  # value to be passed to palette function
            title = 'CORRELACION ESPACIAL HOMICIO Y NARCOMENUDEO')






#aqui termina





#obtenemos el centroide de el poligono
trueCentroids = gCentroid(per,byid=TRUE)
latlon<-coordinates(trueCentroids)
latlon<-data.frame(latlon)

base2<-cbind.data.frame(base,latlon)

Colonias = per # Leyendo datos espaciales
Datos = base2
SPDF = merge(Colonias, Datos) # Uniendo datos
# names (SPDF) # nombres del objeto SPDF
# dim(SPDF)   #--> [1] 666  20
# class(SPDF) #--> "SpatialPolygonsDataFrame" sp
#--------------------------------------------------------------------
# Distribución espacial de la tasa de pobreza.
spplot(SPDF, "HOMICIDIOS", main="Distribución espacial de la tasa de homicidioS \npor AGEB") 




  Estadisticos = gwss(SPDF,
                    vars = c("HOMICIDIOS","NARCOMENUD"), 
                    kernel = "bisquare", # Funcion kernel
                    adaptive = TRUE, # Bw es adaptativo (variable)
                    bw = 48) # Ancho de banda

coords = coordinates(SPDF)

X_nb_queen = poly2nb(SPDF, queen=TRUE)  

# Construyendo lista de vecinos con criterio tipo torre
X_nb_rook = poly2nb(SPDF, queen=FALSE) 


# Construyendo lista de vecinos con k-vecinos más cercanos
IDs = row.names(as(SPDF, "data.frame")) # Id de las filas
X_kn4 = knn2nb(knearneigh(coords, k=4), row.names=IDs) 

par(mfrow=c(1,3))

plot(SPDF, main = "Criterio de contigüidad\n tipo reina")
plot(X_nb_queen, coords, add=T)

plot(SPDF, main = "Criterio de contigüidad\n tipo torre")
plot(X_nb_rook, coords, add=T)

plot(SPDF, main = "Criterio de contigüidad\n 4 vecinos más cercanos")
plot(X_kn4, coords, add=T)





#GRAFICAS----

lm<-Estadisticos$SDF$HOMICIDIOS_LM
per2<-per
per2@data<-cbind.data.frame(per2@data,Estadisticos$SDF$HOMICIDIOS_LM)
per2@data<-cbind.data.frame(per2@data,Estadisticos$SDF$NARCOMENUD_LM)
per2@data<-cbind.data.frame(per2@data,Estadisticos$SDF$HOMICIDIOS_LSD)
per2@data<-cbind.data.frame(per2@data,Estadisticos$SDF$NARCOMENUD_LSD)
per2@data<-cbind.data.frame(per2@data,Estadisticos$SDF$HOMICIDIOS_LVar)
per2@data<-cbind.data.frame(per2@data,Estadisticos$SDF$NARCOMENUD_LVar)
per2@data<-cbind.data.frame(per2@data,Estadisticos$SDF$HOMICIDIOS_LSKe)
per2@data<-cbind.data.frame(per2@data,Estadisticos$SDF$NARCOMENUD_LSKe)
per2@data<-cbind.data.frame(per2@data,Estadisticos$SDF$HOMICIDIOS_LCV)
per2@data<-cbind.data.frame(per2@data,Estadisticos$SDF$NARCOMENUD_LCV)
per2@data<-cbind.data.frame(per2@data,Estadisticos$SDF$Cov_HOMICIDIOS.NARCOMENUD)
per2@data<-cbind.data.frame(per2@data,Estadisticos$SDF$Corr_HOMICIDIOS.NARCOMENUD)
per2@data<-cbind.data.frame(per2@data,Estadisticos$SDF$Spearman_rho_HOMICIDIOS.NARCOMENUD)



polys = list("sp.lines", 
             as(Colonias, "SpatialLines"), 
             col = "black", lwd=1, lty=2)
# Creando una paleta de colores.
col.palette = colorRampPalette(
  c("blue", "sky blue", "green", "yellow", "red"),
  space = "rgb", 
  interpolate = "linear")
# Funcion que grafica las variables
grafica <- function(vble, title) {
  spplot(Estadisticos$SDF, 
         vble, 
         main = title, 
         sp.layout = polys, 
         col = "transparent", 
         col.regions = col.palette(100))
}

library(gridExtra)


#MEDIA LOCAL
x=grafica("HOMICIDIOS_LM","MEDIA LOCAL DE \n HOMICIDIO ML")
y=grafica("NARCOMENUD_LM","MEDIA LOCAL DE NARCOMENUDEO \n ML")
grid.arrange(x,y, nrow=1, ncol=2)




#Desviación estándar local

x=grafica("HOMICIDIOS_LSD","DESVIACIÓN ESTÁNDAR LOCAL DE\n HOMICIDIO DEL")
y=grafica("NARCOMENUD_LSD","DESVIACIÓN ESTÁNDAR LOCAL DE \n NARCOMENUDEO DEL")
grid.arrange(x,y, nrow=1, ncol=2)

X_nb_queen = poly2nb(SPDF, queen=TRUE)  


  #Coeficiente de variación local

library(gridExtra)
x=grafica("HOMICIDIOS_LSD","COEFICIENTE DE VARIACIÓN LOCAL DE PRESUNTO\n HOMICIDIO CVL")
y=grafica("NARCOMENUD_LSD","COEFICIENTE DE VARIACIÓN LOCAL DE \n NARCOMENUDEO CVL")
grid.arrange(x,y, nrow=1, ncol=2)


#Correlación geográficamente ponderada PRESUNTO HOMICIDIO Y ROBO DE VEHICULO CON VIOLENCIA

x=grafica("Corr_HOMICIDIOS.NARCOMENUD","CORRELACIÓN GEOGRÁFICAMENTE DE\n HOMICIDIO\n Y NARCOMENUDEO")
x








#BUFFER<-spTransform(shapefile("AUTODROMO_POLIGONO_BUFF2.shp"), "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

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
year<-as.numeric(substr(Sys.Date(),1,4))
dia<-c(1:(as.numeric(substr(Sys.Date(),9,10))))
mes<-as.numeric(substr(Sys.Date(),6,7))
meses<-c("ENERO","FEBRERO","MARZO","ABRIL","MAYO","JUNIO","JULIO","AGOSTO","SEPTIEMBRE","OCTUBRE","NOVIEMBRE","DICIEMBRE")

title <- tags$div(
  tag.map.title, HTML(paste("ALERTAS SÍSMICAS UNIDADES HABITACIONES" ))
)

#ICONO
ciudad<-iconList(city=makeIcon(iconUrl = "https://i.ibb.co/7131jWL/logo-reducido.png"))



#para legendas personalizadas


colors <- c("RED","GRAY")
labels <- c(paste("ALERTAS SÍSMICAS ","(",nrow(base),")"),  #63+50+70 =183
            "ALCALDÍAS") #36+32+38 =106

sizes <- c(10,10)
shapes <- c("circle","square")
borders <- c("RED","GRAY")

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
  HTML('<a href="https://cran.r-project.org/"> <img border="0" alt="ImageTitle" top= "-50px" src="https://i.ibb.co/p1Gbb5H/imagen.jpg" width="328" height="141"> </a>')
)  


#GENERAMOS MAPA--------------------------------------------------------------------------------------------------------
  leaflet()  %>%addTiles() %>%addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
   # addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  
    addFullscreenControl()%>%clearBounds()%>%
    addControl(title,position = "topleft",className="map-title")%>%
    #addControl(rr,position = "bottomleft")%>% #TABLA DE DATOS
    leafem::addMouseCoordinates() %>% addFullscreenControl(position = "topleft", pseudoFullscreen = F) %>%
    leafem::addLogo(img=ciudad$city,src="remote",position="bottomright",width=403,height=127)%>%
    #  addDrawToolbar(editOptions = editToolbarOptions(selectedPathOptions()),polylineOptions = drawPolylineOptions(metric = T),
    #                circleOptions = F,rectangleOptions = F,circleMarkerOptions = F) %>%
    addResetMapButton()%>%
    #setView(lng = mean(cam1$LONGITUD),lat = mean(cam1$LATITUD),zoom = 15) %>%
    
    addMapPane("polygons",zIndex = 500)%>%
    addMapPane("ce",zIndex = 510)%>%
    addMapPane("li",zIndex=570)%>%
    #addMapPane("col",zIndex = 550)%>%
    addMapPane("lo",zIndex = 580)%>%
    
  
    #addPolygons(data=BUFFER,color = "darkgreen",fillOpacity =0.1,fillColor = "transparent",weight = 3,popup =paste("Buffer Autódromo Hermanos Rodríguez"),
     #           highlightOptions = highlightOptions(color = "darkgreen", weight = 3) , group="Buffer",options = pathOptions(pane="polygons"))%>%
    
  addPolygons(data=per2,color = "GRAY",fillColor = "transparent",fillOpacity =0.01,weight = 2,popup = paste("<b>","AGEB : ","</b>",per2$`Estadisticos$SDF$Spearman_rho_HOMICIDIOS.NARCOMENUD`,"<br>"),
              highlightOptions = highlightOptions(color = "GRAY", weight = 2) , group="AGEB",options = pathOptions(pane="polygons"))%>%
  
  #STVS-------------------------------  

addCircles(data = latlon,lng = latlon$x,lat = latlon$y,color = "RED" ,fillColor = "RED",radius = 10,fillOpacity = T,
           popup = paste("<b>","LONGITUD: ","</b>",as.character(latlon$x),"<br>"),
           group = paste("ALERTAS SÍSMICAS ","(",nrow(latlon),")"),options = pathOptions(pane="li"))%>%
  
  # 
  # addCircles(data = b20t,lng = b20t$LONGITUD,lat = b20t$LATITUD,color = "BLUE" ,fillColor ="BLUE",radius = 10,fillOpacity = T,
  #            popup = paste("<b>","ID: ","</b>",as.character(b20t$ID_BCT_O),"<br>",
  #                          "<b>","TIPO DE POSTE: ","</b>",as.character(b20t$TIPO_POSTE),"<br>",
  #                          "<b>","BOTON: ","</b>",as.character(b20t$BOTON),"<br>",
  #                          "<b>","ALTAVOZ: ","</b>",as.character(b20t$ALTAVOZ),"<br>"),
  #            group = paste("20M","(",nrow(b20t),")"),options = pathOptions(pane="li"))%>%
  # #TOTEMS-------------------
# addCircles(data = totem, lng = totem$LONGITUD, lat = totem$LATITUD,radius = 8,color = "BLACK",fillColor = "BLACK",fillOpacity = T,
#            popup = paste("<b>","ID: ","</b>",(totem$ID_BCT_O),"<br>"), 
#            group = paste("TÓTEMS","(",nrow(totem),")"),options = pathOptions(pane="li"))%>%
#   
# 
# 
# 
#ETIQUETAS----------------------------------------------------------
  addLayersControl(overlayGroups = c( "&nbsp; <b>Capas</b> &nbsp; ",
                                      paste("ALERTAS SÍSMICAS ","(",nrow(base),")"),
                                      "ALCALDÍAS"

                                      ),
                                     options = layersControlOptions(collapsed = T))%>%

  
  htmlwidgets::onRender(jsCode = htmlwidgets::JS("function(btn,map){ 
                                                 var lc=document.getElementsByClassName('leaflet-control-layers-overlays')[0]
                                                 
                                                 lc.getElementsByTagName('input')[0].style.display='none';
                                                
                                                 
                                                 lc.getElementsByTagName('div')[0].style.fontSize='160%';
                                                 lc.getElementsByTagName('div')[0].style.textAlign='center';
                                                 lc.getElementsByTagName('div')[0].style.color='white';
                                                 lc.getElementsByTagName('div')[0].style.backgroundColor='#9F2241';
                                                 
   ;
                                                 }
                                                 ")) %>%
  
  addMarkers(data = base, lng = as.numeric(base$LONGITUDE.PROPUESTA),lat = as.numeric(base$LATITUDE.PROPUESTA), label = c(base$CONS),group = 'CONS')%>%
  #addMarkers(data = totem, lng = as.numeric(totem$LONGITUDE),lat = as.numeric(totem$LATITUDE), label = c(totem$ID_BCT_O),group = 'TOT')%>%
 # addMarkers(data = totem,lng=totem$LONGITUD,lat = totem$LATITUD,label = totem$ID_TECNOLOGIAS,group = "TOT")%>% 
  addSearchFeatures(targetGroups = c("CONS","TOT"),
                    options = searchFeaturesOptions(zoom=20, openPopup = TRUE, firstTipSubmit = TRUE,
                                                    autoCollapse = F, hideMarkerOnCollapse = T,
                                                    textPlaceholder="NOMERO CONSECITIVO"))%>% 
  addLegendCustom(colors, labels, sizes, shapes, borders)%>%  
  #addLegend(position = "bottomleft",labels = c("Menos de 250 personas","250 a 599 personas","600 a 1000 personas","Más de 1000 personas"),colors = c("#FFFF00","#FFFF00","#FF0000","#FF0000"),title = "AFLUENCIA CENTRO HISTÓRICO")%>%
  
  hideGroup(c( "&nbsp; <b>CAPAS</b> &nbsp; ",
               paste("ALERTAS SÍSMICAS ","(",nrow(base),")"),
               "ALCALDÍAS",
               "&nbsp; <b>MI C911E</b> &nbsp; ",
               "TOT","CONS"))
  
  
  #mapa de AGEBs LEAFLET

pal_fun <- colorQuantile("YlOrRd", NULL, n = 5)

leaflet() %>%
  addPolygons(data=per2,
    stroke = FALSE, 
    fillColor = ~pal_fun(per2$`Estadisticos$SDF$Corr_HOMICIDIOS.NARCOMENUD`),
    fillOpacity = 0.8, smoothFactor = 0.5,
    popup = paste("<b>","AGEB : ","</b>",per2$CVE_AGEB,"<br>",
                  "<b>","valor de correlacion : ","</b>",per2$`Estadisticos$SDF$Corr_HOMICIDIOS.NARCOMENUD`,"<br>")) %>%
  addTiles() %>%
  addLegend("bottomleft",  # location
            pal=pal_fun,    # palette function
            values=per2$`Estadisticos$SDF$Spearman_rho_HOMICIDIOS.NARCOMENUD`,  # value to be passed to palette function
            title = 'CORRELACION ESPACIAL HOMICIO Y NARCOMENUDEO')
  
  

#lisa
library(sfweight) # remotes::install_github("Josiahparry/sfweight")

shape <- st_read("AGEB_BAR.shp") # included with sf package

# calucualte the lisa groups
shape_lisa <- shape %>% 
  mutate(nb = st_neighbors(geometry),
         wts = st_weights(nb),
         lag_SID79 = st_lag(SID79, nb, wts),
         lisa = categorize_lisa(SID79, lag_SID79))

# report results
ggplot(data = shape_lisa) +
  geom_sf(aes(fill = lisa))shape <- st_read(system.file("shape/nc.shp", package="sf")) # included with sf package

# calucualte the lisa groups
shape_lisa <- shape %>% 
  mutate(nb = st_neighbors(geometry),
         wts = st_weights(nb),
         lag_SID79 = st_lag(SID79, nb, wts),
         lisa = categorize_lisa(SID79, lag_SID79))

# report results
ggplot(data = shape_lisa) +
  geom_sf(aes(fill = lisa))
  


#remotes::install_github("lixun910/rgeoda")
library(rgeoda)

nat<- st_read("AGEB_BAR.shp")
nat.sp <- as(nat, "Spatial")
class(nat.sp)


# 
library(maptools)
  library(spdep)
library(classInt)# install.packages("classInt")


soco_nbq <- poly2nb(per2)  #queen's neighborhood
soco_nbq_w <- nb2listw(soco_nbq)
locm <- localmoran(per2$HOMICIDIOS, soco_nbq_w)  #calculate the local moran's I
summary(locm)



# manually make a moran plot standarize variables
per2$sPPOV <- scale(per2$HOMICIDIOS)  #save to a new column

# create a lagged variable
per2$lag_sPPOV <- lag.listw(soco_nbq_w, per2$sPPOV)

summary(per2$sPPOV)

plot(x = per2$sPPOV, y = per2$lag_sPPOV, main = " Indice de Moran PPOV")
abline(h = 0, v = 0)
abline(lm(per2$lag_sPPOV ~ per2$sPPOV), lty = 3, lwd = 4, col = "red")

# check out the outliers click on one or twon and then hit escape (or
# click finish)
identify(per2$sPPOV, per2$lab_sPPOV, per2$CNTY_ST, cex = 0.8,labels =labels )


#map
per2$quad_sig <- NA
per2@data[(per2$sPPOV >= 0 & per2$lag_sPPOV >= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 1
per2@data[(per2$sPPOV <= 0 & per2$lag_sPPOV <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 2
per2@data[(per2$sPPOV >= 0 & per2$lag_sPPOV <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 3
per2@data[(per2$sPPOV >= 0 & per2$lag_sPPOV <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 4
per2@data[(per2$sPPOV <= 0 & per2$lag_sPPOV >= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 5 


# Set the breaks for the thematic map classes
breaks <- seq(1, 5, 1)

# Set the corresponding labels for the thematic map classes
labels <- c("high-High", "low-Low", "High-Low", "Low-High", "Not Signif.")

# see ?findInterval - This is necessary for making a map
np <- findInterval(per2$quad_sig, breaks)
factor(per2$quad_sig)

# Assign colors to each map class
colors <- c("red", "blue", "lightpink", "skyblue2", "white")
plot(per2, col = colors[np])  #colors[np] manually sets the color for each county
mtext("Local Moran's I", cex = 1.5, side = 3, line = 1)
legend("topleft", legend = labels, fill = colors, bty = "n")




#nuevo metodo

hunan <- per2

wm_q <- poly2nb(hunan, queen = TRUE)

plot(hunan, border = 'lightgrey')
plot(wm_q, coordinates(hunan), pch = 19, cex = 0.6, add = TRUE, col = "red")


nci <- moran.plot(hunan$HOMICIDIOS, rswm_q, labels = as.character(hunan$CVE_AGEB), xlab = "ageb homicidio 2012", ylab = "Spatially Lag GDPPC 2012")







#proceso julio

# Load libraries ----------------------------------------------------------
install.packages("pacman")

#cargamos las librerias necesarias incluidas las de rgeoda
pacman::p_load(raster, showtext, rgdal, rgeos, stringr, sf, rJava, readxl, tidyverse, fs, gtools, rgeoda)
setwd('/home/jabp/Programacion/C5jabp/analisis lisa-moran/AGEB_BAR/')
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)
dir()

# Add font ----------------------------------------------------------------
font_add_google(family = 'Fira', name = 'Fira code')
font_add_google(family = 'Roboto', name = 'Roboto condensed' )
showtext_auto()

shpf <- 'AGEB_BAR.shp'
shpf <- st_read(shpf)

str(shpf)

colnames(shpf)

#Fuente: http://enrdados.net/post/manual-de-sf-para-sig/

plot(st_geometry(shpf))


#agregamos un identificador a cada columna de shpf
shpf <- mutate(shpf, gid = 1:nrow(shpf))

#peso tipo reyna, i.e con cuantos vecinos limita cada poligono en este caso order = 1 
qnwg <- queen_weights(shpf, order = 1) 
qnwg #MATRIZ DE PESOS



########################################################################
#Realiza el analisis de MORAN de la cantidad de homicidios para el año 2019
morn <- local_moran(qnwg, st_drop_geometry(shpf['NARCOMENUD']));morn
################################################################################

#creamos las etiquetas
mran_lbls <- lisa_labels(morn)
mran_clrs <- setNames(lisa_colors(morn), mran_lbls)

shpf_clst <- shpf %>%
  st_drop_geometry() %>%
  select(gid) %>%
  mutate(cluster_num = lisa_clusters(morn) + 1, # add 1 bc clusters are zero-indexed
         cluster = factor(mran_lbls[cluster_num], levels = mran_lbls)) %>%
  right_join(shpf, by = "gid") %>%
  st_as_sf() %>% 
  st_transform(x = ., crs = st_crs(4326)) #tranformamos el mapa a WGS_1984


# A simple map
ggplot(shpf_clst, aes(fill = cluster)) +
  geom_sf(color = "white", size = 0) +
  scale_fill_manual(values = mran_clrs, na.value = "green") +
  theme_dark()








#bivariable moran----


library(sf)
guerry_path <- system.file("extdata", "Guerry.shp", package = "rgeoda")
guerry <- st_read("C:/Users/iosoriod/Desktop/ARCHIVOS_C5/MORAN_SPEARMAN/AGEB_V1.shp")
queen_w <- queen_weights(guerry)
lisa <- local_bimoran(queen_w, guerry[c('HOMICIDIOS','NARCOMENUD')])
lms <- lisa_values(lisa)
lms

library(dplyr)
library(ggplot2)
library(sf)
library(spdep)
library(rgdal)
library(stringr)
library(UScensus2000tract)

#======================================================
# load data
# data("oregon.tract")

# Variables to use in the correlation: white and black population in each census track
x <- per2$NARCOMENUD
y <- per2$HOMICIDIOS

#======================================================
# Programming some functions

# Bivariate Moran's I
moran_I <- function(x, y = NULL, W){
  if(is.null(y)) y = x
  
  xp <- (x - mean(x, na.rm=T))/sd(x, na.rm=T)
  yp <- (y - mean(y, na.rm=T))/sd(y, na.rm=T)
  W[which(is.na(W))] <- 0
  n <- nrow(W)
  
  global <- (xp%*%W%*%yp)/(n - 1)
  local  <- (xp*W%*%yp)
  
  list(global = global, local  = as.numeric(local))
}


# Permutations for the Bivariate Moran's I
simula_moran <- function(x, y = NULL, W, nsims = 1000){
  
  if(is.null(y)) y = x
  
  n   = nrow(W)
  IDs = 1:n
  
  xp <- (x - mean(x, na.rm=T))/sd(x, na.rm=T)
  W[which(is.na(W))] <- 0
  
  global_sims = NULL
  local_sims  = matrix(NA, nrow = n, ncol=nsims)
  
  ID_sample = sample(IDs, size = n*nsims, replace = T)
  
  y_s = y[ID_sample]
  y_s = matrix(y_s, nrow = n, ncol = nsims)
  y_s <- (y_s - apply(y_s, 1, mean))/apply(y_s, 1, sd)
  
  global_sims  <- as.numeric( (xp%*%W%*%y_s)/(n - 1) )
  local_sims  <- (xp*W%*%y_s)
  
  list(global_sims = global_sims,
       local_sims  = local_sims)
}


#======================================================
# Adjacency Matrix (Queen)

nb <- poly2nb(per2)
lw <- nb2listw(nb, style = "B", zero.policy = T)
W  <- as(lw, "symmetricMatrix")
W  <- as.matrix(W/rowSums(W))
W[which(is.na(W))] <- 0

#======================================================
# Calculating the index and its simulated distribution
# for global and local values

m   <- moran_I(x, y, W)
m[[1]] # global value

m_i <- m[[2]]  # local values

local_sims <- simula_moran(x, y, W)$local_sims

# Identifying the significant values 
alpha <- .1  # for a 95% confidence interval
probs <- c(alpha/2, 1-alpha/2)
intervals <- t( apply(local_sims, 1, function(x) quantile(x, probs=probs)))
sig        <- ( m_i < intervals[,1] )  | ( m_i > intervals[,2] )

#======================================================
# Preparing for plotting

oregon.tract     <- st_as_sf(per2)
oregon.tract$sig <- sig


# Identifying the LISA patterns
xp <- (x-mean(x))/sd(x)
yp <- (y-mean(y))/sd(y)

patterns <- as.character( interaction(xp > 0, W%*%yp > 0) ) 
patterns <- patterns %>% 
  str_replace_all("TRUE","High") %>% 
  str_replace_all("FALSE","Low")
patterns[oregon.tract$sig==0] <- "Not significant"
oregon.tract$patterns <- patterns


# Plotting
ggplot() + geom_sf(data=oregon.tract, aes(fill=patterns), color="NA") +
  scale_fill_manual(values = c("red", "pink", "light blue", "dark blue", "grey95")) + 
  theme_minimal()





#implementacion de mapa bivariable moran ----




factor(base$LISA_CL)
# Call the color function (colorNumeric) to create a new palette function

pal_fun <- colorNumeric(c("red", "pink", "light blue", "dark blue", "grey95"),domain = c(1,2,3,4,0))


per$LISA_CL
pal_fun <- colorFactor(c("red", "pink", "light blue", "dark blue", "grey95"), levels = c(1,2,3,4,0))
pal_fun(0)

pal_fun <- colorQuantile("YlOrRd", NULL, n = 5)

leaflet() %>%
  addPolygons(data=per,
              stroke = FALSE, 
              fillColor = pal_fun(per$LISA_CL),
              fillOpacity = 0.8, smoothFactor = 0.5,
              popup = paste("<b>","AGEB : ","</b>",per$CVE_AGEB,"<br>",
                            "<b>","valor de correlacion : ","</b>",per$LISA_CL,"<br>")) %>%
  addTiles() %>%
  addLegend("bottomleft",  # location
            pal=pal_fun,    # palette function
            values=per$LISA_CL,  # value to be passed to palette function
            title = 'CORRELACION ESPACIAL HOMICIO Y NARCOMENUDEO')
