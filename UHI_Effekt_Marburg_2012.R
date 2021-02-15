### Projekt: Globaler Wandel - UHI in Marburg
### Skript zur Berechnung der UHI in Marburg für das Jahr 2012

### einladen der benötigten Pakete
library(rgdal)
library(gdalUtils)
library(raster)
library(rgeos)


#### Verzeichnisse und Pfade einladen und definieren

shapedir <- 'G:/Studium_Geographie/MSc_Physische_Geographie/Kurse/Globaler Wandel/Stadtklima in Marburg/Lokaler_Teil/Verwaltungdsebenen_GADM'
shapefile <- '/Marburg_Stadtgebiet.shp'
lstdir <- "G:/Studium_Geographie/MSc_Physische_Geographie/Kurse/Globaler Wandel/Stadtklima in Marburg/Lokaler_Teil/LST_Daten/2012"
clcdir <- "G:/Studium_Geographie/MSc_Physische_Geographie/Kurse/Globaler Wandel/Stadtklima in Marburg/Lokaler_Teil/CLC/reclass"

outdir <- 'G:/Studium_Geographie/MSc_Physische_Geographie/Kurse/Globaler Wandel/Stadtklima in Marburg/Lokaler_Teil/UHI_Marburg_Ergebnisse/2012/'

LST_Marburg <-raster(paste0(lstdir,"/LST_2012_mean_AOI.tif"))
LST_Marburg
plot(LST_Marburg)

Marburg_shp <- shapefile(paste0(shapedir,shapefile,sep=''))
Marburg_shp
plot(Marburg_shp, add = T)

LUC_Marburg <- raster(paste0(clcdir,"/CLC_2012_reclass.tif"))
LUC_Marburg
plot(LUC_Marburg)

### Buffer um Marburg erstellen | 0.1 Grad sind ungefähr 10 Kilometer (width)

Marburg_shp_buffer <- gBuffer(Marburg_shp, width = 0.1)
Marburg_shp_buffer
plot(Marburg_shp_buffer)
plot(Marburg_shp, add = T)
outfileBuffer <- 'Marburg_2012_buffer.shp'
shapefile(Marburg_shp_buffer,paste0(outdir,outfileBuffer, sep=''), overwrite=TRUE)

### beide shapefiles aneinanderfügen

Marburg_shp_buffer_area <- erase(Marburg_shp_buffer, Marburg_shp)
plot(Marburg_shp_buffer_area)
outfileBufferNew <- 'Marburg_2012_buffer_area_without_Marburg.shp'
shapefile(Marburg_shp_buffer_area,paste0(outdir,outfileBufferNew,sep=''),overwrite=TRUE)

### Der extent wird auf den Buffer rund um das Marburger Stadtgebiet angepasst
### Da beide raster in der selben Auflösung vorliegen müssen (resolution), wird die Auflösung der LST-Daten erhöht.
### Die Methode bilinear als Interpolation ist im Fall von LST-Daten sinnvoller.


#extent(LUC_Marburg) <- extent(LST_Marburg)
LST_Marburg <- resample(LST_Marburg, LUC_Marburg, method = "bilinear")
LUC_Marburg <- crop(LUC_Marburg, extent(Marburg_shp_buffer))
LST_Marburg <- crop(LST_Marburg, extent(Marburg_shp_buffer))
plot(LUC_Marburg)
plot(Marburg_shp_buffer_area, add = T)
plot(LST_Marburg)
plot(Marburg_shp_buffer_area, add = T)

LSTdataCity <- extract(x=LST_Marburg, y=Marburg_shp,df=TRUE)
LSTdataCity

LUCdataCity <- extract(x=LUC_Marburg, y=Marburg_shp,df=TRUE)
LUCdataCity

### mergen der beiden extrahierten DFs 

LST_LUC_Marburg_Daten <- cbind(LSTdataCity, LUCdataCity)
LST_LUC_Marburg_Daten

### 1 = bebaute Flächen, daher werden alle Werte der LST-Daten auf bebauter Fläche extrahiert

LST_City_LUC4 <- which(LST_LUC_Marburg_Daten$CLC_2012_reclass==1)
LST_City_LUC4


LSTMean_City_LUC1_2 <- mean(LST_LUC_Marburg_Daten$LST_2012_mean_AOI[LST_City_LUC4])
LSTMean_City_LUC1_2

### die gemittelte Temperatur aller Pixel mit babauter Fläche beträgt 296.3361 Kelvin

### nun werden die Daten für das Marburger Umland extrahiert

LSTdataRural <- extract(x=LST_Marburg, y=Marburg_shp_buffer_area,df=TRUE)
LSTdataRural

LUCdataRural <- extract(x=LUC_Marburg, y=Marburg_shp_buffer_area,df=TRUE)
LUCdataRural
 
LST_LUC_Data_Rural <- cbind(LSTdataRural, LUCdataRural)
LST_LUC_Data_Rural


LST_Rural_LUC2to4 <- which(LST_LUC_Data_Rural$CLC_2012_reclass > 1)
LST_Rural_LUC2to4


LSTMean_Rural_LUC2to4 <- mean(LST_LUC_Data_Rural$LST_2012_mean_AOI[LST_Rural_LUC2to4], na.rm = TRUE)
LSTMean_Rural_LUC2to4

### die gemittelte Temperatur aller Pixel ohne babauter Fläche beträgt 295.2459 Kelvin

UHIE <- LSTMean_City_LUC1_2 - LSTMean_Rural_LUC2to4
UHIE

### Der UHI-Effekt für Marburg im Sommer 2012 beträgt 1.090183 Kelvin


### City-Bereich aus Raster ausschneiden

CityRaster_LST_tmp <- crop(LST_Marburg, extent(Marburg_shp))
plot(CityRaster_LST_tmp)
CityRaster_LST <- mask(CityRaster_LST_tmp, Marburg_shp)
plot(CityRaster_LST)

CityRaster_LUC_tmp <- crop(LUC_Marburg, extent(Marburg_shp))
plot(CityRaster_LUC_tmp)
CityRaster_LUC <- mask(CityRaster_LUC_tmp, Marburg_shp)
plot(CityRaster_LUC)

outfile_CityLST <- 'LST_Marburg_2012_Stadtgebiet.tif'
outfile_CityRaster <- paste0(outdir,outfile_CityLST,sep = '') 
outfile_CityRaster
writeRaster(CityRaster_LST,outfile_CityRaster, format="GTiff", overwrite=TRUE)

outfile_CityLUC <- 'LUC_Marburg_2012_Stadtgebiet.tif'
outfile_CityRaster <- paste0(outdir,outfile_CityLUC,sep = '') 
outfile_CityRaster
writeRaster(CityRaster_LUC,outfile_CityRaster, format="GTiff", overwrite=TRUE)


###



UHIE_raster1 <- CityRaster_LST - LSTMean_Rural_LUC2to4
plot(Marburg_shp_buffer_area)
plot(UHIE_raster1, add = T)


### Maskiere nicht-urbane Landnutzungsklassen

CityRaster_LUC[CityRaster_LUC > 1] <- NA
plot(CityRaster_LUC)
UHIE_raster2 <- mask(UHIE_raster1, mask=CityRaster_LUC)
plot(Marburg_shp)
plot(UHIE_raster2, add = T)



outfile_City_UHIE <- 'UHIE_Marburg_2012_LUC1_Sommer.tif'
outfile_City_Raster_UHIE <- paste0(outdir,outfile_City_UHIE,sep = '') #siehe "outfile"
outfile_City_Raster_UHIE
writeRaster(UHIE_raster2,outfile_City_Raster_UHIE, format="GTiff", overwrite=TRUE)


outfile_City_all_UHIE <- 'UHIE_Marburg_2012_LUC_all_Sommer.tif'
outfile_City_all_Raster_UHIE <- paste0(outdir,outfile_City_all_UHIE,sep = '') #siehe "outfile"
outfile_City_all_Raster_UHIE
writeRaster(UHIE_raster1,outfile_City_all_Raster_UHIE, format="GTiff", overwrite=TRUE)

