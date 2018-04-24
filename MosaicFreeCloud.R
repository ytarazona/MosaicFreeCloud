#' MosaicFreeCloud
#' 
#' @author Yonatan Tarazona Coronel
#' 
#' @description MasaicFreeCloud is an algorithm for the generation of cloud-free images through an indeterminate number of optical images of a single band.  
#' 
#' @param list.r is a list where all the images are.
#' 
#' @param MosFCL It's the raster after taking good pixels from the remaining images
#' 
#' @details Check Readme.md

lista.r<-list.files("~/", ".tif", all.files=T, recursive=T, full.names=F)

## Extracting the dimension and resolution of a raster

raster <- raster(lista.r[1])
ext<- extent(raster@extent[1],raster@extent[2],raster@extent[3],raster@extent[4]) #extension
res<- raster(ext, nrows=raster@nrows, ncols=raster@ncols) # spatial resolution

## Equal dimensions for all images

vfs <- c() # empty vector
for(k in 1:length(lista.r)){
  band <- raster(lista.r[k])
  band.r<-resample(band, res, method="ngb")
  band.r[is.na(band.r) | band.r==-1] <- 0
  vfs <- c(vfs,band.r) # all images with same resolution and extension
}

## Getting the order of the images according to the atmospheric noise

lista <- vfs # copying
suma <- c() # empty vector
for(j in 1:length(lista.r)){
  val <- getValues(lista[[j]])
  sum <- sum(na.omit(val))
  suma <- c(suma, sum)
  orden <- order(suma, decreasing = TRUE)
  bmax <- lista[[orden[1]]] # The most free image of atmospheric noise
}

## Calculating the distances (time) of the selected image with respect to the others
time <- as.numeric(substring(lista.r,10,17)) # from 10 to 17 is the time of acquisition of the image
t.max <- time[orden[1]]
t.dif <- order(abs(t.max - time),decreasing = FALSE)

## Generating the cloud-free mosaic
MosFCL <- bmax # copying
for(l in t.dif){
  MosFCL <- MosFCL + lista[[l]]
  MosFCL[bmax > 0] <- 0
  MosFCL[bmax < 0] <- 0
  MosFCL<- MosFCL+bmax
  bmax <- MosFCL
}
## Assigning NA to pixels equal to 0

MosFCL[MosFCL==0]<-NA

## Assigning projection system to the final mosaic

projection(MosFCL)<-CRS(as.character(NA))
proj <- CRS(projection(raster))
projection(MosFCL)<-proj

## Saving the final mosaic in .tif
writeRaster(MosFCL,'Name.tif',drivername="GTiff", datatype = "FLT4S")

#' @ That's all!
#' 