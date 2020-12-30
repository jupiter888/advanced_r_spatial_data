library(rgdal); library(raster); library(ncdf4); library(data.table)
setwd("~/R Folder/R_hw_files/R_data/")
getwd()
#Load data of PRAHA_P_shp 
praha_data=readOGR(dsn="PRAHA_P_shp/PRAHA_P.shp", layer="PRAHA_P")
praha_wgs=spTransform(x=praha_data, CRSobj=CRS(projargs="+init=epsg:4326"))

#Download climate models from raw folder
#fls is a
fls=list.files(path="raw/", pattern=".nc", full.names=TRUE)
i=c(1:length(fls))


#pr->precipitation_flux
#for loop for accessing all the files
#seq_along creates a vector of integers of length inside 
l=list()
for(i in seq_along(along.with=fls)){
  nc=nc_open( fls[i] )
  e<-try(
    expr={pr <- ncvar_get(nc=nc, varid="pr")}, silent=TRUE)
  if(inherits(x=e, what="try-error")){
  e <- try(expr={pr <- ncvar_get(nc = nc,varid="precipitation_flux")}, silent=TRUE)
  }
  e <- try(expr={
  lon <- ncvar_get(nc=nc,varid="lon")
  lat <- ncvar_get(nc=nc,varid="lat")
  },silent=TRUE)

  if (inherits(x=e, what="try-error")){
    e <- try(expr={
        lon <- ncvar_get(nc=nc, varid = "longtitude")
        lat <- ncvar_get(nc=nc, varid = "latitude")
      },silent=TRUE)
  }

#create raster then datatable from the raster
b=brick(pr)
plot(b)
extent(b) = c(range(lon), range(lat))
ext = extent(praha_dt_wgs)*1.75
aux <- as.data.table(t(extract(b, ext)))
names(aux) = as.character(cellsFromExtent(b[[i]], ext))
#Date / forcing
n=nchar(fls[i])
d <- strsplit(substr(fls[i], start=n-27, stop=n-3), split="-")
dt <- data.table(date = seq(from = as.POSIXct(x = d[[1]][1],
                                              format="%Y%m%d%H%M"),to=as.POSIXct(x=d[[1]][2],
                                              format="%Y%m%d%H%M"), by="hour"),aux)
#melting data
mdata <- melt(data=dt, id.vars="date",variable.name="id",variable.factor=FALSE)
mdata[, c("lon", "lat") := as.data.frame(x=xyFromCell(object=b[[1]], cell=as.numeric(x=id)))]
l[[i]] = mdata[, forcing := strsplit(substr(fls[i], start=1, stop=n-29), split="-")]
#saving the files
saveRDS(object=mdata, file="spatial_data_file_#")
}#end forloop

#rbind lists
output=rbindlist(l=l)
