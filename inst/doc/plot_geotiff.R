## ----setup, include=FALSE---------------------------------
library(rmarkdown)
options(continue=" ")
options(width=60)
library(knitr)
library(geoknife)


## ---- warning=FALSE, message=FALSE------------------------
library(geoknife)

## ---- eval=FALSE------------------------------------------
#  knife <- webprocess(algorithm = list('OPeNDAP Subset'="gov.usgs.cida.gdp.wps.algorithm.FeatureCoverageOPeNDAPIntersectionAlgorithm"))
#  fabric <- webdata(url='dods://opendap.larc.nasa.gov/opendap/hyrax/SortByProduct/CERES/EBAF/Surface_Edition2.8/CERES_EBAF-Surface_Edition2.8_200003-201506.nc',
#                    variable="sfc_sw_down_all_mon", #Surface Shortwave Flux Down, Monthly Means, All-Sky conditions
#                    times=c('2014-07-15','2014-07-15'))
#  stencil <- simplegeom(data.frame('point1' = c(-5,32), 'point2' = c(-90,-78))) # big 'ol chunk 'o data
#  job <- geoknife(stencil, fabric, knife, wait = TRUE, OUTPUT_TYPE = "geotiff")

## ---- eval=FALSE------------------------------------------
#  file <- download(job, destination = file.path(tempdir(), 'nasa_data.zip'), overwrite=TRUE)
#  unzip(file, exdir=file.path(tempdir(),'NASA'))
#  tiff.dir <- file.path(tempdir(),'NASA')

## ---- message=FALSE, echo=FALSE---------------------------
file <- system.file('extdata','nasa_data.zip', package='geoknife')
unzip(file, exdir=file.path(tempdir(),'NASA'))
tiff.dir <- file.path(tempdir(),'NASA')

## ---- warning=FALSE, message=FALSE------------------------
library(rgdal)
library(rasterVis)
nasa <- raster(file.path(tiff.dir , dir(tiff.dir)))

## ---- warning=FALSE, message=FALSE------------------------
library(maps)
library(ggmap)
library(ggplot2)



world <- map_data("world2")
gplot(nasa, maxpixels = 5e5) + 
  geom_tile(aes(fill = value), alpha=1) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), color="navy", fill='transparent') +
  scale_fill_gradientn("Surface Shortwave Flux (W/m^2)", colours=rev(rainbow(5))) +
  coord_equal() + 
  theme_classic() + 
  theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) +
  scale_y_continuous(limits=c(nasa@extent@ymin, nasa@extent@ymax)) + 
  scale_x_continuous(limits=c(nasa@extent@xmin, nasa@extent@xmax))

