## ----setup, include=FALSE---------------------------------
library(rmarkdown)
options(continue=" ")
options(width=60)
library(knitr)
library(geoknife)
query <- geoknife::query
`values<-` <- geoknife::`values<-`
id <- geoknife::id


## ---- eval=FALSE------------------------------------------
#  install.packages("geoknife",
#      repos = c("http://owi.usgs.gov/R","http://cran.rstudio.com/"),
#      dependencies = TRUE)

## ---- eval=FALSE------------------------------------------
#  install.packages("devtools")
#  devtools::install_github('USGS-R/geoknife')

## ---------------------------------------------------------
library(geoknife)

## ---------------------------------------------------------
stencil <- simplegeom(c(-89, 46.23))

## ---------------------------------------------------------
stencil <- simplegeom(data.frame(
              'point1' = c(-89, 46), 
              'point2' = c(-88.6, 45.2)))

## ---- eval=FALSE------------------------------------------
#  stencil <- webgeom('state::New Hampshire')
#  stencil <- webgeom('state::New Hampshire,Wisconsin,Alabama')

## ---- eval=FALSE------------------------------------------
#  stencil <- webgeom('HUC8::09020306,14060009')

## ---- echo=FALSE------------------------------------------
load(system.file('extdata', 'HUC8_stencil.RData', package = 'geoknife'))

## ---------------------------------------------------------
stencil

## ---- eval=FALSE------------------------------------------
#  HUCs <- query(stencil, 'values')

## ---- echo=FALSE------------------------------------------
load(system.file('extdata', 'HUC8_query.RData', package = 'geoknife'))

## ---------------------------------------------------------
head(HUCs) 

## ---------------------------------------------------------
fabric <- webdata('prism')
fabric

## ---------------------------------------------------------
times(fabric) <- c('2002-01-01','2010-01-01')
variables(fabric) <- c('tmx')
fabric

## ---- eval=FALSE------------------------------------------
#  job <- geoknife(stencil, fabric)

## ---- eval=FALSE------------------------------------------
#  check(job)
#  running(job)
#  error(job)
#  successful(job)

## ---- eval=FALSE------------------------------------------
#  job <- cancel(job)

## ---- eval=FALSE------------------------------------------
#  job <- geoknife(stencil, fabric, wait = TRUE)

## ---- fig.height=3.5, fig.width=7, eval=FALSE-------------
#  data <- result(job)
#  plot(data[,1:2], ylab = variables(fabric))

## ---- fig.height=3.5, fig.width=7, echo=FALSE-------------
load(system.file('extdata', 'prism_job.RData', package = 'geoknife'))
plot(data[,1:2], ylab = variables(fabric))

## ---- eval=FALSE------------------------------------------
#  job <- geoknife(webgeom('state::Wisconsin'), fabric = 'prism', email = 'fake.email@gmail.com')

## ---------------------------------------------------------
stencil <- simplegeom(c(-89, 45.43))

## ---------------------------------------------------------
stencil <- simplegeom(data.frame(
              'point1' = c(-89, 46), 
              'point2' = c(-88.6, 45.2)))

## ---------------------------------------------------------
library(sp)
Sr1 = Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
Sr2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
Sr3 = Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
Sr4 = Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4)), hole = TRUE)

Srs1 = Polygons(list(Sr1), "s1")
Srs2 = Polygons(list(Sr2), "s2")
Srs3 = Polygons(list(Sr3, Sr4), "s3/4")
stencil <- simplegeom(Srl = list(Srs1,Srs2,Srs3), proj4string = CRS("+proj=longlat +datum=WGS84"))

## ---------------------------------------------------------
stencil <- webgeom()

## ---------------------------------------------------------
stencil

## ---- eval=FALSE------------------------------------------
#  geom(stencil) <- "derivative:CONUS_States"
#  attribute(stencil) <- "STATE"
#  values(stencil) <- c("Wisconsin","Maine")

## ---- eval=FALSE------------------------------------------
#  stencil <- webgeom('state::Wisconsin')
#  webgeom('state::Wisconsin,Maine')
#  webgeom('HUC8::09020306,14060009')
#  webgeom('ecoregion::Colorado Plateaus,Driftless Area')

## ---- eval=FALSE------------------------------------------
#  query(stencil, 'geoms')

## ---- eval=FALSE------------------------------------------
#  query(stencil, 'attributes')

## ---------------------------------------------------------
fabric <- webdata()

## ---------------------------------------------------------
fabric

## ---------------------------------------------------------
times(fabric)
url(fabric) <- 'http://cida.usgs.gov/thredds/dodsC/prism'
variables(fabric) <- 'tmx'

times(fabric)[1] <- as.POSIXct('1990-01-01')

## ---- eval=FALSE------------------------------------------
#  webdatasets = query('webdata')
#  length(webdatasets)

## ---- echo=FALSE------------------------------------------
load(system.file('extdata', 'webdata_query.RData', package = 'geoknife'))
length(webdatasets)

## ---------------------------------------------------------
webdatasets[61:65]

## ---------------------------------------------------------
title(webdatasets[87])
abstract(webdatasets[87])

## ---------------------------------------------------------
fabric <- webdata(webdatasets[99])
evapotran <- webdata(webdatasets['Monthly Conterminous U.S. actual evapotranspiration data'])

## ---------------------------------------------------------
times(fabric) <- c('1990-01-01','2005-01-01')

## ---- eval=FALSE------------------------------------------
#  query(fabric, 'times')
#  query(fabric, 'variables')

## ---------------------------------------------------------
fabric = webdata(url='dods://apdrc.soest.hawaii.edu/dods/public_data/satellite_product/AVHRR/avhrr_mon')

## ---- eval=FALSE------------------------------------------
#  variables(fabric) <- 'sst'
#  query(fabric, 'times')

## ---------------------------------------------------------
times(fabric) <- c('1990-01-01','1999-12-31')

## ---- eval=FALSE------------------------------------------
#  sst = result(geoknife(data.frame('caspian.sea'=c(51,40)), fabric, wait = TRUE))
#  head(sst)
#  july.idx <- months(sst$DateTime) == 'July'
#  plot(sst$DateTime[july.idx], sst$caspian.sea[july.idx], type='l', lwd=2, col='dodgerblue', ylab='Sea Surface Temperature (degC)',xlab=NA)

## ---- echo=FALSE, fig.height=4, fig.width=6---------------
load(system.file('extdata', 'sst_result.RData', package = 'geoknife'))
head(sst)
july.idx <- months(sst$DateTime) == 'July'
plot(sst$DateTime[july.idx], sst$caspian.sea[july.idx], type='l', lwd=2, col='dodgerblue', ylab='Sea Surface Temperature (degC)',xlab=NA)

## ---- eval=FALSE------------------------------------------
#  fabric = webdata('prism')
#  variables(fabric) <- 'ppt'
#  query(fabric, 'times')

## ---- eval=FALSE------------------------------------------
#  query(fabric, 'variables')

## ---------------------------------------------------------
variables(fabric) <- NA

## ----eval=FALSE-------------------------------------------
#  query(fabric, 'times')

## ---- eval=FALSE------------------------------------------
#  knife <- webprocess()
#  query(knife, 'algorithms')

## ----eval=FALSE-------------------------------------------
#  url(knife) <- 'http://cida-test.er.usgs.gov/gdp/process/WebProcessingService'
#  query(knife, 'algorithms')

## ---- eval=FALSE------------------------------------------
#  knife <- webprocess()
#  algorithm(knife) <- query(knife, 'algorithms')[1]
#  # -- or --
#  algorithm(knife) <- list('Area Grid Statistics (weighted)' =
#                             "gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm")

## ---- eval=FALSE------------------------------------------
#  url(knife) <- 'http://cida-test.er.usgs.gov/gdp/process/WebProcessingService'

## ---- eval=FALSE------------------------------------------
#  knife <- webprocess(wait = TRUE)
#  knife

## ---- eval=FALSE------------------------------------------
#  knife <- webprocess(email = 'fake.email@gmail.com')
#  knife

## ---- eval=FALSE------------------------------------------
#  job <- geoknife(stencil, fabric = 'prism', wait = FALSE)
#  check(job)

## ---- eval=FALSE------------------------------------------
#  running(job)
#  error(job)
#  successful(job)

## ---- eval=FALSE------------------------------------------
#  id(job)

## ---- echo=FALSE------------------------------------------
job <- geojob()

## ---------------------------------------------------------
job <- cancel(job)
id(job)

## ---------------------------------------------------------
cancel()

