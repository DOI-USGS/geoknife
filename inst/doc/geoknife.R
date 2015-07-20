## ----setup, include=FALSE---------------------------------
library(rmarkdown)
options(continue=" ")
options(width=60)
library(knitr)
library(geoknife)


## ---- eval=FALSE------------------------------------------
#  install.packages("geoknife",
#      repos = c("http://owi.usgs.gov/R"),
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

## ---------------------------------------------------------
stencil <- webgeom('state::New Hampshire')
stencil <- webgeom('state::New Hampshire,Wisconsin,Alabama')

## ---------------------------------------------------------
stencil <- webgeom('HUC8::09020306,14060009')
# display stencil:
stencil

## ---------------------------------------------------------
HUCs <- query(stencil, 'values')
# there are thousands of results, but head() will only display a few of them
head(HUCs) 

## ---------------------------------------------------------
fabric <- webdata('prism')
# display fabric:
fabric

## ---------------------------------------------------------
fabric <- webdata(list(
            times = as.POSIXct(c('1895-01-01','1899-01-01')),
            url = 'http://cida.usgs.gov/thredds/dodsC/prism',
            variables = 'ppt'))

## ---------------------------------------------------------
times(fabric) <- as.POSIXct(c('1990-01-01','2005-01-01'))

## ---- eval=FALSE------------------------------------------
#  query(fabric, 'times')
#  query(fabric, 'variables')

## ---------------------------------------------------------
job <- geoknife(stencil, fabric)

## ---------------------------------------------------------
check(job)
running(job)
error(job)
successful(job)

## ---------------------------------------------------------
job <- cancel(job)

## ---------------------------------------------------------
job <- geoknife(stencil, fabric, wait = TRUE)

## ---- fig.height=3.5, fig.width=7-------------------------
data <- loadOutput(job)
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

## ---------------------------------------------------------
geom(stencil) <- "derivative:CONUS_States"
version(stencil)
attribute(stencil) <- "STATE"
values(stencil) <- c("Wisconsin","Maine")

## ---------------------------------------------------------
stencil <- webgeom('state::Wisconsin')
stencil
query(stencil, 'values')
webgeom('state::Wisconsin,Maine')
webgeom('HUC8::09020306,14060009')
webgeom('ecoregion::Colorado Plateaus,Driftless Area')

head(query(webgeom('ecoregion::Colorado Plateaus,Driftless Area'), 'values'), 10)

## ---------------------------------------------------------
query(stencil, 'geoms')
query(stencil, 'attributes')
query(stencil, 'values')

## ---------------------------------------------------------
fabric <- webdata()

## ---------------------------------------------------------
fabric

## ---------------------------------------------------------
times(fabric)
url(fabric) <- 'http://cida.usgs.gov/thredds/dodsC/prism'
variables(fabric) <- 'ppt'

times(fabric)[1] <- as.POSIXct('1990-01-01')

## ---------------------------------------------------------
fabric <- webdata('prism')
fabric

## ---------------------------------------------------------
times(fabric) <- c('1990-01-01','2010-01-01')
variables(fabric) <- c('ppt','tmx', 'tmn')
fabric

## ---------------------------------------------------------
variables(fabric) <- 'ppt'
query(fabric, 'times')

## ---------------------------------------------------------
query(fabric, 'variables')

## ---------------------------------------------------------
variables(fabric) <- NA

## ----eval=FALSE-------------------------------------------
#  query(fabric, 'times')

## ---------------------------------------------------------
knife <- webprocess()
query(knife, 'algorithms')

## ---------------------------------------------------------
url(knife) <- 'http://cida-test.er.usgs.gov/gdp/process/WebProcessingService'
query(knife, 'algorithms')

## ---------------------------------------------------------
knife <- webprocess()
algorithm(knife)
algorithm(knife) <- query(knife, 'algorithms')[1]
algorithm(knife)
# -- or --
algorithm(knife) <- list('Area Grid Statistics (weighted)' = 
                           "gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm")

## ---------------------------------------------------------
url(knife)
url(knife) <- 'http://cida-test.er.usgs.gov/gdp/process/WebProcessingService'

## ---------------------------------------------------------
knife <- webprocess(wait = TRUE)
knife

## ---------------------------------------------------------
knife <- webprocess(email = 'fake.email@gmail.com')
knife

## ---------------------------------------------------------
job <- geoknife(stencil, fabric = 'prism', wait = FALSE)
check(job)

## ---- eval=FALSE------------------------------------------
#  running(job)
#  error(job)
#  successful(job)

## ---------------------------------------------------------
id(job)
job <- cancel(job)
id(job)

## ---------------------------------------------------------
cancel()

## ---- eval=FALSE------------------------------------------
#  response <- httr::POST(url = 'http://cida.usgs.gov/gdp/geonetwork/srv/en/csw', body = request, content_type_xml())

