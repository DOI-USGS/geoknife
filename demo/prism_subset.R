library(geoknife)

fabric <- webdata(url = 'http://cida.usgs.gov/thredds/dodsC/prism')
fabric
query(fabric, 'variables')
variables(fabric) <- 'ppt'

query(fabric, 'times')
times(fabric) <- as.POSIXct(c("1985-01-01","1992-01-01"))

# use ALL variables
variables(fabric) <- query(fabric, 'variables')

stencil <- simplegeom(data.frame('point1'=c(-89, 46), 'point2'=c(-78.6, 42.2)))

knife = webprocess(algorithm = list('OPeNDAP Subset'="gov.usgs.cida.gdp.wps.algorithm.FeatureCoverageOPeNDAPIntersectionAlgorithm"))

job <- geoknife(stencil,fabric, knife, wait = TRUE, OUTPUT_TYPE="netcdf")
file <- download(job, destination = '../prism_data.nc')

knife = webprocess(algorithm = list('OPeNDAP Subset'="gov.usgs.cida.gdp.wps.algorithm.FeatureCoverageOPeNDAPIntersectionAlgorithm"))
job <- geoknife(stencil,fabric, knife, wait = TRUE, OUTPUT_TYPE="geotiff")
file <- download(job, destination = '../prism_data.zip')
