library(geoknife)

stencil <- webgeom('ecoregion::Colorado Plateaus,Driftless Area,Wyoming Basin')
stencil

#see what other ecoregions are available:
query(stencil, 'values')

fabric <- webdata(url = 'http://cida.usgs.gov/thredds/dodsC/iclus/hc') #, variables = 

#see what variables are available:
query(fabric, 'variables')
variables(fabric) <- c('housing_classes_iclus_a1_2010', 'housing_classes_iclus_a1_2100')

knife <- webprocess(algorithm = list('Categorical Coverage Fraction'="gov.usgs.cida.gdp.wps.algorithm.FeatureCategoricalGridCoverageAlgorithm"))
job <- geoknife(stencil, fabric, knife, email = 'your.email.here@gardenmail.com')

check(job)
# wait, or check your email inbox for the result when it has finished