## ----init, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE-----------
library(geoknife)

## ----stencil, echo=T, eval=T---------------------------------------------
gconfig(verbose=FALSE)
stencil <- webgeom(url="https://www.sciencebase.gov/catalogMaps/mapping/ows/5b68e7e3e4b006a11f75c06a")

stencil_geoms <- query(stencil, 'geoms')
print(stencil_geoms)

## ----attribute, echo=T, eval=T-------------------------------------------
geom(stencil) <- stencil_geoms[2]

stencil_attributes <- query(stencil, 'attributes')
print(stencil_attributes)

## ----value, echo=T, eval=T-----------------------------------------------
attribute(stencil) <- stencil_attributes[2]

print(query(stencil, 'values'))

## ----fabric, echo=T, eval=T----------------------------------------------
fabric <- webdata(url = 'https://cida.usgs.gov/thredds/dodsC/prism_v2', 
                  variables = c('tmx', 'tmn', 'ppt'),
                  times = as.POSIXct(c('2000-01-01', '2010-01-01')))
print(fabric)

## ----query_fabric, echo=T, eval=T, warning=F-----------------------------
fabric_variables <- query(fabric, "variables")
fabric_times <- query(fabric, "times")

print(paste(fabric@url, "has", 
            paste(fabric_variables, collapse = ", "), 
            "variables for the time range", fabric_times[1], 
            "to", fabric_times[2]))

## ----execute, echo=T, eval=T---------------------------------------------
prism_yahara_result <- result(geoknife(stencil, fabric, wait = TRUE))

## ----print_execute, echo=F, eval=T---------------------------------------
print(paste0("The returned dataframe has ", 
             ncol(prism_yahara_result), 
             " columns with names '", 
             paste(names(prism_yahara_result), collapse = "', '"), 
             "' and ", nrow(prism_yahara_result), 
             " rows from ", 
             prism_yahara_result[1,1], 
             " to ", 
             prism_yahara_result[nrow(prism_yahara_result), 1]))

