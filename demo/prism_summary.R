
library(geoknife)

fabric <- webdata('prism')
fabric
times(fabric)[2] <- '2000-01-01' 

stencil <- webgeom('state::Oregon,Colorado,Connecticut')
stencil

job <- geoknife(stencil, fabric, wait = TRUE)

prism_data <- loadOutput(job)
plot(x = prism_data[,1], y = prism_data[['Oregon']], col = 'red', pch = 15, ylab = variables(fabric), xlab = '')
points(x = prism_data[,1], y = prism_data[['Colorado']], col = 'dodgerblue', pch = 4, lwd = 2)
points(x = prism_data[,1], y = prism_data[['Connecticut']], col = 'green', pch = 2, lwd = 2)

