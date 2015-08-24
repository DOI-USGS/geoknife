library(geoknife)

fabric <- webdata('prism')
fabric

#see what times are avaiable:
query(fabric, 'times')

#set times to a different value:
times(fabric)[2] <- '1900-01-01' 

stencil <- webgeom('state::Oregon,Colorado,Connecticut')
stencil

#see what other states are available:
query(stencil, 'values')

job <- geoknife(stencil, fabric, wait = TRUE)

prism_data <- result(job)
plot(x = prism_data[,1], y = prism_data[['Oregon']], col = 'red', pch = 15, ylab = variables(fabric), xlab = '')
points(x = prism_data[,1], y = prism_data[['Colorado']], col = 'dodgerblue', pch = 4, lwd = 2)
points(x = prism_data[,1], y = prism_data[['Connecticut']], col = 'green', pch = 2, lwd = 2)

