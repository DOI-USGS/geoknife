# show method calls print method
setMethod(f = "show",signature = "geoknife",
	definition = function(object){
		print(object)
	}
)
# print method is a pretty print of the geoknife object, excluding hidden elements
setMethod(f = "print",signature = "geoknife",
	function(x,...){
		cat("*** Class geoknife, method Print *** \n")
		cat("* WFS_URL:\t")
		cat(x@WFS_URL,"\n")
		cat("* WPS_URL:\t")
		cat(x@WPS_URL,"\n")
		cat("* algorithm:\t")
		cat(names(x@algorithm),"\n")
		cat("* ------process inputs------\n")
		PI	<-	x@processInputs
		PI[is.na(PI)] = '[optional]'
		nms	<-	names(PI)		
		for (i in 1:length(nms)){
			if (length(PI[[i]])>1){
				cat("\t-",nms[i], ":\n")
				for (j in 1:length(PI[[i]])){
					cat("\t",j,":", PI[[i]][j], "\n")
				}
			} else {
				cat("\t-", nms[i])
				cat(":", PI[[i]], "\n")
			}
			
		}
		cat("* ------feature------\n")
		PI	<-	x@feature
		nms	<-	names(PI)
		PI[is.na(PI)] = '[optional]'
		for (i in 1:length(nms)){
			
			if (is.list(PI[[i]])){
				cat("\t-",nms[i], ":\n")
				for (j in 1:length(PI[[i]])){
					cat("\t",j,":", PI[[i]][[j]], "\n")
				}
				
			} else {
				# will skip "hidden"
				if (!is.null(PI[[i]]) && PI[[i]]!='hidden'){
					cat("\t-",nms[i])
					cat(":", PI[[i]], "\n")
				} else if (is.null(PI[[i]])){
					cat("\t-", nms[i])
					cat(":", PI[[i]], "\n")
				}
			}
			
		}
		cat("* processID:\t")
		cat(x@processID,"\n")
		cat("**** End Print (geoknife)**** \n")
	}
)