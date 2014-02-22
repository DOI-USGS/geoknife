# show method calls print method
setMethod(f = "show",signature = "rGDP",
	definition = function(object){
		print(object)
	}
)
# print method is a pretty print of the rGDP object, excluding hidden elements
setMethod(f = "print",signature = "rGDP",
	function(x,...){
		cat("*** Class rGDP, method Print *** \n")
		cat("* WFS_URL:\t")
		cat(x@WFS_URL,"\n")
		cat("* WPS_URL:\t")
		cat(x@WPS_URL,"\n")
		cat("* algorithm:\t")
		cat(names(x@algorithm),"\n")
		cat("* ------postInputs------\n")
		PI	<-	x@postInputs
		PI[is.na(PI)] = '[optional]'
		nms	<-	names(PI)		
		for (i in 1:length(nms)){
			cat("\t-", nms[i])
			cat(":", PI[[i]], "\n")
		}
		cat("* ------feature------\n")
		PI	<-	x@feature
		nms	<-	names(PI)
		PI[is.na(PI)] = '[optional]'
		for (i in 1:length(nms)){
			# will skip "hidden"
			if (!is.null(PI[[i]]) && PI[[i]]!='hidden'){
				cat("\t-",nms[i])
				cat(":", PI[[i]], "\n")
			} else if (is.null(PI[[i]])){
				cat("\t-", nms[i])
				cat(":", PI[[i]], "\n")
			}
		}
		cat("* processID:\t")
		cat(x@processID,"\n")
		cat("**** End Print (rGDP)**** \n")
	}
)