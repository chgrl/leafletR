styleSingle <-
function(col, lwd, alpha, fill, fill.alpha, rad, marker) {
	# check arguments
	
	style <- NULL
	if(!missing(marker)) {
		if(length(marker)==1) {
			style <- append(style, paste0("icon: null, color: \"", getHex(marker), "\", size: \"m\""))
		} else if(length(marker)==3) {
			if(is.na(marker[1])) marker[1] <- "null"
			else marker[1] <- paste0("\"", marker[1], "\"")
			if(is.na(marker[2])) marker[2] <- "null"
			else marker[2] <- paste0("\"", getHex(marker[2]), "\"")
			if(is.na(marker[3])) marker[3] <- "\"m\""
			else marker[3] <- paste0("\"", marker[3], "\"")
			style <- append(style, paste0("icon: ", marker[1], ", color: ", marker[2], ", size: ", marker[3]))
		} else stop("Markers are specified by 'icon', 'color' and 'size' or just by 'color' value")
		attr(style, "marker") <- "marker"
	} else {
		if(!missing(col)) {
			if(is.na(col)) style <- append(style, "stroke: false")
			else style <- append(style, paste("color: \"", getHex(col), "\"", sep=""))
		}
		if(!missing(lwd)) {
			if(missing(col)) style <- append(style, paste("weight:", lwd))
			else if(!is.na(col)) style <- append(style, paste("weight:", lwd))
		}
		if(!missing(alpha)) {
			if(missing(col)) style <- append(style, paste("opacity:", alpha))
			else if(!is.na(col)) style <- append(style, paste("opacity:", alpha))
		}
		if(!missing(fill)) {
			if(is.na(fill)) style <- append(style, "fill: false")
			else style <- append(style, paste("fillColor: \"", getHex(fill), "\"", sep=""))
		}
		if(!missing(fill.alpha)) {
			if(missing(fill)) style <- append(style, paste("fillOpacity:", fill.alpha))
			else if(!is.na(fill)) style <- append(style, paste("fillOpacity:", fill.alpha))
		}
		if(!missing(rad)) style <- append(style, paste("radius:", rad))
	}

	if(is.null(style)) stop("No style parameters defined")
	attr(style, "style.type") <- "single"
	class(style) <- c("leafletr.style", "single.style")
	return(style)
}
