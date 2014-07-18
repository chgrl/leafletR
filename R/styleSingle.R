styleSingle <-
function(col, lwd, alpha, fill, fill.alpha, rad) {
	# check arguments
	
	style <- NULL
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

	if(is.null(style)) stop("No style parameters defined")
	attr(style, "style.type") <- "single"
	class(style) <- c("leafletr.style", "single.style")
	return(style)
}
