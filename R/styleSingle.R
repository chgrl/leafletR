styleSingle <-
function(col, lwd, alpha, fill, fill.alpha, rad) {
	# check arguments
	
	style <- NULL
	if(!missing(col)) {
		if(is.na(col)) style <- append(style, "stroke: false")
		else style <- append(style, paste("color: \"", getHex(col), "\"", sep=""))
	}
	if(!missing(lwd)) style <- append(style, paste("weight:", lwd))
	if(!missing(alpha)) style <- append(style, paste("opacity:", alpha))
	if(!missing(fill)) {
		if(is.na(fill)) style <- append(style, "fill: false")
		else style <- append(style, paste("fillColor: \"", getHex(fill), "\"", sep=""))
	}
	if(!missing(fill.alpha)) style <- append(style, paste("fillOpacity:", fill.alpha))
	if(!missing(rad)) style <- append(style, paste("radius:", rad))

	if(is.null(style)) stop("No style parameters defined")
	attr(style, "style.type") <- "single"
	return(style)
}
