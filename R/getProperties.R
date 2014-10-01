getProperties <-
function(data, print=TRUE) {
	
	# if file path is given
	if(is.character(data)) {
		# check if file exists and convert JSON
		if(!file.exists(data)) stop("Data file not found")
		data <- jsonlite::fromJSON(data)
		if(is.null(data$type)) stop("'data' requires GeoJSON or TopoJSON file")
	}
	
	# get properties
	if(tolower(data$type)=="topology") prop <- unique(names(data$objects[[1]]$geometries$properties)) # TODO: takes first topology object only
	else prop <- unique(names(data$features$properties))
	if(is.null(prop)) prop <- NA
	
	# print and return
	if(print) print(prop)
	invisible(prop)
}
