getFeatureType <-
function(data) {
	
	# if file path is given
	if(is.character(data)) {
		# check if file exists and convert JSON
		if(!file.exists(data)) stop("Data file not found")
		data <- jsonlite::fromJSON(data)
		if(is.null(data$type)) stop("'data' requires GeoJSON or TopoJSON file")
	}
	
	# get FeatureType
	if(tolower(data$type)=="topology") ft <- unique(gsub("multi", "", tolower(data$objects[[1]]$geometries$type))) # TODO: takes first topology object only --> getTopologies
	else ft <- unique(gsub("multi", "", tolower(data$features$geometry$type)))
	
	# check and return
	if(length(ft)>1) stop("File contains different geometry types")
	if(ft=="linestring") ft <- "line"
	return(ft)
}
