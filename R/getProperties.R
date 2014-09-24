getProperties <-
function(data, print=TRUE) {
	
	# check if file exists and convert JSON
	if(!file.exists(data)) stop("Data file not found")
	json <- jsonlite::fromJSON(data)
	#the following drops an error, but why?
	#tryCatch(json <- jsonlite::fromJSON(data), error=stop("Invalid GeoJSON", call.=FALSE))
	
	# get properties
	if(tolower(tail(strsplit(basename(data), "[.]")[[1]], 1))=="geojson") prop <- unique(names(json$features$properties))
	else if(tolower(tail(strsplit(basename(data), "[.]")[[1]], 1))=="json") prop <- unique(names(json$objects[[1]]$geometries$properties)) # TODO: takes first topology object only --> getTopologyObjects
	if(print) print(prop)
	invisible(prop)
}
