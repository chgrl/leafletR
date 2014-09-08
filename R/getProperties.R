getProperties <-
function(data, print=TRUE) {
	
	# check if file exists and convert GeoJSON
	if(!file.exists(data)) stop("Data file not found")
	json <- jsonlite::fromJSON(data)
	#the following drops an error, but why?
	#tryCatch(json <- jsonlite::fromJSON(data), error=stop("Invalid GeoJSON", call.=FALSE))
	
	# get properties
	prop <- unique(names(json$features$properties))
	if(print) print(prop)
	invisible(prop)
}
