getProperties <-
function(data, print=TRUE) {
	
	# check if file exists and convert GeoJSON
	if(!file.exists(data)) stop("Data file not found")
	tryCatch(json <- fromJSON(data), error=stop("Invalid GeoJSON", call.=FALSE))
	
	# get properties
	prop <- NULL
	for(n in 1:length(json$features)) prop <- append(prop, names(json$features[[n]]$properties))
	prop <- unique(prop)
	
	if(print) print(prop)
	invisible(prop)
}
