getProperties <-
function(dat) {
	stopifnot(require(RJSONIO))
	
	# check if file exists and validate GeoJSON
	if(!file.exists(dat)) stop("data file not found")	
	if(!isValidJSON(dat)) stop("invalid GeoJSON")
	
	# get properties
	json <- fromJSON(dat)
	prop <- NULL
	for(n in 1:length(json$features)) prop <- append(prop, names(json$features[[n]]$properties))
	prop <- unique(prop)
		
	return(prop)
}