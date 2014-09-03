getFeatureType <-
function(dat) {
	# check if file exists
	if(!file.exists(dat)) stop("Data file not found")
	
	# get JSON
	fl <- jsonlite::fromJSON(dat)
	
	# get FeatureType
	ft <- unique(gsub("multi", "", tolower(fl$features$geometry$type)))
	if(length(ft)>1) stop("GeoJSON contains different geometry types")
	if(ft=="linestring") ft <- "line"
	
	return(ft)
}
