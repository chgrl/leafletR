getFeatureType <-
function(dat) {
	# check if file exists
	if(!file.exists(dat)) stop("Data file not found")
	
	# get JSON
	fl <- jsonlite::fromJSON(dat)
	
	# get FeatureType
	if(tolower(tail(strsplit(basename(dat), "[.]")[[1]], 1))=="geojson") ft <- unique(gsub("multi", "", tolower(fl$features$geometry$type)))
	else if(tolower(tail(strsplit(basename(dat), "[.]")[[1]], 1))=="json") ft <- unique(gsub("multi", "", tolower(fl$objects[[1]]$geometries$type))) # TODO: takes first topology object only --> getTopologyObjects
	if(length(ft)>1) stop("File contains different geometry types")
	if(ft=="linestring") ft <- "line"
	
	return(ft)
}
