getFeatureType <-
function(dat) {
	# check if file exists
	if(!file.exists(dat)) stop("Data file not found")
	
	# get JSON
	fl <- fromJSON(dat)
	
	# get FeatureType
	ft <- fl$features[[1]]$geometry$type
	if(length(fl$features)>1) for(n in 2:length(fl$features)) if(gsub("multi", "", tolower(fl$features[[n]]$geometry$type))!=gsub("multi", "", tolower(ft))) stop("GeoJSON contains different geometry types")
	if(ft=="Point" || ft=="MultiPoint") ft <- "point"
	else if(ft=="LineString" || ft=="MultiLineString") ft <- "line"
	else if(ft=="Polygon" || ft=="MultiPolygon") ft <- "polygon"
	else stop("Geometry type not recognized")
	return(ft)
}
