getFeatureType <-
function(dat) {
	# check if file exists
	if(!file.exists(dat)) stop("Data file not found")
	
	suppressWarnings(rjsonio <- require(RJSONIO, quietly=TRUE))
	
	if(rjsonio) {	# RJSONIO package available
		fl <- fromJSON(dat)
		ft <- fl$features[[1]]$geometry$type
  		if(length(fl$features)>1) for(n in 2:length(fl$features)) if(gsub("multi", "", tolower(fl$features[[n]]$geometry$type))!=gsub("multi", "", tolower(ft))) stop("GeoJSON contains different geometry types")
  		if(ft=="Point" || ft=="MultiPoint") ft <- "point"
  		else if(ft=="LineString" || ft=="MultiLineString") ft <- "line"
  		else if(ft=="Polygon" || ft=="MultiPolygon") ft <- "polygon"
  		else stop("Geometry type not recognized")
	} else {	# fallback if RJSONIO package not available
		con <- file(dat, "rt") 
		
		ft <- NULL
		for(l in 1:100) {
			line <- readLines(con, 1)
			if(length(line)==0) break
			if(length(grep("\"type\":\"Point\"", gsub(" ","", line, fixed=TRUE), ignore.case=TRUE))!=0 || length(grep("\"type\":\"MultiPoint\"", gsub(" ","", line, fixed=TRUE), ignore.case=TRUE))!=0) {
				ft <- "point"
				break
			} else if(length(grep("\"type\":\"LineString\"", gsub(" ","", line, fixed=TRUE), ignore.case=TRUE))!=0 || length(grep("\"type\":\"MultiLineString\"", gsub(" ","", line, fixed=TRUE), ignore.case=TRUE))!=0) {
				ft <- "line"
				break
			} else if(length(grep("\"type\":\"Polygon\"", gsub(" ","", line, fixed=TRUE), ignore.case=TRUE))!=0 || length(grep("\"type\":\"MultiPolygon\"", gsub(" ","", line, fixed=TRUE), ignore.case=TRUE))!=0) {
				ft <- "polygon"
				break
			}
			else stop("Geometry type not recognized")
		}
		
		close(con)
	}
	return(ft)
}
