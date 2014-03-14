getFeatureType <-
function(dat) {
	# check if file exists
	if(!file.exists(dat)) stop("data file not found")
	
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
	}
	
	close(con)
	return(ft)
}