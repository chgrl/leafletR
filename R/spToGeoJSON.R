spToGeoJSON <-
function(data, class, name, dest, overwrite) {
	
	path <- paste0(file.path(dest, name), ".geojson")
	if(file.exists(path) && !overwrite) stop("abort - file already exists\n")
	
	stopifnot(require(sp, quietly=TRUE))
	suppressWarnings(rgdal <- require(rgdal, quietly=TRUE))
    if(rgdal) data=spTransform(data, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
	
	dat <- data@data
	coord <- data@coords
	
	# heading
	cat("{", file=path, sep="\n")
	cat("  \"type\": \"FeatureCollection\",", file=path, append=TRUE, sep="\n")
	cat("  \"features\": [", file=path, append=TRUE, sep="\n")
	
	# features
	for(f in 1:nrow(dat)) {
		cat("    {", file=path, append=TRUE, sep="\n")
		cat("      \"type\": \"Feature\",", file=path, append=TRUE, sep="\n")
		
		# properties
		if(!is.null(dat)) {
			cat("      \"properties\": {", file=path, append=TRUE, sep="\n")
			for(p in 1:length(dat)) {
				cat(paste("        \"", names(dat)[p], "\": \"", dat[f,p], "\"", sep=""), file=path, append=TRUE)
				if(p==length(dat)) cat("\n", file=path, append=TRUE)
				else cat(",", file=path, append=TRUE, sep="\n")
			}
			cat("      },", file=path, append=TRUE, sep="\n")
		}
		
		# geometry
		cat("      \"geometry\": {", file=path, append=TRUE, sep="\n")
		cat("        \"type\": \"Point\",", file=path, append=TRUE, sep="\n")
		cat(paste("        \"coordinates\": [", coord[f,1], ",", coord[f,2], "]", sep=""), file=path, append=TRUE, sep="\n")
		cat("      }", file=path, append=TRUE, sep="\n")
		
		if(f==nrow(data)) cat("    }", file=path, append=TRUE, sep="\n")
		else cat("    },", file=path, append=TRUE, sep="\n")
	}
		
	cat("  ]", file=path, append=TRUE, sep="\n")
	cat("}", file=path, append=TRUE, sep="\n")
	
	return(path)
}
