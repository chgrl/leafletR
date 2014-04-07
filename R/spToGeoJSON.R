spToGeoJSON <-
function(data, class, name, dest, overwrite) {
	
	path <- paste0(file.path(dest, name), ".geojson")
	if(file.exists(path) && !overwrite) stop("Abort - file already exists\n")
	
	stopifnot(require(sp, quietly=TRUE))
	suppressWarnings(rgdal <- require(rgdal, quietly=TRUE))
    if(rgdal) data=spTransform(data, CRS("+proj=longlat +ellps=WGS84"))
	
	# heading
	cat("{", file=path, sep="\n")
	cat("  \"type\": \"FeatureCollection\",", file=path, append=TRUE, sep="\n")
	cat("  \"features\": [", file=path, append=TRUE, sep="\n")
	
	if(class(data)[1]=="SpatialPoints" || class(data)[1]=="SpatialPointsDataFrame") {	# Points
		# features
		coord <- data@coords
		for(f in 1:nrow(coord)) {
			cat("    {", file=path, append=TRUE, sep="\n")
			cat("      \"type\": \"Feature\",", file=path, append=TRUE, sep="\n")
			
			# properties
			if(class(data)[1]=="SpatialPointsDataFrame") { 
				dat <- data@data
				if(!is.null(dat)) {
					cat("      \"properties\": {", file=path, append=TRUE, sep="\n")
					for(p in 1:length(dat)) {
						cat(paste("        \"", names(dat)[p], "\": \"", dat[f,p], "\"", sep=""), file=path, append=TRUE)
						if(p==length(dat)) cat("\n", file=path, append=TRUE)
						else cat(",", file=path, append=TRUE, sep="\n")
					}
					cat("      },", file=path, append=TRUE, sep="\n")
				}
			}
			
			# geometry
			cat("      \"geometry\": {", file=path, append=TRUE, sep="\n")
			cat("        \"type\": \"Point\",", file=path, append=TRUE, sep="\n")
			cat(paste("        \"coordinates\": [", coord[f,1], ",", coord[f,2], "]", sep=""), file=path, append=TRUE, sep="\n")
			cat("      }", file=path, append=TRUE, sep="\n")
			
			if(f==nrow(data)) cat("    }", file=path, append=TRUE, sep="\n")
			else cat("    },", file=path, append=TRUE, sep="\n")
		}
	} else if(class(data)[1]=="SpatialLines" || class(data)[1]=="SpatialLinesDataFrame") {	# Lines
		# features
		num.f <- length(data@lines)
		f.len <- sapply(slot(data, "lines"), function(x) length(slot(x, "Lines")))
		for(f in 1:num.f) {
			cat("    {", file=path, append=TRUE, sep="\n")
			cat("      \"type\": \"Feature\",", file=path, append=TRUE, sep="\n")
			
			if(class(data)[1]=="SpatialLinesDataFrame") {
				dat <- data@data
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
			}
			
			# geometry
			cat("      \"geometry\": {", file=path, append=TRUE, sep="\n")
			if(f.len[f]==1) {
				cat("        \"type\": \"LineString\",", file=path, append=TRUE, sep="\n")
				coord <- paste0("[", coordinates(data@lines[[f]])[[1]][1,1], ",", coordinates(data@lines[[f]])[[1]][1,2], "]")
				for(i in 2:length(coordinates(data@lines[[f]])[[1]][,1])) coord <- append(coord, paste0("[", coordinates(data@lines[[f]])[[1]][i,1], ",", coordinates(data@lines[[f]])[[1]][i,2], "]"))
				coord <- paste(coord, collapse=", ")
			} else {
				cat("        \"type\": \"MultiLineString\",", file=path, append=TRUE, sep="\n")
				coord <- NULL
				for(l in 1:f.len[f]) {
					ln <- paste0("[", coordinates(data@lines[[f]])[[l]][1,1], ",", coordinates(data@lines[[f]])[[l]][1,2], "]")
					for(i in 2:length(coordinates(data@lines[[f]])[[l]][,1])) ln <- append(ln, paste0("[", coordinates(data@lines[[f]])[[l]][i,1], ",", coordinates(data@lines[[f]])[[l]][i,2], "]"))
					ln <- paste0("[ ", paste(ln, collapse=", "), " ]")
					if(is.null(coord)) coord <- ln
					else coord <- append(coord, ln)
				}
				coord <- paste(coord, collapse=", \n          ")
			}
			cat(paste("        \"coordinates\": [ ", coord, " ]", sep=""), file=path, append=TRUE, sep="\n")
			cat("      }", file=path, append=TRUE, sep="\n")
			
			if(f==num.f) cat("    }", file=path, append=TRUE, sep="\n")
			else cat("    },", file=path, append=TRUE, sep="\n")
		}	
	} else if(class(data)[1]=="SpatialPolygons" || class(data)[1]=="SpatialPolygonsDataFrame") {	# Polygons
	
	}
	
	# close
	cat("  ]", file=path, append=TRUE, sep="\n")
	cat("}", file=path, append=TRUE, sep="\n")
	
	return(path)
}
