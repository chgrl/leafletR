spToGeoJSON <-
function(data, class, name, dest, overwrite) {
	
	path <- paste0(file.path(dest, name), ".geojson")
	if(file.exists(path) && !overwrite) stop("Abort - file already exists")
	
	if(!requireNamespace("sp", quietly=TRUE)) stop("'sp' package required for spatial object conversion")
	if(requireNamespace("rgdal", quietly=TRUE)) data <- sp::spTransform(data, sp::CRS("+proj=longlat +ellps=WGS84"))
	
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
						cat(paste0("        \"", names(dat)[p], "\": \"", dat[f,p], "\""), file=path, append=TRUE)
						if(p==length(dat)) cat("\n", file=path, append=TRUE)
						else cat(",", file=path, append=TRUE, sep="\n")
					}
					cat("      },", file=path, append=TRUE, sep="\n")
				}
			}
			
			# geometry
			cat("      \"geometry\": {", file=path, append=TRUE, sep="\n")
			cat("        \"type\": \"Point\",", file=path, append=TRUE, sep="\n")
			cat(paste0("        \"coordinates\": [", coord[f,1], ",", coord[f,2], "]"), file=path, append=TRUE, sep="\n")
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
			
			# properties
			cat("      \"properties\": {", file=path, append=TRUE, sep="\n")
			if(class(data)[1]=="SpatialLinesDataFrame") {
				dat <- data@data
				if(!is.null(dat)) {	
					for(p in 1:length(dat)) {
						cat(paste0("        \"", names(dat)[p], "\": \"", dat[f,p], "\""), file=path, append=TRUE)
						cat(",", file=path, append=TRUE, sep="\n")
					}
				}
			}
			cat(paste0("        \"ID\": \"", slot(slot(data, "lines")[[f]], "ID"), "\""), file=path, append=TRUE)
			cat("\n      },", file=path, append=TRUE, sep="\n")
			
			# geometry
			cat("      \"geometry\": {", file=path, append=TRUE, sep="\n")
			if(f.len[f]==1) {	# SingleLines
				cat("        \"type\": \"LineString\",", file=path, append=TRUE, sep="\n")
				coord <- paste0("[", sp::coordinates(data@lines[[f]])[[1]][1,1], ",", sp::coordinates(data@lines[[f]])[[1]][1,2], "]")
				for(i in 2:length(sp::coordinates(data@lines[[f]])[[1]][,1])) coord <- append(coord, paste0("[", sp::coordinates(data@lines[[f]])[[1]][i,1], ",", sp::coordinates(data@lines[[f]])[[1]][i,2], "]"))
				coord <- paste(coord, collapse=", ")
			} else {	# MultiLines
				cat("        \"type\": \"MultiLineString\",", file=path, append=TRUE, sep="\n")
				coord <- NULL
				for(l in 1:f.len[f]) {
					ln <- paste0("[", sp::coordinates(data@lines[[f]])[[l]][1,1], ",", sp::coordinates(data@lines[[f]])[[l]][1,2], "]")
					for(i in 2:length(sp::coordinates(data@lines[[f]])[[l]][,1])) ln <- append(ln, paste0("[", sp::coordinates(data@lines[[f]])[[l]][i,1], ",", sp::coordinates(data@lines[[f]])[[l]][i,2], "]"))
					ln <- paste("[", paste(ln, collapse=", "), "]")
					if(is.null(coord)) coord <- ln
					else coord <- append(coord, ln)
				}
				coord <- paste(coord, collapse=", \n          ")
			}
			cat(paste("        \"coordinates\": [", coord, "]"), file=path, append=TRUE, sep="\n")
			cat("      }", file=path, append=TRUE, sep="\n")
			
			if(f==num.f) cat("    }", file=path, append=TRUE, sep="\n")
			else cat("    },", file=path, append=TRUE, sep="\n")
		}	
	} else if(class(data)[1]=="SpatialPolygons" || class(data)[1]=="SpatialPolygonsDataFrame") {	# Polygons
		# features
		num.f <- length(data@polygons)
		f.len <- sapply(slot(data, "polygons"), function(x) length(slot(x, "Polygons")))
		for(f in 1:num.f) {
			cat("    {", file=path, append=TRUE, sep="\n")
			cat("      \"type\": \"Feature\",", file=path, append=TRUE, sep="\n")
			
			# properties
			cat("      \"properties\": {", file=path, append=TRUE, sep="\n")
			if(class(data)[1]=="SpatialPolygonsDataFrame") {
				dat <- data@data
				if(!is.null(dat)) {	
					for(p in 1:length(dat)) {
						cat(paste0("        \"", names(dat)[p], "\": \"", dat[f,p], "\""), file=path, append=TRUE)
						cat(",", file=path, append=TRUE, sep="\n")
					}
				}
			}
			cat(paste0("        \"ID\": \"", slot(slot(data, "polygons")[[f]], "ID"), "\""), file=path, append=TRUE)
			cat("\n      },", file=path, append=TRUE, sep="\n")
			
			# geometry
			cat("      \"geometry\": {", file=path, append=TRUE, sep="\n")
			if(f.len[f]==1) {	# SinglePolygon without holes
				cat("        \"type\": \"Polygon\",", file=path, append=TRUE, sep="\n")
				coord.raw <- slot(slot(slot(data, "polygons")[[f]], "Polygons")[[1]], "coords")
				coord <- paste0("[", coord.raw[1,1], ",", coord.raw[1,2], "]")
				for(i in 2:length(coord.raw[,1])) coord <- append(coord, paste0("[", coord.raw[i,1], ",", coord.raw[i,2], "]"))
				coord <- paste("[", paste(coord, collapse=", "), "]")
			} else {
				hole <- sapply(slot(slot(data, "polygons")[[f]], "Polygons"), function(x) slot(x, "hole"))
				if(length(hole[hole==TRUE])==0) {	# MultiPolygon without holes
					cat("        \"type\": \"MultiPolygon\",", file=path, append=TRUE, sep="\n")
					coord.raw <- lapply(slot(slot(data, "polygons")[[f]], "Polygons"), function(x) slot(x, "coords"))
					coord <- NULL
					for(p in 1:length(coord.raw)) {
						coord.p <- paste0("[", coord.raw[[p]][1,1], ",", coord.raw[[p]][1,2], "]")
						for(i in 2:length(coord.raw[[p]][,1])) coord.p <- append(coord.p, paste0("[", coord.raw[[p]][i,1], ",", coord.raw[[p]][i,2], "]"))
						coord.p <- paste("[", paste(coord.p, collapse=", "), "]")
						if(is.null(coord)) coord <- coord.p
						else coord <- append(coord, coord.p)
					}
					coord <- paste("[", paste(coord, collapse=", \n          "), "]")
				} else { 
					if(length(hole[hole==FALSE])==1) {	# SinglePolygon with hole(s)
						cat("        \"type\": \"Polygon\",", file=path, append=TRUE, sep="\n")
						coord.raw <- lapply(slot(slot(data, "polygons")[[f]], "Polygons"), function(x) slot(x, "coords"))
						pol <- which(hole==FALSE)
						coord <- paste0("[", coord.raw[[pol]][1,1], ",", coord.raw[[pol]][1,2], "]")
						for(i in 2:length(coord.raw[[pol]][,1])) coord <- append(coord, paste0("[", coord.raw[[pol]][i,1], ",", coord.raw[[pol]][i,2], "]"))
						coord <- paste("[", paste(coord, collapse=", "), "]")
						coord.raw[[pol]] <- NULL
						for(p in 1:length(coord.raw)) {
							coord.h <- paste0("[", coord.raw[[p]][1,1], ",", coord.raw[[p]][1,2], "]")
							for(i in 2:length(coord.raw[[p]][,1])) coord.h <- append(coord.h, paste0("[", coord.raw[[p]][i,1], ",", coord.raw[[p]][i,2], "]"))
							coord.h <- paste("[", paste(coord.h, collapse=", "), "]")
							coord <- append(coord, coord.h)
						}
						coord <- paste(coord, collapse=", \n          ")
					} else {	# MultiPolygon with hole(s)
						cat("        \"type\": \"MultiPolygon\",", file=path, append=TRUE, sep="\n")
						coord.raw <- lapply(slot(slot(data, "polygons")[[f]], "Polygons"), function(x) slot(x, "coords"))
						pol <- which(hole==FALSE)
						hol <- which(hole==TRUE)
						hol.idx <- NULL
						for(h in 1:length(hol)) {
							for(p in 1:length(pol)) {
								if(checkPolyHole(coord.raw[[pol[p]]], coord.raw[[hol[h]]])) {
									hol.idx <- append(hol.idx, pol[p])
									break
								}
							}
						}
						if(length(hol.idx)<length(hol)) warning("Warning: one or more holes could not be assigned to polygon", call.=FALSE)
						
						coord <- NULL
						for(p in 1:length(pol)) {
							coord.p <- paste0("[", coord.raw[[pol[p]]][1,1], ",", coord.raw[[pol[p]]][1,2], "]")
							for(i in 2:length(coord.raw[[pol[p]]][,1])) coord.p <- append(coord.p, paste0("[", coord.raw[[pol[p]]][i,1], ",", coord.raw[[pol[p]]][i,2], "]"))
							coord.p <- paste("[", paste(coord.p, collapse=", "), "]")
							
							idx <- which(hol.idx==pol[p])
							if(length(idx)>0) {
								coord.hs <- NULL
								for(i in 1:length(idx)) {
									coord.h <- paste0("[", coord.raw[[hol[i]]][1,1], ",", coord.raw[[hol[i]]][1,2], "]")
									for(j in 2:length(coord.raw[[hol[i]]][,1])) coord.h <- append(coord.h, paste0("[", coord.raw[[hol[i]]][j,1], ",", coord.raw[[hol[i]]][j,2], "]"))
									coord.h <- paste("[", paste(coord.h, collapse=", "), "]")
									if(is.null(coord.hs)) coord.hs <- coord.h
									else coord.hs <- append(coord.hs, coord.h)
								}
								coord.p <- paste0(coord.p, ", \n          ", paste(coord.hs, collapse=", \n          "))
							}
							if(is.null(coord)) coord <- coord.p
							else coord <- append(coord, coord.p)
						}
						coord <- paste("[", paste(coord, collapse=" ], \n          [ "), "]")
					}
				}
			}
			cat(paste("        \"coordinates\": [", coord, "]"), file=path, append=TRUE, sep="\n")
			cat("      }", file=path, append=TRUE, sep="\n")
			
			if(f==num.f) cat("    }", file=path, append=TRUE, sep="\n")
			else cat("    },", file=path, append=TRUE, sep="\n")
		}
	}
	
	# close
	cat("  ]", file=path, append=TRUE, sep="\n")
	cat("}", file=path, append=TRUE, sep="\n")
	
	return(path)
}
