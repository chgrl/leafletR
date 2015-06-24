dfToGeoJSON <-
function(data, name, dest, lat.lon, overwrite) {
	# check data
	if(is(data, "tbl_df")) data <- as.data.frame(data)
	
	# assign and check lat/lon
	if(is.null(lat.lon)) {
		lat <- which(names(data)==intersect(names(data), c("y", "Y", "lat", "Lat", "LAT", "latitude", "Latitude", "LATITUDE"))[1])
		lon <- which(names(data)==intersect(names(data), c("x", "X", "lon", "Lon", "LON", "long", "Long", "LONG", "longitude", "Longitude", "LONGITUDE"))[1])
		if(length(lat)==0 || length(lon)==0) {
			lat.lon <- c(1,2)
			message("Latitude and longitude not found - columns 1 (", names(data)[1], ") and 2 (", names(data)[2], ") taken instead")
		} else {
			lat.lon <- c(lat, lon)
			message("Columns ", lat, " (", names(data)[lat], ") and ", lon, " (", names(data)[lon], ") detected as latitude and longitude")
		}
	}
	if(length(lat.lon)!=2) stop("'lat.lon' must be a vector of two: c(latitude, longitude)")
	if(any(!is.numeric(lat.lon))) {
		if(!any(names(data)==lat.lon[1])) stop("Longitude column not found")
		if(!any(names(data)==lat.lon[2])) stop("Latitude column not found")
		lat.lon <- c(which(names(data)==lat.lon[1]), which(names(data)==lat.lon[2]))
	}
	if(is.na(data[,lat.lon[1]]) || is.na(data[,lat.lon[2]])) stop("Coordinate columns not found")
	
	# check for factors
	for(i in 1:ncol(data)) {
		if(is(data[,i], "factor")) {
			data[,i] <- as.character(data[,i])
			message("Column \'", names(data[i]), "\' converted from factor to character type")
		}
	}
	
	# replace line breaks
	for(i in 1:ncol(data)) {
		data[,i] <- gsub("\n","; ",data[,i])
	}
	
	# check file and path
	path <- paste0(file.path(dest, name), ".geojson")
	if(file.exists(path) && !overwrite) stop("Abort - file already exists\n")
	
	# heading
	cat("{", file=path, sep="\n")
	cat("  \"type\": \"FeatureCollection\",", file=path, append=TRUE, sep="\n")
	cat("  \"features\": [", file=path, append=TRUE, sep="\n")
	
	# features
	for(f in 1:nrow(data)) {
		cat("    {", file=path, append=TRUE, sep="\n")
		cat("      \"type\": \"Feature\",", file=path, append=TRUE, sep="\n")
		
		# properties
		if(length(data)>2) {
			cat("      \"properties\": {", file=path, append=TRUE, sep="\n")
			dat <- data[f,-lat.lon]
			if(!is.data.frame(dat)) names(dat) <- names(data)[-lat.lon]
				
			if(length(dat)==1) {
				cat(paste0("        \"", names(data)[-lat.lon], "\": \"", dat, "\"\n"), file=path, append=TRUE)
			} else {
				for(p in 1:length(dat)) {	
					cat(paste0("        \"", names(dat)[p], "\": \"", dat[p], "\""), file=path, append=TRUE)
					if(p==length(dat)) cat("\n", file=path, append=TRUE)
					else cat(",", file=path, append=TRUE, sep="\n")
				}
			}
			cat("      },", file=path, append=TRUE, sep="\n")
		}
		
		# geometry
		cat("      \"geometry\": {", file=path, append=TRUE, sep="\n")
		cat("        \"type\": \"Point\",", file=path, append=TRUE, sep="\n")
		cat(paste0("        \"coordinates\": [", data[f,lat.lon[2]], ",", data[f,lat.lon[1]], "]"), file=path, append=TRUE, sep="\n")
		cat("      }", file=path, append=TRUE, sep="\n")
		
		if(f==nrow(data)) cat("    }", file=path, append=TRUE, sep="\n")
		else cat("    },", file=path, append=TRUE, sep="\n")
	}
		
	cat("  ]", file=path, append=TRUE, sep="\n")
	cat("}", file=path, append=TRUE, sep="\n")
	
	return(path)
}
