leaflet <-
function(data, dest, title, size, base.map="osm", center, zoom, style, popup, incl.data=FALSE, overwrite=TRUE) {	
	if(missing(data)) data <- NA
	if(length(data)>1) for(n in 1:length(data)) {
		if(!is.na(data[[n]])) if(tolower(strsplit(tail(strsplit(data[[n]], "/")[[1]], 1), "[.]")[[1]][2])!="geojson") stop("'data' requires GeoJSON files (file extension should be 'geojson')")
		suppressWarnings(if(require(RJSONIO, quietly=TRUE)) if(!isValidJSON(data[[n]])) stop("'data' is not a valid JSON file"))
	} else {
		if(!is.na(data)) {
			if(tolower(strsplit(tail(strsplit(data, "/")[[1]], 1), "[.]")[[1]][2])!="geojson") stop("'data' requires GeoJSON files (file extension should be 'geojson')")
			suppressWarnings(if(require(RJSONIO, quietly=TRUE)) if(!isValidJSON(data)) stop("'data' is not a valid JSON file"))
		}
	}
	if(missing(dest)) dest <- getwd()
	dest <- gsub("\\\\", "/", dest)
	if(missing(title)) {
		if(any(is.na(data))) title <- "map" 
		else {
			if(length(data)==1) title <- gsub("_", " ", strsplit(tail(strsplit(data, "/")[[1]], 1), "[.]")[[1]][1]) else title <- "map"
		}
	}
	if(missing(size)) size <- NA
	bm <- c("osm", "tls", "mqosm", "mqsat", "water", "toner")
	base.map <- bm[pmatch(base.map, bm)]
	if(any(is.na(base.map))) stop("Invalid base.map")
	if(missing(center)) center <- NA
	if(missing(zoom)) zoom <- NA
	if(missing(style)) style <- NA
	if(missing(popup)) popup <- NA
	
	if(length(data)>1 && !is.na(style)) if((length(style)<length(data) && is.list(style)) || !is.list(style)) stop("Number of styles must correspond to number of data files")
	if(file.exists(file.path(dest, gsub(" ", "_", title))) && !overwrite) stop("Abort - file already exists")
	
	if(!any(is.na(popup))) {
		if(is.list(popup)) {
			for(n in 1:length(popup)) if(length(popup[[n]])==1) if(popup[[n]]=="*") popup[[n]] <- getProperties(data[[n]], FALSE)
		} else {
			if(length(popup)==1) if(popup=="*") popup <- getProperties(data[[1]], FALSE)
		}
	}
	
	dir.create(file.path(dest, gsub(" ", "_", title)), showWarnings=FALSE)
	if(any(!is.na(data)) && !incl.data) {
		for(n in 1:length(data)) file.copy(data[[n]], file.path(dest, gsub(" ", "_", title)))
	}
	if(any(is.na(data))) {
		center <- c(0,0)
		zoom <- 2
	}
	filePath <- file.path(dest, gsub(" ", "_", title), paste0(gsub(" ", "_", title), ".html"))
	leafletInt(data, path=filePath, title, size, base.map, center, zoom, style, popup, incl.data)
	message("\nYour leaflet map has been saved under ", filePath)
	invisible(filePath)
}
