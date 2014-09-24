leaflet <-
function(data, dest, title, size, base.map="osm", center, zoom, style, popup, incl.data=FALSE, overwrite=TRUE) {	
  
  # prepare data
  if(missing(data)) data <- NA
	if(length(data)>1) for(n in 1:length(data)) {
		if(!is.na(data[[n]])) {
			ext <- tolower(tail(strsplit(basename(data[[n]]), "[.]")[[1]], 1))
			if(ext!="geojson" && ext!="json") stop("'data' requires GeoJSON (file extension should be 'geojson' or topoJSON files (file extension should be 'json')")
		}
		json <- jsonlite::fromJSON(data[[n]])  # just for testing
		#the following drops an error, but why?
		#tryCatch(json <- fromJSON(data[[n]]), error=stop("'data' contains invalid JSON file", call.=FALSE))
	} else {
		if(!is.na(data)) {
			ext <- tolower(tail(strsplit(basename(data), "[.]")[[1]], 1))
			if(ext!="geojson" && ext!="json") stop("'data' requires GeoJSON (file extension should be 'geojson' or topoJSON files (file extension should be 'json')")
			json <- jsonlite::fromJSON(data)  # just for testing
			#the following drops an error, but why?
			#tryCatch(json <- jsonlite::fromJSON(data), error=stop("'data' is not a valid JSON file", call.=FALSE))
		}
	}
	
	# prepare output file destination
	if(missing(dest)) dest <- getwd()
	dest <- gsub("\\\\", "/", dest)
	if(missing(title)) {
		if(any(is.na(data))) title <- "map" 
		else {
			if(length(data)==1) title <- gsub("_", " ", paste(head(strsplit(basename(data), "[.]")[[1]], -1), collapse="_")) else title <- "map"
		}
	}
	
	# prepare base map
	basemaps <- getOption("leafletBaseMaps")
	bm <- names(basemaps)
	base.map <- bm[pmatch(base.map, bm)]
	if(any(is.na(base.map))) stop("Invalid base.map")
	
	# prepare style
	if(missing(style)) style <- NA
	if(any(!is.na(style))) {
		if(is.list(style) & !is(style, "leafletr.style")) {
			for(i in 1:length(style)) if(! is(style[[i]], "leafletr.style")) stop("At least one style object not recognized")
		} else if(! is(style, "leafletr.style")) stop("Style object not recognized")
	}
	if(length(data)>1 && !is.na(style)) if(length(style)<length(data) || !is.list(style)) stop("Number of styles must correspond to number of data files")
	if(file.exists(file.path(dest, gsub(" ", "_", title))) && !overwrite) stop("Abort - file already exists")
	
	# prepare popup
	if(missing(popup)) popup <- NA
	if(!any(is.na(popup))) {
		if(is.list(popup)) {
			for(n in 1:length(popup)) if(length(popup[[n]])==1) if(popup[[n]]=="*") popup[[n]] <- getProperties(data[[n]], FALSE)
		} else {
			if(length(popup)==1) if(popup=="*") popup <- getProperties(data[[1]], FALSE)
		}
	}
	if(!is.list(popup)) popup <- list(popup)
	
	# prepare map parameter
	if(missing(size)) size <- NA
	if(missing(center)) center <- NA
	if(missing(zoom)) zoom <- NA
	if(any(is.na(data))) {
		center <- c(0,0)
		zoom <- 2
	}
	
	# prepare file path
	dir.create(file.path(dest, gsub(" ", "_", title)), showWarnings=FALSE)
	if(any(!is.na(data)) && !incl.data) {
		for(n in 1:length(data)) file.copy(data[[n]], file.path(dest, gsub(" ", "_", title)), overwrite=overwrite)
	}
	filePath <- file.path(dest, gsub(" ", "_", title), paste0(gsub(" ", "_", title), ".html"))
	
	# brew
	brew(system.file("templates/main.brew", package="leafletR"), filePath) 
	
	# finish
	class(filePath) <- "leaflet"
	message("\nYour leaflet map has been saved under ", filePath)
	invisible(filePath)
}
