leaflet <-
function(data, dest, title, size, base.map="osm", center, zoom, style, popup, label, controls="all", incl.data=FALSE, overwrite=TRUE) {	
  
	# prepare data
	if(missing(data)) data <- NA
	topojson <- NULL
	json <- list()
	if(length(data)>1) for(n in 1:length(data)) {
		if(!is.na(data[[n]])) {
			json[[n]] <- jsonlite::fromJSON(data[[n]])
			if(is.null(json[[n]]$type)) stop("'data' requires GeoJSON or TopoJSON files")
			if(tolower(json[[n]]$type)=="topology") topojson <- append(topojson, TRUE)
			else topojson <- append(topojson, FALSE)
		}
	} else {
		if(!is.na(data)) {
			json[[1]] <- jsonlite::fromJSON(data)
			if(is.null(json[[1]]$type)) stop("'data' requires GeoJSON or TopoJSON files")
			if(tolower(json[[1]]$type)=="topology") topojson <- TRUE
			else topojson <- FALSE
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
	} else {
		title <- gsub("/", "", title)
	}
	
	# prepare base map
	basemaps <- getOption("leafletBaseMaps")
	bm <- names(basemaps)
	base.map <- bm[pmatch(base.map, bm)]
	
	# prepare style
	if(missing(style)) style <- NA
	if(any(!is.na(style))) {
		if(is.list(style) && !is(style, "leafletr.style")) {
			for(i in 1:length(style)) if(!is(style[[i]], "leafletr.style")) stop("At least one style object not recognized")
		} else if(!is(style, "leafletr.style")) stop("Style object not recognized")
	}
	if(length(data)>1 && !is.na(style)) if(length(style)<length(data) || !is.list(style)) stop("Number of styles must correspond to number of data files")
	if(file.exists(file.path(dest, gsub("[^[:alnum:]!@#&,=+-_()]", "_", title))) && !overwrite) stop("Abort - file already exists")
	
	# prepare popup
	if(!missing(popup)) {
		if(is.list(popup)) {
			for(n in 1:length(popup)) if(length(popup[[n]])==1) if(!is.na(popup[[n]])) if(popup[[n]]=="*") popup[[n]] <- getProperties(json[[n]], FALSE)
		} else {
			if(length(popup)==1) if(!is.na(popup)) if(popup=="*") popup <- getProperties(json[[1]], FALSE)
			popup <- list(popup)
		}
		if(length(popup)==length(unlist(popup))) multi.prop <- FALSE
		else multi.prop <- TRUE
	}
	
	# prepare label
	if(!is.na(data) && !missing(label)) {
		if(length(data)!=length(label)) stop("Number of labels must correspond to number of data files")
		label <- lapply(label, function(x) ifelse(x=="", NA, x))
	}
	
	# prepare map parameter
	if(missing(size)) size <- NA
	if(missing(center)) center <- NA
	if(missing(zoom)) zoom <- NA
	if(any(is.na(data)) && is.na(center) && is.na(zoom)) {
		center <- c(0,0)
		zoom <- 2
	}
	
	# prepare controls
	zoom.ctrl <- scale.ctrl <- layer.ctrl <- legend.ctrl <- FALSE
	if(length(controls)==1 && !is.na(controls)) if(controls=="all") controls <- list("zoom", "scale", "layer", "legend")
	if(!any(is.na(controls))) {
		if(any(controls=="zoom")) zoom.ctrl <- TRUE
		if(any(controls=="scale")) scale.ctrl <- TRUE
		if(any(controls=="layer")) layer.ctrl <- TRUE
		if(any(controls=="legend")) legend.ctrl <- TRUE
	}
	# no legend if only one single style layer 
	if(any(!is.na(style))) if(is(style, "leafletr.style")) if(!is(style, "graduated.style") && !is(style, "categorized.style")) legend.ctrl <- FALSE
	
	# prepare file path
	dir.create(file.path(dest, gsub("[^[:alnum:]!@#&,=+-_()]", "_", title)), showWarnings=FALSE)
	if(any(!is.na(data)) && !incl.data) {
		for(n in 1:length(data)) file.copy(data[[n]], file.path(dest, gsub("[^[:alnum:]!@#&,=+-_()]", "_", title)), overwrite=overwrite)
	}
	filePath <- file.path(dest, gsub("[^[:alnum:]!@#&,=+-_()]", "_", title), paste0(gsub("[^[:alnum:]!@#&,=+-_()]", "_", title), ".html"))
	# brew
	brew(system.file("templates/main.brew", package="leafletR"), filePath) 
	
	# finish
	class(filePath) <- "leaflet"
	message("\nYour leaflet map has been saved under ", filePath)
	invisible(filePath)
}
