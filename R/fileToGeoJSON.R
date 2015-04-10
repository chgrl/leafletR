fileToGeoJSON <-
function(data, name, dest, overwrite) {
	if(!file.exists(data)) stop("Data file not found")
	if(file.exists(paste0(file.path(dest, name), ".geojson")) && !overwrite) stop("Abort - file already exists")
	if(!requireNamespace("httr", quietly=TRUE)) stop("'httr' package required for file conversion")
	
	url <- "http://ogre.adc4gis.com/convert"
	tt <- httr::POST(url, body=list(upload=httr::upload_file(data), skipFailures="yes"))
	out <- httr::content(tt, as="text")
	fileConn <- file(paste0(file.path(dest, name), ".geojson"))
	writeLines(out, fileConn)
	close(fileConn)

	return(paste0(file.path(dest, name), ".geojson"))
}
