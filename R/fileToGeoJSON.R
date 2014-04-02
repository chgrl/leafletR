fileToGeoJSON <-
function(data, name, dest, overwrite) {
	if(!file.exists(data)) stop("data file not found")
	if(file.exists(paste0(file.path(dest, name), ".geojson")) && !overwrite) stop("abort - file already exists\n")
	stopifnot(require(httr, quietly=TRUE))
	
	# taken from rgbif package: cran.r-project.org/package=rgbif‎
	# package import impractical, since rgbif imports several other packages
	url <- "http://ogre.adc4gis.com/convert"
	tt <- POST(url, body = list(upload = upload_file(data)))
	out <- content(tt, as = "text")
	fileConn <- file(paste0(file.path(dest, name), ".geojson"))
	writeLines(out, fileConn)
	close(fileConn)
    # end rgbif code

	return(paste0(file.path(dest, name), ".geojson"))
}
