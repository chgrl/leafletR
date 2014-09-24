getTopologyObjects <-
function(data, print=TRUE) {
	
	# check if file exists and convert JSON
	if(!file.exists(data)) stop("Data file not found")
	if(tolower(tail(strsplit(basename(data), "[.]")[[1]], 1))!="json") stop("'data' requires TopoJSON")
	json <- jsonlite::fromJSON(data)
	#the following drops an error, but why?
	#tryCatch(json <- jsonlite::fromJSON(data), error=stop("Invalid JSON", call.=FALSE))
	
	# get objects
	obj <- unique(names(json$objects))
	if(print) print(obj)
	invisible(obj)
}
