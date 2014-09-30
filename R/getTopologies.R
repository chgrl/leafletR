getTopologies <-
function(data, print=TRUE) {
	
	# if file path is given
	if(is.character(data)) {
		# check if file exists and convert JSON
		if(!file.exists(data)) stop("Data file not found")
		data <- jsonlite::fromJSON(data)
		if(is.null(data$type)) stop("'data' requires TopoJSON file")
		if(tolower(data$type)!="topology") stop("'data' requires TopoJSON file")
	}
	
	# get objects
	obj <- unique(names(data$objects))
	
	# print and return
	if(print) print(obj)
	invisible(obj)
}
