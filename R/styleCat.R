styleCat <-
function(prop, val, style.val, leg, ...) {
	for(i in 1:length(style.val)) style.val[i] <- getHex(style.val[i])
	if(!missing(leg)) leg <- gsub("\n", "<br>", leg)
	
	if(length(val)!=length(style.val) && length(val)!=(length(style.val)-1)) stop("Length of 'val' (", length(val), ") does not match length of 'style.val' (", length(style.val),")")	
	
	stl.val <- style.val
	stl.val <- paste0("\"", style.val, "\"")
	cat.style <- paste0("return x == \"", val[1], "\" ? ", stl.val[1], " :")
	for(n in 2:length(val)) cat.style <- append(cat.style, paste0("       x == \"", val[n], "\" ? ", stl.val[n], " :"))
	if(length(style.val)>length(val)) cat.style <- append(cat.style, paste0("       ", stl.val[length(val)+1], ";"))
	else cat.style <- append(cat.style, paste("       \"\";", sep="")) 
	
	s <- list(...)
	single.style <- NULL
	if(length(s)>0) {
		if(is.na(s$col)) single.style <- append(single.style, "\"stroke\": false")
		else single.style <- append(single.style, paste0("\"color\": \"", getHex(s$col), "\""))
		if(any(names(s)=="lwd")) single.style <- append(single.style, paste("\"weight\":", s$lwd))
		if(any(names(s)=="alpha")) single.style <- append(single.style, paste("\"opacity\":", s$alpha))
		if(any(names(s)=="fill")) {
			if(is.na(s$fill)) single.style <- append(single.style, "\"fill\": false")
			else single.style <- append(single.style, paste0("\"fillColor\": \"", getHex(s$fill), "\""))
		}
		if(any(names(s)=="fill.alpha")) single.style <- append(single.style, paste("\"fillOpacity\":", s$fill.alpha))
		else single.style <- append(single.style, "\"fillOpacity\": 0.5")
	} else {
		single.style <- append(single.style, "\"fillOpacity\": 0.5")
	}
	
	cat.style <- list(style=cat.style, add=single.style)
	attr(cat.style, "style.type") <- "categorized"
	attr(cat.style, "property") <- prop
	attr(cat.style, "values") <- val
	attr(cat.style, "style.par") <- "col"
	if(length(style.val)>length(val)) {
		attr(cat.style, "na") <- "other"
		#attr(cat.style, "na.val") <- stl.val[length(val)+1]
	}
	if(!missing(leg)) attr(cat.style, "leg") <- leg
	class(cat.style) <- c("leafletr.style", "categorized.style")
	return(cat.style)
}
