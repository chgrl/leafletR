styleCat <-
function(prop, val, style.par="col", style.val, leg, ...) {
	sp <- c("col", "rad")
	style.par <- sp[pmatch(style.par, sp)]
	
	if(style.par=="col") for(i in 1:length(style.val)) style.val[i] <- getHex(style.val[i])
	if(!missing(leg)) leg <- gsub("\n", "<br>", leg)
	
	stl.val <- style.val
	if(style.par=="col") stl.val <- paste0("\"", style.val, "\"")
	cat.style <- paste0("return x == \"", val[1], "\" ? ", stl.val[1], " :")
	for(n in 2:length(val)) cat.style <- append(cat.style, paste0("       x == \"", val[n], "\" ? ", stl.val[n], " :"))
	if(length(style.val)>length(val)) cat.style <- append(cat.style, paste0("       ", stl.val[length(val)+1], ";"))
	else cat.style <- append(cat.style, paste("       \"\";", sep="")) 
	
	s <- list(...)
	single.style <- NULL
	if(length(s)>0) {
		if(any(names(s)=="col")) {
			if(is.na(s$col)) single.style <- append(single.style, "\"stroke\": false")
			else single.style <- append(single.style, paste0("\"color\": \"", getHex(s$col), "\""))
		} else if(style.par=="rad") single.style <- append(single.style, "\"color\": \"#0033ff\"")
		if(any(names(s)=="lwd")) single.style <- append(single.style, paste("\"weight\":", s$lwd))
		else if(style.par=="rad") single.style <- append(single.style, "\"weight\": 2")
		if(any(names(s)=="alpha")) single.style <- append(single.style, paste("\"opacity\":", s$alpha))
		else if(style.par=="rad") single.style <- append(single.style, "\"opacity\": 0.5")
		if(any(names(s)=="fill")) {
			if(is.na(s$fill)) single.style <- append(single.style, "\"fill\": false")
			else single.style <- append(single.style, paste0("\"fillColor\": \"", getHex(s$fill), "\""))
		} else if(style.par=="rad") single.style <- append(single.style, "\"fillColor\": \"#0033ff\"")
		if(any(names(s)=="fill.alpha")) single.style <- append(single.style, paste("\"fillOpacity\":", s$fill.alpha))
		else single.style <- append(single.style, "\"fillOpacity\": 0.5")
		if(any(names(s)=="rad")) single.style <- append(single.style, paste("\"radius\":", s$rad))
	} else {
		single.style <- append(single.style, "\"fillOpacity\": 0.5")
		if(style.par=="rad") {
			single.style <- append(single.style, "\"weight\": 2")
			single.style <- append(single.style, "\"color\": \"#0033ff\"")
			single.style <- append(single.style, "\"fillColor\": \"#0033ff\"")
			single.style <- append(single.style, "\"opacity\": 0.5")
		}
	}
	
	cat.style <- list(style=cat.style, add=single.style)
	attr(cat.style, "style.type") <- "categorized"
	attr(cat.style, "property") <- prop
	attr(cat.style, "values") <- val
	attr(cat.style, "style.par") <- style.par
	if(length(style.val)>length(val)) {
		attr(cat.style, "na") <- "other"
		#attr(cat.style, "na.val") <- stl.val[length(val)+1]
	}
	if(!missing(leg)) attr(cat.style, "leg") <- leg
	class(cat.style) <- "leafletr.style"
	return(cat.style)
}
