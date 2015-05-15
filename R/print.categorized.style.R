print.categorized.style <- 
function(x, ...) {
	if(attr(x, "style.par")=="col") cat("\tCategorized color style\n\n")
	else if(attr(x, "style.par")=="rad") cat("\tCategorized radius style\n\n")
	
	sty <- x$style
	sty <- gsub("return ", "", sty, fixed=TRUE)
	sty <- gsub("       ", "", sty, fixed=TRUE)
	sty <- gsub(" :", "", sty, fixed=TRUE)
	sty <- gsub(";", "", sty, fixed=TRUE)
	prop <- attr(x, "property")
	sty <- gsub("x", prop, sty, fixed=TRUE)
	sty <- strsplit(sty, " ? ", fixed=TRUE)
	
	sty.tbl <- NULL
	for(i in 1:length(sty)) {
		if(length(sty[[i]])==1) {
			sty[[i]] <- append(sty[[i]], sty[[i]][1])
			sty[[i]][1] <- ""
		}
		if(is.null(sty.tbl)) sty.tbl <- c(sty[[i]][1], sty[[i]][2])
		else sty.tbl <- rbind(sty.tbl, c(sty[[i]][1], sty[[i]][2]))
	}
	sty.tbl <- data.frame(sty.tbl, row.names=c(1:length(sty)))
	if(attr(x, "style.par")=="col") names(sty.tbl) <- c("Class", "Color")
	if(attr(x, "style.par")=="rad") names(sty.tbl) <- c("Class", "Radius [px]")
	print(sty.tbl, right=FALSE, row.names=FALSE)
	
	if(!is.null(x$add)) {
		cat("\nAdditional style parameters:\n")
		add <- strsplit(x$add, ": ", fixed=TRUE)
		
		for(i in 1:length(add)) {
			if(add[[i]][1]=="\"stroke\"") cat(" No line\n")
			if(add[[i]][1]=="\"color\"") cat(" Line color:", add[[i]][2], "\n")
			if(add[[i]][1]=="\"weight\"") cat(" Line width:", add[[i]][2], "\n")
			if(add[[i]][1]=="\"opacity\"") cat(" Line alpha:", add[[i]][2], "\n")
			if(add[[i]][1]=="\"fillColor\"") cat(" Fill color:", add[[i]][2], "\n")
			if(add[[i]][1]=="\"fillOpacity\"") cat(" Fill opacity:", add[[i]][2], "\n")
			if(add[[i]][1]=="\"radius\"") cat(" Point radius:", add[[i]][2], "\n")
		}
	}
	
	if(!is.null(attr(x, "leg"))) {
		leg <- strsplit(attr(x, "leg"), "<br>", fixed=TRUE)
		cat("\nLegend title:\n")
		for(i in 1:length(leg[[1]])) cat(" ", leg[[1]][i], "\n", sep="")
	}	
}
