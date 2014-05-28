print.leafletr.style <- 
function(leafletr.style) {
	if(attr(leafletr.style, "style.type")=="single") { # SINGLE STYLE
		cat("\tSingle symbol style\n\n")
		sty <- strsplit(leafletr.style, ": ", fixed=TRUE)
		
		for(i in 1:length(sty)) {
			if(sty[[i]][1]=="stroke") cat("No line\n")
			if(sty[[i]][1]=="color") cat("Line color:", sty[[i]][2], "\n")
			if(sty[[i]][1]=="weight") cat("Line width:", sty[[i]][2], "\n")
			if(sty[[i]][1]=="opacity") cat("Line alpha:", sty[[i]][2], "\n")
			if(sty[[i]][1]=="fill") cat("No fill\n")
			if(sty[[i]][1]=="fillColor") cat("Fill color:", sty[[i]][2], "\n")
			if(sty[[i]][1]=="fillOpacity") cat("Fill opacity:", sty[[i]][2], "\n")
			if(sty[[i]][1]=="radius") cat("Point radius:", sty[[i]][2], "\n")
		}
	} else if(attr(leafletr.style, "style.type")=="graduated") { # GRADUATED STYLE
		if(attr(leafletr.style, "style.par")=="col") cat("\tGraduated color style\n\n")
		else if(attr(leafletr.style, "style.par")=="rad") cat("\tGraduated radius style\n\n")
		
		sty <- leafletr.style$style
		sty <- gsub("return ", "", sty, fixed=TRUE)
		sty <- gsub("       ", "", sty, fixed=TRUE)
		sty <- gsub(" :", "", sty, fixed=TRUE)
		sty <- gsub(";", "", sty, fixed=TRUE)
		prop <- attr(leafletr.style, "property")
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
		if(attr(leafletr.style, "style.par")=="col") names(sty.tbl) <- c("Classes", "Color")
		if(attr(leafletr.style, "style.par")=="rad") names(sty.tbl) <- c("Classes", "Radius [px]")
		print(sty.tbl, right=FALSE, row.names=FALSE)
		
		if(!is.null(leafletr.style$add)) {
			cat("\nAdditional style parameters:\n")
			add <- strsplit(leafletr.style$add, ": ", fixed=TRUE)
			
			for(i in 1:length(add)) {
				if(add[[i]][1]=="\"stroke\"") cat(" No line\n")
				if(add[[i]][1]=="\"color\"") cat(" Line color:", add[[i]][2], "\n")
				if(add[[i]][1]=="\"weight\"") cat(" Line width:", add[[i]][2], "\n")
				if(add[[i]][1]=="\"opacity\"") cat(" Line alpha:", add[[i]][2], "\n")
				if(add[[i]][1]=="\"fill\"") cat(" No fill\n")
				if(add[[i]][1]=="\"fillColor\"") cat(" Fill color:", add[[i]][2], "\n")
				if(add[[i]][1]=="\"fillOpacity\"") cat(" Fill opacity:", add[[i]][2], "\n")
				if(add[[i]][1]=="\"radius\"") cat(" Point radius:", add[[i]][2], "\n")
			}
		}
		
		if(!is.null(attr(leafletr.style, "leg"))) {
			leg <- strsplit(attr(leafletr.style, "leg"), "<br>", fixed=TRUE)
			cat("\nLegend title:\n")
			for(i in 1:length(leg[[1]])) cat(" ", leg[[1]][i], "\n", sep="")
		}
	} else if(attr(leafletr.style, "style.type")=="categorized") { # CATEGORIZED STYLE
		if(attr(leafletr.style, "style.par")=="col") cat("\tCategorized color style\n\n")
		else if(attr(leafletr.style, "style.par")=="rad") cat("\tCategorized radius style\n\n")
		
		sty <- leafletr.style$style
		sty <- gsub("return ", "", sty, fixed=TRUE)
		sty <- gsub("       ", "", sty, fixed=TRUE)
		sty <- gsub(" :", "", sty, fixed=TRUE)
		sty <- gsub(";", "", sty, fixed=TRUE)
		prop <- attr(leafletr.style, "property")
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
		if(attr(leafletr.style, "style.par")=="col") names(sty.tbl) <- c("Classes", "Color")
		if(attr(leafletr.style, "style.par")=="rad") names(sty.tbl) <- c("Classes", "Radius [px]")
		print(sty.tbl, right=FALSE, row.names=FALSE)
		
		if(!is.null(leafletr.style$add)) {
			cat("\nAdditional style parameters:\n")
			add <- strsplit(leafletr.style$add, ": ", fixed=TRUE)
			
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
		
		if(!is.null(attr(leafletr.style, "leg"))) {
			leg <- strsplit(attr(leafletr.style, "leg"), "<br>", fixed=TRUE)
			cat("\nLegend title:\n")
			for(i in 1:length(leg[[1]])) cat(" ", leg[[1]][i], "\n", sep="")
		}
	}	
}
