print.single.style <- 
function(x, ...) {
	cat("\tSingle symbol style\n\n")
	sty <- strsplit(x, ": ", fixed=TRUE)
	
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
}
