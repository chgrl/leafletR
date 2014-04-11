checkPolyHole <- 
function(polyCoord, holeCoord) {
# checks if hole is in polygon

	pgX <- polyCoord[,1]
	pgY <- polyCoord[,2]
	hlX <- holeCoord[,1]
	hlY <- holeCoord[,2]
	sides <- length(pgX)
	s2 <- sides-1
	holeInPolygon <- TRUE
	
	for(h in 1:length(hlX)) {
		odd <- FALSE
		for (s in 1:sides) {
			if ((pgY[s]<hlY[h] && pgY[s2]>=hlY[h] || pgY[s2]<hlY[h] && pgY[s]>=hlY[h]) && (pgX[s]<=hlX[h] || pgX[s2]<=hlX[h])) {
				if (pgX[s]+(hlY[h]-pgY[s])/(pgY[s2]-pgY[s])*(pgX[s2]-pgX[s])<hlX[h]) odd <- !odd
			}
			s2 <- s
		}
		if(!odd) {
			holeInPolygon <- FALSE
			break
		}
	}
	
	return(holeInPolygon)
}
