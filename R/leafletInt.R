leafletInt <-
function(dat, path, title, size, base.map, center, zoom, style, popup, incl.data) {
	# opening
	cat("<!DOCTYPE html>", file=path, sep="\n")
	cat("<html>", file=path, append=TRUE, sep="\n")
	cat("<head>", file=path, append=TRUE, sep="\n")
	
	# web site title
	cat(paste0("\t<title>", title, "</title>"), file=path, append=TRUE, sep="\n")
	
	# meta
	cat("\t<meta charset=\"utf-8\" />", file=path, append=TRUE, sep="\n")
	cat("\t<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">", file=path, append=TRUE, sep="\n")
	
	# stylesheet
	cat("\t<link rel=\"stylesheet\" href=\"http://cdn.leafletjs.com/leaflet-0.7.2/leaflet.css\" />", file=path, append=TRUE, sep="\n")
	
	# leaflet lib
	cat("\t<script src=\"http://cdn.leafletjs.com/leaflet-0.7.2/leaflet.js?2\"></script>", file=path, append=TRUE, sep="\n")
	
	# data
	if(any(!is.na(dat)) && !incl.data) {
		cat("\t<script src=\"http://code.jquery.com/jquery-1.10.2.min.js\"></script>", file=path, append=TRUE, sep="\n")
		for(n in 1:length(dat)) cat(paste0("\t<link rel=\"", paste0("dat", n), "\" type=\"application/json\" href=\"", tail(strsplit(dat[[n]], "/")[[1]], 1), "\" />"), file=path, append=TRUE, sep="\n")
	}
		
	### extra style instructions start #########################################################
	cat("\t<style type=\"text/css\">", file=path, append=TRUE, sep="\n")
	cat("\t\tbody {", file=path, append=TRUE, sep="\n")
	cat("\t\t\tpadding: 0;", file=path, append=TRUE, sep="\n")
	cat("\t\t\tmargin: 0;", file=path, append=TRUE, sep="\n")
	cat("\t\t}", file=path, append=TRUE, sep="\n")
	
	# fullscreen
	if(any(is.na(size))) {
		cat("\t\thtml, body, #map {", file=path, append=TRUE, sep="\n")
		cat("\t\t\theight: 100%;", file=path, append=TRUE, sep="\n")
		cat("\t\t}", file=path, append=TRUE, sep="\n")
	
	# manual size
	} else {
		cat("\t\t#map {", file=path, append=TRUE, sep="\n")
		cat(paste0("\t\t\twidth: ", size[1], "px;"), file=path, append=TRUE, sep="\n")
		cat(paste0("\t\t\theight: ", size[2], "px;"), file=path, append=TRUE, sep="\n")
		cat("\t\t}", file=path, append=TRUE, sep="\n")
	}
	
	# popup
	if(!any(is.na(popup))) {
		if(is.list(popup)) {
			for(n in 1:length(popup)) if(length(popup[[n]])>1) {
				cat("\t\ttable, td {", file=path, append=TRUE, sep="\n")
				cat("\t\t\tborder-collapse: collapse;", file=path, append=TRUE, sep="\n")
				cat("\t\t\tborder-style: solid;", file=path, append=TRUE, sep="\n")
				cat("\t\t\tborder-width: 1px;", file=path, append=TRUE, sep="\n")
				cat("\t\t\tborder-color: #e9e9e9;", file=path, append=TRUE, sep="\n")
				cat("\t\t\tpadding: 5px;", file=path, append=TRUE, sep="\n")
				cat("\t\t}", file=path, append=TRUE, sep="\n")
				cat("\t\t.evenrowcol{", file=path, append=TRUE, sep="\n")
				cat("\t\t\tbackground-color: #f6f6f6;", file=path, append=TRUE, sep="\n")
				cat("\t\t}", file=path, append=TRUE, sep="\n")
				break
			}
		} else {
			if(length(popup)>1) {
				cat("\t\ttable, td {", file=path, append=TRUE, sep="\n")
				cat("\t\t\tborder-collapse: collapse;", file=path, append=TRUE, sep="\n")
				cat("\t\t\tborder-style: solid;", file=path, append=TRUE, sep="\n")
				cat("\t\t\tborder-width: 1px;", file=path, append=TRUE, sep="\n")
				cat("\t\t\tborder-color: #e9e9e9;", file=path, append=TRUE, sep="\n")
				cat("\t\t\tpadding: 5px;", file=path, append=TRUE, sep="\n")
				cat("\t\t}", file=path, append=TRUE, sep="\n")
				cat("\t\t.evenrowcol{", file=path, append=TRUE, sep="\n")
				cat("\t\t\tbackground-color: #f6f6f6;", file=path, append=TRUE, sep="\n")
				cat("\t\t}", file=path, append=TRUE, sep="\n")
			}
		}
	}
	
	# legend
	if(!any(is.na(style))) {
		if(class(style)=="leafletr.style" || class(style)=="list") {
			sty <- NULL
			if(class(style)=="list") {
				for(i in 1:length(style)) sty <- append(sty, attr(style[[i]], "style.type"))
				if(all(sty=="single")) {
					cat("\t\t.legend {", file=path, append=TRUE, sep="\n")
					cat("\t\t\tpadding: 6px 8px;", file=path, append=TRUE, sep="\n")
					cat("\t\t\tfont: 14px/16px Arial, Helvetica, sans-serif;", file=path, append=TRUE, sep="\n")
					cat("\t\t\tbackground: white;", file=path, append=TRUE, sep="\n")
					cat("\t\t\tbackground: rgba(255,255,255,0.8);", file=path, append=TRUE, sep="\n")
					cat("\t\t\tbox-shadow: 0 0 15px rgba(0,0,0,0.2);", file=path, append=TRUE, sep="\n")
					cat("\t\t\tborder-radius: 5px;", file=path, append=TRUE, sep="\n")
				    cat("\t\t\tline-height: 18px;", file=path, append=TRUE, sep="\n")
				    cat("\t\t\tcolor: #555;", file=path, append=TRUE, sep="\n")
					cat("\t\t}", file=path, append=TRUE, sep="\n")
					cat("\t\t.legend i {", file=path, append=TRUE, sep="\n")
				    cat("\t\t\twidth: 18px;", file=path, append=TRUE, sep="\n")
				    cat("\t\t\theight: 18px;", file=path, append=TRUE, sep="\n")
				    cat("\t\t\tfloat: left;", file=path, append=TRUE, sep="\n")
				    cat("\t\t\tmargin-right: 8px;", file=path, append=TRUE, sep="\n")
					cat("\t\t}", file=path, append=TRUE, sep="\n")
					cat("\t\ttable, td {", file=path, append=TRUE, sep="\n")
					cat("\t\t\tborder: none;", file=path, append=TRUE, sep="\n")
					cat("\t\t}", file=path, append=TRUE, sep="\n")
					cat("\t\t.shape {", file=path, append=TRUE, sep="\n")
					cat("\t\t\tpadding: 0px;", file=path, append=TRUE, sep="\n")
					cat("\t\t\ttext-align: center;", file=path, append=TRUE, sep="\n")
					cat("\t\t\tvertical-align: middle;", file=path, append=TRUE, sep="\n")
					cat("\t\t}", file=path, append=TRUE, sep="\n")
					cat("\t\t.value {", file=path, append=TRUE, sep="\n")
					cat("\t\t\tpadding: 0px 0px 0px 8px;", file=path, append=TRUE, sep="\n")
					cat("\t\t\ttext-align: left;", file=path, append=TRUE, sep="\n")
					cat("\t\t\tvertical-align: middle;", file=path, append=TRUE, sep="\n")
					cat("\t\t}", file=path, append=TRUE, sep="\n")
					
					# get vector of featureTypes and add corresponding styles
					ft <- getFeatureType(dat[[1]])
					for(i in 2:length(dat)) {
						f <- getFeatureType(dat[[i]])
						if(!any(ft==f)) ft <- append(ft, f)
					}
					if(any(ft=="point")) {
						cat("\t\t.crcl {", file=path, append=TRUE, sep="\n")
						cat("\t\t\tfill: #0033ff;", file=path, append=TRUE, sep="\n")
						cat("\t\t\tfill-opacity: 0.5;", file=path, append=TRUE, sep="\n")
						cat("\t\t\tstroke: #0033ff;", file=path, append=TRUE, sep="\n")
						cat("\t\t\tstroke-width: 2;", file=path, append=TRUE, sep="\n")
						cat("\t\t\tstroke-opacity: 0.5;", file=path, append=TRUE, sep="\n")
						cat("\t\t}", file=path, append=TRUE, sep="\n")
					}
					if(any(ft=="line")) {
						cat("\t\t.ln {", file=path, append=TRUE, sep="\n")
						cat("\t\t\tstroke: #0033ff;", file=path, append=TRUE, sep="\n")
						cat("\t\t\tstroke-width: 5;", file=path, append=TRUE, sep="\n")
						cat("\t\t\tstroke-opacity: 0.5;", file=path, append=TRUE, sep="\n")
						cat("\t\t\tstroke-linecap: round;", file=path, append=TRUE, sep="\n")
						cat("\t\t}", file=path, append=TRUE, sep="\n")
					}
					if(any(ft=="polygon")) {
						cat("\t\t.plgn {", file=path, append=TRUE, sep="\n")
						cat("\t\t\tfill: #0033ff;", file=path, append=TRUE, sep="\n")
						cat("\t\t\tfill-opacity: 0.5;", file=path, append=TRUE, sep="\n")
						cat("\t\t\tstroke: #0033ff;", file=path, append=TRUE, sep="\n")
						cat("\t\t\tstroke-width: 4;", file=path, append=TRUE, sep="\n")
						cat("\t\t\tstroke-opacity: 0.5;", file=path, append=TRUE, sep="\n")
						cat("\t\t\tstroke-linejoin: round;", file=path, append=TRUE, sep="\n")
						cat("\t\t}", file=path, append=TRUE, sep="\n")
					}
				}
			}	
			else if(attr(style, "style.type")=="graduated" || attr(style, "style.type")=="categorized") {
				if(attr(style, "style.par")=="col") {
					cat("\t\t.legend {", file=path, append=TRUE, sep="\n")
					cat("\t\t\tpadding: 6px 8px;", file=path, append=TRUE, sep="\n")
					cat("\t\t\tfont: 14px/16px Arial, Helvetica, sans-serif;", file=path, append=TRUE, sep="\n")
					cat("\t\t\tbackground: white;", file=path, append=TRUE, sep="\n")
					cat("\t\t\tbackground: rgba(255,255,255,0.8);", file=path, append=TRUE, sep="\n")
					cat("\t\t\tbox-shadow: 0 0 15px rgba(0,0,0,0.2);", file=path, append=TRUE, sep="\n")
					cat("\t\t\tborder-radius: 5px;", file=path, append=TRUE, sep="\n")
				    cat("\t\t\tline-height: 18px;", file=path, append=TRUE, sep="\n")
				    cat("\t\t\tcolor: #555;", file=path, append=TRUE, sep="\n")
					cat("\t\t}", file=path, append=TRUE, sep="\n")
					cat("\t\t.legend i {", file=path, append=TRUE, sep="\n")
				    cat("\t\t\twidth: 18px;", file=path, append=TRUE, sep="\n")
				    cat("\t\t\theight: 18px;", file=path, append=TRUE, sep="\n")
				    cat("\t\t\tfloat: left;", file=path, append=TRUE, sep="\n")
				    cat("\t\t\tmargin-right: 8px;", file=path, append=TRUE, sep="\n")
				    opa <- style[[2]][grep("fillOpacity", style[[2]])]
				    cat(paste0("\t\t\topacity: ", as.numeric(gsub(".+\\s+", "", opa)), ";"), file=path, append=TRUE, sep="\n")
					cat("\t\t}", file=path, append=TRUE, sep="\n")
				} else if(attr(style, "style.par")=="rad") {
					cat("\t\t.legend {", file=path, append=TRUE, sep="\n")
					cat("\t\t\tpadding: 6px 8px;", file=path, append=TRUE, sep="\n")
					cat("\t\t\tfont: 14px/16px Arial, Helvetica, sans-serif;", file=path, append=TRUE, sep="\n")
					cat("\t\t\tbackground: white;", file=path, append=TRUE, sep="\n")
					cat("\t\t\tbackground: rgba(255,255,255,0.8);", file=path, append=TRUE, sep="\n")
					cat("\t\t\tbox-shadow: 0 0 15px rgba(0,0,0,0.2);", file=path, append=TRUE, sep="\n")
					cat("\t\t\tborder-radius: 5px;", file=path, append=TRUE, sep="\n")
				    cat("\t\t\tline-height: 18px;", file=path, append=TRUE, sep="\n")
				    cat("\t\t\tcolor: #555;", file=path, append=TRUE, sep="\n")
					cat("\t\t}", file=path, append=TRUE, sep="\n")
					cat("\t\t.circle {", file=path, append=TRUE, sep="\n")
					cat("\t\t\tborder: none;", file=path, append=TRUE, sep="\n")
					cat("\t\t\tpadding: 0px;", file=path, append=TRUE, sep="\n")
					cat("\t\t\ttext-align: center;", file=path, append=TRUE, sep="\n")
					cat("\t\t\tvertical-align: middle;", file=path, append=TRUE, sep="\n")
					cat("\t\t}", file=path, append=TRUE, sep="\n")
					cat("\t\t.value {", file=path, append=TRUE, sep="\n")
					cat("\t\t\tborder: none;", file=path, append=TRUE, sep="\n")
					cat("\t\t\tpadding: 0px 0px 0px 8px;", file=path, append=TRUE, sep="\n")
					cat("\t\t\ttext-align: left;", file=path, append=TRUE, sep="\n")
					cat("\t\t\tvertical-align: middle;", file=path, append=TRUE, sep="\n")
					cat("\t\t}", file=path, append=TRUE, sep="\n")
					cat("\t\t.crcl {", file=path, append=TRUE, sep="\n")
					clr <- style[[2]][grep("fillColor", style[[2]])]
					cat(paste0("\t\t\tfill: ", substr(clr, nchar(clr)-7, nchar(clr)-1), ";"), file=path, append=TRUE, sep="\n")
					opa <- style[[2]][grep("fillOpacity", style[[2]])]
					cat(paste0("\t\t\tfill-opacity: ", as.numeric(gsub(".+\\s+", "", opa)), ";"), file=path, append=TRUE, sep="\n")
					brd <- style[[2]][grep("color", style[[2]])]
					cat(paste0("\t\t\tstroke: ", substr(brd, nchar(brd)-7, nchar(brd)-1), ";"), file=path, append=TRUE, sep="\n")
					wght <- style[[2]][grep("weight", style[[2]])]
					cat(paste0("\t\t\tstroke-width: ", as.numeric(gsub(".+\\s+", "", wght)), ";"), file=path, append=TRUE, sep="\n")
					opa <- style[[2]][grep("opacity", style[[2]])]
					cat(paste0("\t\t\tstroke-opacity: ", as.numeric(gsub(".+\\s+", "", opa)), ";"), file=path, append=TRUE, sep="\n")
					cat("\t\t}", file=path, append=TRUE, sep="\n")
				}
			}
		}
	}
	
	# extra style instructions end 
	cat("\t</style>", file=path, append=TRUE, sep="\n")
	#############################################################################################
	
	# end of head, start of body
	cat("</head>", file=path, append=TRUE, sep="\n")
	cat("<body>", file=path, append=TRUE, sep="\n")
	
	# map anchor
	cat("\t<div id=\"map\"></div>", file=path, append=TRUE, sep="\n")
	
	### map script start #######################################################################
	cat("\t<script type=\"text/javascript\">", file=path, append=TRUE, sep="\n")	
	
	# initialize the map
	if(is.na(center) || is.na(zoom)) cat("\t\tvar map = L.map('map')", file=path, append=TRUE, sep="\n")
	else cat(paste0("\t\tvar map = L.map('map').setView([", center[1], ", ", center[2], "],", zoom, ");"), file=path, append=TRUE, sep="\n")
	
	# base layer	
	for(n in 1: length(base.map)) {
		if(base.map[[n]]=="osm") { # OpenStreetMap default
			cat(paste0("\t\tvar baseMap", n, " = L.tileLayer('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {"), file=path, append=TRUE, sep="\n")
			cat("\t\t\tattribution: '&copy; <a href=\"http://openstreetmap.org/copyright\", target=\"_blank\">OpenStreetMap contributors</a>'", file=path, append=TRUE, sep="\n")
		} else if(base.map[[n]]=="tls") { # Thunderforest Landscape
			cat(paste0("\t\tvar baseMap", n, " = L.tileLayer('http://{s}.tile.thunderforest.com/landscape/{z}/{x}/{y}.png', {"), file=path, append=TRUE, sep="\n")
			cat("\t\t\tattribution: 'Tiles &copy; <a href=\"http://thunderforest.com\", target=\"_blank\">Thunderforest</a>, Map data &copy; <a href=\"http://openstreetmap.org/copyright\", target=\"_blank\">OpenStreetMap contributors</a>'", file=path, append=TRUE, sep="\n")
		} else if(base.map[[n]]=="mqosm") { # MapQuest OSM
			cat(paste0("\t\tvar baseMap", n, " = L.tileLayer('http://otile{s}.mqcdn.com/tiles/1.0.0/{type}/{z}/{x}/{y}.png', {"), file=path, append=TRUE, sep="\n")
			cat("\t\t\tsubdomains: '1234',", file=path, append=TRUE, sep="\n")
			cat("\t\t\ttype: 'osm',", file=path, append=TRUE, sep="\n")
			cat("\t\t\tattribution: 'Tiles &copy; <a href=\"http://www.mapquest.com\", target=\"_blank\">MapQuest</a>, Map data &copy; <a href=\"http://openstreetmap.org/copyright\", target=\"_blank\">OpenStreetMap contributors</a>'", file=path, append=TRUE, sep="\n")
		} else if(base.map[[n]]=="mqsat") { # MapQuest Open Aerial
			cat(paste0("\t\tvar baseMap", n, " = L.tileLayer('http://otile{s}.mqcdn.com/tiles/1.0.0/{type}/{z}/{x}/{y}.png', {"), file=path, append=TRUE, sep="\n")
			cat("\t\t\tsubdomains: '1234',", file=path, append=TRUE, sep="\n")
			cat("\t\t\ttype: 'sat',", file=path, append=TRUE, sep="\n")
			cat("\t\t\tmaxZoom: 11,", file=path, append=TRUE, sep="\n")
			cat("\t\t\tattribution: 'Tiles &copy; <a href=\"http://www.mapquest.com\", target=\"_blank\">MapQuest</a>, Imagery &copy; NASA/JPL-Caltech and USDA Farm Service Agency'", file=path, append=TRUE, sep="\n")
		} else if(base.map[[n]]=="water") { # Stamen Watercolor
			cat(paste0("\t\tvar baseMap", n, " = L.tileLayer('http://{s}.tile.stamen.com/watercolor/{z}/{x}/{y}.png', {"), file=path, append=TRUE, sep="\n")
			cat("\t\t\tattribution: 'Tiles &copy; <a href=\"http://stamen.com\", target=\"_blank\">Stamen Design</a> (<a href=\"http://creativecommons.org/licenses/by/3.0\", target=\"_blank\">CC BY 3.0</a>), Map data &copy; <a href=\"http://openstreetmap.org\", target=\"_blank\">OpenStreetMap</a> under <a href=\"http://creativecommons.org/licenses/by-sa/3.0\", target=\"_blank\">CC BY SA</a>'", file=path, append=TRUE, sep="\n")
		} else if(base.map[[n]]=="toner") { # Stamen Toner
			cat(paste0("\t\tvar baseMap", n, " = L.tileLayer('http://{s}.tile.stamen.com/toner/{z}/{x}/{y}.png', {"), file=path, append=TRUE, sep="\n")
			cat("\t\t\tattribution: 'Tiles &copy; <a href=\"http://stamen.com\", target=\"_blank\">Stamen Design</a> (<a href=\"http://creativecommons.org/licenses/by/3.0\", target=\"_blank\">CC BY 3.0</a>), Map data &copy; <a href=\"http://openstreetmap.org\", target=\"_blank\">OpenStreetMap</a> (<a href=\"http://creativecommons.org/licenses/by-sa/3.0\", target=\"_blank\">CC BY SA</a>)'", file=path, append=TRUE, sep="\n")
		}
		cat("\t\t});", file=path, append=TRUE, sep="\n")
		cat(paste0("\t\tbaseMap", n, ".addTo(map);"), file=path, append=TRUE, sep="\n") # add base layer
	}	
	
	# data layer
	if(any(!is.na(dat))) {
		# popup
		if(!any(is.na(popup))) {
			cat("\t\tfunction onEachFeature(feature, layer) {", file=path, append=TRUE, sep="\n")
			if(is.list(popup)) { # multilayer
				for(n in 1:length(popup)) {
					cat(paste0("\t\t\tif (feature.properties && ", paste0("feature.properties.", popup[[n]], collapse=" && \n\t\t\t\t"), ") {"), file=path, append=TRUE, sep="\n")
					if(length(popup[[n]])==1) {
						cat(paste0("\t\t\t\tlayer.bindPopup(\"", popup[[n]], ": \" + ", "feature.properties.", popup[[n]], ");"), file=path, append=TRUE, sep="\n")
					} else {
						cat("\t\t\t\tlayer.bindPopup(\"<table>\" +", file=path, append=TRUE, sep="\n")
						for(i in 1:length(popup[[n]])) {
							if(i%%2==0) cat(paste0("\t\t\t\t\t\"<tr class='evenrowcol'><td>", popup[[n]][i], "</td><td>\" + feature.properties.", popup[[n]][i], " + \"</td></tr>\" +"), file=path, append=TRUE, sep="\n")						
							else cat(paste0("\t\t\t\t\t\"<tr><td>", popup[[n]][i], "</td><td>\" + feature.properties.", popup[[n]][i], " + \"</td></tr>\" +"), file=path, append=TRUE, sep="\n")
						}
						cat("\t\t\t\t\t\"</table>\"", file=path, append=TRUE, sep="\n")
						cat("\t\t\t\t);", file=path, append=TRUE, sep="\n")
					}
					cat("\t\t\t}", file=path, append=TRUE, sep="\n")
				}
			} else { # one layer
				cat(paste0("\t\t\tif (feature.properties && ", paste0("feature.properties.", popup, collapse=" && \n\t\t\t\t"), ") {"), file=path, append=TRUE, sep="\n")
					if(length(popup)==1) {
						cat(paste0("\t\t\t\tlayer.bindPopup(\"", popup, ": \" + ", "feature.properties.", popup, ");"), file=path, append=TRUE, sep="\n")
					} else {
						cat("\t\t\t\tlayer.bindPopup(\"<table>\" +", file=path, append=TRUE, sep="\n")
						for(i in 1:length(popup)) {
							if(i%%2==0) cat(paste0("\t\t\t\t\t\"<tr class='evenrowcol'><td>", popup[i], "</td><td>\" + feature.properties.", popup[i], " + \"</td></tr>\" +"), file=path, append=TRUE, sep="\n")						
							else cat(paste0("\t\t\t\t\t\"<tr><td>", popup[i], "</td><td>\" + feature.properties.", popup[i], " + \"</td></tr>\" +"), file=path, append=TRUE, sep="\n")
						}
						cat("\t\t\t\t\t\"</table>\"", file=path, append=TRUE, sep="\n")
						cat("\t\t\t\t);", file=path, append=TRUE, sep="\n")
					}
					cat("\t\t\t}", file=path, append=TRUE, sep="\n")
			}
			cat("\t\t};", file=path, append=TRUE, sep="\n")
		}
		
		# styling
		if(any(!is.na(style))) {
			if(class(style)=="list") {
				for(n in 1:length(style)) {
					if(any(!is.na(style[[n]]))) {	
						cat(paste0("\t\tvar style", n, " = {"), file=path, append=TRUE, sep="\n")
						if(length(style[[n]])==1) cat(paste0("\t\t\t", style[[n]]), file=path, append=TRUE, sep="\n")
						else {
							for(i in 1:(length(style[[n]])-1)) cat(paste0("\t\t\t", style[[n]][i], ","), file=path, append=TRUE, sep="\n")
							cat(paste0("\t\t\t", style[[n]][length(style[[n]])]), file=path, append=TRUE, sep="\n")
						}
						cat("\t\t};", file=path, append=TRUE, sep="\n")
					}
				}
			} else {
				if(attr(style, "style.type")=="single") {
					cat(paste0("\t\tvar style1= {"), file=path, append=TRUE, sep="\n")
					if(length(style)==1) cat(paste0("\t\t\t", style), file=path, append=TRUE, sep="\n")
					else {
						for(i in 1:(length(style)-1)) cat(paste0("\t\t\t", style[i], ","), file=path, append=TRUE, sep="\n")
						cat(paste0("\t\t\t", style[length(style)]), file=path, append=TRUE, sep="\n")
					}
					cat("\t\t};", file=path, append=TRUE, sep="\n")
				}
				if(attr(style, "style.type")=="graduated") {
					cat("\t\tfunction getValue(x) {", file=path, append=TRUE, sep="\n")
					for(n in 1:length(style[[1]])) cat(paste0("\t\t\t", style[[1]][n]), file=path, append=TRUE, sep="\n")
					cat("\t\t}", file=path, append=TRUE, sep="\n")
					
					cat("\t\tfunction style(feature) {", file=path, append=TRUE, sep="\n")
					cat("\t\t\treturn {", file=path, append=TRUE, sep="\n")
					if(is.null(style[[2]])) {
						if(attr(style, "style.par")=="col") cat(paste0("\t\t\t\t\"color\": getValue(feature.properties.", attr(style, "property"), ")"), file=path, append=TRUE, sep="\n")
						else if(attr(style, "style.par")=="rad") cat(paste0("\t\t\t\t\"radius\": getValue(feature.properties.", attr(style, "property"), ")"), file=path, append=TRUE, sep="\n")
					} else {
						if(attr(style, "style.par")=="col") cat(paste0("\t\t\t\t\"color\": getValue(feature.properties.", attr(style, "property"), "),"), file=path, append=TRUE, sep="\n")
						else if(attr(style, "style.par")=="rad") cat(paste0("\t\t\t\t\"radius\": getValue(feature.properties.", attr(style, "property"), "),"), file=path, append=TRUE, sep="\n")
						if(length(style[[2]])==1) cat(paste0("\t\t\t\t", style[[2]]), file=path, append=TRUE, sep="\n")
						else {
							for(i in 1:(length(style[[2]])-1)) cat(paste0("\t\t\t\t", style[[2]][i], ","), file=path, append=TRUE, sep="\n")
							cat(paste0("\t\t\t\t", style[[2]][length(style[[2]])]), file=path, append=TRUE, sep="\n")
						}
					}
					cat("\t\t\t};", file=path, append=TRUE, sep="\n")
					cat("\t\t}", file=path, append=TRUE, sep="\n")
				}
				if(attr(style, "style.type")=="categorized") {
					cat("\t\tfunction getValue(x) {", file=path, append=TRUE, sep="\n")
					for(n in 1:length(style[[1]])) cat(paste0("\t\t\t", style[[1]][n]), file=path, append=TRUE, sep="\n")
					cat("\t\t}", file=path, append=TRUE, sep="\n")
					
					cat("\t\tfunction style(feature) {", file=path, append=TRUE, sep="\n")
					cat("\t\t\treturn {", file=path, append=TRUE, sep="\n")
					if(is.null(style[[2]])) {
						if(attr(style, "style.par")=="col") cat(paste0("\t\t\t\t\"color\": getValue(feature.properties.", attr(style, "property"), ")"), file=path, append=TRUE, sep="\n")
						else if(attr(style, "style.par")=="rad") cat(paste0("\t\t\t\t\"radius\": getValue(feature.properties.", attr(style, "property"), ")"), file=path, append=TRUE, sep="\n")
					} else {
						if(attr(style, "style.par")=="col") cat(paste0("\t\t\t\t\"color\": getValue(feature.properties.", attr(style, "property"), "),"), file=path, append=TRUE, sep="\n")
						else if(attr(style, "style.par")=="rad") cat(paste0("\t\t\t\t\"radius\": getValue(feature.properties.", attr(style, "property"), "),"), file=path, append=TRUE, sep="\n")
						if(length(style[[2]])==1) cat(paste0("\t\t\t\t", style[[2]]), file=path, append=TRUE, sep="\n")
						else {
							for(i in 1:(length(style[[2]])-1)) cat(paste0("\t\t\t\t", style[[2]][i], ","), file=path, append=TRUE, sep="\n")
							cat(paste0("\t\t\t\t", style[[2]][length(style[[2]])]), file=path, append=TRUE, sep="\n")
						}
					}
					cat("\t\t\t};", file=path, append=TRUE, sep="\n")
					cat("\t\t}", file=path, append=TRUE, sep="\n")
				}
			}
		}
		
		# data layer
		if(incl.data) {
			fit.bounds <- TRUE
			for(n in 1:length(dat)) {
				cat(paste0("\t\tvar data", n," ="), file=path, append=TRUE, sep="\n")
				if(!file.exists(dat[[n]])) stop("data file not found")
				con <- file(dat[[n]], "rt") 
				lns <- readLines(con)
				for(i in 1:length(lns)) {
					cat(paste0("\t\t\t", lns[i]), file=path, append=TRUE, sep="\n")
				}
				close(con)
				
				ft <- getFeatureType(dat[[n]])
				if(ft=="point") {
					cat(paste0("\t\tvar dat", n, " = L.geoJson(data", n, ", {"), file=path, append=TRUE, sep="\n")
					if(!any(is.na(popup))) cat("\t\t\tonEachFeature: onEachFeature,", file=path, append=TRUE, sep="\n")
					cat("\t\t\tpointToLayer: function (feature, latlng) {", file=path, append=TRUE, sep="\n")
			        if(any(is.na(style))) cat("\t\t\t\treturn L.circleMarker(latlng);", file=path, append=TRUE, sep="\n")
			        else {
			        	if(class(style)=="list") {
				        	if(attr(style[[n]], "style.type")=="single") cat(paste0("\t\t\t\treturn L.circleMarker(latlng, style", n, ");"), file=path, append=TRUE, sep="\n")
				        	else cat("\t\t\t\treturn L.circleMarker(latlng, style(feature));", file=path, append=TRUE, sep="\n")
			        	} else {
				        	if(attr(style, "style.type")=="single") cat(paste0("\t\t\t\treturn L.circleMarker(latlng, style", n, ");"), file=path, append=TRUE, sep="\n")
				        	else cat("\t\t\t\treturn L.circleMarker(latlng, style(feature));", file=path, append=TRUE, sep="\n")
			        	}
			        }
			    	cat("\t\t\t}", file=path, append=TRUE, sep="\n")
					cat("\t\t});", file=path, append=TRUE, sep="\n")
				} else if(ft=="line") {
					cat(paste0("\t\tvar dat", n, " = L.geoJson(data", n, ", {"), file=path, append=TRUE, sep="\n")
					if(any(!is.na(style)))   {
						if(!any(is.na(popup))) cat("\t\t\tonEachFeature: onEachFeature,", file=path, append=TRUE, sep="\n")
				        if(class(style)=="list") {
				        	if(attr(style[[n]], "style.type")=="single") cat(paste0("\t\t\tstyle: style", n), file=path, append=TRUE, sep="\n")
				        	else cat("\t\t\tstyle: style", file=path, append=TRUE, sep="\n")
				        } else {
				        	if(attr(style, "style.type")=="single") cat(paste0("\t\t\tstyle: style", n), file=path, append=TRUE, sep="\n")
				        	else cat("\t\t\tstyle: style", file=path, append=TRUE, sep="\n")
						}
					} else if(!is.na(popup)) cat("\t\t\tonEachFeature: onEachFeature", file=path, append=TRUE, sep="\n")
					cat("\t\t});", file=path, append=TRUE, sep="\n")
				} else if(ft=="polygon") {
					cat(paste0("\t\tvar dat", n, " = L.geoJson(data", n, ", {"), file=path, append=TRUE, sep="\n")
					if(any(!is.na(style)))   {
						if(!any(is.na(popup))) cat("\t\t\tonEachFeature: onEachFeature,", file=path, append=TRUE, sep="\n")
				        if(class(style)=="list") {
				        	cat(attr(style, "style.type"))
				        	if(attr(style[[n]], "style.type")=="single") cat(paste0("\t\t\tstyle: style", n), file=path, append=TRUE, sep="\n")
				        	else cat("\t\t\tstyle: style", file=path, append=TRUE, sep="\n")
				        } else {
				        	if(attr(style, "style.type")=="single") cat(paste0("\t\t\tstyle: style", n), file=path, append=TRUE, sep="\n")
				        	else cat("\t\t\tstyle: style", file=path, append=TRUE, sep="\n")
				        }
					} else if(!any(is.na(popup))) cat("\t\t\tonEachFeature: onEachFeature", file=path, append=TRUE, sep="\n")
					cat("\t\t});", file=path, append=TRUE, sep="\n")
				}
				if(is.na(center) || is.na(zoom)) {
					if(fit.bounds) {
						cat(paste0("\t\tmap.fitBounds(dat", n, ".getBounds());"), file=path, append=TRUE, sep="\n")
						fit.bounds <- FALSE
					}
				}
				cat(paste0("\t\tdat", n, ".addTo(map);"), file=path, append=TRUE, sep="\n")
			}			
		} else {
			fit.bounds <- TRUE
			for(n in 1:length(dat)) {
				ft <- getFeatureType(dat[[n]])
				if(ft=="point") {
					cat(paste0("\t\t$.getJSON($(\"link[rel=\'", paste0("dat", n), "\']\").attr(\"href\"), function(data) {"), file=path, append=TRUE, sep="\n")
					cat("\t\t\tvar dat = L.geoJson(data, {", file=path, append=TRUE, sep="\n")
					if(!any(is.na(popup))) cat("\t\t\t\tonEachFeature: onEachFeature,", file=path, append=TRUE, sep="\n")
					cat("\t\t\t\tpointToLayer: function (feature, latlng) {", file=path, append=TRUE, sep="\n")
			        if(any(is.na(style))) cat("\t\t\t\t\treturn L.circleMarker(latlng);", file=path, append=TRUE, sep="\n")
			        else {
			        	if(class(style)=="list") {
				        	if(attr(style[[n]], "style.type")=="single") cat(paste0("\t\t\t\t\treturn L.circleMarker(latlng, style", n, ");"), file=path, append=TRUE, sep="\n")
				        	else cat("\t\t\t\t\treturn L.circleMarker(latlng, style(feature));", file=path, append=TRUE, sep="\n")
			        	} else {
				        	if(attr(style, "style.type")=="single") cat(paste0("\t\t\t\t\treturn L.circleMarker(latlng, style", n, ");"), file=path, append=TRUE, sep="\n")
				        	else cat("\t\t\t\t\treturn L.circleMarker(latlng, style(feature));", file=path, append=TRUE, sep="\n")
			        	}
			        }
			    	cat("\t\t\t\t}", file=path, append=TRUE, sep="\n")
					cat("\t\t\t});", file=path, append=TRUE, sep="\n")
					if(is.na(center) || is.na(zoom)) {
						if(fit.bounds) {
							cat("\t\t\tmap.fitBounds(dat.getBounds());", file=path, append=TRUE, sep="\n")
							fit.bounds <- FALSE
						}
					}
					cat("\t\t\tdat.addTo(map);", file=path, append=TRUE, sep="\n")
					cat("\t\t});", file=path, append=TRUE, sep="\n")
				} else if(ft=="line") {
					cat(paste0("\t\t$.getJSON($(\"link[rel=\'", paste0("dat", n), "\']\").attr(\"href\"), function(data) {"), file=path, append=TRUE, sep="\n")
					cat("\t\t\tvar dat = L.geoJson(data, {", file=path, append=TRUE, sep="\n")
					if(any(!is.na(style)))   {
						if(!any(is.na(popup))) cat("\t\t\t\tonEachFeature: onEachFeature,", file=path, append=TRUE, sep="\n")
				        if(class(style)=="list") {
				        	if(attr(style[[n]], "style.type")=="single") cat(paste0("\t\t\t\tstyle: style", n), file=path, append=TRUE, sep="\n")
				        	else cat("\t\t\t\tstyle: style", file=path, append=TRUE, sep="\n")
				        } else {
				        	if(attr(style, "style.type")=="single") cat(paste0("\t\t\t\tstyle: style", n), file=path, append=TRUE, sep="\n")
				        	else cat("\t\t\t\tstyle: style", file=path, append=TRUE, sep="\n")
						}
					} else if(!any(is.na(popup))) cat("\t\t\t\tonEachFeature: onEachFeature", file=path, append=TRUE, sep="\n")
					cat("\t\t\t});", file=path, append=TRUE, sep="\n")
					if(is.na(center) || is.na(zoom)) {
						if(fit.bounds) {
							cat("\t\t\tmap.fitBounds(dat.getBounds());", file=path, append=TRUE, sep="\n")
							fit.bounds <- FALSE
						}
					}
					cat("\t\t\tdat.addTo(map);", file=path, append=TRUE, sep="\n")
					cat("\t\t});", file=path, append=TRUE, sep="\n")
				} else if(ft=="polygon") {
					cat(paste0("\t\t$.getJSON($(\"link[rel=\'", paste0("dat", n), "\']\").attr(\"href\"), function(data) {"), file=path, append=TRUE, sep="\n")
					cat("\t\t\tvar dat = L.geoJson(data, {", file=path, append=TRUE, sep="\n")
					if(any(!is.na(style)))   {
						if(!any(is.na(popup))) cat("\t\t\t\tonEachFeature: onEachFeature,", file=path, append=TRUE, sep="\n")
				        if(class(style)=="list") {
				        	if(attr(style[[n]], "style.type")=="single") cat(paste0("\t\t\t\tstyle: style", n), file=path, append=TRUE, sep="\n")
				        	else cat("\t\t\t\tstyle: style", file=path, append=TRUE, sep="\n")
				        } else {
				        	if(attr(style, "style.type")=="single") cat(paste0("\t\t\t\tstyle: style", n), file=path, append=TRUE, sep="\n")
				        	else cat("\t\t\t\tstyle: style", file=path, append=TRUE, sep="\n")
				        }
					} else if(!any(is.na(popup))) cat("\t\t\t\tonEachFeature: onEachFeature", file=path, append=TRUE, sep="\n")
					cat("\t\t\t});", file=path, append=TRUE, sep="\n")
					if(is.na(center) || is.na(zoom)) {
						if(fit.bounds) {
							cat("\t\t\tmap.fitBounds(dat.getBounds());", file=path, append=TRUE, sep="\n")
							fit.bounds <- FALSE
						}
					}
					cat("\t\t\tdat.addTo(map);", file=path, append=TRUE, sep="\n")
					cat("\t\t});", file=path, append=TRUE, sep="\n")
				}
			}
		}
	}
	
	# layer control
	if(length(base.map)>1) {
		cat("\t\tvar baseMaps = {", file=path, append=TRUE, sep="\n")
		for(n in 1:(length(base.map)-1)) {
			if(base.map[[n]]=="osm") cat(paste0("\t\t\t\"OpenStreetMap\": baseMap", n, ","), file=path, append=TRUE, sep="\n")
			if(base.map[[n]]=="tls") cat(paste0("\t\t\t\"Thunderforest Landscape\": baseMap", n, ","), file=path, append=TRUE, sep="\n")
			if(base.map[[n]]=="mqosm") cat(paste0("\t\t\t\"MapQuest OSM\": baseMap", n, ","), file=path, append=TRUE, sep="\n")
			if(base.map[[n]]=="mqsat") cat(paste0("\t\t\t\"MapQuest Open Aerial\": baseMap", n, ","), file=path, append=TRUE, sep="\n")
			if(base.map[[n]]=="water") cat(paste0("\t\t\t\"Stamen Watercolor\": baseMap", n, ","), file=path, append=TRUE, sep="\n")
			if(base.map[[n]]=="toner") cat(paste0("\t\t\t\"Stamen Toner\": baseMap", n, ","), file=path, append=TRUE, sep="\n")
		}
		if(base.map[[length(base.map)]]=="osm") cat(paste0("\t\t\t\"OpenStreetMap\": baseMap", length(base.map)), file=path, append=TRUE, sep="\n")
		if(base.map[[length(base.map)]]=="tls") cat(paste0("\t\t\t\"Thunderforest Landscape\": baseMap", length(base.map)), file=path, append=TRUE, sep="\n")
		if(base.map[[length(base.map)]]=="mqosm") cat(paste0("\t\t\t\"MapQuest OSM\": baseMap", length(base.map)), file=path, append=TRUE, sep="\n")
		if(base.map[[length(base.map)]]=="mqsat") cat(paste0("\t\t\t\"MapQuest Open Aerial\": baseMap", length(base.map)), file=path, append=TRUE, sep="\n")
		if(base.map[[length(base.map)]]=="water") cat(paste0("\t\t\t\"Stamen Watercolor\": baseMap", length(base.map)), file=path, append=TRUE, sep="\n")
		if(base.map[[length(base.map)]]=="toner") cat(paste0("\t\t\t\"Stamen Toner\": baseMap", length(base.map)), file=path, append=TRUE, sep="\n")
		cat("\t\t};", file=path, append=TRUE, sep="\n")
		cat("\t\tL.control.layers(baseMaps).addTo(map);", file=path, append=TRUE, sep="\n")
	}
	
	# add legend
	if(!any(is.na(style))) {
		if(class(style)=="list") { # multi single style
			sty <- NULL
			for(i in 1:length(style)) sty <- append(sty, attr(style[[i]], "style.type"))
			if(all(sty=="single")) {
				cat("\t\tvar legend = L.control({position: 'bottomright'});", file=path, append=TRUE, sep="\n")
				cat("\t\tlegend.onAdd = function(map) {", file=path, append=TRUE, sep="\n")
				cat("\t\t\tvar div = L.DomUtil.create('div', 'legend');", file=path, append=TRUE, sep="\n")
				if(!is.null(attr(style, "leg"))) cat(paste0("\t\t\tdiv.innerHTML += \'", attr(style, "leg"), "<br>\'"), file=path, append=TRUE, sep="\n")
				# rearrange layers for legend (point > line > polygon)
				n.dat <- 0
				dat.ra <- style.ra <- list()
				for(i in 1:length(dat)) {
					if(getFeatureType(dat[[i]])=="point") {
						dat.ra[[n.dat+1]] <- dat[[i]]
						style.ra[[n.dat+1]] <- style[[i]]
						if(!is.null(names(dat)[i])) names(dat.ra)[n.dat+1] <- names(dat)[i]
						n.dat <- n.dat+1
					}
				}
				if(n.dat<length(dat)) {
					for(i in 1:length(dat)) {
						if(getFeatureType(dat[[i]])=="line") {
							dat.ra[[n.dat+1]] <- dat[[i]]
							style.ra[[n.dat+1]] <- style[[i]]
							if(!is.null(names(dat)[i])) names(dat.ra)[n.dat+1] <- names(dat)[i]
							n.dat <- n.dat+1
						}
					}
				}
				if(n.dat<length(dat)) {
					for(i in 1:length(dat)) {
						if(getFeatureType(dat[[i]])=="polygon") {
							dat.ra[[n.dat+1]] <- dat[[i]]
							style.ra[[n.dat+1]] <- style[[i]]
							if(!is.null(names(dat)[i])) names(dat.ra)[n.dat+1] <- names(dat)[i]
							n.dat <- n.dat+1
						}
					}
				}
				
				# get max column width/height
				max.width <- 24
				max.lwd <- 2
				for(i in 1:length(style.ra)) {
					rad <- style.ra[[i]][grep("rad", style.ra[[i]])]
					if(length(rad)==0) rad <- "radius: 10"
					lwd <- style.ra[[i]][grep("weight", style.ra[[i]])]
					if(length(lwd)==0) lwd <- "weight: 2"
					rad <- substr(rad, 9, nchar(rad))
					lwd <- substr(lwd, 9, nchar(lwd))
					width <- as.numeric(rad)*2+as.numeric(lwd)
					if(getFeatureType(dat.ra[[i]])=="polygon") width <- as.numeric(lwd)*2
					if(width>max.width) max.width <- width
					
					lwd <- style.ra[[i]][grep("weight", style.ra[[i]])]
					if(length(lwd)==0) lwd <- "weight: 5"
					lwd <- substr(lwd, 9, nchar(lwd))
					l <- as.numeric(lwd)
					if(l>max.lwd) max.lwd <- l
				}
				# write legend
				for(i in 1:length(style.ra)) {
					cat("\t\t\tdiv.innerHTML +=", file=path, append=TRUE, sep="\n")
					fill <- style.ra[[i]][grep("fillColor", style.ra[[i]])]
					clr <- style.ra[[i]][grep("color", style.ra[[i]])]
					if(length(fill)==0) fill <- clr
					if(length(fill)==0) fill <- "color: \"#0033ff\""
					if(length(clr)==0) clr <- "color: \"#0033ff\""
					rad <- style.ra[[i]][grep("rad", style.ra[[i]])]
					if(length(rad)==0) rad <- "radius: 10"
					fill.opa <- style.ra[[i]][grep("fillOpacity", style.ra[[i]])]
					if(length(fill.opa)==0) fill.opa <- "fillOpacity: 0.2"
					opa <- style.ra[[i]][grep("opacity", style.ra[[i]])]
					if(length(opa)==0) opa <- "opacity: 0.5"
					lwd <- style.ra[[i]][grep("weight", style.ra[[i]])]
					
					ft <- getFeatureType(dat.ra[[i]])
					ttl <- names(dat.ra)[i]
					if(is.null(ttl)) ttl <- i
					else if(ttl=="") ttl <- i
										
					if(ft=="point") {
						rd <- substr(rad, 9, nchar(rad))
						if(length(lwd)==0) lwd <- "weight: 2"
						lwd <- substr(lwd, 9, nchar(lwd))
						st <- paste0("fill: ", substr(fill, nchar(fill)-7, nchar(fill)-1), "; stroke: ", substr(clr, nchar(clr)-7, nchar(clr)-1), "; fill-opacity: ", substr(fill.opa, 14, nchar(fill.opa)), "; stroke-opacity: ", substr(opa, 10, nchar(opa)), "; stroke-width: ", lwd, ";")
						
						cat(paste0("\t\t\t\t\t\'<table><tr><td class=\"shape\"><svg style=\"width: ", max.width, "px; height: ", as.numeric(rd)*2+as.numeric(lwd), "px;\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\"><circle class=\"crcl\" style=\"", st, "\" cx=\"", max.width/2, "\" cy=\"", (as.numeric(rd)*2+as.numeric(lwd))/2, "\" r=\"", rd, "\" /></svg></td><td class=\"value\">", ttl,"</td></tr></table>\'"), file=path, append=TRUE, sep="\n")
					} else if(ft=="line") {
						if(length(lwd)==0) lwd <- "weight: 5"
						lwd <- substr(lwd, 9, nchar(lwd))
						st <- paste0("stroke: ", substr(clr, nchar(clr)-7, nchar(clr)-1), "; stroke-opacity: ", substr(opa, 10, nchar(opa)), "; stroke-width: ", lwd, ";")
						if(as.numeric(lwd)<18) hght <- 18
						else hght <- as.numeric(lwd)
						cat(paste0("\t\t\t\t\t\'<table><tr><td class=\"shape\"><svg style=\"width: ", max.width, "px; height: ", hght, "px;\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\"><line class=\"ln\" style=\"", st, "\" x1=\"", max.lwd+1-(max.lwd-as.numeric(lwd)/2), "\" y1=\"", hght/2, "\" x2=\"", max.width-max.lwd-1+(max.lwd-as.numeric(lwd)/2), "\" y2=\"", hght/2, "\" /></svg></td><td class=\"value\">", ttl, "</td></tr></table>\'"), file=path, append=TRUE, sep="\n")
					} else if(ft=="polygon") {
						if(length(lwd)==0) lwd <- "weight: 5"
						lwd <- substr(lwd, 9, nchar(lwd))
						st <- paste0("fill: ", substr(fill, nchar(fill)-7, nchar(fill)-1), "; stroke: ", substr(clr, nchar(clr)-7, nchar(clr)-1), "; fill-opacity: ", substr(fill.opa, 14, nchar(fill.opa)), "; stroke-opacity: ", substr(opa, 10, nchar(opa)), "; stroke-width: ", lwd, ";")
						if(as.numeric(lwd)<11) hght <- 22
						else hght <- as.numeric(lwd)*2
						cat(paste0("\t\t\t\t\t\'<table><tr><td class=\"shape\"><svg style=\"width: ", max.width, "px; height: ", hght, "px;\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\"><polygon class=\"plgn\" style=\"", st, "\" points=\"", max.lwd+1-(max.lwd-as.numeric(lwd)/2), ",", as.numeric(lwd)/2, " ", max.width-max.lwd-1+(max.lwd-as.numeric(lwd)/2), ",", hght/2, " ", max.width-max.lwd-1+(max.lwd-as.numeric(lwd)/2), ",", hght-as.numeric(lwd)/2, " ", max.lwd+1-(max.lwd-as.numeric(lwd)/2), ",", hght-as.numeric(lwd)/2, "\" /></svg></td><td class=\"value\">", ttl, "</td></tr></table>\'"), file=path, append=TRUE, sep="\n")
					}
				}
				cat("\t\t\treturn div;", file=path, append=TRUE, sep="\n")
				cat("\t\t};", file=path, append=TRUE, sep="\n")
				cat("\t\tlegend.addTo(map);", file=path, append=TRUE, sep="\n")
			}
		}
		else if(class(style)=="leafletr.style") {
			if(attr(style, "style.type")=="graduated") { # graduated style
				cat("\t\tvar legend = L.control({position: 'bottomright'});", file=path, append=TRUE, sep="\n")
				cat("\t\tlegend.onAdd = function(map) {", file=path, append=TRUE, sep="\n")
				cat("\t\t\tvar div = L.DomUtil.create('div', 'legend'),", file=path, append=TRUE, sep="\n")
		        cat("\t\t\tlabels = [],", file=path, append=TRUE, sep="\n")
		        cat(paste0("\t\t\tgrades = [", paste(attr(style, "breaks"), collapse=", "), "];"), file=path, append=TRUE, sep="\n")
		        if(!is.null(attr(style, "leg"))) cat(paste0("\t\t\tdiv.innerHTML += \'", attr(style, "leg"), "<br>\'"), file=path, append=TRUE, sep="\n")
				if(attr(style, "style.par")=="col") { # color scale
				    if(attr(style, "out")==0) { # left and right closed
					    cat("\t\t\tfor (var i = 0; i < grades.length-1; i++) {", file=path, append=TRUE, sep="\n")
				        cat("\t\t\t\tdiv.innerHTML +=", file=path, append=TRUE, sep="\n")
				        if(attr(style, "right")) cat("\t\t\t\t\t\'<i style=\"background:\' + getValue(grades[i]) + \'\"></i> \' +", file=path, append=TRUE, sep="\n")
				        else cat("\t\t\t\t\t\'<i style=\"background:\' + getValue(grades[i]+(grades[1]-grades[0])*0.01) + \'\"></i> \' +", file=path, append=TRUE, sep="\n")
						cat("\t\t\t\t\tgrades[i] + \'&ndash;\' + grades[i + 1] + \'<br>\';", file=path, append=TRUE, sep="\n")
						cat("\t\t\t}", file=path, append=TRUE, sep="\n")
					} else if(attr(style, "out")==1) { # left closed and right open
					    cat("\t\t\tfor (var i = 0; i < grades.length; i++) {", file=path, append=TRUE, sep="\n")
				        cat("\t\t\t\tdiv.innerHTML +=", file=path, append=TRUE, sep="\n")
				        if(attr(style, "right")) {
				        	cat("\t\t\t\t\t\'<i style=\"background:\' + getValue(grades[i]) + \'\"></i> \' +", file=path, append=TRUE, sep="\n")
				        	cat("\t\t\t\t\t(grades[i + 1] ? grades[i] + \'&ndash;\' + grades[i + 1] + \'<br>\' : \'&ge;\' + grades[i]);", file=path, append=TRUE, sep="\n")
				        } else {
				        	cat("\t\t\t\t\t\'<i style=\"background:\' + getValue(grades[i]+(grades[1]-grades[0])*0.01) + \'\"></i> \' +", file=path, append=TRUE, sep="\n")
				        	cat("\t\t\t\t\t(grades[i + 1] ? grades[i] + \'&ndash;\' + grades[i + 1] + \'<br>\' : \'&gt;\' + grades[i]);", file=path, append=TRUE, sep="\n")
				        }
						cat("\t\t\t}", file=path, append=TRUE, sep="\n")
					} else if(attr(style, "out")==2) { # left open and right closed
						cat("\t\t\tdiv.innerHTML +=", file=path, append=TRUE, sep="\n")
						if(attr(style, "right")) cat("\t\t\t\t\'<i style=\"background:\' + getValue(grades[0]-(grades[1]-grades[0])*0.01) + \'\"></i> &lt;\' + grades[0] + \'<br>\';", file=path, append=TRUE, sep="\n")
						else cat("\t\t\t\t\'<i style=\"background:\' + getValue(grades[0]-(grades[1]-grades[0])*0.01) + \'\"></i> &le;\' + grades[0] + \'<br>\';", file=path, append=TRUE, sep="\n")
						cat("\t\t\tfor (var i = 0; i < grades.length-1; i++) {", file=path, append=TRUE, sep="\n")
						cat("\t\t\t\tdiv.innerHTML +=", file=path, append=TRUE, sep="\n")
						if(attr(style, "right")) cat("\t\t\t\t\t\'<i style=\"background:\' + getValue(grades[i]) + \'\"></i> \' +", file=path, append=TRUE, sep="\n")
						else cat("\t\t\t\t\t\'<i style=\"background:\' + getValue(grades[i]+(grades[1]-grades[0])*0.01) + \'\"></i> \' +", file=path, append=TRUE, sep="\n")
						cat("\t\t\t\t\t(i<grades.length-1 ? grades[i] + \'&ndash;\' + grades[i+1] + \'<br>\' : grades[i] + \'&ndash;\' + grades[i+1]);", file=path, append=TRUE, sep="\n")
						cat("\t\t\t}", file=path, append=TRUE, sep="\n")
					} else { # left and right open
						cat("\t\t\tdiv.innerHTML +=", file=path, append=TRUE, sep="\n")
						if(attr(style, "right")) cat("\t\t\t\t\'<i style=\"background:\' + getValue(grades[0]-(grades[1]-grades[0])*0.01) + \'\"></i> &lt;\' + grades[0] + \'<br>\';", file=path, append=TRUE, sep="\n")
						else cat("\t\t\t\t\'<i style=\"background:\' + getValue(grades[0]-(grades[1]-grades[0])*0.01) + \'\"></i> &le;\' + grades[0] + \'<br>\';", file=path, append=TRUE, sep="\n")
						cat("\t\t\tfor (var i = 0; i < grades.length; i++) {", file=path, append=TRUE, sep="\n")
				        cat("\t\t\t\tdiv.innerHTML +=", file=path, append=TRUE, sep="\n")
				        if(attr(style, "right")) {
				        	cat("\t\t\t\t\t\'<i style=\"background:\' + getValue(grades[i]) + \'\"></i> \' +", file=path, append=TRUE, sep="\n")
				        	cat("\t\t\t\t\t(i<grades.length-1 ? grades[i] + \'&ndash;\' + grades[i+1] + \'<br>\' : \'&ge;\' + grades[i]);", file=path, append=TRUE, sep="\n")
						} else {
				        	cat("\t\t\t\t\t\'<i style=\"background:\' + getValue(grades[i]+(grades[1]-grades[0])*0.01) + \'\"></i> \' +", file=path, append=TRUE, sep="\n")
							cat("\t\t\t\t\t(i<grades.length-1 ? grades[i] + \'&ndash;\' + grades[i+1] + \'<br>\' : \'&gt;\' + grades[i]);", file=path, append=TRUE, sep="\n")
				        }
						cat("\t\t\t}", file=path, append=TRUE, sep="\n")
					}
				} else if(attr(style, "style.par")=="rad") { # radius scale
					if(attr(style, "out")==0) { # left and right closed
					    cat("\t\t\tfor (var i = 0; i < grades.length-1; i++) {", file=path, append=TRUE, sep="\n")
					    cat("\t\t\t\tdiv.innerHTML +=", file=path, append=TRUE, sep="\n")
					    wght <- style[[2]][grep("weight", style[[2]])]
					    cat(paste0("\t\t\t\t\t\'<table style=\"border: none;\"><tr><td class=\"circle\" style=\"width: \' + (getValue(grades[grades.length-2])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\"><svg style=\"width: \' + (getValue(grades[i])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px; height: \' + (getValue(grades[i])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\"><circle class=\"crcl\" cx=\"\' + (getValue(grades[i])+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" cy=\"\' + (getValue(grades[i])+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" r=\"\' + getValue(grades[i]) + \'\" /></svg></td><td class=\"value\">\' + grades[i] + \'&ndash;\' + grades[i+1] + \'</td></tr></table>\'"), file=path, append=TRUE, sep="\n")
						cat("\t\t\t}", file=path, append=TRUE, sep="\n")
					} else if(attr(style, "out")==1) { # left closed and right open
					    wght <- style[[2]][grep("weight", style[[2]])]
					    if(attr(style, "right")) {
					    	cat("\t\t\tfor (var i = 0; i < grades.length-1; i++) {", file=path, append=TRUE, sep="\n")
					    	cat("\t\t\t\tdiv.innerHTML +=", file=path, append=TRUE, sep="\n")
					    	cat(paste0("\t\t\t\t\t\'<table style=\"border: none;\"><tr><td class=\"circle\" style=\"width: \' + (getValue(grades[grades.length-1])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\"><svg style=\"width: \' + (getValue(grades[i])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px; height: \' + (getValue(grades[i])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\"><circle class=\"crcl\" cx=\"\' + (getValue(grades[i])+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" cy=\"\' + (getValue(grades[i])+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" r=\"\' + getValue(grades[i]) + \'\" /></svg></td><td class=\"value\">\' + grades[i] + \'&ndash;\' + grades[i+1] + \'</td></tr></table>\'"), file=path, append=TRUE, sep="\n")
					    	cat("\t\t\t}", file=path, append=TRUE, sep="\n")
							cat("\t\t\tdiv.innerHTML +=", file=path, append=TRUE, sep="\n")
							cat(paste0("\t\t\t\t\'<table style=\"border: none;\"><tr><td class=\"circle\" style=\"width: \' + (getValue(grades[grades.length-1])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\"><svg style=\"width: \' + (getValue(grades[grades.length-1])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px; height: \' + (getValue(grades[grades.length-1])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\"><circle class=\"crcl\" cx=\"\' + (getValue(grades[grades.length-1])+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" cy=\"\' + (getValue(grades[grades.length-1])+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" r=\"\' + getValue(grades[grades.length-1]) + \'\" /></svg></td><td class=\"value\">\' + \'&ge;\' + grades[grades.length-1] + \'</td></tr></table>\'"), file=path, append=TRUE, sep="\n")
					    } else {
					    	cat("\t\t\tfor (var i = 1; i < grades.length; i++) {", file=path, append=TRUE, sep="\n")
					    	cat("\t\t\t\tdiv.innerHTML +=", file=path, append=TRUE, sep="\n")
					    	cat(paste0("\t\t\t\t\t\'<table style=\"border: none;\"><tr><td class=\"circle\" style=\"width: \' + (getValue(grades[grades.length-1]+1)*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\"><svg style=\"width: \' + (getValue(grades[i])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px; height: \' + (getValue(grades[i])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\"><circle class=\"crcl\" cx=\"\' + (getValue(grades[i])+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" cy=\"\' + (getValue(grades[i])+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" r=\"\' + getValue(grades[i]) + \'\" /></svg></td><td class=\"value\">\' + grades[i-1] + \'&ndash;\' + grades[i] + \'</td></tr></table>\'"), file=path, append=TRUE, sep="\n")
					    	cat("\t\t\t}", file=path, append=TRUE, sep="\n")
							cat("\t\t\tdiv.innerHTML +=", file=path, append=TRUE, sep="\n")
							cat(paste0("\t\t\t\t\'<table style=\"border: none;\"><tr><td class=\"circle\" style=\"width: \' + (getValue(grades[grades.length-1]+1)*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\"><svg style=\"width: \' + (getValue(grades[grades.length-1]+1)*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px; height: \' + (getValue(grades[grades.length-1]+1)*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\"><circle class=\"crcl\" cx=\"\' + (getValue(grades[grades.length-1]+1)+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" cy=\"\' + (getValue(grades[grades.length-1]+1)+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" r=\"\' + getValue(grades[grades.length-1]+1) + \'\" /></svg></td><td class=\"value\">\' + \'&gt;\' + grades[grades.length-1] + \'</td></tr></table>\'"), file=path, append=TRUE, sep="\n")
					    }
					} else if(attr(style, "out")==2) { # left open and right closed
						if(attr(style, "right")) {
							cat("\t\t\tdiv.innerHTML +=", file=path, append=TRUE, sep="\n")
							cat(paste0("\t\t\t\t\'<table style=\"border: none;\"><tr><td class=\"circle\" style=\"width: \' + (getValue(grades[grades.length-2])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\"><svg style=\"width: \' + (getValue(grades[0]-1)*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px; height: \' + (getValue(grades[0]-1)*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\"><circle class=\"crcl\" cx=\"\' + (getValue(grades[0]-1)+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" cy=\"\' + (getValue(grades[0]-1)+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" r=\"\' + getValue(grades[0]-1) + \'\" /></svg></td><td class=\"value\">\' + \'&lt;\' + grades[0] + \'</td></tr></table>\'"), file=path, append=TRUE, sep="\n")
							cat("\t\t\tfor (var i = 1; i < grades.length; i++) {", file=path, append=TRUE, sep="\n")
					    	cat("\t\t\t\tdiv.innerHTML +=", file=path, append=TRUE, sep="\n")
					    	cat(paste0("\t\t\t\t\t\'<table style=\"border: none;\"><tr><td class=\"circle\" style=\"width: \' + (getValue(grades[grades.length-2])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\"><svg style=\"width: \' + (getValue(grades[i-1])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px; height: \' + (getValue(grades[i-1])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\"><circle class=\"crcl\" cx=\"\' + (getValue(grades[i-1])+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" cy=\"\' + (getValue(grades[i-1])+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" r=\"\' + getValue(grades[i-1]) + \'\" /></svg></td><td class=\"value\">\' + grades[i-1] + \'&ndash;\' + grades[i] + \'</td></tr></table>\'"), file=path, append=TRUE, sep="\n")
					    	cat("\t\t\t}", file=path, append=TRUE, sep="\n")
						} else {
							cat("\t\t\tdiv.innerHTML +=", file=path, append=TRUE, sep="\n")
							cat(paste0("\t\t\t\t\'<table style=\"border: none;\"><tr><td class=\"circle\" style=\"width: \' + (getValue(grades[grades.length-1])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\"><svg style=\"width: \' + (getValue(grades[0])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px; height: \' + (getValue(grades[0])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\"><circle class=\"crcl\" cx=\"\' + (getValue(grades[0])+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" cy=\"\' + (getValue(grades[0])+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" r=\"\' + getValue(grades[0]) + \'\" /></svg></td><td class=\"value\">\' + \'&le;\' + grades[0] + \'</td></tr></table>\'"), file=path, append=TRUE, sep="\n")
							cat("\t\t\tfor (var i = 1; i < grades.length; i++) {", file=path, append=TRUE, sep="\n")
					    	cat("\t\t\t\tdiv.innerHTML +=", file=path, append=TRUE, sep="\n")
					    	cat(paste0("\t\t\t\t\t\'<table style=\"border: none;\"><tr><td class=\"circle\" style=\"width: \' + (getValue(grades[grades.length-1])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\"><svg style=\"width: \' + (getValue(grades[i])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px; height: \' + (getValue(grades[i])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\"><circle class=\"crcl\" cx=\"\' + (getValue(grades[i])+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" cy=\"\' + (getValue(grades[i])+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" r=\"\' + getValue(grades[i]) + \'\" /></svg></td><td class=\"value\">\' + grades[i-1] + \'&ndash;\' + grades[i] + \'</td></tr></table>\'"), file=path, append=TRUE, sep="\n")
					    	cat("\t\t\t}", file=path, append=TRUE, sep="\n")
						}
					} else { # left and right open
						if(attr(style, "right")) {
							cat("\t\t\tdiv.innerHTML +=", file=path, append=TRUE, sep="\n")
							cat(paste0("\t\t\t\t\'<table style=\"border: none;\"><tr><td class=\"circle\" style=\"width: \' + (getValue(grades[grades.length-1])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\"><svg style=\"width: \' + (getValue(grades[0]-1)*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px; height: \' + (getValue(grades[0]-1)*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\"><circle class=\"crcl\" cx=\"\' + (getValue(grades[0]-1)+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" cy=\"\' + (getValue(grades[0]-1)+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" r=\"\' + getValue(grades[0]-1) + \'\" /></svg></td><td class=\"value\">\' + \'&lt;\' + grades[0] + \'</td></tr></table>\'"), file=path, append=TRUE, sep="\n")
							cat("\t\t\tfor (var i = 1; i < grades.length; i++) {", file=path, append=TRUE, sep="\n")
					    	cat("\t\t\t\tdiv.innerHTML +=", file=path, append=TRUE, sep="\n")
					    	cat(paste0("\t\t\t\t\t\'<table style=\"border: none;\"><tr><td class=\"circle\" style=\"width: \' + (getValue(grades[grades.length-1])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\"><svg style=\"width: \' + (getValue(grades[i-1])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px; height: \' + (getValue(grades[i-1])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\"><circle class=\"crcl\" cx=\"\' + (getValue(grades[i-1])+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" cy=\"\' + (getValue(grades[i-1])+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" r=\"\' + getValue(grades[i-1]) + \'\" /></svg></td><td class=\"value\">\' + grades[i-1] + \'&ndash;\' + grades[i] + \'</td></tr></table>\'"), file=path, append=TRUE, sep="\n")
					    	cat("\t\t\t}", file=path, append=TRUE, sep="\n")
							cat("\t\t\tdiv.innerHTML +=", file=path, append=TRUE, sep="\n")
							cat(paste0("\t\t\t\t\'<table style=\"border: none;\"><tr><td class=\"circle\" style=\"width: \' + (getValue(grades[grades.length-1])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\"><svg style=\"width: \' + (getValue(grades[grades.length-1])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px; height: \' + (getValue(grades[grades.length-1])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\"><circle class=\"crcl\" cx=\"\' + (getValue(grades[grades.length-1])+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" cy=\"\' + (getValue(grades[grades.length-1])+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" r=\"\' + getValue(grades[grades.length-1]) + \'\" /></svg></td><td class=\"value\">\' + \'&ge;\' + grades[grades.length-1] + \'</td></tr></table>\'"), file=path, append=TRUE, sep="\n")
						} else {
							cat("\t\t\tdiv.innerHTML +=", file=path, append=TRUE, sep="\n")
							cat(paste0("\t\t\t\t\'<table style=\"border: none;\"><tr><td class=\"circle\" style=\"width: \' + (getValue(grades[grades.length-1])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\"><svg style=\"width: \' + (getValue(grades[0])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px; height: \' + (getValue(grades[0])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\"><circle class=\"crcl\" cx=\"\' + (getValue(grades[0])+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" cy=\"\' + (getValue(grades[0])+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" r=\"\' + getValue(grades[0]) + \'\" /></svg></td><td class=\"value\">\' + \'&le;\' + grades[0] + \'</td></tr></table>\'"), file=path, append=TRUE, sep="\n")
							cat("\t\t\tfor (var i = 1; i < grades.length; i++) {", file=path, append=TRUE, sep="\n")
					    	cat("\t\t\t\tdiv.innerHTML +=", file=path, append=TRUE, sep="\n")
					    	cat(paste0("\t\t\t\t\t\'<table style=\"border: none;\"><tr><td class=\"circle\" style=\"width: \' + (getValue(grades[grades.length-1])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\"><svg style=\"width: \' + (getValue(grades[i])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px; height: \' + (getValue(grades[i])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\"><circle class=\"crcl\" cx=\"\' + (getValue(grades[i])+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" cy=\"\' + (getValue(grades[i])+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" r=\"\' + getValue(grades[i]) + \'\" /></svg></td><td class=\"value\">\' + grades[i-1] + \'&ndash;\' + grades[i] + \'</td></tr></table>\'"), file=path, append=TRUE, sep="\n")
					    	cat("\t\t\t}", file=path, append=TRUE, sep="\n")
							cat("\t\t\tdiv.innerHTML +=", file=path, append=TRUE, sep="\n")
							cat(paste0("\t\t\t\t\'<table style=\"border: none;\"><tr><td class=\"circle\" style=\"width: \' + (getValue(grades[grades.length-1]+1)*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\"><svg style=\"width: \' + (getValue(grades[grades.length-1]+1)*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px; height: \' + (getValue(grades[grades.length-1]+1)*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\"><circle class=\"crcl\" cx=\"\' + (getValue(grades[grades.length-1]+1)+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" cy=\"\' + (getValue(grades[grades.length-1]+1)+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" r=\"\' + getValue(grades[grades.length-1]+1) + \'\" /></svg></td><td class=\"value\">\' + \'&gt;\' + grades[grades.length-1] + \'</td></tr></table>\'"), file=path, append=TRUE, sep="\n")
						}
					}
				}
				
				cat("\t\t\treturn div;", file=path, append=TRUE, sep="\n")
				cat("\t\t};", file=path, append=TRUE, sep="\n")
				cat("\t\tlegend.addTo(map);", file=path, append=TRUE, sep="\n")
			}
			else if(attr(style, "style.type")=="categorized") { # categorized style
				cat("\t\tvar legend = L.control({position: 'bottomright'});", file=path, append=TRUE, sep="\n")
				cat("\t\tlegend.onAdd = function(map) {", file=path, append=TRUE, sep="\n")
				cat("\t\t\tvar div = L.DomUtil.create('div', 'legend'),", file=path, append=TRUE, sep="\n")
		        cat("\t\t\tlabels = [],", file=path, append=TRUE, sep="\n")
		        cat(paste0("\t\t\tcats = [\"", paste(attr(style, "values"), collapse="\", \""), "\"];"), file=path, append=TRUE, sep="\n")
		        if(!is.null(attr(style, "leg"))) cat(paste0("\t\t\tdiv.innerHTML += \'", attr(style, "leg"), "<br>\'"), file=path, append=TRUE, sep="\n")
				if(attr(style, "style.par")=="col") { # color scale
					cat("\t\t\tfor (var i = 0; i < cats.length; i++) {", file=path, append=TRUE, sep="\n")
				    cat("\t\t\t\tdiv.innerHTML +=", file=path, append=TRUE, sep="\n")
				    cat("\t\t\t\t\t\'<i style=\"background:\' + getValue(cats[i]) + \'\"></i> \' +", file=path, append=TRUE, sep="\n")
					cat("\t\t\t\t\tcats[i] + \'<br>\';", file=path, append=TRUE, sep="\n")
					cat("\t\t\t}", file=path, append=TRUE, sep="\n")
					if(!is.null(attr(style, "na"))) {
						cat("\t\t\tdiv.innerHTML +=", file=path, append=TRUE, sep="\n")
				    	cat(paste0("\t\t\t\t\'<i style=\"background:\' + getValue() + \'\"></i> ", attr(style, "na"), "\'"), file=path, append=TRUE, sep="\n")
					}
				} else if(attr(style, "style.par")=="rad") { # radius scale
					cat("\t\t\tfor (var i = 0; i < cats.length; i++) {", file=path, append=TRUE, sep="\n")
				    cat("\t\t\t\tdiv.innerHTML +=", file=path, append=TRUE, sep="\n")
				    wght <- style[[2]][grep("weight", style[[2]])]
				    cat(paste0("\t\t\t\t\t\'<table style=\"border: none;\"><tr><td class=\"circle\" style=\"width: \' + (getValue(cats[cats.length-1])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\"><svg style=\"width: \' + (getValue(cats[i])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px; height: \' + (getValue(cats[i])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\"><circle class=\"crcl\" cx=\"\' + (getValue(cats[i])+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" cy=\"\' + (getValue(cats[i])+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" r=\"\' + getValue(cats[i]) + \'\" /></svg></td><td class=\"value\">\' + cats[i] + \'</td></tr></table>\'"), file=path, append=TRUE, sep="\n")
					cat("\t\t\t}", file=path, append=TRUE, sep="\n")
					if(!is.null(attr(style, "na"))) {
						cat("\t\t\tdiv.innerHTML +=", file=path, append=TRUE, sep="\n")
						cat(paste0("\t\t\t\t\t\'<table style=\"border: none;\"><tr><td class=\"circle\" style=\"width: \' + (getValue(cats[cats.length-1])*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\"><svg style=\"width: \' + (getValue()*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px; height: \' + (getValue()*2+", as.numeric(gsub(".+\\s+", "", wght))*2-1, ") + \'px;\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\"><circle class=\"crcl\" cx=\"\' + (getValue()+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" cy=\"\' + (getValue()+", as.numeric(gsub(".+\\s+", "", wght)), ") + \'\" r=\"\' + getValue() + \'\" /></svg></td><td class=\"value\">\' + \'", attr(style, "na"), "\' + \'</td></tr></table>\'"), file=path, append=TRUE, sep="\n")
					}
				}
				cat("\t\t\treturn div;", file=path, append=TRUE, sep="\n")
				cat("\t\t};", file=path, append=TRUE, sep="\n")
				cat("\t\tlegend.addTo(map);", file=path, append=TRUE, sep="\n")
			}
		}
	}
		
	# map script end 
	cat("\t</script>", file=path, append=TRUE, sep="\n")
	#############################################################################################
	
	# closing
	cat("</body>", file=path, append=TRUE, sep="\n")
	cat("</html>", file=path, append=TRUE)	
}
