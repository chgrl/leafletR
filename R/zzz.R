.onAttach <- 
function(libname, pkgname) {
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), fields="Version")
    packageStartupMessage(" ")
    packageStartupMessage(paste("This is", pkgname, ver))
    packageStartupMessage(" ")
    packageStartupMessage("Type changes(\"leafletR\") to see changes/bug fixes, help(leafletR) for documentation")
    packageStartupMessage("or citation(\"leafletR\") for how to cite leafletR.")
    packageStartupMessage(" ")
}


.onLoad <- function(libname, pkgname) {
	options(leafletBaseMaps= list(
		osm = list(
			title = "OpenStreetMap",
			url = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
			options = list(
				attribution='&copy; <a href="http://openstreetmap.org/copyright", target="_blank">OpenStreetMap contributors</a>'
			)
		),
		tls = list(
			title= "Thunderforest Landscape",
			url = "http://{s}.tile.thunderforest.com/landscape/{z}/{x}/{y}.png",
			options = list(
				attribution='Tiles &copy; <a href="http://thunderforest.com", target="_blank">Thunderforest</a>, Map data &copy; <a href="http://openstreetmap.org/copyright", target="_blan\">OpenStreetMap contributors</a>'
			)
		),
		mqosm = list(
			title = "MapQuest OSM",
			url = "http://otile{s}.mqcdn.com/tiles/1.0.0/{type}/{z}/{x}/{y}.png",
			options = list(
				subdomains= '1234',
				type= 'osm',
				attribution='Tiles &copy; <a href=\"http://www.mapquest.com\", target=\"_blank\">MapQuest</a>, Map data &copy; <a href=\"http://openstreetmap.org/copyright\", target=\"_blank\">OpenStreetMap contributors</a>'
			)
		),
		mqsat = list(
			title = "MapQuest Open Aerial",
			url = "http://otile{s}.mqcdn.com/tiles/1.0.0/{type}/{z}/{x}/{y}.png",
			options = list(
				subdomains= '1234',
				type= 'sat',
				maxZoom= 11,
				attribution='Tiles &copy; <a href=\"http://www.mapquest.com\", target=\"_blank\">MapQuest</a>, Imagery &copy; NASA/JPL-Caltech and USDA Farm Service Agency'
			)
		),
		water = list(
			title = "Stamen Watercolor",
			url = "http://{s}.tile.stamen.com/watercolor/{z}/{x}/{y}.png",
			options = list(
				attribution='Tiles &copy; <a href=\"http://stamen.com\", target=\"_blank\">Stamen Design</a> (<a href=\"http://creativecommons.org/licenses/by/3.0\", target=\"_blank\">CC BY 3.0</a>), Map data &copy; <a href=\"http://openstreetmap.org\", target=\"_blank\">OpenStreetMap</a> under <a href=\"http://creativecommons.org/licenses/by-sa/3.0\", target=\"_blank\">CC BY SA</a>'
			)
		),
		toner = list(
			title = "Stamen Toner",
			url = "http://{s}.tile.stamen.com/toner/{z}/{x}/{y}.png",
			options = list(
				attribution='Tiles &copy; <a href=\"http://stamen.com\", target=\"_blank\">Stamen Design</a> (<a href=\"http://creativecommons.org/licenses/by/3.0\", target=\"_blank\">CC BY 3.0</a>), Map data &copy; <a href=\"http://openstreetmap.org/copyright\", target=\"_blank\">OpenStreetMap contributors</a>'
			)
		),
		tonerbg = list(
			title = "Stamen Toner background",
			url = "http://{s}.tile.stamen.com/toner-background/{z}/{x}/{y}.png",
			options = list(
				attribution='Tiles &copy; <a href=\"http://stamen.com\", target=\"_blank\">Stamen Design</a> (<a href=\"http://creativecommons.org/licenses/by/3.0\", target=\"_blank\">CC BY 3.0</a>), Map data &copy; <a href=\"http://openstreetmap.org\", target=\"_blank\">OpenStreetMap</a> (<a href=\"http://creativecommons.org/licenses/by-sa/3.0\", target=\"_blank\">CC BY SA</a>)'
			)
		),
		tonerlite = list(
			title = "Stamen Toner lite",
			url = "http://{s}.tile.stamen.com/toner-lite/{z}/{x}/{y}.png",
			options = list(
				attribution='Tiles &copy; <a href=\"http://stamen.com\", target=\"_blank\">Stamen Design</a> (<a href=\"http://creativecommons.org/licenses/by/3.0\", target=\"_blank\">CC BY 3.0</a>), Map data &copy; <a href=\"http://openstreetmap.org\", target=\"_blank\">OpenStreetMap</a> (<a href=\"http://creativecommons.org/licenses/by-sa/3.0\", target=\"_blank\">CC BY SA</a>)'
			)
		),
		positron = list(
			title= "CartoDB Positron",
			url = "http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png",
			options = list(
				attribution='Tiles &copy; <a href="http://cartodb.com/attributions", target="_blank">CartoDB</a>, Map data &copy; <a href="http://openstreetmap.org/copyright", target="_blan\">OpenStreetMap contributors</a>'
			)
		),
		darkmatter = list(
			title= "CartoDB Dark matter",
			url = "http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png",
			options = list(
				attribution='Tiles &copy; <a href="http://cartodb.com/attributions", target="_blank">CartoDB</a>, Map data &copy; <a href="http://openstreetmap.org/copyright", target="_blan\">OpenStreetMap contributors</a>'
			)
		)
	))
}


changes <- 
function(pkg="leafletR") {
    if(pkg=="leafletR") file.show(file.path(system.file(package="leafletR"), "NEWS"))
}


### short name wrapper functions

cats <- function(prop, val, style.val, leg, ...) {
	styleCat(prop, val, style.val, leg, ...)
}

grads <- function(prop, breaks, closure="left", out=0, style.par, style.val, leg, ...) {
	styleGrad(prop, breaks, closure, out, style.par, style.val, leg, ...)
}

leaf <- function(data, dest, title, size, base.map="osm", center, zoom, style, popup, label, controls="all", incl.data=FALSE, overwrite=TRUE) {
	leaflet(data, dest, title, size, base.map, center, zoom, style, popup, label, controls, incl.data, overwrite)
}

singles <- function(col, lwd, alpha, fill, fill.alpha, rad, marker) {
	styleSingle(col, lwd, alpha, fill, fill.alpha, rad, marker)
}

tg <- function(data, name, dest, lat.lon, overwrite=TRUE) {
	toGeoJSON(data, name, dest, lat.lon, overwrite)
}

prop <- function(data, print=TRUE) {
	getProperties(data, print)
}

base <- function(name, title, url, options) {
	addBaseMap(name, title, url, options)
}

topo <- function(data, print=TRUE) {
	getTopologies(data, print)
}
