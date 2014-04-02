.onAttach <- 
function(libname, pkgname) {
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), fields="Version")
    packageStartupMessage(" ")
    packageStartupMessage("**********************")
    packageStartupMessage(" ")
    packageStartupMessage(paste("This is", pkgname, ver))
    packageStartupMessage(" ")
    packageStartupMessage("Type changes(\"leafletR\") to see changes/bug fixes, help(leafletR) for documentation")
    packageStartupMessage("or citation(\"leafletR\") for how to cite leafletR.")
    packageStartupMessage(" ")
    packageStartupMessage("**********************")
    packageStartupMessage(" ")
}


changes <- 
function(pkg="leafletR") {
    if(pkg=="leafletR") file.show(file.path(system.file(package="leafletR"), "NEWS"))
}


### short name wrapper functions

cats <- function(prop, val, style.par, style.val, leg, ...) {
	styleCat(prop, val, style.par, style.val, leg, ...)
}

grads <- function(prop, breaks, right=TRUE, out=0, style.par, style.val, leg, ...) {
	styleGrad(prop, breaks, right, out, style.par, style.val, leg, ...)
}

leaf <- function(data, dest, title, size, base.map="osm", center, zoom, style, popup, incl.data=FALSE, overwrite=TRUE) {
	leaflet(data, dest, title, size, base.map, center, zoom, style, popup, incl.data, overwrite)
}

singles <- function(col, lwd, alpha, fill, fill.alpha, rad) {
	styleSingle(col, lwd, alpha, fill, fill.alpha, rad)
}

tg <- function(data, name, dest, lat.lon, overwrite=TRUE) {
	toGeoJSON(data, name, dest, lat.lon, overwrite)
}

prop <- function(data, print=TRUE) {
	getProperties(data, print)
}
