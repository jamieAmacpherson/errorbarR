#' Add error bars to a scatter plot
#'
#' @param Xval x values
#' @param Yval y values
#' @param sdval length of error bar along the X axis (ie. standard deviation or standard error of the mean)
#' @param eb.width width of error bar
#' @param eb.length length of error bar
#' @return error bars
#' @export 


scatter_eb = function(Xval, Yval, sdval, eb.width, eb.length, ...){

	# set default values if sdval are 
	# not supplied
	if(missing(sdval)) {
		sdval = 0
	}

	if(missing(eb.width)) {
		eb.width = 0.6
	}

	if(missing(eb.length)) {
		eb.length = 0.025
	}

	# generate arrows as error bars
	eb.arr = arrows(Xval, Yval - sdval,
		Xval, Yval + sdval,
		length = eb.length,
		angle = 90,
		code = 3,
		lwd = eb.width)

	# return the error bars
	return(eb.arr)
}