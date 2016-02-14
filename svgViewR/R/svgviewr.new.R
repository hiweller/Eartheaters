svgviewr.new <- function(file, window.title="SVG Viewer", animate.duration = 1, 
	animate.reverse = FALSE, animate.repeat = -1, margin = 20, col = "white", 
	show.control = TRUE, start.rotate = TRUE, fdir = NULL){

	# CHECK THAT FILE IS OF TYPE HTML
	if(!grepl('.html$', file, ignore.case=TRUE)) stop("File must have extension '.html'.")

	if(is.null(file)) return(0)

	n <- rep(NA, 0)

	n <- c(n, "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")
	n <- c(n, paste("<title>", window.title,"</title>", sep=""))
	n <- c(n, "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" >\n")
	
	# Set file directory for extra files to copy into viewer document
	if(is.null(fdir)) fdir <- paste0(path.package("svgViewR"), "/extdata/")

	# Parameters set by user
	n <- c(n, "<script type=\"text/javascript\" >")
	n <- c(n, "\t// Parameters set by user")
	n <- c(n, paste0("\tvar animation_reverse = ", ifelse(animate.reverse, 1, 0), ";"))
	n <- c(n, paste0("\tvar animation_duration = ", animate.duration, ";"))
	n <- c(n, paste0("\tvar animation_repeat = ", animate.repeat, ";"))
	n <- c(n, paste0("\tvar animation_count = ", 0, ";"))
	n <- c(n, paste0("\tvar margin = ", margin, ";"))
	n <- c(n, paste0("\tvar background_color = '", col, "';"))
	if(is.null(fdir)) n <- c(n, paste0("\tvar svgviewr_version = '", packageVersion('svgViewR'), "';"))
	n <- c(n, paste0("\tvar start_rotate = ", ifelse(start.rotate, 1, 0), ";"))
	n <- c(n, paste0("\tvar show_control_panel = ", ifelse(show.control, 1, 0), ";"))
	n <- c(n, "</script>\n")

	n <- c(n, "<svg_doc xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" style=\"visibility:hidden;\" >")
	n <- c(n, "</svg_doc>\n")

	n <- c(n, "<body style=\"margin:0px;background-color:;overflow:hidden;\" >")
	n <- c(n, "\t<a id='keydown' type='checkbox' onkeydown=\"javascript:;\" ></a>")

	n <- c(n, paste("\t", paste(readLines(paste0(fdir, 'html/control_panel.html')), collapse="\n\t"), "\n", sep=""))

	n <- c(n, "\t<svg id=\"world\" style=\"background-color:;width:100%;height:100%;position:absolute;top:0;left:0;z-index:-1;\" onload=\"Javascript:onLoadFunctions();\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\"></svg>")
	n <- c(n, "</body>\n")
	
	# Copy in css files
	for(css_file in list.files(paste0(fdir, 'css/')))
		n <- c(n, paste("<style>\n\t", paste(readLines(paste0(fdir, 'css/', css_file)), collapse="\n\t"), "\n</style>\n", sep=""))

	n <- c(n, "<script type=\"text/javascript\" >")
	n <- c(n, "\tvar svgDocument = document.getElementById(\"world\");")
	n <- c(n, "</script>\n")

	# Copy in javascript files
	n <- c(n, paste("<script type=\"text/javascript\" >\n\t", paste(readLines(paste0(fdir, 'js/math.js')), collapse="\n\t"), "\n</script>", sep=""))
	n <- c(n, paste("<script type=\"text/javascript\" >\n\t", paste(readLines(paste0(fdir, 'js/ui_functions.js')), collapse="\n\t"), "\n</script>", sep=""))
	n <- c(n, paste("<script type=\"text/javascript\" >\n\t", paste(readLines(paste0(fdir, 'js/shape_operations.js')), collapse="\n\t"), "\n</script>", sep=""))
	n <- c(n, paste("<script type=\"text/javascript\" >\n\t", paste(readLines(paste0(fdir, 'js/shapes.js')), collapse="\n\t"), "\n</script>", sep=""))
	n <- c(n, paste("<script type=\"text/javascript\" >\n\t", paste(readLines(paste0(fdir, 'js/control_panel.js')), collapse="\n\t"), "\n</script>", sep=""))

	write(n, file)
	
	# SET CURRENT SVG FPATH AS GLOBAL SVG SAVE AS FPATH
	#write.SVG.current <<- file
}