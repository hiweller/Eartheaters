\name{svgviewr.new}
\alias{svgviewr.new}
\title{Create new SVG Viewer file}
\description{Creates a new SVG Viewer file as an HTML document to which SVG objects can be added, with optional specification of various animation parameters.}
\usage{
svgviewr.new(file, window.title="SVG Viewer", animate.duration = 1, 
             animate.reverse = FALSE, animate.repeat = -1)
}
\arguments{
  \item{file}{File path and name (having the extenstion ".html") where SVG Viewer will be created.}
  \item{window.title}{An optional custom title of the SVG Viewer, visible at the top of the web browser window.}
  \item{animate.duration}{The approximate duration in seconds of the animation. When the number of objects to be displayed is large, the actual duration might exceed this number.}
  \item{animate.reverse}{A logical indicating whether the animation is to be played in reverse after each iteration.}
  \item{animate.repeat}{An integer specifying the number of times the animation will repeat. A value of \code{-1} will cause the animation to repeat indefinitely.}
}
\details{
This function is used to initialize a new SVG Viewer file. Before adding shapes to an SVG Viewer, this function is called to create the HTML document structure to which the SVG objects can be added. The SVG Viewer file can be opened with any compatible web browser (Safari, Firefox, Chrome, Opera). When the file is opened, the SVG shapes will be automatically scaled to fit the web browser and drawn onto a white background. Re-sizing the browswer window will automatically re-size the shapes to fit the browser window. If the shapes are animated, the animation will begin immediately upon opening the window. The shapes and animation are interactive and can be controlled via the keyboard/mouse commands listed below.

	\itemize{
		\item \bold{spacebar} : Pauses and plays the animation
		\item \bold{browser refresh} : Returns shapes to state when browser was originally opened
		\item \bold{scroll up/down} : Zoom in/out by moving the shapes into and out of the screen
		\item \bold{'j'} : Zoom out by moving shapes into the screen
		\item \bold{'k'} : Zoom in by moving shapes out of the screen
		\item \bold{up/down/left/right arrow} : Move shapes up/down/left/right
		\item \bold{hold 'shift' + Up/down/left/right arrow} : Move shapes up/down/left/right in larger steps
		\item \bold{hold 'r' + click-and-drag} : Rotates shapes about y- and x- axes. Click-and-drag the mouse anywhere within the browser window. Clicking-and-dragging left and right will rotate the shapes about the y- (vertical) axis. Clicking-and-dragging up and down will rotate the shapes about the x- (horizontal) axis.
		\item \bold{hold 'r' +  up/down arrow} : Rotate shapes about the x-axis in 10 degree increments
		\item \bold{hold 'r' +  left/right arrow} : Rotate shapes about the y-axis in 10 degree increments
		\item \bold{hold 'r' + 'k'} : Rotate shapes clockwise within the z-plane in 10 degree increments (screen)
		\item \bold{hold 'r' + 'j'} : Rotate shapes counter-clockwise within the z-plane in 10 degree increments (screen)
		\item \bold{hold 'shift' + hold 'r' + 'j'/'k'/up/down/left/right arrow} : Rotate shapes as stated above, but in 45 degree increments
	}
}
\value{The return value of the \code{write()} function.}
\author{Aaron Olsen}
\seealso{
\code{\link{svgviewr.points}}, 
\code{\link{svgviewr.lines}}, 
\code{\link{svgviewr.pathsC}}
}
\examples{
### CREATE A MOVING SINUSOIDAL PATH ###
## SET FILE PATH (REPLACE NULL) TO WHERE VIEWER SHOULD BE SAVED
## EXAMPLE: file <- "/Users/aaron/Desktop/svgViewR.html"
file <- NULL

## CREATE NEW SVG VIEWER
svgviewr.new(file=file, window.title="Animated Sinusoid", animate.duration=2)

## CREATE POINTS WITH VARYING SINE PHASE
n <- 100
x <- array(NA, dim=c(40, 2, n))
x_seq <- seq(-pi, pi, length=dim(x)[1])
n_seq <- seq(0, 2*pi, length=n)
for(i in 1:dim(x)[3]) x[, , i] <- cbind(x_seq, sin(x_seq + n_seq[i]))
	
# ADD POINTS TO SVG VIEWER
svg_points <- svgviewr.points(x, cex=2, lwd=1, col="blue", file=file, animate=TRUE)

## OPEN THE SPECIFIED FILE IN A WEB BROWSER TO VIEW

}