\name{svgviewr.points}
\alias{svgviewr.points}
\title{Write points to SVG Viewer}
\description{Draws a sequence of points at specified coordinates in an SVG Viewer.}
\usage{
svgviewr.points(x, file=NULL, y=NULL, type="p", col=NULL, col.fill="black",
               col.stroke="black", z.index=0, layer="", label="", cex=2,
               lwd=2, opacity.stroke=1, opacity.fill=1, animate=TRUE, 
               append=TRUE)
}
\arguments{
  \item{x}{A vector, matrix or array of 2D or 3D coordinates.}
  \item{file}{File path and name (having the extenstion ".html") of an existing SVG Viewer. See \code{\link{svgviewr.new}}.}
  \item{y}{If \code{x} is a vector, the coordinates on the y-axis.}
  \item{type}{character indicating the type of plotting. Currently, only \code{"p"} is supported.}
  \item{col}{The fill and stroke color of the points(s). If non-\code{NULL}, \code{col} overrides \code{col.fill} and \code{col.stroke}.}
  \item{col.fill}{The fill color of the points(s).}
  \item{col.stroke}{The stroke (border) color of the points(s).}
  \item{z.index}{A number indicating the relative order in which the SVG object will be drawn in the viewer. Higher numbers correspond to closer to the front or top.}
  \item{layer}{A text string indicating the layer in which the SVG object belongs. This can be used to show/hide layers in the SVG Viewer but does not affect relative ordering.}
  \item{label}{A label to be added to the SVG object in SVG tag.}
  \item{cex}{The size (radius) of the point(s).}
  \item{lwd}{The thickness of the border of the point(s).}
  \item{opacity.stroke}{A number between \code{0} and \code{1} indicating the opacity of the border of the point(s).}
  \item{opacity.fill}{A number between \code{0} and \code{1} indicating the opacity of the fill of the point(s).}
  \item{animate}{If \code{x} is an array, a logical indicating whether the lines should be animated.}
  \item{append}{A logical indicating whether the lines should be appended to existing SVG objects in \code{file}. If \code{FALSE}, previously existing objects should be overwritten.}
}
\details{
This function accepts input similar to the native plot function \code{points()}. If \code{x} and \code{y} are vectors, they are combined into a matrix using \code{cbind()}. If \code{x} is a matrix, this matrix is used directly. Each row of the matrix is drawn as a point. If \code{x} is an array, the array is interpreted as a series of matrices, each representing a state of the point or point(s) in an animation of length \code{dim(x)[3]}. Each of the \code{dim(x)[3]} matrices is used to draw each state in a manner identical to when \code{x} is a matrix. If \code{animate} is \code{TRUE}, each state of points will be drawn as an animation. If \code{animate} is \code{FALSE}, all states will be drawn superimposed on one another in a single, still graphic.

The graphical parameters \code{col}, \code{col.fill}, \code{col.stroke}, \code{z.index}, \code{layer}, \code{label}, \code{lwd}, \code{opacity.stroke} and \code{opacity.fill} can all be vectors of length one or of the same length as the number of lines to be drawn. This allows different parameters to be specified for each point. This does not extend to the animation states; that is, each point will retain the same properties throughout the animation.
}
\value{If file is non-\code{NULL}, 1 if write was successful. If file is \code{NULL}, a vector SVG point elements.}
\author{Aaron Olsen}
\seealso{
\code{\link{svgviewr.new}}, 
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

## CREATE POINTS WITH VARYING SIN PHASE
n <- 100
x <- array(NA, dim=c(40, 2, n))
x_seq <- seq(-pi, pi, length=dim(x)[1])
n_seq <- seq(0, 2*pi, length=n)
for(i in 1:dim(x)[3]) x[, , i] <- cbind(x_seq, sin(x_seq + n_seq[i]))
	
# ADD POINTS TO SVG VIEWER
svg_points <- svgviewr.points(x, cex=2, lwd=1, col="blue", file=file, animate=TRUE)

## OPEN THE SPECIFIED FILE IN A WEB BROWSER TO VIEW


### EFFECT OF VARYING THE EXPONENT OF A POLYNOMIAL AS AN ANIMATION ###
## SET FILE PATH (REPLACE NULL) TO WHERE VIEWER SHOULD BE SAVED
## EXAMPLE: file <- "/Users/aaron/Desktop/svgViewR.html"
file <- NULL

## CREATE NEW SVG VIEWER
svgviewr.new(file=file, window.title="Animated Polynomial", animate.duration=2,
	animate.reverse=TRUE)

## CREATE ARRAY OF COORDINATES
n <- 40
x <- array(NA, dim=c(40, 2, n))
x_seq <- seq(0, 1, length=dim(x)[1])
n_seq <- seq(1, 10, length=n)
for(i in 1:dim(x)[3]) x[, , i] <- cbind(x_seq, x_seq^n_seq[i])

## ADD POINTS TO SVG VIEWER
svgviewr.points(x, cex=2, lwd=1, col="blue", file=file, animate=TRUE)

## OPEN THE SPECIFIED FILE IN A WEB BROWSER TO VIEW

}