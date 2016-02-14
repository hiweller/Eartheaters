\name{svgviewr.lines}
\alias{svgviewr.lines}
\title{Write lines to SVG Viewer}
\description{A function taking coordinates given in various ways and joining the corresponding points with line segments in an SVG Viewer.}
\usage{
svgviewr.lines(x, file=NULL, y=NULL, col=NULL, z.index=0, layer="", 
               label="", lwd=1, opacity=1, animate=TRUE, append=TRUE)
}
\arguments{
  \item{x}{A vector, matrix or array of 2D or 3D coordinates to be joined by a line or lines.}
  \item{file}{File path and name (having the extenstion ".html") of an existing SVG Viewer. See \code{\link{svgviewr.new}}.}
  \item{y}{If \code{x} is a vector, the coordinates on the y-axis to be joined by a line or lines.}
  \item{col}{The color of the line(s).}
  \item{z.index}{A number indicating the relative order in which the SVG object will be drawn in the viewer. Higher numbers correspond to closer to the front or top.}
  \item{layer}{A text string indicating the layer in which the SVG object belongs. This can be used to show/hide layers in the SVG Viewer but does not affect relative ordering.}
  \item{label}{A label to be added to the SVG object in SVG tag.}
  \item{lwd}{The thickness of the line(s).}
  \item{opacity}{A number between \code{0} and \code{1} indicating the opacity of the line(s).}
  \item{animate}{If \code{x} is an array, a logical indicating whether the lines should be animated.}
  \item{append}{A logical indicating whether the lines should be appended to existing SVG objects in \code{file}. If \code{FALSE}, previously existing objects should be overwritten.}
}
\details{
This function accepts input similar to the native plot function \code{lines()}. If \code{x} and \code{y} are vectors, they are combined into a matrix using \code{cbind()}. If \code{x} is a matrix, this matrix is used directly. Lines are then drawn between points indicated by consecutive rows. So for a 2-row matrix one line would be drawn, for a 3-row matrix two lines would be drawn, etc.

If \code{x} is an array, the array is interpreted as a series of matrices, each representing a state of the line or line(s) in an animation of length \code{dim(x)[3]}. Each of the \code{dim(x)[3]} matrices is used to draw each state in a manner identical to when \code{x} is a matrix. If \code{animate} is \code{TRUE}, each state of lines will be drawn as an animation. If \code{animate} is \code{FALSE}, all states will be drawn superimposed on one another in a single, still graphic.

The graphical parameters \code{col}, \code{z.index}, \code{layer}, \code{label}, \code{lwd}, and \code{opacity} can all be vectors of length one or of the same length as the number of lines to be drawn (see Examples). This allows different parameters to be specified for each line. This does not extend to the animation states; that is, each line will retain the same properties throughout the animation.
}
\value{If file is non-\code{NULL}, 1 if write was successful. If file is \code{NULL}, a vector SVG line elements.}
\author{Aaron Olsen}
\seealso{
\code{\link{svgviewr.new}}, 
\code{\link{svgviewr.points}}, 
\code{\link{svgviewr.pathsC}}
}
\examples{
### PLOT A SERIES OF 3D TRIANGLES ###
## SET FILE PATH (REPLACE NULL) TO WHERE VIEWER SHOULD BE SAVED
## EXAMPLE: file <- "/Users/aaron/Desktop/svgViewR.html"
file <- NULL

## CREATE NEW SVG VIEWER
svgviewr.new(file=file, window.title="3D Triangles")

## CREATE A SERIES OF 20 TRIANGLES (3D)
n <- 20
x_seq <- seq(0, 1, length=n)
x_pts <- matrix(c(0,0,0, 0,0,1, 1,0,0, 0,0,0), nrow=4, ncol=3, byrow=TRUE)
x <- array(NA, dim=c(4, 3, n))
for(i in 1:dim(x)[3]) x[, , i] <- x_pts + matrix(x_seq[i], nrow=4, ncol=3)

## ADD LINES TO SVG VIEWER
svgviewr.lines(x, file=file, opacity=0.5, col=c("blue", "green", "red"), lwd=2, animate=FALSE)

## OPEN THE SPECIFIED FILE IN A WEB BROWSER TO VIEW TRIANGLES


### PLOT A SERIES OF ANIMATED 3D TRIANGLES ###
## SET FILE PATH (REPLACE NULL) TO WHERE VIEWER SHOULD BE SAVED
## EXAMPLE: file <- "/Users/aaron/Desktop/svgViewR.html"
file <- NULL

## CREATE NEW SVG VIEWER
svgviewr.new(file=file, window.title="3D Animated Triangles")

## CREATE A SERIES OF 20 TRIANGLES (3D)
n <- 50
x_seq <- seq(0, 1, length=n)
x_pts <- matrix(c(0,0,0, 0,0,1, 1,0,0, 0,0,0), nrow=4, ncol=3, byrow=TRUE)
x <- array(NA, dim=c(4, 3, n))
for(i in 1:dim(x)[3]) x[, , i] <- x_pts + matrix(x_seq[i], nrow=4, ncol=3)

## ADD LINES TO SVG VIEWER
svgviewr.lines(x, file=file, opacity=0.5, col="blue", lwd=2, animate=TRUE)

## OPEN THE SPECIFIED FILE IN A WEB BROWSER TO VIEW TRIANGLES


### EFFECT OF VARYING THE EXPONENT OF A POLYNOMIAL AS AN ANIMATION ###
## SET FILE PATH (REPLACE NULL) TO WHERE VIEWER SHOULD BE SAVED
## EXAMPLE: file <- "/Users/aaron/Desktop/svgViewR.html"
file <- NULL

## CREATE NEW SVG VIEWER
svgviewr.new(file=file, window.title="3D Animated Polynomials")

## CREATE ARRAY OF COORDINATES
n <- 40
x <- array(NA, dim=c(40, 2, n))
x_seq <- seq(0, 1, length=dim(x)[1])
n_seq <- seq(1, 10, length=n)
for(i in 1:dim(x)[3]) x[, , i] <- cbind(x_seq, x_seq^n_seq[i])

## ADD LINES TO SVG VIEWER
svgviewr.lines(x=x, file=file, opacity=1, col="blue", lwd=2, z.index=-1, animate=TRUE)

## OPEN THE SPECIFIED FILE IN A WEB BROWSER TO VIEW

}