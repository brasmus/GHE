\name{gheonearth}
\alias{gheonearth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Make animation of the greenhouse gas
%%  ~~function to do ... ~~
}
\description{A function that provides an illustration of an increased
  greenhouse effect through simple animation
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gheonearth(url.earth = "http://eoimages.gsfc.nasa.gov/images/imagerecords/36000/36019/AS8-16-2593_lrg.jpg", url.sun = "http://www.swpc.noaa.gov/primer/primer_graphics/Sun.png")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{url.earth}{URL for an image of the earth
%%     ~~Describe \code{url.earth} here~~
}
  \item{url.sun}{URL of an image of the sun
%%     ~~Describe \code{url.sun} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{GIF animation
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (url.earth = "http://eoimages.gsfc.nasa.gov/images/imagerecords/36000/36019/AS8-16-2593_lrg.jpg", 
    url.sun = "http://www.swpc.noaa.gov/primer/primer_graphics/Sun.png") 
{
    image.sun <- "sun.jpg"
    image.earth <- "earth.jpg"
    N <- 30
    mklayer <- function(path = "ghe") {
        if (!file.exists(path)) 
            dir.create(path)
        list <- list.files(path = path)
        num <- substr(as.character(1000 + length(list) + 1), 
            2, 4)
        dev2bitmap(file = paste(path, "/layer-", num, ".png", 
            sep = ""), res = 150)
    }
    newfig <- function() {
        dev.new(width = 10, height = 10)
        par(xaxt = "n", yaxt = "n", bty = "n", mar = rep(0, 4), 
            bg = "black", col.main = "white")
        plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "")
        rasterImage(img.earth, 0.7, 0.5, 1.5, 1.3, angle = 180)
        rasterImage(img.sun, 0, 0.7, 0.3, 1)
        arrows(0, 0.45, 0, 0.75, lty = 2, col = "white")
        arrows(0, 0.45, 0.75, 0.45, lty = 2, col = "white")
        text(0.7, 0.4, "Temperature", col = "white")
        text(0, 0.7, "height", srt = 90, col = "white", pos = 2)
        arrows(0.15, 0.85, 0.25, 0.45, col = "yellow", length = 0.1)
        arrows(0.14, 0.85, 0.24, 0.45, col = "yellow", length = 0.1)
        arrows(0.16, 0.85, 0.26, 0.45, col = "yellow", length = 0.1)
        lines(rep(0.3, 2), c(0.45, 0.7), lty = 3, col = "grey")
        text(0.3, 0.435, expression(T[E]))
        text(0.8, 0.95, "Energy balance:", col = "grey30")
        text(0.8, 0.9, expression(frac((1 - A), 4) * S == sigma * 
            T[E]^4), col = "grey30")
        text(0.5, 0.97, "The Greenhouse effect", col = "white", 
            cex = 1.25, font = 2)
        r <- 0.5
        x <- r * cos(pi * seq(0, 360, by = 1)/180) + 0.3
        y <- r * sin(pi * seq(0, 360, by = 1)/180) + 0.1
        lines(x, y, col = "grey", lty = 3)
        h <- r + 0.1
        dTdz = 6 * 0.01
        lines(c(0.3, h - 0.15), c(h, 0.45), lty = 3, col = "steelblue")
    }
    IR <- function(h) {
        n <- 100
        x <- 0.01 * sin(4 * pi * seq(0, 1, length = n)) + 0.3
        y <- seq(h, h + 0.1, length = n)
        lines(x, y, lty = 2, col = "pink")
        arrows(0.3, h + 0.1, 0.31, h + 0.11, length = 0.05, lty = 2, 
            col = "pink")
    }
    if (!file.exists(image.earth)) {
        download.file(url.earth, image.earth)
        img.earth <- readJPEG(image.earth)
    }
    if (!file.exists(image.sun)) {
        download.file(url.sun, "sun.png")
        system("convert sun.png sun.jpg")
        img.sun <- readJPEG(image.sun)
    }
    for (i in 1:N) {
        newfig()
        r <- 0.5 + 0.1 * (i - 1)/N
        x <- r * cos(pi * seq(0, 360, by = 1)/180) + 0.3
        y <- r * sin(pi * seq(0, 360, by = 1)/180) + 0.1
        lines(x, y, lwd = 2, col = "grey")
        h <- r + 0.1
        lines(c(0.3, h - 0.15), c(h, 0.45), lwd = 3, col = "steelblue")
        IR(h)
        mklayer()
        dev.off()
    }
    ani.options(interval = 0.05)
    im.convert("ghe/*.png", output = "ghe.gif")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
