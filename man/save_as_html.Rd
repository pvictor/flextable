% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/printers.R
\name{save_as_html}
\alias{save_as_html}
\title{save a flextable in an HTML file}
\usage{
save_as_html(x, path)
}
\arguments{
\item{x}{a flextable object}

\item{path}{HTML file to be created}
}
\description{
save a flextable in an HTML file. This function
has been implemented to help users that do not understand
R Markdown. It is highly recommanded to use R Markdown
instead.
}
\examples{
ft <- flextable( head( mtcars ) )
tf <- tempfile(fileext = ".html")
save_as_html(ft, tf)
}
\seealso{
Other flextable print function: 
\code{\link{as_raster}()},
\code{\link{docx_value}()},
\code{\link{format.flextable}()},
\code{\link{htmltools_value}()},
\code{\link{knit_print.flextable}()},
\code{\link{plot.flextable}()},
\code{\link{print.flextable}()},
\code{\link{save_as_image}()}
}
\concept{flextable print function}
