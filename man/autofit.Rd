% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flextable_sizes.R
\name{autofit}
\alias{autofit}
\title{Adjusts cell widths and heights}
\usage{
autofit(x, add_w = 0.1, add_h = 0.1)
}
\arguments{
\item{x}{flextable object}

\item{add_w}{extra width to add in inches}

\item{add_h}{extra height to add in inches}
}
\description{
compute and apply optimized widths and heights.
This function is to be used when the table widths and heights
should automatically be adjusted to fit the size of the content.
}
\examples{
ft <- flextable(mtcars)
\donttest{ft <- autofit(ft)}
ft
}
\seealso{
Other flextable dimensions: 
\code{\link{dim.flextable}()},
\code{\link{dim_pretty}()},
\code{\link{fit_to_width}()},
\code{\link{flextable_dim}()},
\code{\link{height}()},
\code{\link{width}()}
}
\concept{flextable dimensions}
