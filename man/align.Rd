% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/styles.R
\name{align}
\alias{align}
\alias{align_text_col}
\alias{align_nottext_col}
\title{Set text alignment}
\usage{
align(x, i = NULL, j = NULL, align = "left", part = "body")

align_text_col(x, align = "left", header = TRUE, footer = TRUE)

align_nottext_col(x, align = "right", header = TRUE, footer = TRUE)
}
\arguments{
\item{x}{a flextable object}

\item{i}{rows selection}

\item{j}{columns selection}

\item{align}{text alignment - a single character value, expected value
is one of 'left', 'right', 'center', 'justify'.}

\item{part}{partname of the table (one of 'all', 'body', 'header', 'footer')}

\item{header}{should the header be aligned with the body}

\item{footer}{should the footer be aligned with the body}
}
\description{
change text alignment of selected rows and columns of a flextable.
}
\examples{
ft <- flextable(mtcars)
ft <- align(ft, align = "center")
ft <- flextable(mtcars)
ft <- align_text_col(ft, align = "left")
ft <- align_nottext_col(ft, align = "right")
ft
}
\seealso{
Other sugar functions for table style: 
\code{\link{bg}()},
\code{\link{bold}()},
\code{\link{color}()},
\code{\link{empty_blanks}()},
\code{\link{fontsize}()},
\code{\link{font}()},
\code{\link{italic}()},
\code{\link{padding}()},
\code{\link{rotate}()},
\code{\link{valign}()}

Other sugar functions for table style: 
\code{\link{bg}()},
\code{\link{bold}()},
\code{\link{color}()},
\code{\link{empty_blanks}()},
\code{\link{fontsize}()},
\code{\link{font}()},
\code{\link{italic}()},
\code{\link{padding}()},
\code{\link{rotate}()},
\code{\link{valign}()}
}
\concept{sugar functions for table style}
