
#' @export
flextableOutput <- function(outputId, width = "100%", ...) {
  htmltools::tags$div(
    id = outputId, class = "shiny-html-output flextable-output",
    style = if (!is.null(width)) paste0("width:", htmltools::validateCssUnit(width), ";"),
    ...
  )
}
#' @export
renderFlextable <- function(expr, condensed = TRUE,
                            fontsize = 14, env = parent.frame(),
                            quoted = FALSE, outputArgs = list()) {
  shiny::installExprFunction(expr, "func", env, quoted)
  shiny::createRenderFunction(
    func = func,
    transform = function(result, shinysession, name, ...) {
      if (is.null(result) || length(result) == 0)
        return(NULL)
      if (inherits(result, "data.frame")) {
        result <- flextable(result)
      }
      if (!inherits(result, "flextable")) {
        stop("'expr' must return a data.frame or flextable object.")
      }
      if (!is.null(fontsize)) {
        result <- fontsize(x = result, size = fontsize, part = "all")
      }
      class <- "table"
      if (isTRUE(condensed))
        class <- paste(class, "table-condensed")
      codes <- html_str(result, class = class)
      HTML(as.character(codes))
    }, flextableOutput, outputArgs
  )
}



