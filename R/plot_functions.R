#' @include loading_functions.R
NULL

#' Plot frequency distribution
#'
#' This is an extension of \code{\link{plot}} to visualize frequency distribution from \code{\linkS4class{PROsetta_data}} object.
#'
#' @param x a \code{\linkS4class{PROsetta_data}} object.
#' @param y unused argument, exists for compatibility with \code{\link{plot}} in the base R package.
#' @param scale_id scale ID to plot. \code{combined} represents the combined scale. (default = \code{combined})
#' @param title the title of the figure.
#' @param xlim the range of scores to plot.
#' @param color the color to fill the histogram. (default = \code{blue})
#' @param nbar the number of histogram bars. (default = \code{20})
#' @param rug if \code{TRUE}, display the actual distribution of scores below each bar. (default = \code{FALSE})
#' @param width the width of the plot. (default = \code{6})
#' @param height the height of the plot. (default = \code{6})
#' @param savefile if \code{TRUE}, save the figure as a file. (default = \code{FALSE})
#' @param filename the filename to write if \code{savefile} argument is \code{TRUE}.
#' @param filetype the type of file to write if \code{savefile} argument is \code{TRUE}. Accepts \code{pdf}, \code{jpeg}, \code{png}, and \code{tiff}. (default = \code{pdf})
#' @param bg the background color to use when saving the figure as a file. (default = \code{white})
#' @param pointsize the point size to use when saving the figure as a file. (default = \code{12})
#'
#' @examples
#' plot(data_asq)
#' plot(data_asq, scale_id = 1)
#' plot(data_asq, scale_id = 2)
#'
#' @docType methods
#' @rdname plot-methods
#' @export
setMethod(
  f = "plot",
  signature = "PROsetta_data",
  definition = function(
    x, y,
    scale_id = "combined",
    filename = NULL, title = NULL, xlim = NULL,
    color = "blue", nbar = 20, rug = FALSE, filetype = "pdf", savefile = FALSE,
    bg = "white", width = 6, height = 6, pointsize = 12) {

    item_names   <- x@itemmap[[x@item_id]]
    resp         <- x@response[, item_names]
    idx_complete <- !apply(is.na(resp), 1, any)
    resp         <- resp[idx_complete, ]

    if (scale_id != "combined") {
      item_names <- subset(x@itemmap, x@itemmap[[x@scale_id]] == scale_id)[[x@item_id]]
      if (length(item_names) == 0) {
        stop(sprintf("argument 'scale_id': unrecognized value '%s'", scale_id))
      }
    }

    resp      <- resp[, item_names]
    n         <- nrow(resp)
    ni        <- ncol(resp)
    raw_score <- rowSums(resp, na.rm = FALSE)
    raw_score <- raw_score[!is.na(raw_score)]

    stats     <- summary(raw_score)

    N      <- n
    Min    <- stats[1]
    Q1     <- stats[2]
    Median <- stats[3]
    Mean   <- round(stats[4], digits = 2)
    Q3     <- stats[5]
    Max    <- stats[6]
    SD     <- round(sd(raw_score), digits = 2)

    string <- paste(
      "N:", N,
      "Min:", Min,
      "Median:", Median,
      "Mean:", Mean,
      "SD:", SD,
      "Max:", Max
    )

    if (is.null(xlim)) {
      xlim <- c(Min, Max)
    }

    if (!is.null(filename) && savefile) {
      if (filetype == "pdf") {
        pdf(
          file = paste0(filename, ".pdf"),
          width = width, height = height, pointsize = pointsize, bg = bg
        )
      } else if (filetype == "jpeg") {
        jpeg(
          filename = paste0(filename, ".jpeg"), quality = 100,
          width = width, height = height, pointsize = pointsize, bg = bg
        )
      } else if (filetype == "png") {
        png(
          filename = paste0(filename, ".png"),
          width = width, height = height, pointsize = pointsize, bg = bg
        )
      } else if (filetype == "tiff") {
        tiff(
          filename = paste0(filename, ".tif"),
          width = width, height = height, pointsize = pointsize, bg = bg
        )
      } else {
        stop(sprintf(
          "argument 'filetype': unrecognized value '%s' (accepts 'pdf', 'jpeg', 'png', 'tiff')",
          filetype
        ))
      }
    }

    hist1 <- hist(
      raw_score, nbar, xlim = xlim,
      xlab = "Raw Summed Score", ylab = "Frequency",
      sub = string, main = title,
      col = color)

    text(
      hist1$mids, hist1$counts + strheight("1") * 0.5,
      hist1$counts,
      cex = .8
    )
    text(
      hist1$mids, hist1$counts / 2,
      paste(round(hist1$counts * 100 / (sum(hist1$counts)), digits = 0), "%", sep = ""),
      col = "white", cex = .7
    )
    if (rug) {
      points(raw_score, rep(0, length(raw_score)), pch = "|")
    }
    box()

    if (!is.null(filename) && savefile) {
      dev.off()
    }

  }
)

#' Plot scale information
#'
#' \code{\link{plotInfo}} is a plotting function to visualize scale-level information.
#'
#' @param object a \code{\linkS4class{SingleGroupClass}} object from \code{\link{runCalibration}}.
#' @param data a \code{\linkS4class{PROsetta_data}} object.
#' @param theta (optional) theta values to plot on the x-axis. (default = \code{seq(-4, 4, .1)})
#' @param t_score (optional) set to \code{TRUE} to plot T-scores on the x-axis instead of thetas. (default = \code{FALSE})
#' @param scale_label (optional) names of each scale. This must have three values. (default = \code{c(1, 2, "Combined")})
#' @param color (optional) line colors to plot. This must have three values. (default = \code{c("red", "blue", "black")})
#' @param lty (optional) line types to plot. This must have three values. (default = \code{c(3, 2, 1)})
#'
#' @examples
#' \donttest{
#' o <- runCalibration(data_asq, technical = list(NCYCLES = 1000))
#' plotInfo(o, data_asq)
#' }
#'
#' @docType methods
#' @rdname plotInfo-methods
#' @export
setGeneric(
  name = "plotInfo",
  def = function(object, data, theta = seq(-4, 4, .1), t_score = FALSE, scale_label = c(1, 2, "Combined"), color = c("red", "blue", "black"), lty = c(3, 2, 1)) {
    standardGeneric("plotInfo")
  }
)

#' @docType methods
#' @rdname plotInfo-methods
#' @export
setMethod(
  f = "plotInfo",
  signature = "SingleGroupClass",
  definition = function(object, data, theta = seq(-4, 4, .1), t_score = FALSE, scale_label = c(1, 2, "Combined"), color = c("red", "blue", "black"), lty = c(3, 2, 1)) {

    info  <- list()
    for (scale_id in c(unique(data@itemmap[[data@scale_id]]), 0)) {

      if (scale_id != 0) {
        item_names <- subset(data@itemmap, data@itemmap[[data@scale_id]] == scale_id)[[data@item_id]]
      } else {
        item_names <- data@itemmap[[data@item_id]]
      }

      sid <- as.character(scale_id)
      info[[sid]] <- matrix(NA, length(item_names), length(theta))
      for (i in 1:length(item_names)) {
        item <- mirt::extract.item(object, item_names[i])
        info[[sid]][i, ] <- mirt::iteminfo(item, Theta = theta)
      }
      info[[sid]] <- apply(info[[sid]], 2, sum)

    }

    ymax <- do.call("max", info)
    if (t_score) {
      x <- theta * 10 + 50
      xlab <- "T-score"
    } else {
      x <- theta
      xlab <- "Theta"
    }

    plot(
      x, x, xlim = range(x), ylim = range(0, ymax), type = "n",
      xlab = xlab, ylab = "Scale Information"
    )
    grid()
    color <- rep(color, length.out = length(info))
    lty <- rep(lty, length.out = length(info))
    for (i in 1:length(info)) {
      lines(x, info[[i]], col = color[i], lty = lty[i])
    }

    legend(
      x = "topleft",
      legend = scale_label,
      lty = lty,
      col = color
    )
  }
)
