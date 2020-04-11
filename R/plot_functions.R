#' @include configPROsetta.R
NULL

#' Plot from objects
#'
#' Plot from objects in the PROsetta package.
#'
#' @param x A \code{\linkS4class{PROsetta_data}} object.
#' @param y Unused argument, exists for compatibility with \code{\link{plot}} in the base R package.
#' @param scale_id Instrument ID to plot. 0 represents the combined scale.
#' @param filename Filename to write if 'savefile' is TRUE.
#' @param title The title of the figure.
#' @param xlim The range of scores to plot.
#' @param color The color to fill the histogram.
#' @param nbar The number of histogram bars.
#' @param rug If TRUE, display the actual distribution of scores below each bar.
#' @param filetype The type of file to write if 'savefile' is TRUE. Accepts 'pdf', 'jpeg', 'png', and 'tiff'.
#' @param savefile If TRUE, save the figure as a file.
#' @param bg The background color of the plot.
#' @param width The width of the plot.
#' @param height The height of the plot.
#' @param pointsize Point size to pass onto file writing functions.
#'
#' @examples
#'
#' \donttest{
#' cfg <- createConfig(
#'   response_file = "inst/data-raw/dat_decesd_v2.csv",
#'   anchor_file = "inst/data-raw/anchor_decesd.csv",
#'   itemmap_file = "inst/data-raw/imap_decesd.csv"
#' )
#' plot(cfg)
#' }
#'
#' @docType methods
#' @rdname plot-methods
#' @export

setMethod(
  f = "plot",
  signature = "PROsetta_data",
  definition = function(
    x, y,
    scale_id = 0,
    filename = NULL, title = NULL, xlim = NULL,
    color = "blue", nbar = 20, rug = FALSE, filetype = "pdf", savefile = FALSE,
    bg = "white", width = 6, height = 6, pointsize = 12) {

    item_names   <- x@itemmap[[x@item_id]]
    resp         <- x@response[, item_names]
    idx_complete <- !apply(is.na(resp), 1, any)
    resp         <- resp[idx_complete, ]

    if (scale_id != "combined") {
      item_names <- subset(x@itemmap, x@itemmap[[x@scale_id]] == scale_id)[[x@item_id]]
    }

    resp      <- resp[, item_names]
    n         <- nrow(resp)
    ni        <- ncol(resp)
    raw_score <- rowSums(resp, na.rm = F)
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

    string <- paste("N:", N,
                    "Min:", Min,
                    "Median:", Median,
                    "Mean:", Mean,
                    "SD:", SD,
                    "Max:", Max)

    if (is.null(xlim)) {
      xlim <- c(Min, Max)
    }

    if (!is.null(filename) && savefile) {
      if (filetype == "pdf") {
        fn = paste0(filename, ".pdf")
        pdf(file = fn,
            width = width, height = height, pointsize = pointsize, bg = bg)
      } else if (filetype == "jpeg") {
        jpeg(filename = paste0(filename, ".jpeg"), quality = 100,
             width = width, height = height, pointsize = pointsize, bg = bg)
      } else if (filetype == "png") {
        png(filename = paste0(filename, ".png"),
            width = width, height = height, pointsize = pointsize, bg = bg)
      } else if (filetype == "tiff") {
        tiff(filename = paste0(filename, ".tif"),
             width = width, height = height, pointsize = pointsize, bg = bg)
      } else {
        stop(sprintf("Unrecognized 'filetype' argument: %s", filetype))
      }
    }

    hist1 <- hist(
      raw_score, nbar, xlim = xlim,
      xlab = "Raw Summed Score", ylab = "Frequency", sub = string, main = title, col = color)

    text(hist1$mids, hist1$counts + 2.5,
         hist1$counts,
         cex = .8)
    text(hist1$mids, hist1$counts / 2,
         paste(round(hist1$counts * 100 / (sum(hist1$counts)), digits = 0), "%", sep = ""),
         col = "white", cex = .7)
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
#' Plot scale information
#'
#' @param object An \code{\linkS4class{SingleGroupClass}} object from \code{\link{runClassical}}.
#' @param data A \code{\linkS4class{PROsetta_data}} object.
#' @param theta Theta values to plot on the x-axis.
#' @param t_score Set to \code{TRUE} to convert thetas into T-scores.
#' @param scale_label Names of each scale.
#' @param color Line colors to plot.
#' @param lty Line types to plot.
#' @docType methods
#' @rdname plotInfo-methods
#' @export

setGeneric(
  name = "plotInfo",
  def = function(object, data, theta = seq(-4, 4, .1), t_score = FALSE, scale_label, color = "blue", lty = 1) {
    standardGeneric("plotInfo")
  }
)

#' @docType methods
#' @rdname plotInfo-methods
#' @export

setMethod(
  f = "plotInfo",
  signature = "SingleGroupClass",
  definition = function(object, data, theta = seq(-4, 4, .1), t_score = FALSE, scale_label, color = "blue", lty = 1) {

    info  <- list()
    for (scale_id in c(unique(data@itemmap[[data@scale_id]]), 0)) {

      if (scale_id != 0) {
        item_names <- subset(data@itemmap, data@itemmap[[data@scale_id]] == scale_id)[[data@item_id]]
      } else {
        item_names <- data@itemmap[[data@item_id]]
      }

      sid = as.character(scale_id)
      info[[sid]] <- matrix(NA, length(item_names), length(theta))
      for (i in 1:length(item_names)) {
        item <- mirt::extract.item(object, item_names[i])
        info[[sid]][i, ] <- mirt::iteminfo(item, Theta = theta)
      }
      info[[sid]] <- apply(info[[sid]], 2, sum)

    }

    ymax = do.call('max', info)
    if (t_score) {
      x = theta * 10 + 50
      xlab = "T-score"
    } else {
      x = theta
      xlab = "Theta"
    }

    plot(x, x, xlim = range(x), ylim = range(0, ymax), type = 'n', xlab = xlab, ylab = "Scale Information")
    grid()
    color = rep(color, length.out = length(info))
    lty = rep(lty, length.out = length(info))
    for (i in 1:length(info)) {
      lines(x, info[[i]], col = color[i], lty = lty[i])
    }

    legend(
      "topleft",
      scale_label,
      lty = lty,
      col = color
    )
  }
)
