#' Save a ggplot with specific defaults
#'
#' This function is vectorized over all argument except 'plot': so if you
#' want to save multiple versions, simply provide vectors. Vectors of length
#' 1 will be recycled using [rep()]; otherwise vectors have to all be the same
#' length as `file`.
#'
#' @param file The file where to save to.
#' @param plot The plot to save; if omitted, the last drawn plot is saved.
#' @param height,width The dimensions of the plot, specified in `units`.
#' @param units The units, `'cm'`, '`mm`', or `'in'`.
#' @param dpi The resolution (dots per inch). This argument is vectorized.
#' @param type An additional arguments for the graphic device.
#' @param device The graphic device; is inferred from the file if not specified.
#' @param bg The background (e.g. 'white').
#' @param ... Any additional arguments are passed on to [ggplot2::ggsave()].
#'
#' @return The plot, invisibly.
#' @export
#'
#' @examples plot <- ufs::ggBoxplot(mtcars, 'mpg');
#' ggSave(file=tempfile(fileext=".png"), plot=plot);
ggSave <- function(file=NULL, plot = ggplot2::last_plot(),
                   height=8, width=8, units="in",
                   dpi = 300, device=NULL, type=NULL, bg="transparent", ...) {

  extension <-
    gsub("^.*\\.(.+)$", "\\1", file);

  if (!is.null(device) && (length(device) == 1)) {
    device <- rep(device, length(file));
  } else if (is.null(device)) {
    device <- rep(tolower(extension), length(file));
  } else if (length(device) != length(file)) {
    stop("If vectors of length>1 are provided for file and device, they must be the same length!");
  }

  device <- ifelse(unlist(lapply(device, is.null)),
                   tolower(extension),
                   device);

  if (!is.null(file) && length(file) > 1) {
    if (!is.null(type) && (length(type) == 1)) {
      type <- rep(type, length(file));
    } else if (is.null(type)) {
      type <- rep(NULL, length(file));
    } else if (length(type) != length(file)) {
      stop("If vectors of length>1 are provided for file and type, they must be the same length!");
    }
    if (length(dpi) == 1) {
      dpi <- rep(dpi, length(file));
    } else if (length(dpi) != length(file)) {
      stop("If vectors of length>1 are provided for file and dpi, they must be the same length!");
    }
    if (length(height) == 1) {
      height <- rep(height, length(file));
    } else if (length(height) != length(file)) {
      stop("If vectors of length>1 are provided for file and height, they must be the same length!");
    }
    if (length(width) == 1) {
      width <- rep(width, length(file));
    } else if (length(width) != length(file)) {
      stop("If vectors of length>1 are provided for file and width, they must be the same length!");
    }
    if (length(bg) == 1) {
      bg <- rep(bg, length(file));
    } else if (length(bg) != length(file)) {
      stop("If vectors of length>1 are provided for file and bg, they must be the same length!");
    }
    if (length(units) == 1) {
      units <- rep(units, length(file));
    } else if (length(units) != length(file)) {
      stop("If vectors of length>1 are provided for file and units, they must be the same length!");
    }
  }

  for (i in seq_along(file)) {

    if (device[i] == 'jpg') {
      device[i] <- 'jpeg';
      if (is.null(type[i])) {
        type[i] <- "cairo";
      }
    }

    if ((is.null(type[i])) && (device[i]=="png")) {
      type[i] <- "cairo-png";
    }

    if (device[i]=="png") {
      ggplot2::ggsave(file=file[i], plot=plot, device=device[i],
                      height=height[i], width=width[i], units=units[i],
                      dpi=dpi[i], type=type[i], bg = bg[i], ...);
    } else if (device[i]=="svg") {
      ### The 'svg' device doesn't have a 'type' argument
      ggplot2::ggsave(file=file[i], plot=plot, device=device[i],
                      height=height[i], width=width[i], units=units[i],
                      dpi=dpi[i], bg = bg[i], ...);
    } else if (device[i]=="pdf") {
      ### The 'pdf' device doesn't have a 'type' argument
      ggplot2::ggsave(file=file[i], plot=plot, device=device[i],
                      height=height[i], width=width[i], units=units[i],
                      dpi=dpi[i], bg = bg[i], ...);
    } else {
      ggplot2::ggsave(file=file[i], plot=plot, device=device[i],
                      height=height[i], width=width[i], units=units[i],
                      dpi=dpi[i], type=type[i], bg = bg[i], ...);
    }

  }

  return(invisible(plot));
}

