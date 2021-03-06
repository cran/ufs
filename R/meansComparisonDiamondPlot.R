#' meansComparisonDiamondPlot and duoComparisonDiamondPlot
#'
#' These are two diamond plot functions to conveniently make diamond plots to
#' compare subgroups or different samples. They are both based on a univariate
#' diamond plot where colors are used to distinguish the data points and
#' diamonds of each subgroup or sample. The means comparison diamond plot
#' produces only this plot, while the duo comparison diamond plot combines it
#' with a diamond plot visualising the effect sizes of the associations. The
#' latter currently only works for two subgroups or samples, while the simple
#' meansComparisonDiamondPlot also works when comparing more than two sets of
#' datapoints. These functions are explained more in detail in Peters (2017).
#'
#' These functions are explained in Peters (2017).
#'
#' @aliases meansComparisonDiamondPlot duoComparisonDiamondPlot
#' @rdname comparisonDiamondPlots
#' @param dat The dataframe containing the relevant variables.
#' @param items The variables to plot (on the y axis).
#' @param compareBy The variable by which to compare (i.e. the variable
#' indicating to which subgroup or sample a row in the dataframe belongs).
#' @param labels The labels to use on the y axis; these values will replace the
#' variable names in the dataframe (specified in `items`).
#' @param compareByLabels The labels to use to replace the value labels of the
#' `compareBy` variable.
#' @param decreasing Whether to sort the variables by their mean values
#' (`NULL` to not sort, `TRUE` to sort in descending order (i.e.
#' items with lower means are plotted more to the bottom), and `FALSE` to
#' sort in ascending order (i.e. items with lower means are plotted more to the
#' top).
#' @param sortBy If the variables should be sorted (see `decreasing`),
#' this variable specified which subgroup should be sorted by. Therefore, the
#' value specified here must be a value label ('level label') of the
#' `compareBy` variable.
#' @param conf.level The confidence level of the confidence intervals specified
#' by the diamonds for the means (for `meansComparisonDiamondPlot`) and
#' for both the means and effect sizes (for `duoComparisonDiamondPlot`).
#' @param showData Whether to plot the data points.
#' @param dataAlpha The transparency (alpha channel) value for the data points:
#' a value between 0 and 1, where 0 denotes complete transparency and 1 denotes
#' complete opacity.
#' @param dataSize The size of the data points.
#' @param comparisonColors The colors to use for the different subgroups or
#' samples. This should be a vector of valid colors with at least as many
#' elements as sets of data points that should be plotted.
#' @param associationsColor For `duoComparisonDiamondPlot`, the color to
#' use to plot the effect sizes in the right-hand plot.
#' @param alpha The alpha channel (transparency) value for the diamonds: a
#' value between 0 and 1, where 0 denotes complete transparency and 1 denotes
#' complete opacity.
#' @param jitterWidth,jitterHeight How much noise to add to the data points (to
#' prevent overplotting) in the horizontal (x axis) and vertical (y axis)
#' directions.
#' @param xlab,ylab The label to use for the x and y axes (for
#' `duoComparisonDiamondPlot`, must be vectors of two elements). Use
#' `NULL` to not use a label.
#' @param plotTitle Optionally, for `meansComparisonDiamondPlot`, a title
#' for the plot (can also be specified for `duoComparisonDiamondPlot`, in
#' which case it's passed on to `meansComparisonDiamondPlot` for the left panel -
#' but note that this messes up the alignment of the two panels).
#' @param theme The theme to use for the plots.
#' @param showLegend Whether to show the legend (which color represents which
#' subgroup/sample).
#' @param legend.position Where to place the legend in `meansComparisonDiamondPlot`
#' (can also be specified for `duoComparisonDiamondPlot`, in
#' which case it's passed on to `meansComparisonDiamondPlot` for the left panel -
#' but note that this messes up the alignment of the two panels).
#' @param lineSize The thickness of the lines (the diamonds' strokes).
#' @param drawPlot Whether to draw the plot, or only (invisibly) return it.
#' @param xbreaks Where the breaks (major grid lines, ticks, and labels) on the
#' x axis should be.
#' @param outputFile A file to which to save the plot.
#' @param outputWidth,outputHeight Width and height of saved plot (specified in
#' centimeters by default, see `ggsaveParams`).
#' @param ggsaveParams Parameters to pass to ggsave when saving the plot.
#' @param \dots Any additional arguments are passed to
#' [diamondPlot()] by `meansComparisonDiamondPlot` and to both
#' `meansComparisonDiamondPlot` and [associationsDiamondPlot()]
#' by `duoComparisonDiamondPlot`.
#' @return A Diamond plots: a [ggplot2::ggplot()] plot
#' `meansComparisonDiamondPlot`, and a [gtable()] by
#' `duoComparisonDiamondPlot`.
#' @author Gjalt-Jorn Peters
#'
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso [diamondPlot()], [meansDiamondPlot()], the `CIBER()` function in
#' the `behaviorchange` package
#' @references Peters, G.-J. Y. (2017). Diamond Plots: a tutorial to introduce
#' a visualisation tool that facilitates interpretation and comparison of
#' multiple sample estimates while respecting their inaccuracy.
#' *PsyArXiv.* http://doi.org/10.17605/OSF.IO/9W8YV
#' @keywords hplot
#' @examples
#'
#' meansComparisonDiamondPlot(mtcars,
#'                            items=c('disp', 'hp'),
#'                            compareBy='vs',
#'                            xbreaks=c(100,200, 300, 400));
#' meansComparisonDiamondPlot(chickwts,
#'                            items='weight',
#'                            compareBy='feed',
#'                            xbreaks=c(100,200,300,400),
#'                            showData=FALSE);
#' duoComparisonDiamondPlot(mtcars,
#'                          items=c('disp', 'hp'),
#'                          compareBy='vs',
#'                          xbreaks=c(100,200, 300, 400));
#'
#' @export meansComparisonDiamondPlot
meansComparisonDiamondPlot <- function(dat, items = NULL,
                                       compareBy = NULL,
                                       labels = NULL,
                                       compareByLabels = NULL,
                                       decreasing=NULL,
                                       sortBy=NULL,
                                       conf.level=.95,
                                       showData = TRUE, dataAlpha = .1, dataSize=3,
                                       comparisonColors = viridisPalette(length(unique(dat[, compareBy]))),
                                       alpha = .33,
                                       jitterWidth = .5,
                                       jitterHeight = .4,
                                       xlab='Scores and means',
                                       ylab=NULL,
                                       plotTitle = NULL,
                                       theme=ggplot2::theme_bw(),
                                       showLegend=TRUE,
                                       legend.position = "top",
                                       lineSize=1,
                                       xbreaks = "auto",
                                       outputFile = NULL,
                                       outputWidth = 10,
                                       outputHeight = 10,
                                       ggsaveParams = ufs::opts$get("ggsaveParams"),
                                       ...) {

  res <- list();
  res$intermediate <- list();

  if (is.null(items)) items <- names(dat)[2:ncol(dat)-1];
  if (is.null(compareBy)) compareBy <- names(dat)[ncol(dat)];

  res$intermediate$rawDat <- split(dat, dat[, compareBy]);

  ### Get diamondPlotDf's, but don't sort anything yet
  res$intermediate$dat <- lapply(res$intermediate$rawDat,
                                 ufs::varsToDiamondPlotDf,
                                 items = items,
                                 labels = labels,
                                 decreasing=NULL,
                                 conf.level=conf.level);

  ### Check whether we should sort, and if so, sort. One of these
  ### can be missing, so set default value if one is.
  if (!is.null(sortBy) && is.null(decreasing)) decreasing <- TRUE;
  if (!is.null(decreasing)) {
    if (is.null(sortBy)) sortBy <- names(res$intermediate$rawDat)[1];
    res$intermediate$sortOrder <-
      order(res$intermediate$rawDat[[sortBy]][, 'mean'],
            decreasing = decreasing);

    ### Invert because ggplot plots first elements on y axis lowest
    res$intermediate$sortOrder <- rev(res$intermediate$sortOrder);

    res$intermediate$dat <- lapply(res$intermediate$dat,
                                   function(df, s = res$intermediate$sortOrder) {
                                     return(df[s, ]);
                                   });
  } else {
    res$intermediate$sortOrder <- 1:nrow(res$intermediate$dat[[1]]);

    ### Invert because ggplot plots first elements on y axis lowest
    res$intermediate$sortOrder <- rev(res$intermediate$sortOrder);

  }

  ### Get labels from one of these dataframes,
  ### because they may have been sorted
  labels <- res$intermediate$dat[[1]]$label;
  if (is.null(compareByLabels)) compareByLabels <- names(res$intermediate$dat);

  ### Get diamond layers
  res$intermediate$diamondLayers <- list();
  for (i in 1:length(res$intermediate$dat)) {
    res$intermediate$diamondLayers[[compareByLabels[i]]] <-
      ufs::diamondPlot(res$intermediate$dat[[compareByLabels[i]]],
                       ciCols=c('lo', 'mean', 'hi'),
                       yLabels = labels, colorCol=comparisonColors[i],
                       alpha = alpha,
                       returnLayerOnly = TRUE,
                       size=lineSize, ...);
  }

  plot <- ggplot2::ggplot();

  ### If requested, get data layers and add these to the plot
  if (showData) {
    res$intermediate$dataLayers <- list();
    for (i in 1:length(res$intermediate$dat)) {
      ### Note that we revert the order here again, because
      ### rawData uses the order as provided (whereas some
      ### other functions invert it)
      res$intermediate$dataLayers[[compareByLabels[i]]] <-
        ufs::rawDataDiamondLayer(res$intermediate$rawDat[[compareByLabels[i]]],
                                 items=items,
                                 itemOrder = rev(res$intermediate$sortOrder),
                                 dataAlpha = dataAlpha,
                                 dataColor = comparisonColors[i],
                                 jitterWidth = jitterWidth,
                                 jitterHeight = jitterHeight,
                                 size=dataSize);
      plot <- plot + res$intermediate$dataLayers[[compareByLabels[i]]];
    }
  }

  ### Add diamond layers
  for (i in 1:length(res$intermediate$dat)) {
    plot <- plot +
      res$intermediate$diamondLayers[[compareByLabels[i]]];
  }

  plot <- plot +
    ggplot2::scale_y_continuous(breaks=sort(res$intermediate$sortOrder),
                                minor_breaks=NULL,
                                labels=labels) +
    theme +
    ggplot2::ylab(ylab) +
    ggplot2::xlab(xlab) +
    ggplot2::theme(panel.grid.minor.y=ggplot2::element_blank());

  if (!is.null(plotTitle)) {
    plot <-
      plot + ggplot2::ggtitle(plotTitle);
  }

  if (showLegend) {
    ### First have to add a ribbon layer so that we can actually
    ### map the fill aesthetic to something in the plot
    plot <- plot +
      ggplot2::geom_ribbon(data.frame(colorColumn = factor(compareByLabels),
                             x=rep(Inf, length(compareByLabels)),
                             ymin=rep(Inf, length(compareByLabels)),
                             ymax=rep(Inf, length(compareByLabels))),
                  mapping=ggplot2::aes_string(x='x', ymin='ymin', ymax='ymax',
                                     fill='colorColumn'),
                  show.legend=TRUE) +
      ### Override the colors and legend position
#      ggplot2::guides(fill=ggplot2::guide_legend(override.aes=list(fill=comparisonColors[1:length(compareByLabels)]),
      ggplot2::guides(fill=ggplot2::guide_legend(override.aes=list(fill=comparisonColors[order(compareByLabels)]),
                               title=NULL)) +
      ggplot2::theme(legend.position=legend.position);
  }

  if (!is.null(xbreaks) &&
      length(xbreaks) == 1 &&
      tolower(xbreaks) == "auto") {
    plot <- plot + ggplot2::scale_x_continuous(breaks=sort(unique(unlist(dat[, items]))));
  } else if (is.numeric(xbreaks)) {
    plot <- plot + ggplot2::scale_x_continuous(breaks=xbreaks);
  }

  if (!is.null(outputFile)) {
    ggsaveParameters <- c(list(filename = outputFile,
                               plot = plot,
                               width = outputWidth,
                               height = outputHeight),
                          ggsaveParams);
    do.call(ggplot2::ggsave, ggsaveParameters);
  }

  attr(plot, 'itemOrder') <- res$intermediate$sortOrder;

  return(plot);
}

