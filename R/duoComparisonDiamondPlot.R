#' @rdname comparisonDiamondPlots
#' @export
duoComparisonDiamondPlot <- function(dat, items = NULL,
                                     compareBy = NULL,
                                     labels = NULL,
                                     compareByLabels = NULL,
                                     decreasing=NULL,
                                     conf.level=c(.95, .95),
                                     showData = TRUE, dataAlpha = .1, dataSize=3,
                                     comparisonColors = viridisPalette(length(unique(dat[, compareBy]))),
                                     associationsColor = 'grey',
                                     alpha = .33,
                                     jitterWidth = .5,
                                     jitterHeight = .4,
                                     xlab=c('Scores and means',
                                            'Effect size estimates'),
                                     ylab=c(NULL, NULL),
                                     plotTitle=NULL,
                                     theme=ggplot2::theme_bw(),
                                     showLegend=TRUE,
                                     legend.position="top",
                                     lineSize=1,
                                     drawPlot = TRUE,
                                     xbreaks="auto",
                                     outputFile = NULL,
                                     outputWidth = 10,
                                     outputHeight = 10,
                                     ggsaveParams = ufs::opts$get("ggsaveParams"),
                                     ...) {

  if (length(unique(stats::na.omit(dat[, compareBy]))) != 2) {
    stop("The variable you compare by ('", compareBy,
         "') has to have exactly two levels. ",
         "It has ", length(unique(stats::na.omit(dat[, compareBy]))), ".");
  }

  dat[, compareBy] <- factor(dat[, compareBy]);

  associationsDf <- ufs::associationsToDiamondPlotDf(dat = dat,
                                                     covariates = items,
                                                     criterion = compareBy,
                                                     decreasing=NULL,
                                                     esMetric = "d");

  if (is.null(decreasing)) {
    sortOrder <- 1:nrow(associationsDf);
  } else {
    ### Again, for some reason, R returns a list instead of a vector
    ### when slicing one column from this dataframe
    sortOrder <- order(unlist(associationsDf[, "es"]),
                       decreasing = decreasing);
  }

  ### Invert because ggplot plots first elements on y axis lowest
  #sortOrder <- rev(sortOrder);

  plot1 <- ufs::meansComparisonDiamondPlot(dat,
                                           items=items[sortOrder],
                                           compareBy=compareBy,
                                           labels=labels,
                                           compareByLabels = compareByLabels,
                                           decreasing=NULL,
                                           sortBy=NULL,
                                           conf.level=conf.level[1],
                                           showData = showData,
                                           dataAlpha = dataAlpha,
                                           dataSize=dataSize,
                                           comparisonColors = comparisonColors,
                                           alpha = alpha,
                                           jitterWidth = jitterWidth,
                                           jitterHeight = jitterHeight,
                                           xlab=xlab[1],
                                           plotTitle=plotTitle,
                                           theme=theme,
                                           ylab=ylab[1],
                                           showLegend=showLegend,
                                           legend.position=legend.position,
                                           lineSize=lineSize,
                                           xbreaks=xbreaks,
                                           ...);

  plot2 <- ufs::associationsDiamondPlot(dat,
                                        covariates=items[sortOrder],
                                        criteria=compareBy,
                                        labels = rep("", length(items)),
                                        criteriaLabels = NULL,
                                        decreasing=NULL,
                                        sortBy=NULL,
                                        conf.level=conf.level[2],
                                        criterionColor = associationsColor,
                                        returnLayerOnly = FALSE,
                                        esMetric = "d",
                                        theme=theme,
                                        ylab="",
                                        xlab=xlab[2],
                                        lineSize = lineSize,
                                        ...)

  builtMeansPlot <- ggplot2::ggplot_build(plot1);
  yMajor <- builtMeansPlot$layout$panel_ranges[[1]]$y.major_source;
  yRange <- range(builtMeansPlot$layout$panel_ranges[[1]]$y.range);

  builtAssocPlot <- ggplot2::ggplot_build(plot2);
  builtAssocPlot$layout$panel_ranges[[1]]$y.range <- yRange;
  builtAssocPlot$layout$panel_ranges[[1]]$y.major <-
    builtMeansPlot$layout$panel_ranges[[1]]$y.major;

  plot1grob <- ggplot2::ggplot_gtable(builtMeansPlot);
  plot2grob <- ggplot2::ggplot_gtable(builtAssocPlot);

  #browser();

  ### Add row in plot2 for the legend; first get row position & height
  legendRow <- plot1grob$layout[grep("guide-box-top", plot1grob$layout$name),
                                c("t", "b")];
#  if (length(legendRow[['b']]) == 0) {
  #   legendHeight <- grid::unit(1, "mm");
  #   rowBelowLegendHeight <- grid::unit(1, "mm");
  # } else {
    legendHeight <- plot1grob$heights[legendRow[['b']]];
    rowBelowLegendHeight <- plot1grob$heights[legendRow[['b']]+1];
#  }

  rowAboveLegend <- min(legendRow) - 1;

  plot2grob <- gtable::gtable_add_rows(plot2grob,
                                        rowBelowLegendHeight,
                                        pos = rowAboveLegend);
  plot2grob <- gtable::gtable_add_rows(plot2grob,
                                        legendHeight,
                                        pos = rowAboveLegend);

  plot <- gtable::gtable_add_cols(plot1grob,
                                  grid::unit(1, "null"));

  plot <- gtable::gtable_add_grob(plot,
                                  plot2grob,
                                  t=1,
                                  b=length(plot$heights),
                                  l=length(plot$widths));

  if (drawPlot) {
    grid::grid.newpage();
    grid::grid.draw(plot);
  }

  if (!is.null(outputFile)) {
    ggsaveParameters <- c(list(filename = outputFile,
                               plot = plot,
                               width = outputWidth,
                               height = outputHeight),
                          ggsaveParams);
    do.call(ggplot2::ggsave, ggsaveParameters);
  }

  invisible(plot);

}
