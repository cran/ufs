#' Conceptual Independence Matrix
#'
#' @param data The dataframe containing the variables.
#' @param scales The scales: a names list of character vectors,
#' where the character vectors specify the variable names, and the
#' names of each character vector specifies the relevant scale.
#' @param outputFile The file to write the output to.
#' @param outputWidth,outputHeight,outputUnits The width, height,
#' and units for the output file.
#' @param faMethod The method to pass on to [psych::fa()].
#' @param n.iter The number of iterations to pass on to [psych::fa()].
#' @param skipRegex A character vector of length 2 containing two
#' regular expressions; if the two scales both match one or both
#' of those regular expressions, that cell is skipped.
#'
#' @return A [ggplot2::ggplot()] plot.
#' @examples ### Load data from psych package
#' data(bfi,package= 'psych');
#'
#' ### Specify scales
#' bfiScales <-
#'   list(Agree = paste0("A", 1:5),
#'        Consc = paste0("C", 1:5),
#'        Extra = paste0("E", 1:5),
#'        Neuro = paste0("N", 1:5),
#'        Openn = paste0("O", 1:5));
#'
#' ### Only select first two and the first three items to
#' ### keep it quick; just pass the full 'bfiScales'
#' ### object to run for all five the full scales
#' CIM(bfi,
#'     scales=lapply(bfiScales, head, 3)[1:2],
#'     n.iter=10);
#'
#' @export
CIM <- function(data,
                scales,
                outputFile = NULL,
                outputWidth = 100,
                outputHeight = 100,
                outputUnits = "cm",
                faMethod = "minres",
                n.iter = 100,
                skipRegex = NULL) {

  ###--------------------------------------------------------------------------
  ### Uses two suggested packages; check their presence
  ###--------------------------------------------------------------------------

  if (!requireNamespace("psych", quietly = TRUE)) {
    stop("To do the exploratory factor analysis, the \"psych\" package is required. ",
         "Please install it using `install.packages('psych');`.",
         call. = FALSE);
  }

  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("To do the confirmatory factor analysis, the \"lavaan\" package is required. ",
         "Please install it using `install.packages('lavaan');`.",
         call. = FALSE);
  }

  ###--------------------------------------------------------------------------
  ### Helper functions
  ###--------------------------------------------------------------------------

  ### Generate tableGrob with headers for the
  ### table with factor loadings (see
  ### http://stackoverflow.com/questions/33214671/merging-table-header-cells-using-tablegrob
  ### and https://github.com/baptiste/gridextra/wiki/tableGrob
  makeHeaderTable <- function(vector, startCol=2, colSpan=3) {
    nCol <- length(vector);
    tmpDf <- t(matrix(1:nCol));
    colnames(tmpDf) <- vector;
    rownames(tmpDf) <- 'rowName';
    headerTable <- gridExtra::tableGrob(tmpDf);
    headerTable$grobs[[1]] <- grid::textGrob('');
    rm(tmpDf);
    headerTable <- headerTable[1, ];

    ### Compute where each column should start and end
    startCols <- seq(startCol, colSpan * nCol + startCol - 1, by=colSpan);
    endCols <- startCols + colSpan - 1;

    headerTable$layout[3:nrow(headerTable$layout), c("l", "r")] <-
      list(startCols, endCols);
    return(headerTable);
  }

  headerTable <- makeHeaderTable(c('Factor 1', 'Factor 2'));

  ###--------------------------------------------------------------------------
  ### Prepare input and object to return
  ###--------------------------------------------------------------------------

  ### Create object for output
  grobsList <- list();
  dfList <- list();

  dataframeName <- deparse(substitute(data));

  ###--------------------------------------------------------------------------
  ### Reminders for the future
  ###--------------------------------------------------------------------------

  ### Reminder: maybe change order of variables

  ### Reminder: compare variances of scales (homogeneity)

  ###--------------------------------------------------------------------------
  ### The loop
  ###--------------------------------------------------------------------------

  ### Initialize progress bar
  pb <- utils::txtProgressBar(style=3,
                              min = 1,
                              max = length(scales) ^ 2);

  for (rowVar in names(scales)) {
    ### Get index of this row
    rowIndex <- which(names(scales) == rowVar);
    ### Generate object to store the columns of this row in
    grobsList[[rowIndex]] <-list();

    for (colVar in names(scales)) {

      ### Get index of this column
      colIndex <- which(names(scales) == colVar);
      ### Generate object to store results for this cell in
      grobsList[[rowIndex]][[colIndex]] <- list();

      progressIndex <- ((rowIndex - 1) * length(scales)) + colIndex;

      ### Update progress bar
      utils::setTxtProgressBar(pb,
                               value=progressIndex);

      if (rowVar == colVar) {
        ### On the main diagonal
        grobsList[[rowIndex]][[colIndex]] <-
          grid::textGrob("");

      } else {
        ### On the upper or lower diagonal

        ### Continue with next variable if we match the regexes that
        ### specify when to skip a combination
        if (!is.null(skipRegex) &&
            (any(grepl(skipRegex[1],
                       x = c(colVar, rowVar))) &&
             any(grepl(skipRegex[2],
                       x = c(colVar, rowVar))))) {
          grobsList[[rowIndex]][[colIndex]] <-
            grid::textGrob(paste0("Skipping this cell!\n(", rowVar,
                                  " & ", colVar, ")"));

        ### Otherwise, do the factor analyses
        } else {

          ### Get a convenient list of the items in both scales
          vars <- c(scales[[rowVar]],
                    scales[[colVar]]);

          ### Get both measurement models
          oneFactor <- paste0(' variable =~ ',
                              paste0(vars, collapse=" + "));
          twoFactor <- paste0(' ', rowVar, ' =~ ',
                              paste0(scales[[rowVar]], collapse=" + "),
                              '\n',
                              ' ', colVar, ' =~ ',
                              paste0(scales[[colVar]], collapse=" + "));

          ### First do an EFA
          efa <-
            suppressMessages(psych::fa(data[, vars],
                             nfactors=2,
                             fm=faMethod,
                             n.iter=n.iter));

          ### Then the CFAs
          nrOfOneFactorCFAwarnings <- 0;
          oneFactorCFA <- lavaan::cfa(oneFactor, data=data);
          nrOfTwoFactorCFAwarnings <- 0;
          twoFactorCFA <- lavaan::cfa(twoFactor, data=data);

          ### Then run and compare the two CFAs
          cfa <- lavaan::anova(oneFactorCFA, twoFactorCFA);

          ### In cfa$Chisq, the first number is the chi square of the
          ### TWO-factor model, and the second one of the ONE-factor model!
          bestFittingModel <- ifelse(cfa$Chisq[1] < cfa$Chisq[2],
                                     '2-factor', '1-factor');

          fitText <- paste0(bestFittingModel, ' fits better; ',
                            'Chisq[',
                            cfa[2, 'Df diff'],
                            ']=', round(cfa[2, 'Chisq diff'], 2),
                            ", ",
                            ufs::formatPvalue(cfa[2, 'Pr(>Chisq)']));

          ### Make dataframe with factor loading confidence intervals
          faDf <- matrix(unlist(ufs::faConfInt(efa)), ncol=6);

          ### Get abbreviated scale names
          abbr <- abbreviate(names(scales));

          ### Set row and column names
          rownames(faDf) <- c(paste0(abbr[rowVar], 1:length(scales[[rowVar]])),
                              paste0(abbr[colVar], 1:length(scales[[colVar]])));
          colnames(faDf) <- c(rep(c('lo', 'est', 'hi'), 2));

          ### To do: add explained variance per factor

          if (rowIndex < colIndex) {
            ### Upper diagonal

            grobsList[[rowIndex]][[colIndex]] <-
              factorLoadingDiamondCIplot(efa) +
              #faDfDiamondCIplot(faDf, xlab=NULL) +
              ggplot2::ggtitle(paste0(rowVar, ' & ', colVar, "\n",
                                      fitText));
            #           textGrob(paste0("Upper diag:\n", rowVar,
            #                           " and ", colVar));

          } else {
            ### Lower diagonal

            grobsList[[rowIndex]][[colIndex]] <-
              gridExtra::tableGrob(round(faDf, 2));

            grobsList[[rowIndex]][[colIndex]] <-
              gridExtra::gtable_combine(headerTable,
                                        grobsList[[rowIndex]][[colIndex]],
                                        along=2);

            prevWidths <- grobsList[[rowIndex]][[colIndex]]$widths;

            ### Add variable names
            grobsList[[rowIndex]][[colIndex]] <-
              gridExtra::gtable_combine(makeHeaderTable(paste0(rowVar, ' & ', colVar),
                                                        colSpan=6),
                                        grobsList[[rowIndex]][[colIndex]],
                                        along=2);

            ### Set widths (again, based on
            ### https://github.com/baptiste/gridextra/wiki/tableGrob
            grobsList[[rowIndex]][[colIndex]]$widths <-
              grid::unit(rep(.95 * (1/ncol(grobsList[[rowIndex]][[colIndex]])),
                             ncol(grobsList[[rowIndex]][[colIndex]])),
                         "npc");
          }
        }
      }
      ### Finally, construct content; for diagonal, omega etc; for
      ### upper diagonal, diamondPlots; for lower diagnoal, numbers

    }
  }

  ### Close progress indicator;
  close(pb);

  CIM <- gridExtra::arrangeGrob(grobs=unlist(grobsList,
                                             recursive=FALSE),
                                ncol=length(grobsList));

  ### Return and/or save conceptual independence matrix
  if (is.null(outputFile)) {
    grid::grid.newpage();
    return(grid::grid.draw(CIM));
  } else {
    ggplot2::ggsave(file=outputFile,
                    plot=CIM,
                    width = outputWidth,
                    height = outputHeight,
                    units = outputUnits);
    return(invisible(CIM));
  }

}
