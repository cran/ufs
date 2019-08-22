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
#' @param headingLevel The level for the heading; especially useful when
#' knitting an Rmd partial.
#' @param printAbbreviations Whether to print a table with the abbreviations
#' that are used.
#' @param drawPlot Whether to draw the plot or only return it.
#' @param returnPlotOnly Whether to return the plot only, or the entire object.
#' @param x The object to print.
#' @param quiet Whether to be quiet or chatty.
#' @param echoPartial Whether to `echo` the code in the Rmd partial.
#' @param partialFile Can be used to override the Rmd partial file.
#' @param ... Additional arguments are passed on the respective default methods.
#'
#' @return A [ggplot2::ggplot()] plot.
#' @examples ### Load data from psych package
#' data(bfi, package= 'psych');
#'
#' ### Specify scales
#' bfiScales <-
#'   list(Agreeableness     = paste0("Agreeableness_item_", 1:5),
#'        Conscientiousness = paste0("Conscientiousness_item_", 1:5),
#'        Extraversion      = paste0("Extraversion_item_", 1:5),
#'        Neuroticism       = paste0("Neuroticism_item_", 1:5),
#'        Openness          = paste0("Openness_item_", 1:5));
#'
#' names(bfi) <- c(unlist(bfiScales),
#'                 c('gender', 'education', 'age'));
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
                skipRegex = NULL,
                headingLevel=2,
                printAbbreviations = TRUE,
                drawPlot=TRUE,
                returnPlotOnly=TRUE) {

  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());

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

  ### Create objects for output
  grobsList <- list();
  errorsList <- list();
  warningsList <- list();
  messagesList <- list();
  dfList <- list();

  dataframeName <- deparse(substitute(data));


  abbrScaleNames <- abbreviate(names(scales));
  abbrScales <-
    lapply(seq_along(scales), function(i) {
      return(paste0(abbrScaleNames[i], 1:length(scales[[i]])));
    });
  names(abbrScales) <- abbrScaleNames;
  names(abbrScaleNames) <- names(scales);
  abbreviationLegend <-
    data.frame(scale = rep(names(scales),
                           times=unlist(lapply(scales, length))),
               abbreviatedScale = rep(names(abbrScales),
                                      times=unlist(lapply(abbrScales, length))),
               variable = unlist(scales),
               abbreviatedVariable = unlist(abbrScales),
               stringsAsFactors = FALSE);
  row.names(abbreviationLegend) <- NULL;

  ###--------------------------------------------------------------------------
  ### Reminders for the future
  ###--------------------------------------------------------------------------

  ### Reminder: maybe change order of variables

  ### Reminder: compare variances of scales (homogeneity)

  ###--------------------------------------------------------------------------
  ### The loop
  ###--------------------------------------------------------------------------

  res$intermediate <- list(efas = list(),
                           cfas = list(),
                           diamondplots = list(),
                           scales = scales,
                           abbrScales = abbrScales,
                           abbrScaleNames = abbrScaleNames,
                           abbreviationLegend = abbreviationLegend);

  if (!isTRUE(getOption('knitr.in.progress'))) {
    ### Initialize progress bar
    pb <- utils::txtProgressBar(style=3,
                                min = 1,
                                max = length(scales) ^ 2);
  }

  for (rowVar in names(scales)) {

    res$intermediate$efas[[rowVar]] <- list();
    res$intermediate$cfas[[rowVar]] <- list();
    res$intermediate$diamondplots[[rowVar]] <- list();

    ### Get index of this row
    rowIndex <- which(names(scales) == rowVar);
    ### Generate object to store the columns of this row in
    grobsList[[rowIndex]] <-list();
    errorsList[[rowIndex]] <-list();
    warningsList[[rowIndex]] <-list();
    messagesList[[rowIndex]] <-list();

    for (colVar in names(scales)) {

      ### Get index of this column
      colIndex <- which(names(scales) == colVar);
      ### Generate object to store results for this cell in
      grobsList[[rowIndex]][[colIndex]] <- list();
      errorsList[[rowIndex]][[colIndex]] <- list(efa = character(),
                                                 cfa1 = character(),
                                                 cfa2 = character());
      warningsList[[rowIndex]][[colIndex]] <- list(efa = character(),
                                                   cfa1 = character(),
                                                   cfa2 = character());
      messagesList[[rowIndex]][[colIndex]] <- list(efa = character(),
                                                   cfa1 = character(),
                                                   cfa2 = character());

      if (!isTRUE(getOption('knitr.in.progress'))) {
        progressIndex <- ((rowIndex - 1) * length(scales)) + colIndex;
        ### Update progress bar
        utils::setTxtProgressBar(pb,
                                 value=progressIndex);
      }

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

          ### Check the extremely useful explanation at
          ### https://cran.r-project.org/web/packages/tryCatchLog/vignettes/tryCatchLog-intro.html


          ### First do an EFA
          abbrVarsDat <- data[, rev(vars)];
          abbrVarNames <- c(abbrScales[[abbrScaleNames[rowVar]]],
                            abbrScales[[abbrScaleNames[colVar]]]);
          names(abbrVarsDat) <- abbrVarNames;

          ### Store original warning level
          oldWarn <- getOption('warn', 0);

          ### Throw warnings as messages
          options(warn=1);

          warningsList[[rowVar]][[colVar]]$efa <-
            utils::capture.output({
              res$intermediate$efas[[rowVar]][[colVar]] <-
                efa <-
                psych::fa(abbrVarsDat,
                          nfactors=2,
                          fm=faMethod,
                          n.iter=n.iter)
            }, type="message");

          # res$intermediate$efas[[rowVar]][[colVar]] <-
          #   efa <-
          #
          #   # psych::fa(abbrVarsDat,
          #   #           nfactors=2,
          #   #           fm=faMethod,
          #   #           n.iter=n.iter)
          #
          #
          #
          #   tryCatch(withCallingHandlers(psych::fa(abbrVarsDat,
          #                                          nfactors=2,
          #                                          fm=faMethod,
          #                                          n.iter=n.iter),
          #                            error = function(e) {
          #                              errorsList[[rowVar]][[colVar]]$efa <-
          #                                c(errorsList[[rowVar]][[colVar]]$efa,
          #                                  e$message);
          #                            },
          #                            warning = function(w) {
          #                              warningsList[[rowVar]][[colVar]]$efa <-
          #                                c(warningsList[[rowVar]][[colVar]]$efa,
          #                                  w$message);
          #                              invokeRestart("muffleWarning");
          #                            },
          #                            message - function(m) {
          #                              messagesList[[rowVar]][[colVar]]$efa <-
          #                                c(messagesList[[rowVar]][[colVar]]$efa,
          #                                  m$message);
          #                              invokeRestart("muffleMessage");
          #                            }),
          #        error = function(e) {
          #          errorsList[[rowVar]][[colVar]]$efa <-
          #            c(errorsList[[rowVar]][[colVar]]$efa,
          #              e$message);
          #        });

          ### Then the CFA for one factor
          # oneFactorCFA <-
          #   lavaan::cfa(oneFactor, data=data)

          warningsList[[rowVar]][[colVar]]$cfa1 <-
            utils::capture.output({
              oneFactorCFA <-
                lavaan::cfa(oneFactor, data=data)
            }, type="message");

            # tryCatch(withCallingHandlers(lavaan::cfa(oneFactor, data=data),
            #                              error = function(e) {
            #                                errorsList[[rowVar]][[colVar]]$cfa1 <-
            #                                  c(errorsList[[rowVar]][[colVar]]$cfa1,
            #                                    e$message);
            #                              },
            #                              warning = function(w) {
            #                                warningsList[[rowVar]][[colVar]]$cfa1 <-
            #                                  c(warningsList[[rowVar]][[colVar]]$cfa1,
            #                                    w$message);
            #                                invokeRestart("muffleWarning");
            #                              },
            #                              message - function(m) {
            #                                messagesList[[rowVar]][[colVar]]$cfa1 <-
            #                                  c(messagesList[[rowVar]][[colVar]]$cfa1,
            #                                    m$message);
            #                                invokeRestart("muffleMessage");
            #                              }),
            #          error = function(e) {
            #            errorsList[[rowVar]][[colVar]]$cfa1 <-
            #              c(errorsList[[rowVar]][[colVar]]$cfa1,
            #                e$message);
            #          });

          ### Then the CFA for two factors
          warningsList[[rowVar]][[colVar]]$cfa2 <-
            utils::capture.output({
              twoFactorCFA <-
                lavaan::cfa(twoFactor, data=data)
            }, type="message");

          # twoFactorCFA <-
          #   lavaan::cfa(twoFactor, data=data)

            # tryCatch(withCallingHandlers(lavaan::cfa(twoFactor, data=data),
            #                              error = function(e) {
            #                                errorsList[[rowVar]][[colVar]]$cfa1 <-
            #                                  c(errorsList[[rowVar]][[colVar]]$cfa1,
            #                                    e$message);
            #                              },
            #                              warning = function(w) {
            #                                warningsList[[rowVar]][[colVar]]$cfa1 <-
            #                                  c(warningsList[[rowVar]][[colVar]]$cfa1,
            #                                    w$message);
            #                                invokeRestart("muffleWarning");
            #                              },
            #                              message - function(m) {
            #                                messagesList[[rowVar]][[colVar]]$cfa1 <-
            #                                  c(messagesList[[rowVar]][[colVar]]$cfa1,
            #                                    m$message);
            #                                invokeRestart("muffleMessage");
            #                              }),
            #          error = function(e) {
            #            errorsList[[rowVar]][[colVar]]$cfa1 <-
            #              c(errorsList[[rowVar]][[colVar]]$cfa1,
            #                e$message);
            #          });

          ### Then run and compare the two CFAs
          if (("lavaan" %in% class('oneFactorCFA')) && ('lavaan' %in% class('twoFactorCFA'))) {
            res$intermediate$cfas[[rowVar]][[colVar]] <-
              cfa <-
              lavaan::anova(oneFactorCFA, twoFactorCFA);

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

            ### Set options back to original setting
            options(warn=oldWarn);

          } else {
            res$intermediate$cfas[[rowVar]][[colVar]] <-
              cfa <-
              NA;
            bestFittingModel <- "not computed";
            fitText <- "Not computed which model fits better."
          }

          ### Make dataframe with factor loading confidence intervals
          if ('psych' %in% class(efa)) {
            faDf <- matrix(unlist(ufs::faConfInt(efa)), ncol=6);
          } else {
            faDf <- matrix(rep(NA, 6*ncol(abbrVarsDat)),
                           ncol=6);
          }

          ### Get abbreviated scale names
          abbr <- abbreviate(names(scales));

          ### Set row and column names
          rownames(faDf) <- c(abbrScales[[abbrScaleNames[rowVar]]],
                              abbrScales[[abbrScaleNames[colVar]]]);
                              # paste0(abbr[rowVar], 1:length(scales[[rowVar]])),
                              # paste0(abbr[colVar], 1:length(scales[[colVar]])));
          colnames(faDf) <- c(rep(c('lo', 'est', 'hi'), 2));

          ###------------------------------------------------------------------
          ###------------------------------------------------------------------
          ### To do: add explained variance per factor
          ###------------------------------------------------------------------
          ###------------------------------------------------------------------

          if (rowIndex < colIndex) {
            ### Upper diagonal

            if ('psych' %in% class(efa)) {
              grobsList[[rowIndex]][[colIndex]] <-
                res$intermediate$diamondplots[[rowVar]][[colVar]] <-
                  factorLoadingDiamondCIplot(efa) +
              #faDfDiamondCIplot(faDf, xlab=NULL) +
                  ggplot2::ggtitle(paste0(rowVar, ' & ', colVar, "\n",
                                          fitText));
            #           textGrob(paste0("Upper diag:\n", rowVar,
            #                           " and ", colVar));
            } else {
              grobsList[[rowIndex]][[colIndex]] <-
                grid::textGrob('Not possible');
            }

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

  if (!isTRUE(getOption('knitr.in.progress'))) {
    ### Close progress indicator;
    close(pb);
  }

  CIM <- gridExtra::arrangeGrob(grobs=unlist(grobsList,
                                             recursive=FALSE),
                                ncol=length(grobsList));

  if (returnPlotOnly && (!isTRUE(getOption('knitr.in.progress')))) {
    res <- CIM;
  } else {
    res$output <- list(CIM = CIM);
    res$intermediate <- list(grobsList = grobsList,
                             abbreviationLegend = abbreviationLegend,
                             errorsList = errorsList,
                             warningsList = warningsList,
                             messagesList = messagesList);
    class(res) <-
      "CIM";
  }

  if (printAbbreviations && (!isTRUE(getOption('knitr.in.progress')))) {
    cat0("\n\nUsed scales and items:\n\n");
    print(abbreviationLegend);
  }

  ### Return and/or save conceptual independence matrix
  if (drawPlot && (!isTRUE(getOption('knitr.in.progress')))) {
    grid::grid.newpage();
    grid::grid.draw(CIM)
  }
  if (is.null(outputFile)) {
    if (isTRUE(getOption('knitr.in.progress'))) {
      return(res);
    } else {
      return(invisible(res));
    }
  } else {
    ggplot2::ggsave(file=outputFile,
                    plot=CIM,
                    width = outputWidth,
                    height = outputHeight,
                    units = outputUnits);
    if (isTRUE(getOption('knitr.in.progress'))) {
      return(res);
    } else {
      return(invisible(res));
    }
  }

}


#' @rdname CIM
#' @method knit_print CIM
#' @importFrom knitr knit_print
#' @export
knit_print.CIM <- function(x,
                           headingLevel = x$input$headingLevel,
                           quiet=TRUE,
                           echoPartial = FALSE,
                           partialFile = NULL,
                           ...) {

  ### Get filename
  if (!is.null(partialFile) && file.exists(partialFile)) {
    rmdPartialFilename <-
      partialFile;
  } else {
    rmdPartialFilename <-
      system.file("partials", "_CIM.rmd", package="ufs");
  }

  rmdpartials__partial(input=rmdPartialFilename);

}
