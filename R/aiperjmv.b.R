
# This file is a generated template, your changes will not be overwritten

aiperjmvClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "aiperjmvClass",
    inherit = aiperjmvBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

            if ((is.numeric(self$options$r) &&
                 (self$options$r > -1) &&
                 (self$options$r < 1)) &&
                (is.numeric(self$options$w) &&
                 (self$options$w > 0) &&
                 (self$options$w < 1)) &&
                (is.numeric(self$options$conf.level) &&
                 (self$options$conf.level > 0) &&
                 (self$options$conf.level < 100))) {

              res <- pwr.confIntR(r = self$options$r,
                                  w = self$options$w,
                                  conf.level = self$options$conf.level/100);

              self$results$text$setContent(
                  paste0("To estimate a Pearson correlation of ",
                         self$options$r, " or stronger with a ",
                         self$options$conf.level, "% confidence ",
                         "interval with a maximum half-width of ",
                         self$options$w, " or less, at least ",
                         res, " participants are required."));

              self$results$aipePlot$setState(list(r=self$options$r,
                                                  n=res,
                                                  conf.level=self$options$conf.level));

            }

        },
        .plot = function(aipePlot, ggtheme, theme, ...) {

            if (!is.null(aipePlot$state)) {
                r <- aipePlot$state$r;
                n <- aipePlot$state$n;
                conf.level <- aipePlot$state$conf.level / 100;
                res <-
                    ufs::confIntR(r = r,
                                  N = n,
                                  conf.level=conf.level,
                                  plot=TRUE);
                plot <- attr(res, 'plot') +
                    ggplot2::theme(panel.background = ggplot2::element_rect(fill="transparent",
                                                                            color="transparent"),
                                   plot.background = ggplot2::element_rect(fill="transparent",
                                                                           color="transparent"),
                                   legend.background = ggplot2::element_rect(fill="transparent",
                                                                             color="transparent"),
                                   legend.box.background = ggplot2::element_rect(fill="transparent",
                                                                                 color="transparent")) +
                    ggtheme +
                    ggplot2::theme(plot.title = ggplot2::element_text(margin=ggplot2::margin(b = 5.5 * 1.2)),
                                   plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5)) +
                    NULL;
                print(res);
            }

            TRUE;

            })
)
