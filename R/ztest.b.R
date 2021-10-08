#' @export
ztestClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "ztestClass",
    inherit = ztestBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
          mu <- self$options$populationmean
          sigma <- self$options$populationsd
          x <- self$options$samplemean
          n <- self$options$n
          se <- sigma /  sqrt(n)
          z <- (x - mu) / se
          cohend <- (x - mu) / sigma
          p <- stats::pnorm(-abs(z)) * (1 + (self$options$alt == "notEqual"))
          if (self$options$alt == "greaterthan" & z < 0) p <- 1 - p
          if (self$options$alt == "lessthan" & z > 0) p <- 1 - p
          
          table <- self$results$ztest
          table$setRow(rowNo=1, values=list(
            z = formatC(z, digits = 2, format = "f"),
            se = formatC(se, digits = 2),
            p = formatC(p, digits = 2),
            cohend = formatC(cohend, digits = 2)
          ))
          image <- self$results$plot
          
          plotData <- list(mu = mu, sigma = sigma, x = x, n = n, se = se, z = z, p = p)
          image$setState(plotData)
          
          interpretation <- ifelse(p < self$options$alpha, 
                                   "<h2>Decision</h2><p><strong>Reject the null hypothesis.</strong></p><p>The sample mean is in the critical region. The sample mean is significantly different from the null population mean.</p>", 
                                   "<h2>Decision</h2><p><strong>Retain the null hypothesis.</strong></p><p>The sample mean is not in the critical region.  The sample mean not is significantly different from the null population mean.</p>")
          text <- self$results$text
          self$results$text$setContent(interpretation)
          
        },
        .plot=function(image, ...) {
          library(ggplot2)
          plotData <- image$state
          mu <- plotData$mu
          sigma <-  plotData$sigma
          se <- plotData$se
          plot <-
            ggplot(data.frame(x = c(mu - 4 * sigma, mu + 4 * sigma))) +
            stat_function(
              fun = function(x)
                stats::dnorm(x, mean = mu, sd = sigma) / stats::dnorm(mu, mean = mu, sd = sigma),
              geom = "area",
              xlim = c(-4 * sigma + mu, 4 * sigma + mu),
              fill = "royalblue",
              alpha = .2
            ) +
            stat_function(
              fun = function(x)
                .6 * stats::dnorm(x, mean = mu, sd = se) / stats::dnorm(mu, mean = mu, sd = se),
              geom = "area",
              fill = "royalblue",
              alpha = .4,
              xlim = c(-4 * se + mu, 4 * se + mu)
            ) +
            annotate(geom = "segment", x = plotData$x, xend = plotData$x, y = 0, yend = 1) +
            annotate(geom = "text", 
                     x = plotData$x, y = 1.05, 
                     label =  paste0("Sample\nMean = ",plotData$x), 
                     vjust = 0.5) +
            theme_classic(base_size = 14,
                          base_line_size = .25) +
            theme(
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.line.y = element_blank()
            ) +
            labs(x = NULL, y = NULL) +
            scale_x_continuous(breaks = seq(-4, 4) * sigma + mu, labels = label_parsed) + 
            scale_y_continuous(NULL, expand = expansion(mult = c(0,.05))) 
          
          if (self$options$alt == "notEqual") {
            plot <- plot + 
              stat_function(
                fun = function(x)
                  0.6 * stats::dnorm(x, mean = mu, sd = se) / stats::dnorm(mu, mean = mu, sd = se),
                geom = "area",
                xlim = c(-4 * se + mu, stats::qnorm(self$options$alpha / 2, mu, se)),
                fill = "firebrick",
                alpha = .6
              ) + 
              stat_function(
                fun = function(x)
                  0.6 * stats::dnorm(x, mean = mu, sd = se) / stats::dnorm(mu, mean = mu, sd = se),
                geom = "area",
                xlim = c(stats::qnorm(1 - self$options$alpha / 2, mu, se), 4 * se + mu),
                fill = "firebrick",
                alpha = .6
              ) 
            
          }
          
          if (self$options$alt == "greaterthan") {
            plot <- plot + 
              stat_function(
                fun = function(x)
                  0.6 * stats::dnorm(x, mean = mu, sd = se) / stats::dnorm(mu, mean = mu, sd = se),
                geom = "area",
                xlim = c(stats::qnorm(1 - self$options$alpha, mu, se), 4 * se + mu),
                fill = "firebrick",
                alpha = .6
              )
            
          }
          
          if (self$options$alt == "lessthan") {
            plot <- plot + 
              stat_function(
                fun = function(x)
                  0.6 * stats::dnorm(x, mean = mu, sd = se) / stats::dnorm(mu, mean = mu, sd = se),
                geom = "area",
                xlim = c(-4 * se + mu, stats::qnorm(self$options$alpha, mu, se)),
                fill = "firebrick",
                alpha = .6
              ) 
            
          }
          print(plot)
          TRUE
        })
)
