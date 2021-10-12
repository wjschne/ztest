#' @export
ztestClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
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
          alpha <- self$options$alpha
          criticalz <- ifelse(self$options$alt == "lessthan", -1, 1) * stats::qnorm(alpha / (1 + (self$options$alt == "notEqual")))
          
          se <- sigma /  sqrt(n)
          z <- (x - mu) / se
          cohend <- (x - mu) / sigma
          tails <- 1 + (self$options$alt == "notEqual")
          althypotheses <- c(notEqual = 1, greaterthan = 2, lessthan = 3)
          p <- stats::pnorm(-abs(z)) * (tails)
          if (self$options$alt == "greaterthan" & z < 0) p <- 1 - p
          if (self$options$alt == "lessthan" & z > 0) p <- 1 - p
          
          table <- self$results$ztest
          table$setRow(rowNo = 1, values = list(
            z = z,
            se = formatC(se, digits = 2, format = "f"),
            p = p,
            cohend = formatC(cohend, digits = 2, format = "f")
          ))
          image <- self$results$plot
          
          plotData <- list(mu = mu, sigma = sigma, x = x, n = n, se = se, z = z, p = p)
          image$setState(plotData)
          
          interpretation <- paste0(
            "<h2>Decision</h2><p><strong>",
            ifelse(p < self$options$alpha, "Reject", "Retain"),
            " the null hypothesis.</strong></p><p>",
            "In a ",
            tails,
            "-tailed <em>z</em>-test with <em>&alpha;</em> = ",
            alpha,
            ", the sample mean (<em>m</em> = ",
            x,
            ") is ",
            ifelse(p < self$options$alpha, "", "not"),
            " significantly ",
            switch(self$options$alt, 
                   notEqual = "different from", 
                   greaterthan = "greater than", 
                   lessthan = "less than"),
            " the null population mean (<em>&mu;</em> = ",
            mu,
            ").</p>")
          
          text <- self$results$text
          self$results$text$setContent(interpretation)
          
        },
        .plot = function(image, ...) {
          library(ggplot2)
          library(ggtext)
          plotData <- image$state
          mu <- plotData$mu
          sigma <-  plotData$sigma
          se <- plotData$se
          dsm_height = 0.3
          sd_height = dnorm(1) / dnorm(0)
          se_height = sd_height * dsm_height
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
                dsm_height * stats::dnorm(x, mean = mu, sd = se) / stats::dnorm(mu, mean = mu, sd = se),
              geom = "area",
              fill = "royalblue",
              alpha = .4,
              xlim = c(-4 * se + mu, 4 * se + mu)
            ) +
            annotate(geom = "point", x = plotData$x, xend = plotData$x, y = 0, size = 3) +
            annotate(geom = "text", 
                     x = plotData$x, y = 0, 
                     label =  paste0("Sample\nMean = ",plotData$x), 
                     lineheight = .95, size = 5,
                     vjust = 1.9) +
            annotate(geom = "label", 
                     x = mu + sigma, 
                     y = sd_height, 
                     label = paste0("Standard\nDeviation = ", sigma), 
                     hjust = 0, 
                     fill = NA,
                     color = "gray30",
                     label.size = 0,
                     label.padding = unit(0.5, "lines"),
                     lineheight = .95) +
            annotate(geom = "label", 
                     x = mu + se, 
                     y = se_height, 
                     label = paste0("Standard\nError = ", formatC(se, digits = 2, format = "f")), 
                     hjust = 0, 
                     color = "gray30",
                     fill = NA,
                     label.size = 0, 
                     label.padding = unit(0.5, "lines"),
                     lineheight = .95) +
            annotate(geom = "text", x = mu, y = 1, 
                     label = paste0("Population\nMean = ", mu), 
                     vjust = -0.2, color = "gray30", lineheight = .95) +
            annotate(geom = "text", x = mu, y = dsm_height, 
                     label = "Distribution of\n Sample Means", 
                     vjust = -0.2, color = "gray30", lineheight = .95) +
            annotate(geom = "segment", 
                     x = mu, 
                     xend = mu + sigma, 
                     y = sd_height, 
                     yend = sd_height, 
                     color = "gray30",
                     arrow = arrow(15, type = "closed", length = unit(3, "mm"))) +
            annotate(geom = "segment", 
                     x = mu, 
                     xend = mu + se, 
                     y = se_height, 
                     color = "gray30",
                     yend = se_height, 
                     arrow = arrow(15, type = "closed", length = unit(2, "mm"))) +
            theme_classic(base_size = 14,
                          base_line_size = .25) +
            theme(
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.line.y = element_blank(),
              plot.margin = margin(t = 5, r = 1, l = 1, b = 12, unit = "mm"),
              plot.background = element_rect(fill = NA, color = NA),
              panel.background = element_rect(fill = NA, color = NA)
            ) +
            labs(x = NULL, y = NULL) +
            scale_x_continuous(breaks = seq(-4, 4) * sigma + mu, labels = label_parsed) + 
            scale_y_continuous(NULL, expand = expansion(mult = c(0,.05))) +
            coord_cartesian(clip = "off")
          
          if (self$options$alt == "notEqual") {
            plot <- plot + 
              stat_function(
                fun = function(x)
                  dsm_height * stats::dnorm(x, mean = mu, sd = se) / stats::dnorm(mu, mean = mu, sd = se),
                geom = "area",
                xlim = c(-4 * se + mu, stats::qnorm(self$options$alpha / 2, mu, se)),
                fill = "firebrick",
                alpha = .6
              ) + 
              stat_function(
                fun = function(x)
                  dsm_height * stats::dnorm(x, mean = mu, sd = se) / stats::dnorm(mu, mean = mu, sd = se),
                geom = "area",
                xlim = c(stats::qnorm(1 - self$options$alpha / 2, mu, se), 4 * se + mu),
                fill = "firebrick",
                alpha = .6
              ) + 
              annotate(geom = "richtext", 
                       x = mu + 4 * sigma, 
                       y = 1, 
                       hjust = 1, 
                       vjust = 1,
                       label = paste0(
                         "<span style='font-size:14pt'>**Critical Region**</span>:<br>Reject the null hypothesis<br>if the sample mean<br>is greater than ", 
                         formatC(stats::qnorm(1 - self$options$alpha / 2, mu, se), digits = 2, format = "f"), 
                         "<br>or less than ", 
                         formatC(stats::qnorm(self$options$alpha / 2, mu, se), digits = 2, format = "f"),
                         "."), 
                       color = "firebrick", 
                       label.color = NA,
                       fill = NA)
            
          }
          
          if (self$options$alt == "greaterthan") {
            plot <- plot + 
              stat_function(
                fun = function(x)
                  dsm_height * stats::dnorm(x, mean = mu, sd = se) / stats::dnorm(mu, mean = mu, sd = se),
                geom = "area",
                xlim = c(stats::qnorm(1 - self$options$alpha, mu, se), 4 * se + mu),
                fill = "firebrick",
                alpha = .6
              ) + 
              annotate(geom = "richtext", 
                       x = mu + 4 * sigma, 
                       y = 1, hjust = 1, 
                       vjust = 1, 
                       label = paste0(
                         "<span style='font-size:14pt'>**Critical Region**</span>:<br>Reject the null hypothesis<br>if the sample mean<br>is greater than ", 
                         formatC(stats::qnorm(1 - self$options$alpha, mu, se), digits = 2, format = "f"), 
                         "."), 
                       color = "firebrick", 
                       label.color = NA,
                       fill = NA)
          }
          
          if (self$options$alt == "lessthan") {
            plot <- plot + 
              stat_function(
                fun = function(x)
                  dsm_height * stats::dnorm(x, mean = mu, sd = se) / stats::dnorm(mu, mean = mu, sd = se),
                geom = "area",
                xlim = c(-4 * se + mu, stats::qnorm(self$options$alpha, mu, se)),
                fill = "firebrick",
                alpha = .6
              ) + 
              annotate(geom = "richtext", 
                       x = mu - 4 * sigma, 
                       y = 1, 
                       hjust = 0, vjust = 1,
                       label = paste0(
                         "<span style='font-size:14pt'>**Critical Region**</span>:<br>Reject the null hypothesis<br>if the sample mean<br>is less than ", 
                         formatC(stats::qnorm(self$options$alpha, mu, se), digits = 2, format = "f"), 
                         "."),
                       color = "firebrick", 
                       label.color = NA, 
                       fill = NA)
          }
          print(plot)
          TRUE
        })
)
