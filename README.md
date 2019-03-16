ggparty
================

ggplot2 visualizations for the partykit package.

Install
-------

``` r
devtools::install_github("mmostly-harmless/ggparty", 
                          dependencies=TRUE)
```

Example
-------

``` r
library(ggparty)

data("TeachingRatings", package = "AER")
tr <- subset(TeachingRatings, credits == "more")

tr_tree <- lmtree(eval ~ beauty | minority + age + gender + division + native +
                    tenure, data = tr, weights = students, caseweights = FALSE)

ggparty(tr_tree, terminal_space = 0.5) +
  geom_edge(size = 1.5) +
  geom_node_splitvar(fontface = "bold", size = 8) +
  geom_edge_label(colour = "grey", size = 6) +
  geom_nodeplot(gglist = list(geom_point(aes(x = beauty,
                                             y = eval,
                                             col = tenure,
                                             shape = minority),
                                         alpha = 0.8),
                              geom_smooth(aes(x = beauty, y = eval),
                                          method = "lm"),
                              theme_bw(base_size = 15)),
                scales = "fixed",
                id = "terminal",
                shared_axes_labels = TRUE)
```

![](README_files/figure-markdown_github/unnamed-chunk-2-1.png)

[![Travis build status](https://travis-ci.org/mmostly-harmless/ggparty.svg?branch=master)](https://travis-ci.org/mmostly-harmless/ggparty)
