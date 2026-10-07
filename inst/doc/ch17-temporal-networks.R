## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.align = "center",
                      fig.width = 7, fig.height = 4.5, dpi = 96)

## ----data---------------------------------------------------------------------
library(Dynet)
head(mooc_posts)
head(mooc_people)

## ----build--------------------------------------------------------------------
dn_full <- dynet(mooc_posts, from = "sender", to = "receiver",
                 time = "timestamp", thread = "discussion",
                 nodes = mooc_people, time_unit = "days",
                 min_thread_posts = 2)
summary(dn_full)

## ----active-------------------------------------------------------------------
dn <- induce_subgraph(dn_full, degree > 20)
dn

## ----net-plot, fig.width = 7, fig.height = 6----------------------------------
plot(dn, type = "network")

## ----snapshots, fig.width = 9, fig.height = 3---------------------------------
plot(dn, type = "snapshots", panels = 4)

## ----snapshot-counts----------------------------------------------------------
weekly <- snapshots(dn, start = 1, end = 22, step = 7, window = 7)
summary(weekly)

## ----timeline, fig.height = 5-------------------------------------------------
plot(dn, type = "timeline", top = 25)

## ----turnover-----------------------------------------------------------------
turnover <- events(dn, measure = c("formation", "dissolution"),
                   start = 0, end = 72, step = 1, window = 1)
summary(turnover)
plot(turnover)

## ----proximity, fig.width = 8, fig.height = 6---------------------------------
plot(dn, type = "proximity", slices = 20)

## ----density------------------------------------------------------------------
weekly_density <- metrics(dn, measure = "density",
                          start = 14, end = 60, step = 1, window = 7)
summary(weekly_density)
plot(weekly_density)

## ----density-scalars----------------------------------------------------------
scalars <- metrics(dn, measure = c("edges", "density", "temporal_density"),
                   window = "all")
scalars

## ----recip--------------------------------------------------------------------
reciprocity <- metrics(dn_full, measure = "reciprocity",
                       start = 1, end = 73, step = 1, window = 1)
summary(reciprocity)
plot(reciprocity)

## ----dyads--------------------------------------------------------------------
dyad_census <- metrics(dn, measure = c("mutual", "asymmetric", "null"),
                       start = 0, end = 72, step = 1, window = 0)
summary(dyad_census)
plot(dyad_census)

## ----centralization-----------------------------------------------------------
centralisation <- metrics(dn, measure = "centralization_degree",
                          start = 1, end = 73, step = 1, window = 1)
summary(centralisation)
plot(centralisation)

## ----degree-------------------------------------------------------------------
degree_series <- centrality_series(dn, measure = "degree",
                                   mode = c("all", "in", "out"),
                                   start = 1, end = 73, step = 1, window = 1)
summary(degree_series, by = "measure")

## ----degree-plot, fig.height = 4.5--------------------------------------------
degree <- centrality_series(dn, measure = "degree",
                            start = 1, end = 73, step = 1, window = 1)
plot(degree, top = 10)

## ----other-centrality---------------------------------------------------------
other_centrality <- centrality_series(dn,
                                      measure = c("closeness", "betweenness",
                                                  "eigenvector"),
                                      start = 1, end = 73, step = 1, window = 1)
summary(other_centrality, by = "measure")

## ----betweenness-plot, fig.height = 4.5---------------------------------------
betweenness <- centrality_series(dn, measure = "betweenness",
                                 start = 1, end = 73, step = 1, window = 1)
plot(betweenness, top = 10)

## ----node-table---------------------------------------------------------------
ranked <- as.data.frame(dn, what = "nodes",
                        measure = c("degree", "betweenness", "eigenvector"))
head(ranked)

## ----paths--------------------------------------------------------------------
forward <- paths(dn, from = "444", direction = "forward")
forward
summary(forward)

## ----path-plot, fig.width = 8, fig.height = 6---------------------------------
plot(forward, base_size = 9)

## ----trajectories, fig.width = 9, fig.height = 6------------------------------
tree <- path_trajectories(forward)
plot_path_trajectories(tree, measure = "time", base_size = 9)

## ----mixing-------------------------------------------------------------------
mix <- mixing(dn, attribute = "expert_level",
              start = 1, end = 73, step = 1, window = 1)
summary(mix)
plot(mix)

