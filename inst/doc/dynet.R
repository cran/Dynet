## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>",
                      fig.width = 7, fig.height = 4.5)
library(Dynet)

## ----data---------------------------------------------------------------------
head(school_contacts)

## ----build--------------------------------------------------------------------
dn <- dynet(school_contacts)
dn

## ----summary------------------------------------------------------------------
summary(dn)

## ----activity-----------------------------------------------------------------
plot(dn, type = "activity")

## ----density------------------------------------------------------------------
density <- metrics(dn, measure = "density")
density

## ----density-summary----------------------------------------------------------
summary(density)

## ----density-rolling----------------------------------------------------------
density_weekly <- metrics(dn, measure = "density", step = 1, window = 7)
plot(density_weekly)

## ----degree-------------------------------------------------------------------
degree <- centrality_series(dn, measure = "degree")
degree

## ----degree-summary-----------------------------------------------------------
summary(degree)

## ----degree-heatmap-----------------------------------------------------------
plot(degree, type = "heatmap")

## ----closeness----------------------------------------------------------------
closeness <- path_centrality(dn, measure = "closeness")
closeness

## ----betweenness--------------------------------------------------------------
betweenness <- path_centrality(dn, measure = "betweenness")
betweenness

## ----paths--------------------------------------------------------------------
from_ana <- paths(dn, from = "Ana")
from_ana

## ----paths-summary------------------------------------------------------------
summary(from_ana)

## ----paths-late---------------------------------------------------------------
from_ana_late <- paths(dn, from = "Ana", start = 18)
summary(from_ana_late)

## ----paths-backward-----------------------------------------------------------
into_ana <- paths(dn, from = "Ana", direction = "backward")
summary(into_ana)

## ----pathways-----------------------------------------------------------------
pathways(dn, from = "Ana")

## ----tree, fig.height = 5-----------------------------------------------------
plot(from_ana)

## ----trajectories-------------------------------------------------------------
tree <- path_trajectories(from_ana)
tree

## ----events-------------------------------------------------------------------
turnover <- events(dn)
summary(turnover)

## ----durations----------------------------------------------------------------
lengths <- durations(dn, measure = "mean")
lengths

## ----burstiness---------------------------------------------------------------
rhythm <- burstiness(dn)
summary(rhythm)

