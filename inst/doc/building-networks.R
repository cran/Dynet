## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(Dynet)

## ----school-data--------------------------------------------------------------
head(school_contacts, 4)

## ----school-------------------------------------------------------------------
school <- dynet(school_contacts)
school

## ----forum-data---------------------------------------------------------------
head(forum_posts, 3)

## ----clicks-------------------------------------------------------------------
clicks <- dynet(forum_posts, time = "timestamp")
clicks

## ----forum--------------------------------------------------------------------
forum <- dynet(forum_posts, thread = "thread", nodes = forum_people)
forum

## ----clicks-summary-----------------------------------------------------------
summary(clicks, temporal_density = TRUE)

## ----forum-summary------------------------------------------------------------
summary(forum, temporal_density = TRUE)

## ----seminar-data-------------------------------------------------------------
head(seminar_attendance, 3)

## ----seminars-----------------------------------------------------------------
seminars <- dynet(seminar_attendance, actor = "student", group = "seminar")
seminars

## ----auto---------------------------------------------------------------------
auto <- dynet(forum_posts)
auto

## ----school-summary-----------------------------------------------------------
summary(school)

## ----spells-------------------------------------------------------------------
spells <- as.data.frame(school)
head(spells, 4)

## ----nodes--------------------------------------------------------------------
forum_nodes <- as.data.frame(forum, what = "nodes")
head(forum_nodes, 4)

## ----bins---------------------------------------------------------------------
bins <- as.data.frame(school, what = "bins")
head(bins, 4)

## ----network------------------------------------------------------------------
pairs <- as.data.frame(school, what = "network")
head(pairs, 4)

## ----tiny---------------------------------------------------------------------
tiny <- data.frame(
  from  = c("A", "B", "A", "C", "A"),
  to    = c("B", "A", "C", "A", "A"),
  start = c(0, 1, 2, 3, 4),
  end   = c(2, 3, 5, 4, 6),
  posts = c(3, 1, 2, 5, 1)
)

## ----undirected---------------------------------------------------------------
undirected <- dynet(tiny, directed = FALSE, weight = "posts")
undirected

## ----loops--------------------------------------------------------------------
with_loops <- dynet(tiny, loops = TRUE)
with_loops

## ----interval-----------------------------------------------------------------
tiny_dn <- dynet(tiny, interval = 2)
summary(tiny_dn)

## ----roles--------------------------------------------------------------------
roles <- dynet(forum_posts, thread = "thread",
               nodes = forum_people, groups = "role")
role_nodes <- as.data.frame(roles, what = "nodes")
head(role_nodes, 4)

## ----mixing-------------------------------------------------------------------
role_mixing <- mixing(roles, attribute = "role")
head(role_mixing, 4)

## ----sessioned----------------------------------------------------------------
sessioned <- set_tie_sessions(school, breaks = c(7, 14),
                              labels = c("week_1", "week_2", "week_3"))
summary(sessioned)

## ----collapse-----------------------------------------------------------------
collapsed <- paths(sessioned, from = "Ana", sessions = "collapse")
summary(collapsed)

## ----bounded------------------------------------------------------------------
inside <- paths(sessioned, from = "Ana", sessions = "bounded")
summary(inside)

## ----separate-----------------------------------------------------------------
per_session <- paths(sessioned, from = "Ana", sessions = "separate")
summary(per_session)

## ----observation--------------------------------------------------------------
bounded <- dynet(school_contacts, observation_start = 0, observation_end = 14)
summary(bounded)

## ----gapped-------------------------------------------------------------------
gapped <- dynet(
  school_contacts,
  observation_spells = data.frame(start = c(0, 12), end = c(8, 21))
)
as.data.frame(gapped, what = "observations")

## ----gapped-summary-----------------------------------------------------------
summary(gapped)

## ----set-observations---------------------------------------------------------
narrowed <- set_observations(school, start = 2, end = 10)
as.data.frame(narrowed, what = "observations")

## ----clear-observations-------------------------------------------------------
continuous <- clear_observations(gapped)
as.data.frame(continuous, what = "observations")

## ----arrivals-----------------------------------------------------------------
arrivals <- data.frame(
  node  = c("Ana", "Ben"),
  start = c(0, 7),
  end   = c(21.52, 21.52)
)
scheduled <- dynet(school_contacts, vertex_spells = arrivals)
as.data.frame(scheduled, what = "vertex_spells")

## ----school-degree------------------------------------------------------------
school_degree <- centrality_series(school, measure = "degree")
head(school_degree, 4)

## ----scheduled-degree---------------------------------------------------------
scheduled_degree <- centrality_series(scheduled, measure = "degree")
head(scheduled_degree, 4)

## ----school-degree-summary----------------------------------------------------
school_degree_summary <- summary(school_degree)
head(school_degree_summary, 3)

## ----scheduled-degree-summary-------------------------------------------------
scheduled_degree_summary <- summary(scheduled_degree)
head(scheduled_degree_summary, 3)

## ----vertex-spells-edit-------------------------------------------------------
activity <- set_vertex_spells(school, arrivals)
extended <- add_vertex_spells(activity,
                              data.frame(node = "Cara", start = 3, end = 12))
as.data.frame(extended, what = "vertex_spells")

## ----add----------------------------------------------------------------------
step1 <- add_nodes(school, data.frame(name = "Nova", role = "exchange"))
step2 <- add_ties(step1, data.frame(
  from = "Ana", to = "Nova", start = 4, end = 6
))
summary(step2)

## ----original-----------------------------------------------------------------
summary(school)

## ----remove-------------------------------------------------------------------
step3 <- remove_ties(step2, from = "Ana", to = "Nova", start = 4)
summary(step3)

## ----rename-------------------------------------------------------------------
renamed <- rename_nodes(step2, c(Nova = "Nova B."))
renamed_nodes <- as.data.frame(renamed, what = "nodes")
tail(renamed_nodes, 3)

## ----induce-------------------------------------------------------------------
five <- induce_subgraph(school, nodes = c("Ana", "Ben", "Cara", "Dan", "Eve"))
summary(five)

## ----basics-------------------------------------------------------------------
basics <- metrics(school, measure = c("density", "edges", "active_nodes"))
head(basics, 6)

## ----six----------------------------------------------------------------------
six <- metrics(school, measure = c("density", "edges", "active_nodes",
                                   "components", "transitivity",
                                   "reciprocity"))
summary(six)

## ----rolling------------------------------------------------------------------
rolling <- metrics(school, measure = "density", step = 1, window = 7)
head(rolling, 4)

## ----density-plot-------------------------------------------------------------
school_density <- metrics(school, measure = "density")
plot(school_density)

## ----integrated---------------------------------------------------------------
integrated <- metrics(school, measure = c("temporal_density", "onset_intensity"),
                      step = 7, window = 7)
integrated

## ----snapshot-----------------------------------------------------------------
at_five <- snapshots(school, at = 5)
at_five

## ----events-------------------------------------------------------------------
changes <- events(school)
head(changes, 6)

## ----durations----------------------------------------------------------------
tie_durations <- durations(school)
head(tie_durations, 6)

## ----spell-durations----------------------------------------------------------
spell_durations <- durations(school, unit = "spell")
head(spell_durations, 4)

## ----node-durations-----------------------------------------------------------
node_durations <- durations(school, unit = "node_ties", mode = "all")
head(node_durations, 4)

## ----collapse-network---------------------------------------------------------
flat <- collapse_network(school, start = 0, end = 7)
flat

## ----first-week---------------------------------------------------------------
first_week <- collapse_network(school, start = 0, end = 7,
                               weight = "union_duration")
first_week

