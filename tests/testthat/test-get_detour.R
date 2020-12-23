context("Get detour")
library(cppRouting)

test_that("directed graph distance matrix", {
## Given a simple graph      3
##                        d3 d6 d13
##                     1 d2 2  d6d  4  d22  5
    from <- c(1, 1, 2, 3, 4)
    to   <- c(2, 3,  4, 4, 5)
    dist <- c(2, 3, 6, 13, 22)
    graph_df <- data.frame(from = from, to = to, dist = dist)
    graph <- cppRouting::makegraph(graph_df, directed = FALSE)
    distance <- get_distance_pair(graph, from = 1, to = 5)
    detour <- get_detour(graph, from = 1, to = 5, extra = 1)
    expect_equal(length(detour[[1]]), 4)
})
