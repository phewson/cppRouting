context("Get isochrone")
library(cppRouting)

## A simple graph
##         +------->-------3
##         |             ^ |
##         |           d2 d4 
##         |          /    v
##         ^        0-d3-->1-d11->2-d12>5
##         |                \     |     |
##         |                 d3   d5    ^
##         |                  v  v      |
##         d1 --------<------- 4 --->---d6
edges <- data.frame(
    from_vertex = c(0, 0, 1, 1, 2, 2, 3, 4, 4),
    to_vertex =   c(1, 3, 2, 4, 4, 5, 1, 3, 5),
    cost =        c(9, 2, 11, 3, 5, 12, 4, 1, 6))
directed_graph <- makegraph(edges, directed = TRUE)

expected <- data.frame(origin = as.character(c(4, 4, 4)),
                       node = as.character(c(4, 3, 4)),
                       lim = as.character(c(1, 2, 2)))

test_that("isochrones by limit", {
    iso <- get_isochrone(Graph = directed_graph, from = "4", lim = c(1, 2), long = TRUE)
    expect_equal(iso, expected)
})

test_that("isochrones by bucket", {
    iso2 <- get_isochrone(Graph = directed_graph, from = "4", lim = c(1, 2),
                          setdif = TRUE, long = TRUE)
    expect_equal(iso2, expected[-3, ])
})
