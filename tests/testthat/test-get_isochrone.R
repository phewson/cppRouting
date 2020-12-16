context("Get isochrone")
library(cppRouting)

test_that("directed graph distance matrix", {
## Given a simple graph      3
##                        d3 d6 d13
##                     1 d2 2  d6d  4  d22  5
    edges <- data.frame(
        from_vertex = c(0, 0, 1, 1, 2, 2, 3, 4, 4),
        to_vertex = c(1, 3, 2, 4, 4, 5, 1, 3, 5),
        cost = c(9, 2, 11, 3, 5, 12, 4, 1, 6))

directed_graph <- makegraph(edges, directed = TRUE)

#Get nodes reachable around node 4 with maximum distances of 1 and 2
iso <- get_isochrone(Graph = directed_graph, from = "4", lim = c(1, 2))

#With setdif set to TRUE
iso2 <- get_isochrone(Graph = directed_graph, from = "4", lim = c(1, 2), setdif = TRUE)
print(iso)
print(iso2)

  expect_equal(iso[[1]][[1]], "4")
})
