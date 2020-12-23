context("Simplify")
library(cppRouting)

## Messy graph
##
## 1 -> 0         8 <- 7
## ^              |    ^
## |              v    |
## 2 <- 3 <- 4    5 -> 6
x <- c(1, 2, 3, 4, 5, 6, 7, 8)
y <- c(0, 1, 2, 3, 6, 7, 8, 5)
edges <- data.frame(from = LETTERS[x + 1],
                    to = LETTERS[y + 1],
                    dist = c(1, 1, 1, 1, 1, 1, 1, 1))
coord <- data.frame(node = LETTERS[c(0:8) + 1], X = c(0:8), Y = c(0:8))
graph <- makegraph(edges, directed = TRUE, coords = coord)

test_that("simplify 4 to 0", {
    simp <- cpp_simplify(graph, rm_loop = FALSE)
    # The output data have five rows (a loop of four and one for 4 to 0
    expect_equal(dim(simp$data), c(5, 3))
    # The total distance described by the graph is unchanged
    expect_equal(sum(simp$data$dist), sum(edges$dist))
    # The coordinates for X contain 4, 5, 6, 7, 8, 0
    expect_equal(sum(simp$coords$X), 30)
})

test_that("simplify 4 to 0 and remove loop", {
    simp <- cpp_simplify(graph, rm_loop = TRUE)
    expected <- data.frame(from = c(0), to = c(1), dist = c(4))
    expect_equal(simp$data, expected)
})

test_that("simplify 4 to 0 but retain 2 and remove loop", {
    simp <- cpp_simplify(graph, keep = LETTERS[2 + 1], rm_loop = TRUE)
    # Two nodes are retained in simplifed graph
    expect_equal(dim(simp$data), c(2, 3))
    # The distance is four (the distance from four to zero
    expect_equal(sum(simp$data$dist), 4)
})
