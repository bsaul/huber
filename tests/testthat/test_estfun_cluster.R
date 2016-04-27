library(sandwich)
library(sandwichShop)
library(inferference)

context('estfun_cluster')

test_that('function returns n x p matrix',
          {
            data("vaccinesim", package = 'inferference')
            m <- glm(y ~ A + X1, data = vaccinesim)

            ee <- estfun_cluster(m, vaccinesim$group)

            n <- 250
            p <- 3

            expect_equal(dim(ee), c(n, p))
          })
