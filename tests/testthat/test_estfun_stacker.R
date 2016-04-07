library(sandwich)
library(sandwichShop)
library(lme4)
library(geepack)
library(inferference)

context('estfun_stacker')

test_that('function returns n x p matrix',
          {
            data("vaccinesim", package = 'inferference')
            models = list(
              model_outcome = list(
                formula = A ~ X1 + (1|group),
                method  = 'glmer',
                options = list(family = binomial)
              ) ,
              model_treatment = list(
                formula = y ~ X1 + A,
                method  = 'geeglm',
                options = list(id = quote(group), family = binomial)
              ) )

            n <- 250
            p <- 6

            ee <- estfun_stacker(models, data = vaccinesim)

            expect_equal(dim(ee), c(n, p))
          })
