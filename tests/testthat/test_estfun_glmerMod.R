context('estfun.glmerMod')

test_that('function returns n x p matrix',
          {
            data("ohio", package = 'geepack')
            gm <- lme4::glmer(resp ~ age + smoke + (1 | id),
                               data = ohio, family = binomial)
            n <- length(unique(lme4::getME(gm, 'flist')[[1]]))
            p <- length(unlist(lme4::getME(gm, c('beta', 'theta'))))
            ee <- estfun(gm, grad_args = list(method = 'simple') )

            expect_equal(dim(ee), c(n, p))
          })


