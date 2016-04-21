context('estfun.glmerMod')

test_that('function returns n x p matrix',
          {
            data("ohio", package = 'geepack')
            gm <- lme4::glmer(resp ~ age + smoke + (1 | id),
                               data = ohio, family = binomial)
            n <- length(unique(lme4::getME(gm, 'flist')[[1]]))
            p <- length(unlist(lme4::getME(gm, c('beta', 'theta'))))
            ee <- estfun(gm, grad_method = 'simple')

            expect_equal(dim(ee), c(n, p))
          })

test_that('function returns correct values (to 6th decimal) for logistic model',
          {
            data("vaccinesim", package = "inferference")
            gm <- lme4::glmer(A ~ X1 + (1|group), family = binomial, data = vaccinesim)
            ee_tail <- tail(estfun(gm, grad_method = 'simple'))
            compare <- matrix(c(-1.2159647, -6.467456,  0.1849089,
                                -0.6187342, -2.859741, -0.3726988,
                                 0.3742982,  4.421626, -0.7586125,
                                -0.2187377,  1.817043, -0.8955525,
                                -1.4486301, -4.367776,  0.6811228,
                                 0.2794834,  5.581020, -0.6629418),
                              ncol = 3, byrow = T)
            expect_equal(ee_tail, compare, tolerance = 1e-6,
                         check.attributes = FALSE)
          })

