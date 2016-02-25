context('estfun.glmerMod')

test_that('function returns n x p matrix',
          {
            gm1 <- lme4::glmer((height > 30) ~ I(age/10) + (1 | Seed),
                               data = Loblolly, family = binomial)
            n <- length(unique(lme4::getME(gm1, 'flist')[[1]]))
            p <- length(unlist(lme4::getME(gm1, c('beta', 'theta'))))
            ee <- estfun(gm1, grad.method = 'simple')

            expect_equal(dim(ee), c(n, p))
          })


