context('estfun.geeglm')

test_that('function returns n x p matrix',
          {
            data("ohio", package = 'geepack')
            gm <- geepack::geeglm(resp ~ age + smoke + age:smoke, id=id, data=ohio,
                             family=binomial, corstr="independence", scale.fix= F)
            n <- length(gm$geese$clusz)
            p <- length(coef(gm))
            ee <- estfun(gm)

            expect_equal(dim(ee), c(n, p))
          })
