context("Testing the mediate function")

set.seed(1234)


###################################################
# sim_data example - 1 mediator
###################################################

data("sim_data", package = "intmed")
tol = 0.002
tol2 = 0.003
med_res <- mediate(y = "y", med = c("m"), treat = "x", ymodel = "regression", mmodel = c("regression"), treat_lv = 1, control_lv = 0, incint = TRUE, inc_mmint = FALSE, data = sim_data, sim = 5000, digits = 5)

test_that("Testing direct effect using sim_data", {
  expect_equal(mean(med_res$combined$direct), 0.523, tol)
})
test_that("Testing indirect effect using sim_data", {
  expect_equal(mean(med_res$combined$indirect[[1]]), 0.207, tol)
})
test_that("Testing total effect using sim_data", {
  expect_equal(mean(med_res$combined$total), 0.730, tol)
})
test_that("Testing proportion mediated using sim_data", {
  expect_equal(median(med_res$combined$prop[[1]]), 0.283, tol2)
})

###################################################
# substance example - 2 mediator
###################################################
tol = 0.002
tol2 = 0.003

data("substance", package = "intmed")
med_res <- intmed::mediate(y = "sub_misuse", med = c("dev_peer","sub_exp"), treat = "fam_int", c = c("conflict","gender"), ymodel = "logistic regression", mmodel = c("logistic regression","logistic regression"), treat_lv = 1, control_lv = 0, data = substance, sim = 1000, digits = 5)

test_that("Testing direct effect using substance data", {
  expect_equal(mean(med_res$combined$direct), -0.05318 , tol)
})
test_that("Testing indirect effect through mediator 1 (dev_peer)", {
  expect_equal(mean(med_res$combined$indirect[[1]]), -0.01811, tol)
})
test_that("Testing indirect effect through mediator 2 (sub_exp)", {
  expect_equal(mean(med_res$combined$indirect[[2]]), -0.00622, tol)
})
test_that("Testing total effect", {
  expect_equal(mean(med_res$combined$total), -0.07670, tol)
})
test_that("Testing proportion mediated through mediator 1 (dev_peer)", {
  expect_equal(median(med_res$combined$prop[[1]]), 0.22564, tol2)
})
test_that("Testing proportion mediated through mediator 2 (sub_exp)", {
  expect_equal(median(med_res$combined$prop[[2]]), 0.07174, tol2)
})

###################################################
# simulating data - 3 mediator
###################################################

tol = 0.025

n = 10000
x = runif(n)
x = ifelse(x < 0.5, 0, 1)

c = rnorm(n,0,0.1)

m1 = 0.2*x + rnorm(n, 0, 0.1) + 0.1*c
m2 = 0.3*x + 0.9*m1 + rnorm(n, 0, 0.1) + 0.2*c
m3 = 0.4*x + m1 + 1.1*m2 + rnorm(n, 0, 0.1) + 0.3*c

y = 0.8*x + 0.5*m1 + 0.6*m2 + 0.7*m3 + rnorm(n, 0, 0.1) + 2 + 0.4*c

simdata <- data.frame(x,m1,m2,m3,y,c)
med_res <- mediate(y = "y", med = c("m1","m2","m3"), treat = "x", c = "c", ymodel = "regression", mmodel = c("regression", "regression", "regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = simdata, sim = 5000, digits = 5)

test_that("Testing direct effect using simulated data", {
  expect_equal(mean(med_res$combined$direct), 0.8, tol)
})
test_that("Testing indirect effect through M1", {
  expect_equal(mean(med_res$combined$indirect[[1]]), 0.1, tol)
})
test_that("Testing indirect effect through M2", {
  expect_equal(mean(med_res$combined$indirect[[2]]), 0.288, tol)
})
test_that("Testing indirect effect through M3", {
  expect_equal(mean(med_res$combined$indirect[[3]]), 0.7896, tol)
})
test_that("Testing the total effect", {
  expect_equal(mean(med_res$combined$total), 1.9776, tol)
})

###################################################
# simulating data with missing data - 3 mediator
###################################################

simdata$rand = runif(nrow(simdata),0,1)
simdata$y[simdata$rand < 0.1] <- NA
simdata$rand = runif(nrow(simdata),0,1)
simdata$x[simdata$rand < 0.05] <- NA
simdata$rand = runif(nrow(simdata),0,1)
simdata$m1[simdata$rand < 0.02] <- NA
simdata$rand = runif(nrow(simdata),0,1)
simdata$m2[simdata$rand < 0.01] <- NA
simdata$rand = runif(nrow(simdata),0,1)
simdata$m3[simdata$rand < 0.01] <- NA

med_res <- mediate(y = "y", med = c("m1","m2","m3"), treat = "x", c = "c", ymodel = "regression", mmodel = c("regression", "regression", "regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = simdata, sim = 1000, digits = 5)

test_that("Testing direct effect using simulated data", {
  expect_equal(mean(med_res$combined$direct), 0.8, tol)
})
test_that("Testing indirect effect through M1", {
  expect_equal(mean(med_res$combined$indirect[[1]]), 0.1, tol)
})
test_that("Testing indirect effect through M2", {
  expect_equal(mean(med_res$combined$indirect[[2]]), 0.288, tol)
})
test_that("Testing indirect effect through M3", {
  expect_equal(mean(med_res$combined$indirect[[3]]), 0.7896, tol)
})
test_that("Testing the total effect", {
  expect_equal(mean(med_res$combined$total), 1.9776, tol)
})
