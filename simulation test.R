#data generation for simulation
library(mediation)

#Test 1
n = 5000
x = runif(n)
x = ifelse(x < 0.5, 0, 1)

m2 = 0.25*x + rnorm(n, 0, 0.1)
m1 = 0.4*x + 0.5*m2 + rnorm(n, 0, 0.1)
y = 0.8*x + 0.25*m1 + 0.35*m2 + rnorm(n, 0, 0.1)

simdata <- data.frame(x, m1, m2,y)
ff <- medi("y",c("m1", "m2"),"x", c = NULL, ymodel = "regression", mmodel = c("regression","regression"),incint = NULL, inc_mmint = TRUE, data = simdata, sim = 1000)
ff <- medi("y",c("m2", "m1"),"x", c = NULL, ymodel = "regression", mmodel = c("regression","regression"),incint = NULL, inc_mmint = TRUE, data = simdata, sim = 1000)

#Test 2
n = 5000
x = runif(n)
x = ifelse(x < 0.5, 0, 1)

m2 = 0.25*x + rnorm(n, 0, 0.1)
m1 = 0.4*x + 0.5*m2 + rnorm(n, 0, 0.1)
y = 0.8*x + 0.25*m1 + 0.35*m2 + 0.3*m1*m2 +rnorm(n, 0, 0.1)

simdata <- data.frame(x, m1, m2,y)
ff <- medi("y",c("m1", "m2"),"x", c = NULL, ymodel = "regression", mmodel = c("regression","regression"),incint = NULL, inc_mmint = TRUE, data = simdata, sim = 1000)
ff <- medi("y",c("m2", "m1"),"x", c = NULL, ymodel = "regression", mmodel = c("regression","regression"),incint = NULL, inc_mmint = TRUE, data = simdata, sim = 1000)

ff <- medi("y",c("m1", "m2"),"x", c = NULL, ymodel = "regression", mmodel = c("regression","regression"),incint = NULL, inc_mmint = FALSE, data = simdata, sim = 1000)
ff <- medi("y",c("m2", "m1"),"x", c = NULL, ymodel = "regression", mmodel = c("regression","regression"),incint = NULL, inc_mmint = FALSE, data = simdata, sim = 1000)

#Test 3
n = 5000
x = runif(n)
x = ifelse(x < 0.5, 0, 1)

m2 = 0.25*x + rnorm(n, 0, 0.1)
m2 = ifelse(m2 < 0.2, 0, 1)
m2cat = as.factor(m2)
m1 = 0.4*x + 0.5*m2 + rnorm(n, 0, 0.3)
y = 0.8*x + 0.25*m1 + 0.35*m2 + 0.3*m1*m2 +rnorm(n, 0, 0.1)
simdata <- data.frame(x, m1, m2cat,y, m2)

y = "y"
med =c("m1","m2cat")
treat = "x"
mod = NULL
c = NULL
moc = NULL
ymodel = "regression"
mmodel = c("regression","logistic regression")
incint = NULL
inc_mmint = TRUE
data = simdata
sim = 1000
treat_lv = 1
control_lv = 0
out_scale = "difference"
complete_analysis = FALSE
conf.level = 0.95
digits = 2

ff <- mediate(y = "y", med = c("m1", "m2cat"), treat = "x", c = NULL, ymodel = "regression", mmodel = c("regression","logistic regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = TRUE, data = simdata, sim = 1000)
ff <- medi(y = "y", med = c("m1", "m2cat"), treat = "x", c = NULL, ymodel = "regression", mmodel = c("regression","logistic regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = TRUE, data = simdata, sim = 1000)
ff <- medi(y = "y", med = c("m1", "m2"), treat = "x", c = NULL, ymodel = "regression", mmodel = c("regression","regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = TRUE, data = simdata, sim = 1000)
ff <- medi(y = "y", med = c("m2cat", "m1"), treat = "x", c = NULL, ymodel = "regression", mmodel = c("logistic regression","regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = TRUE, data = simdata, sim = 1000)
ff$ymodel
ff$m1_model
ff$m2_model_cond
ff$m2_model_marg

#Test 4
n = 5000
x = runif(n)
x = ifelse(x < 0.5, 0, 1)

m2 = 0.25*x + rnorm(n, 0, 0.1)
m2 = ifelse(m2 < 0.2, 0, 1)
m2cat = as.factor(m2)
m1 = 0.4*x + 0.5*m2 + rnorm(n, 0, 0.3)
m1 = ifelse(m1 < 0.3, 0, 1)
m1cat = as.factor(m1)
y = 0.8*x + 0.25*m1 + 0.35*m2 + 0.3*m1*m2 +rnorm(n, 0, 0.1)
simdata <- data.frame(x, m1, m2cat,y, m2, m1cat)

ff <- medi(y = "y", med = c("m1cat", "m2cat"), treat = "x", c = NULL, ymodel = "regression", mmodel = c("logistic regression","logistic regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = TRUE, data = simdata, sim = 1000)
ff <- medi(y = "y", med = c("m1", "m2"), treat = "x", c = NULL, ymodel = "regression", mmodel = c("regression","regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = TRUE, data = simdata, sim = 1000)
ff <- medi(y = "y", med = c("m1", "m2cat"), treat = "x", c = NULL, ymodel = "regression", mmodel = c("regression","logistic regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = TRUE, data = simdata, sim = 1000)

#Test 5
#This seems problematic - need to check how the probabilities are computed.
#The method is ok - The data is problematic!
n = 5000
x = runif(n)
x = ifelse(x < 0.5, 0, 1)

m2 = 0.25*x + rnorm(n, 0, 0.1)
m2 = ifelse(m2 < 0.2, 0, 1)
m2cat = as.factor(m2)
m1 = 0.4*x + 0.5*m2 + rnorm(n, 0, 0.3)
m1 = ifelse(m1 < 0.3, 0, 1)
m1cat = as.factor(m1)
y = 0.8*x + 0.25*m1 + 0.35*m2 + 0.3*m1*m2 +rnorm(n, 0, 0.1)
y = ifelse(y < 1, 0, 1)
ycat = as.factor(y)
simdata <- data.frame(x, m1, m2cat,y, m2, m1cat, ycat)
ff <- medi(y = "ycat", med = c("m1cat", "m2cat"), treat = "x", c = NULL, ymodel = "logistic regression", mmodel = c("logistic regression","logistic regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = TRUE, data = simdata, sim = 1000, out_scale = "difference")
ff <- medi(y = "y", med = c("m1cat", "m2cat"), treat = "x", c = NULL, ymodel = "regression", mmodel = c("logistic regression","logistic regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = TRUE, data = simdata, sim = 1000, out_scale = "difference")

#Test 6
#Test 1 mediator
n = 5000
x = runif(n)
x = ifelse(x < 0.5, 0, 1)


m1 = x + rnorm(n, 0, 3)
m1 = ifelse(m1 < 0.3, 0, 1)
m1cat = as.factor(m1)
y = 0.8*x + m1 + rnorm(n, 0, 3)
y = ifelse(y < 1, 0, 1)
ycat = as.factor(y)
simdata <- data.frame(x, m1, y, ycat, m1cat)
ff <- medi(y = "ycat", med = c("m1"), treat = "x", c = NULL, ymodel = "logistic regression", mmodel = c("regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = TRUE, data = simdata, sim = 1000, out_scale = "difference")
ff <- medi(y = "y", med = c("m1"), treat = "x", c = NULL, ymodel = "regression", mmodel = c("regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = TRUE, data = simdata, sim = 1000, out_scale = "difference")
ff <- medi(y = "ycat", med = c("m1cat"), treat = "x", c = NULL, ymodel = "logistic regression", mmodel = c("logistic regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = TRUE, data = simdata, sim = 1000, out_scale = "difference")

yres = glm(y ~ m1 + x)
m1res = glm(m1 ~ x)
mediate_package_res <- mediate(m1res, yres, sims = 1000, treat = "x", mediator = "m1")
summary(mediate_package_res)

yres = glm(y ~ m1 + x, family = "binomial")
m1res = glm(m1 ~ x, family = "binomial")
mediate_package_res <- mediate(m1res, yres, sims = 1000, treat = "x", mediator = "m1")
summary(mediate_package_res)

#Test 7
#test 3 mediators, no relationship between Ms
n = 5000
x = runif(n)
x = ifelse(x < 0.5, 0, 1)

m1 = 0.2*x + rnorm(n, 0, 0.5)
m2 = 0.3*x + rnorm(n, 0, 0.5)
m3 = 0.4*x + rnorm(n, 0, 0.5)

y = 0.8*x + 0.5*m1 + 0.6*m2 + 0.7*m3 + rnorm(n, 0, 0.5)

simdata <- data.frame(x,m1,m2,m3,y)
ff <- medi(y = "y", med = c("m1","m2","m3"), treat = "x", c = NULL, ymodel = "regression", mmodel = c("regression", "regression", "regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = simdata, sim = 5000, out_scale = "difference")

#test 8
#test 3 mediators
n = 5000
x = runif(n)
x = ifelse(x < 0.5, 0, 1)

m1 = 0.2*x + rnorm(n, 0, 0.5)
m2 = 0.3*x + 0.5*m1 + rnorm(n, 0, 0.5)
m3 = 0.4*x + rnorm(n, 0, 0.5)

y = 0.9*x + 0.6*m1 + 0.7*m2 + 0.8*m3 + rnorm(n, 0, 0.5) + 2

simdata <- data.frame(x,m1,m2,m3,y)
ff <- medi(y = "y", med = c("m1","m2","m3"), treat = "x", c = NULL, ymodel = "regression", mmodel = c("regression", "regression", "regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = simdata, sim = 5000, out_scale = "difference")

#test 9
#test 3 mediators
n = 10000
x = runif(n)
x = ifelse(x < 0.5, 0, 1)

m1 = 0.2*x + rnorm(n, 0, 0.2)
m2 = 0.3*x + 0.9*m1 + rnorm(n, 0, 0.2)
m3 = 0.4*x + m1 + rnorm(n, 0, 0.2)

y = 0.8*x + 0.5*m1 + 0.6*m2 + 0.7*m3 + rnorm(n, 0, 0.2) + 2

simdata <- data.frame(x,m1,m2,m3,y)
ff <- medi(y = "y", med = c("m3","m1","m2"), treat = "x", c = NULL, ymodel = "regression", mmodel = c("regression", "regression", "regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = simdata, sim = 5000, out_scale = "difference")


#test 10
#test 3 mediators
n = 10000
x = runif(n)
x = ifelse(x < 0.5, 0, 1)

c = rnorm(n,0,0.1)

m1 = 0.2*x + rnorm(n, 0, 0.1) + 0.1*c
m2 = 0.3*x + 0.9*m1 + rnorm(n, 0, 0.1) + 0.2*c
m3 = 0.4*x + m1 + 1.1*m2 + rnorm(n, 0, 0.1) + 0.3*c

y = 0.8*x + 0.5*m1 + 0.6*m2 + 0.7*m3 + rnorm(n, 0, 0.1) + 2 + 0.4*c

simdata <- data.frame(x,m1,m2,m3,y,c)
ff <- medi(y = "y", med = c("m3","m1","m2"), treat = "x", c = "c", ymodel = "regression", mmodel = c("regression", "regression", "regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = simdata, sim = 5000, out_scale = "difference")

summary(ff$ymodel)


#test 11
#3 mediators with binary outcome
#using real dataset

ff <- medi(y = "heavyalcohol3", med = c("alcohol","peer","conflict"), treat = "supervision", c = NULL, ymodel = "logistic regression", mmodel = c("regression", "regression", "regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = example_data, sim = 1000, out_scale = "difference")
ff <- medi(y = "heavyalcohol4", med = c("alcohol","peer","conflict"), treat = "supervision", c = NULL, ymodel = "regression", mmodel = c("regression", "regression", "regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = example_data, sim = 1000, out_scale = "difference")

y = "heavyalcohol3"
med =c("alcohol","peer2","conflict")
treat = "supervision"
mod = NULL
c = NULL
moc = NULL
ymodel = "logistic regression"
mmodel = c("regression","logistic regression", "regression")
incint = FALSE
inc_mmint = FALSE
data = example_data
sim = 2
treat_lv = 1
control_lv = 0

#test 12
#1/2 mediators with binary outcome
ff <- medi(y = "heavyalcohol3", med = c("alcohol","conflict", "peer2"), treat = "supervision", c = NULL, ymodel = "logistic regression", mmodel = c("regression", "regression", "logistic regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = example_data, sim = 1000, out_scale = "difference")
ff <- medi(y = "heavyalcohol3", med = c("alcohol"), treat = "supervision", c = c("peer2","conflict"), ymodel = "logistic regression", mmodel = c("regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = example_data, sim = 1000, out_scale = "difference")

#test 13
#comparing with the mediation package
ff <- medi(y = "heavyalcohol3", med = c("peer2_factor"), treat = "supervision", c = c("alcohol", "conflict"), ymodel = "logistic regression", mmodel = c("logistic regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = example_data, sim = 1000, out_scale = "difference")
yres =  glm(heavyalcohol3 ~ peer2 + supervision + alcohol + conflict, family = "binomial", data = example_data)
m1res = glm(peer2 ~ supervision + alcohol + conflict, family = "binomial", data = example_data)
mediate_package_res <- mediate(m1res, yres, sims = 1000, treat = "supervision", mediator = "peer2")
summary(mediate_package_res)

#test 14
#testing Poisson mediator
ff <- medi(y = "heavyalcohol", med = c("peer"), treat = "supervision", c = "alcohol", ymodel = "regression", mmodel = c("poisson regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = example_data, sim = 5000, out_scale = "difference")
ff <- medi(y = "heavyalcohol", med = c("peer"), treat = "supervision", c = "alcohol", ymodel = "regression", mmodel = c("regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = example_data, sim = 5000, out_scale = "difference")

yres = glm(heavyalcohol ~ peer + supervision + alcohol, data = example_data)
m1res = glm(peer ~ supervision + alcohol, family = "poisson", data = example_data)
mediate_package_res <- mediate(m1res, yres, sims = 1000, treat = "supervision", mediator = "peer")
summary(mediate_package_res)

#test 15
ff <- medi(y = "heavyalcohol", med = c("peer"), treat = "supervision", c = "alcohol", ymodel = "poisson regression", mmodel = c("poisson regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = example_data, sim = 5000, out_scale = "difference")
ff <- medi(y = "heavyalcohol", med = c("peer"), treat = "supervision", c = "alcohol", ymodel = "regression", mmodel = c("regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = example_data, sim = 5000, out_scale = "difference")

yres = glm(heavyalcohol ~ peer + supervision + alcohol, family = "poisson", data = example_data)
m1res = glm(peer ~ supervision + alcohol, family = "poisson", data = example_data)
mediate_package_res <- mediate(m1res, yres, sims = 1000, treat = "supervision", mediator = "peer")
summary(mediate_package_res)

#test 16 - this is used in the paper

n = 1000
x = runif(n)
x = ifelse(x < 0.5, 0, 1)

m = 0.1 + 0.2*x + rnorm(n, 0, 1)
y = 0.3 + 0.4*x + 0.5*m + +0.6*x*m + rnorm(n, 0, 1)
summary(lm(y ~ x * m))


df <- data.frame(x,m,y)
sim_data <- df
save(sim_data, file = "sim_data.rda")
write.csv(sim_data, file = "sim_data.csv")

ptm <-proc.time()
ff <- intmed::mediate(y = "y", med = c("m"), treat = "x", ymodel = "regression", mmodel = c("regression"), treat_lv = 1, control_lv = 0, incint = TRUE, inc_mmint = FALSE, data = sim_data, sim = 10000, out_scale = "difference", digits = 3)
proc.time() - ptm

ptm <- proc.time()
yres = ff$individual$ymodel
m1res = ff$individual$m1_model
ff3 <- mediation::mediate(m1res, yres, sims = 10000, treat = "x", mediator = "m")
summary(ff3)
proc.time() - ptm

ff <- intmed::mediate(y = "y", med = c("m"), treat = "x", ymodel = "regression", mmodel = c("regression"), treat_lv = 1, control_lv = 0, incint = TRUE, inc_mmint = FALSE, data = df, sim = 10000, out_scale = "difference", digits = 3)
yres = ff$individual$ymodel
m1res = ff$individual$m1_model
ff3 <- mediation::mediate(m1res, yres, sims = 10000, treat = "x", mediator = "m")
summary(ff3)
