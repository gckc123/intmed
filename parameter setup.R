
setwd("C:/Users/Gary/Documents/My programs/mediation/mediation simulation project/mediation/R")
setwd("C:/Users/gckc1/Documents/My programs/mediation/mediation simulation project/mediation/R")
example_data <- read.csv("example data 2.csv")
example_data$c1 <- runif(length(example_data$heavyalcohol))
example_data$moc1 <- runif(length(example_data$heavyalcohol))
example_data$mod1 <- runif(length(example_data$heavyalcohol))
example_data$mod2 <- runif(length(example_data$heavyalcohol))
example_data$cat1 <- runif(length(example_data$heavyalcohol))
example_data$cat1[example_data$cat1 >= 0.8] = "KAKA"
example_data$cat1[example_data$cat1 < 0.8 & example_data$cat1 >= 0.5] = "HAHA"
example_data$cat1[example_data$cat1 < 0.5] = "LALA"

example_data$cat2 <- runif(length(example_data$heavyalcohol))
example_data$cat2[example_data$cat2 >= 0.7] = "MAMA"
example_data$cat2[example_data$cat2 < 0.7 & example_data$cat2 >= 0.5] = "WAWA"
example_data$cat2[example_data$cat2 < 0.5] = "BABA"

example_data$alcohol2 = as.factor(example_data$alcohol)
example_data$heavyalcohol2 = ifelse(example_data$heavyalcohol < 4, 0, 1)
example_data$heavyalcohol2 = as.factor(example_data$heavyalcohol2)

example_data$heavyalcohol3 = ifelse(example_data$heavyalcohol < 4, 0, 1)
example_data$heavyalcohol3 = as.factor(example_data$heavyalcohol3)

example_data$heavyalcohol4 = ifelse(example_data$heavyalcohol < 4, 0, 1)

example_data$supervision_2cat = ifelse(example_data$supervision < 1.5, 0, 1)
example_data$supervision_2cat = as.factor(example_data$supervision_2cat)

example_data$peer2 = ifelse(example_data$peer < 3, 0,1)
example_data$peer2_factor = as.factor(example_data$peer2)



write.csv(example_data, "example data stata.csv")

y = "heavyalcohol"
med = c("alcohol")
treat = "supervision"
mod = NULL
c = "conflict"
moc = NULL
ymodel = "regression"
mmodel = "regression"
incint = TRUE
data = example_data
sim = 1000
treat_lv = 1
control_lv = 0
conf.level = 0.95

#Y11-Y00 != Y1 - Y0 when there is mediator-treatment interaction??
ff <- medi("heavyalcohol",c("alcohol"),"supervision", c = "conflict", ymodel = "regression", mmodel = "regression",incint = FALSE, data = example_data, sim = 1000)
ff <- medi("heavyalcohol",c("alcohol"),"supervision", c = "conflict", ymodel = "regression", mmodel = "regression",incint = TRUE, data = example_data, sim = 1000)

ff$total2 <- sort(ff$total2)
mean(ff$total2)
ff$total2[25]
ff$total2[975]

yres = glm(heavyalcohol ~ alcohol + supervision + conflict, data = example_data)
m1res = glm(alcohol ~ supervision + conflict, data = example_data)
mediate_package_res <- mediate(m1res, yres, sims = 1000, treat = "supervision", mediator = "alcohol")
summary(mediate_package_res)

ff <- medi("heavyalcohol3", c("alcohol"), "supervision", c = "conflict", ymodel = "logistic regression", mmodel = "regression", incint = FALSE, data = example_data, sim = 1000, out_scale = "difference")
ff <- medi("heavyalcohol3", c("alcohol"), "supervision", c = "conflict", ymodel = "logistic regression", mmodel = "regression", incint = FALSE, data = example_data, sim = 1000, out_scale = "ratio")
ff <- medi("heavyalcohol3", c("alcohol2"), "supervision", c = "conflict", ymodel = "logistic regression", mmodel = "logistic regression", incint = FALSE, data = example_data, sim = 1000, out_scale = "difference")
ff <- medi("heavyalcohol3", c("alcohol2"), "supervision_2cat", ymodel = "logistic regression", mmodel = "logistic regression", incint = FALSE, data = example_data, sim = 1000, out_scale = "difference")

yres = glm(heavyalcohol3 ~ alcohol + supervision + conflict, data = example_data, family = "binomial")
m1res = glm(alcohol ~ supervision + conflict, data = example_data)
mediate_package_res <- mediate(m1res, yres, sims = 1000, treat = "supervision", mediator = "alcohol")
summary(mediate_package_res)

ff <- medi("heavyalcohol",c("alcohol2"),"supervision", c = "conflict", ymodel = "regression", mmodel = "logistic regression",incint = FALSE, data = example_data, sim = 1000)
ff <- medi("heavyalcohol",c("alcohol2"),"supervision", ymodel = "regression", mmodel = "logistic regression",incint = FALSE, data = example_data, sim = 10000)
ff <- medi("heavyalcohol",c("alcohol2"),"supervision", ymodel = "regression", mmodel = "logistic regression",incint = FALSE, data = example_data, sim = 1000)
ff <- medi("heavyalcohol",c("alcohol","peer"),"supervision", ymodel = "regression", mmodel = c("regression","regression"),incint = FALSE, data = example_data, sim = 1000)

ff <- medi("heavyalcohol",c("alcohol2","peer"),"supervision", ymodel = "regression", mmodel = c("logistic regression","regression"),incint = FALSE, data = example_data, sim = 1000)
ff <- medi("heavyalcohol",c("alcohol2","peer"),"supervision", ymodel = "regression", mmodel = c("logistic regression","regression"),incint = FALSE, data = example_data, sim = 1000, out_scale ="ratio")

ff <- medi("heavyalcohol3",c("alcohol2"),"supervision", c = "conflict", ymodel = "logistic regression", mmodel = "logistic regression",incint = FALSE, data = example_data, sim = 1000)
ff <- medi("heavyalcohol3",c("alcohol2"),"supervision", c = "conflict", ymodel = "logistic regression", mmodel = "logistic regression",incint = FALSE, data = example_data, sim = 1000, out_scale = "ratio")




yres = glm(heavyalcohol3 ~ alcohol + supervision, family = "binomial", data = example_data)
m1res = glm(alcohol ~ supervision, family = "binomial", data = example_data)
mediate_package_res <- mediate(m1res, yres, sims = 1000, treat = "supervision", mediator = "alcohol")
summary(mediate_package_res)


yres = glm(heavyalcohol4 ~ alcohol + supervision, data = example_data)
m1res = glm(alcohol ~ supervision, family = "binomial", data = example_data)
mediate_package_res <- mediate(m1res, yres, sims = 1000, treat = "supervision", mediator = "alcohol")
summary(mediate_package_res)

ff <- medi("heavyalcohol4", c("alcohol2"), "supervision", ymodel = "regression", mmodel = "logistic regression", incint = FALSE, data = example_data, sim = 1000, out_scale = "difference")
ff <- medi("heavyalcohol3", c("alcohol2"), "supervision", ymodel = "logistic regression", mmodel = "logistic regression", incint = FALSE, data = example_data, sim = 1000, out_scale = "difference")
ff <- medi("heavyalcohol4", c("alcohol"), "supervision", ymodel = "regression", mmodel = "regression", incint = FALSE, data = example_data, sim = 1000, out_scale = "difference")

yres = glm(heavyalcohol4 ~ alcohol + supervision, family = "binomial", data = example_data)
m1res = glm(alcohol ~ supervision, family = "binomial", data = example_data)
mediate_package_res <- mediate(m1res, yres, sims = 1000, treat = "supervision", mediator = "alcohol")
summary(mediate_package_res)

yres = glm(heavyalcohol4 ~ alcohol + supervision, data = example_data)
m1res = glm(alcohol ~ supervision, data = example_data)
mediate_package_res <- mediate(m1res, yres, sims = 1000, treat = "supervision", mediator = "alcohol")
summary(mediate_package_res)

#############################################################################################################################################################################################

#Two mediators
y = "heavyalcohol"
med =c("alcohol","peer")
treat = "supervision"
mod = NULL
c = "conflict"
moc = NULL
ymodel = "regression"
mmodel = c("regression","regression")
incint = FALSE
inc_mmint = TRUE
data = example_data
sim = 1000
treat_lv = 1
control_lv = 0

ff <- medi("heavyalcohol",c("alcohol", "peer"),"supervision", c = "conflict", ymodel = "regression", mmodel = c("regression","regression"),incint = NULL, data = example_data, sim = 1000)

y = "heavyalcohol3"
med =c("alcohol2","conflict")
treat = "supervision"
mod = NULL
c = NULL
moc = NULL
ymodel = "logistic regression"
mmodel = c("logistic regression","regression")
incint = FALSE
inc_mmint = FALSE
data = example_data
sim = 2
treat_lv = 1
control_lv = 0
out_scale = "difference"

out_scale = "ratio"
model = ymodel

ys <- data.frame(y_000_cond, y_100_cond, y_110_marg, y_100_marg, y_101_marg, y_111_cond, y_111_marg, y0, y1)

ff <- medi("heavyalcohol3",c("alcohol2", "peer2"), "supervision", c = "conflict", ymodel = "logistic regression", mmodel = c("logistic regression","logistic regression"),incint = FALSE, inc_mmint = TRUE, data = example_data, sim = 1000, out_scale = "ratio")

#################

y = "heavyalcohol3"
med =c("alcohol2","peer2","conflict")
treat = "supervision"
mod = NULL
c = NULL
moc = NULL
ymodel = "logistic regression"
mmodel = c("logistic regression","logistic regression", "regression")
incint = FALSE
inc_mmint = FALSE
data = example_data
sim = 2
treat_lv = 1
control_lv = 0

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

y = "y"
med = c("m1","m2","m3")
treat = "x"
mod = NULL
c = NULL
moc = NULL
ymodel = "regression"
mmodel = c("regression", "regression", "regression")
incint = FALSE
inc_mmint = FALSE
data = simdata
sim = 2
treat_lv = 1
control_lv = 0

y = "heavyalcohol3"
med =c("alcohol2","conflict")
treat = "supervision"
mod = NULL
c = "peer2"
moc = NULL
ymodel = "logistic regression"
mmodel = c("logistic regression","regression")
incint = FALSE
inc_mmint = FALSE
data = example_data
sim = 2
treat_lv = 1
control_lv = 0
out_scale = "difference"




ff<- medi(y = y, med = med, treat = treat, c = NULL, ymodel = "poisson regression", mmodel = c("regression", "regression"), treat_lv = treat_lv, control_lv = control_lv, incint = incint, inc_mmint = inc_mmint, data = example_data, sim = sim, conf.level = 0.95, out_scale = out_scale)
ff<- medi(y = y, med = med, treat = treat, mod = mod, c = c, ymodel = ymodel, mmodel = mmodel, treat_lv = treat_lv, control_lv = control_lv, incint = incint, inc_mmint = inc_mmint, data = example_data, sim = sim, conf.level = conf.level, out_scale = out_scale)

y = "heavyalcohol"
med =c("peer","alcohol","conflict")
treat = "supervision"
mod = NULL
c = NULL
moc = NULL
ymodel = "poisson regression"
mmodel = c("regression","regression","regression")
incint = NULL
inc_mmint = FALSE
data = alcohol
sim = 1000
treat_lv = 1
control_lv = 0
out_scale = "difference"
complete_analysis = FALSE
conf.level = 0.95
digits = 2


ff <- mediate(y = "heavyalcohol", med = c("peer","alcohol"), treat = "supervision", c = "conflict", ymodel = "poisson regression", mmodel = c("poisson regression","regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = alcohol, sim = 1000, out_scale = "difference")

#parallel computing vs old mediaiton package
example_data <- tibble::add_column(example_data, missing = rowSums(sapply(example_data, is.na)))
example_data <- data[example_data$missing == 0, 1:length(example_data)-1]

ptm <- proc.time()
ff <- medi(y = "heavyalcohol", med = c("peer"), treat = "supervision", c = NULL, ymodel = "regression", mmodel = c("regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = alcohol, sim = 2000, out_scale = "difference")
proc.time() - ptm

ptm <- proc.time()
mediate_package_res <- mediation::mediate(ff$m1_model, ff$ymodel, sims = 2000, treat = "supervision", mediator = "peer")
summary(mediate_package_res)
proc.time() - ptm

ptm <- proc.time()
ff <- medi(y = "heavyalcohol", med = c("peer", "alcohol2"), treat = "supervision", c = NULL, ymodel = "poisson regression", mmodel = c("regression", "logistic regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = example_data, sim = 10000, out_scale = "difference")
proc.time() - ptm


ff <- medi(y = "heavyalcohol", med = c("peer"), treat = "supervision", c = "alcohol", ymodel = "poisson regression", mmodel = c("poisson regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = alcohol, sim = 2000, out_scale = "difference")
mediate_package_res <- mediation::mediate(ff$m1_model, ff$ymodel, sims = 2000, treat = "supervision", mediator = "peer")
summary(mediate_package_res)

ff <- medi(y = "heavyalcohol", med = c("peer", "conflict"), treat = "supervision", c = "alcohol", ymodel = "regression", mmodel = c("regression","regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = alcohol, sim = 5000, out_scale = "difference")
mediate_package_res <- mediation::mediate(ff$m1_model, ff$ymodel, sims = 5000, treat = "supervision", mediator = "peer")
summary(mediate_package_res)
