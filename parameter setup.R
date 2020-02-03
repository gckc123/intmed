
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
mmodel = c("poisson regression","regression","regression")
incint = NULL
inc_mmint = FALSE
data = alcohol
sim = 1000
treat_lv = 1
control_lv = 0
out_scale = "difference"
complete_analysis = FALSE
conf.level = 0.95
digits = 3






ff <- mediate(y = "heavyalcohol", med = c("peer","alcohol","conflict"), treat = "supervision", c = "alcohol", ymodel = "regression", mmodel = c("regression","logistic regression","regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = TRUE, data = alcohol, sim = 1000, out_scale = "difference")

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

ff <- mediate(y = "heavyalcohol", med = c("peer","alcohol","conflict"), treat = "supervision",  ymodel = "poisson regression", mmodel = c("poisson regression","regression","regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = TRUE, data = alcohol, sim = 1000, out_scale = "difference", digits = 3)
ff <- mediate(y = "heavyalcohol", med = c("peer","alcohol","conflict"), treat = "supervision",  ymodel = "poisson regression", mmodel = c("poisson regression","regression","regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = alcohol, sim = 10000, out_scale = "difference", digits = 2)



ff <- mediate(y = "heavyalcohol", med = c("peer"), treat = "supervision",  ymodel = "poisson regression", c = c("conflict", "alcohol"), mmodel = c("poisson regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = alcohol, sim = 10000, out_scale = "difference", digits = 3)

alcohol1 <- tibble::add_column(alcohol, missing = rowSums(sapply(alcohol, is.na)))
alcohol1 <- alcohol1[alcohol1$missing == 0, 1:length(alcohol1)-1]

y = "heavyalcohol2"
med =c("peer2_factor")
treat = "alcohol2"
mod = NULL
c = "conflict"
moc = NULL
ymodel = "logistic regression"
mmodel = c("logistic regression")
incint = NULL
inc_mmint = FALSE
data = alcohol
sim = 1000
treat_lv = 1
control_lv = 0
out_scale = "difference"
complete_analysis = FALSE
conf.level = 0.95
digits = 3


ff <- intmed::mediate(y = "heavyalcohol2", med = c("peer"), treat = "alcohol2", c = "conflict", ymodel = "logistic regression", mmodel = c("regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = alcohol, sim = 10000, out_scale = "difference", digits = 3)
ff <- intmed::mediate(y = "heavyalcohol2", med = c("peer"), treat = "alcohol", c = "conflict", ymodel = "logistic regression", mmodel = c("regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = alcohol, sim = 10000, out_scale = "difference", digits = 3)
ff <- intmed::mediate(y = "heavyalcohol2", med = c("peer2_factor"), treat = "alcohol", c = "conflict", ymodel = "logistic regression", mmodel = c("logistic regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = alcohol, sim = 10000, out_scale = "difference", digits = 3)

yres <- glm(heavyalcohol2 ~ alcohol2+peer+conflict, family = "binomial", data = alcohol1)
m1res <- lm(peer ~ alcohol2+conflict, data = alcohol1)
mediate_package_res <- mediation::mediate(m1res, yres, sims = 10000, treat = "alcohol2", mediator = "peer", data = alcohol1)
summary(mediate_package_res)

ff <- intmed::mediate(y = "heavyalcohol", med = c("peer"), treat = "alcohol2", c = "conflict", ymodel = "poisson regression", mmodel = c("regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = alcohol1, sim = 10000, out_scale = "difference", digits = 3)

yres <- glm(heavyalcohol ~ alcohol2+peer+conflict, family = "poisson", data = alcohol1)
m1res <- lm(peer ~ alcohol2+conflict, data = alcohol1)
mediate_package_res <- mediation::mediate(m1res, yres, sims = 10000, treat = "alcohol2", mediator = "peer", data = alcohol1)
summary(mediate_package_res)

ff <- intmed::mediate(y = "heavyalcohol2", med = c("peer"), treat = "alcohol2", c = "conflict", ymodel = "logistic regression", mmodel = c("regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = alcohol, sim = 1000, out_scale = "ratio", digits = 3)

#start testing 8.1.2020
ff <- intmed::mediate(y = "heavyalcohol2", med = c("peer2_factor","supervision"), treat = "alcohol2", c = "conflict", ymodel = "logistic regression", mmodel = c("logistic regression","regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = alcohol1, sim = 1000, out_scale = "difference", digits = 3)

ff <- intmed::mediate(y = "heavyalcohol", med = c("peer2","supervision"), treat = "alcohol2", c = "conflict", ymodel = "regression", mmodel = c("regression","regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = alcohol, sim = 1000, out_scale = "difference", digits = 3)
ff <- intmed::mediate(y = "heavyalcohol", med = c("peer2","supervision"), treat = "alcohol2", c = "conflict", ymodel = "regression", mmodel = c("regression","regression"), treat_lv = 1, control_lv = 0, incint = TRUE, inc_mmint = FALSE, data = alcohol, sim = 1000, out_scale = "difference", digits = 3)
ff <- intmed::mediate(y = "heavyalcohol", med = c("peer2","supervision"), treat = "alcohol2", c = "conflict", ymodel = "regression", mmodel = c("regression","regression"), treat_lv = 1, control_lv = 0, incint = TRUE, inc_mmint = TRUE, data = alcohol, sim = 1000, out_scale = "difference", digits = 3)
ff <- intmed::mediate(y = "heavyalcohol", med = c("peer2","supervision"), treat = "alcohol2", c = "conflict", ymodel = "regression", mmodel = c("regression","regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = TRUE, data = alcohol, sim = 1000, out_scale = "difference", digits = 3)

ff <- intmed::mediate(y = "heavyalcohol", med = c("peer2","supervision"), treat = "alcohol2", c = "conflict", ymodel = "regression", mmodel = c("regression","regression"), treat_lv = "haha", control_lv = 0, incint = NULL, inc_mmint = TRUE, data = alcohol, sim = 1000, out_scale = "difference", digits = 3)

ff <- intmed::mediate(y = "heavyalcohol", med = c("peer2","supervision"), treat = "alcohol2", c = "conflict", ymodel = "poisson regression", mmodel = c("regression","regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = alcohol, sim = 1000, out_scale = "difference", digits = 2)

ff <- intmed::mediate(y = "heavyalcohol", med = c("peer2","supervision"), treat = "alcohol2", c = "conflict", ymodel = "poisson regression", mmodel = c("regression","regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = TRUE, data = alcohol, sim = 1000, out_scale = "difference", digits = 2)

ff <- intmed::mediate(y = "heavyalcohol", med = c("peer2","supervision"), treat = "alcohol2", c = "conflict", ymodel = "poisson regression", mmodel = c("regression","regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = alcohol, sim = 1000, out_scale = "difference", digits = 2)

ff <- intmed::mediate(y = "heavyalcohol", med = c("peer2","supervision"), treat = "alcohol2", c = "conflict", ymodel = "poisson regression", mmodel = c("regression","regression"), treat_lv = 1, control_lv = 0, incint = TRUE, inc_mmint = FALSE, data = alcohol, sim = 1000, out_scale = "difference", digits = 2)

ff <- intmed::mediate(y = "heavyalcohol", med = c("peer2","supervision"), treat = "alcohol2", c = c("conflict","depress"), ymodel = "poisson regression", mmodel = c("regression","regression"), treat_lv = 1, control_lv = 0, incint = TRUE, inc_mmint = FALSE, data = alcohol, sim = 1000, out_scale = "difference", digits = 2)

ff <- intmed::mediate(y = "heavyalcohol", med = c("peer2","supervision"), treat = "alcohol2", c = c("conflict","depress","cat1"), ymodel = "poisson regression", mmodel = c("regression","regression"), treat_lv = 1, control_lv = 0, incint = TRUE, inc_mmint = FALSE, data = alcohol, sim = 1000, out_scale = "difference", digits = 2)

ff <- intmed::mediate(y = "heavyalcohol", med = c("peer2","supervision"), treat = "alcohol2", c = c("conflict","depress","cat1"), ymodel = "regression", mmodel = c("regression","regression"), treat_lv = 1, control_lv = 0, incint = TRUE, inc_mmint = FALSE, data = alcohol, sim = 1000, out_scale = "difference", digits = 2)

ff <- intmed::mediate(y = "heavyalcohol", med = c("peer2","supervision"), treat = "alcohol2", c = c("conflict","depress","cat1"), ymodel = "regression", mmodel = c("regression","regression"), treat_lv = 1, control_lv = 0, incint = TRUE, inc_mmint = FALSE, data = alcohol, sim = 1000, out_scale = "difference", digits = 2)

ff <- intmed::mediate(y = "heavyalcohol", med = c("peer2","supervision"), treat = "alcohol2", c = c("conflict","depress"), ymodel = "poisson regression", mmodel = c("regression","regression"), treat_lv = 1, control_lv = 0, incint = TRUE, inc_mmint = TRUE, data = alcohol, sim = 1000, out_scale = "difference", digits = 5)

#may need to fix this - when the treatment variables have multiple categories. 8.1.2020
ff <- intmed::mediate(y = "heavyalcohol", med = c("peer2","supervision"), treat = "cat1", c = c("conflict","depress"), ymodel = "regression", mmodel = c("regression","regression"), treat_lv = "HAHA", control_lv = "LALA", incint = TRUE, inc_mmint = FALSE, data = alcohol, sim = 1000, out_scale = "difference", digits = 2)
#may need to implement a check input function 8.1.2020
ff <- intmed::mediate(y = "heavyalcohol", med = c("peer2","supervision"), treat = "alcohol2", c = "conflict", ymodel = "regression", mmodel = c("regression","regression"), treat_lv = "haha", control_lv = 0, incint = NULL, inc_mmint = TRUE, data = alcohol, sim = 1000, out_scale = "difference", digits = 3)

ff <- intmed::mediate(y = "heavyalcohol2", med = c("peer2","heavyalcohol"), treat = "alcohol2", c = "conflict", ymodel = "regression", mmodel = c("regression","regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = TRUE, data = alcohol, sim = 100, out_scale = "difference", digits = 3)

#comparing mediation package and intmed 10.1.2020
#results are consistent
alcohol1 <- tibble::add_column(alcohol, missing = rowSums(sapply(alcohol, is.na)))
alcohol1 <- alcohol1[alcohol1$missing == 0, 1:length(alcohol1)-1]

#Binary outcome
ff <- intmed::mediate(y = "heavyalcohol2", med = c("peer2_factor"), treat = "supervision", c = c("conflict","depress"), ymodel = "logistic regression", mmodel = c("logistic regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = alcohol1, sim = 50000, out_scale = "difference", digits = 5)

yres <- glm(heavyalcohol2 ~ peer2+supervision+conflict+depress, family = "binomial", data = alcohol1)
m1res <- glm(peer2 ~ supervision+conflict+depress, family = "binomial", data = alcohol1)
mediate_package_res <- mediation::mediate(m1res, yres, sims = 50000, treat = "supervision", mediator = "peer2", data = alcohol1)
summary(mediate_package_res)

#Count
ff <- intmed::mediate(y = "heavyalcohol", med = c("peer2_factor"), treat = "supervision", c = c("conflict","depress"), ymodel = "poisson regression", mmodel = c("logistic regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = alcohol1, sim = 50000, out_scale = "difference", digits = 5)

yres <- glm(heavyalcohol ~ peer2+supervision+conflict+depress, family = "poisson", data = alcohol1)
m1res <- glm(peer2 ~ supervision+conflict+depress, family = "binomial", data = alcohol1)
mediate_package_res <- mediation::mediate(m1res, yres, sims = 50000, treat = "supervision", mediator = "peer2", data = alcohol1)
summary(mediate_package_res)

#evaluating total effect from Y11-Y00 vs Y1-Y0
yres <- glm(heavyalcohol2 ~ peer2+supervision_2cat, family = "binomial", data = alcohol1)
m1res <- glm(peer2 ~ supervision_2cat, family = "binomial", data = alcohol1)
mediate_package_res <- mediation::mediate(m1res, yres, sims = 50000, treat = "supervision_2cat", mediator = "peer2", data = alcohol1)
summary(mediate_package_res)
yres_te <- glm(heavyalcohol2 ~ supervision_2cat, family = "binomial", data = alcohol1)
newdata = data.frame(supervision_2cat = c("0","1"))
predict(yres_te, newdata, type = "response")
newdata = data.frame(supervision_2cat = "1")

ff <- intmed::mediate(y = "heavyalcohol2", med = c("peer2_factor"), treat = "supervision", c = c("conflict","depress"), ymodel = "logistic regression", mmodel = c("logistic regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = alcohol, sim = 1000, out_scale = "difference", digits = 3)

yres <- glm(heavyalcohol2 ~ peer2+supervision+conflict+depress, family = "binomial", data = alcohol1)
m1res <- glm(peer2 ~ supervision+conflict+depress, family = "binomial", data = alcohol1)
mediate_package_res <- mediation::mediate(m1res, yres, sims = 50000, treat = "supervision", mediator = "peer2", data = alcohol1)
summary(mediate_package_res)
yres_te <- glm(heavyalcohol2 ~ supervision, family = "binomial", data = alcohol1)
newdata = data.frame(supervision = c(0,1))
predict(yres_te, newdata, type = "response")

#When do Y11-Y00 != Y1 - Y0??? Need to find out these conditions 10.1.2020

#need to check the below code - Just need to implement validation check for mediators 10.1.2020
ff <- intmed::mediate(y = "heavyalcohol2", med = "heavyalcohol5", treat = "supervision", c = "conflict", ymodel = "logistic regression", mmodel = c("poisson regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = alcohol1, sim = 1000, out_scale = "difference", digits = 3)
ff <- intmed::mediate(y = "heavyalcohol2", med = c("peer2"), treat = "supervision", c = c("conflict","depress"), ymodel = "logistic regression", mmodel = c("logistic regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = alcohol1, sim = 1000, out_scale = "difference", digits = 3)

ff <- intmed::medi(y = "heavyalcohol2", med = c("peer2_factor","supervision"), treat = "alcohol2", c = "conflict", ymodel = "logistic regression", mmodel = c("logistic regression","regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = alcohol1, sim = 1000, out_scale = "difference")

ff <- intmed::medi(y = "heavyalcohol2", med = c("peer2_factor","supervision"), treat = "alcohol2", c = "conflict", ymodel = "logistic regression", mmodel = c("logistic regression","regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = alcohol1, sim = 50000, out_scale = "difference")

fff <- intmed::mediate(y = "heavyalcohol2", med = c("peer2_factor","supervision"), treat = "alcohol2", c = "conflict", ymodel = "logistic regression", mmodel = c("logistic regression","regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = alcohol1, sim = 50000, out_scale = "difference", digits = 5)

#Need to catch this in validation 17.1.2020
ff <- intmed::mediate(y = "heavyalcohol2", med = c("peer2_factor","alcohol2"), treat = "supervision", c = "conflict", ymodel = "logistic regression", mmodel = c("logistic regression","regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = alcohol, sim = 1000, out_scale = "difference", digits = 3)


ff <- intmed::mediate(y = "heavyalcohol", med = c("peer2_factor"), treat = "supervision", c = c("conflict"), ymodel = "poisson regression", mmodel = c("logistic regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = alcohol1, sim = 10000, out_scale = "difference", digits = 3)
yres <- glm(heavyalcohol ~ peer2+supervision+conflict, family = "poisson", data = alcohol1)
m1res <- glm(peer2 ~ supervision+conflict+depress, family = "binomial", data = alcohol1)
mediate_package_res <- mediation::mediate(m1res, yres, sims = 10000, treat = "supervision", mediator = "peer2", data = alcohol1)
summary(mediate_package_res)

ff <- intmed::mediate(y = "heavyalcohol2", med = c("peer2_factor"), treat = "supervision_2cat", c = "conflict", ymodel = "logistic regression", mmodel = c("logistic regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = alcohol, sim = 1000, out_scale = "difference", digits = 3)

ff <- intmed::mediate(y = "heavyalcohol2", med = c("peer2_factor","alcohol2"), treat = "supervision_2cat", c = "conflict", ymodel = "logistic regression", mmodel = c("logistic regression","logistic regression"), treat_lv = 1, control_lv = 0, incint = NULL, inc_mmint = FALSE, data = alcohol1, sim = 1000, out_scale = "difference", digits = 3)

yres <- glm(heavyalcohol4 ~ peer2+supervision+conflict, family = "poisson", data = alcohol1)
m1res <- glm(peer2 ~ supervision+conflict, family = "binomial", data = alcohol1)
mediate_package_res <- mediation::mediate(m1res, yres, sims = 10000, treat = "supervision", mediator = "peer2", data = alcohol1)
summary(mediate_package_res)

ff <- intmed::mediate(y = "binge", med = c("peer_alc","alcohol_exp"), treat = "monitoring", c = c("conflict","gender"), ymodel = "logistic regression", mmodel = c("logistic regression","logistic regression"), treat_lv = 1, control_lv = 0, data = alcohol, sim = 1000, digits = 3)

med_res <- mediate(y = "binge", med = c("peer_alc","alcohol_exp"), treat = "monitoring", c = c("conflict","gender"), ymodel = "logistic regression", mmodel = c("logistic regression","logistic regression"), treat_lv = 1, control_lv = 0, data = alcohol, sim = 1000, digits = 3)

med_res <- intmed::mediate(y = "sub_misuse", med = c("dev_peer","sub_exp"), treat = "fam_int", c = c("conflict","gender"), ymodel = "logistic regression", mmodel = c("logistic regression","logistic regression"), treat_lv = 1, control_lv = 0, data = substance, sim = 100000, digits = 5)
