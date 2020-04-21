# A survey analysis example
# updated on April 3, 2020 by Thomas Lumley !
# ref https://cran.r-project.org/web/packages/survey/vignettes/survey.pdf
# Load library and data set -----------------------------------------------


library(survey)
data(api)
str(apiclus1)
head(apiclus1)


# Sampling design ---------------------------------------------------------


dclus1 <- svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
summary(dclus1)


# Compute summary statistics ----------------------------------------------

svymean(~api00, dclus1)
svytotal(~stype, dclus1)

svytotal(~enroll, dclus1)

# ratio estimator 
svyratio(~api.stu, ~enroll, dclus1)

svyratio(~api.stu, ~enroll, design = subset(dclus1, stype=='H'))


# Multi-variable formula --------------------------------------------------

vars <- names(apiclus1)[c(12:13, 16:23, 27:37)]
svymean(make.formula(vars), dclus1, na.rm = TRUE)


# Summary stats for subsets -----------------------------------------------

svyby(~ell + meals, ~stype, design = dclus1, svymean)


# Regression models -------------------------------------------------------

regmodel <- svyglm(api00 ~ ell + meals, design = dclus1)
summary(regmodel)

logitmodel <- svyglm(I(sch.wide == "Yes") ~ ell + meals, 
                     design = dclus1, family = quasibinomial())
summary(logitmodel)


# Calibrate the sampling --------------------------------------------------

gclus1 <- calibrate(dclus1, formula = ~api99, 
                    population = c(6194, 3914069))


svymean(~api00, gclus1)
svyquantile(~api00, gclus1, quantile = c(0.25, 0.5, 0.75), ci=TRUE)
svytotal(~enroll, gclus1)
svyratio(~api.stu, ~enroll, gclus1)
