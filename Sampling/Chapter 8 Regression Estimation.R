# Chapter 8 Regression Estimation
# ref: https://rpubs.com/trjohns/survey-ratioreg
library(survey)
library(downloader)
source_url("https://db.tt/VGK0FP4z", prompt = FALSE) # not working


# Regression Estimators and Calibration -----------------------------------
head(trees)
trees$N <- 1000 # total
trees.srs <- svydesign(id = ~1, data = trees, fpc = ~N)

# ratio estimator
predict(svyratio(~Volume, ~Girth, design = trees.srs), total = 14000)

# use calibrate
trees.cal <- calibrate(trees.srs, formula = ~Girth - 1, population = 14000,
                       variance = 1)
svytotal(~Volume, design = trees.srs)

svymean(~Volume, design = trees.cal)
sum(weights(trees.cal)*trees$Girth)
sum(weights(trees.cal)*trees$Volume)

# scatter plot to check common sense
with(trees, plot(Girth, Volume))
with(trees, abline(0, mean(Volume)/mean(Girth))) # no good

# regression estimator
trees.cal <- calibrate(trees.srs, 
                       formula = ~ Girth, 
                       population = c(`(Intercept)` = 1000,
                       Girth = 14000))

svytotal(~ Volume, trees.cal)
sum(weights(trees.cal))
sum(weights(trees.cal) * trees$Girth)

# two predictors
trees.cal <- calibrate(trees.srs, 
                       formula = ~ Girth + Height, 
                       population = c(`(Intercept)` = 1000,
                                      Girth = 14000,
                                      Height = 77000))

svytotal(~Volume, trees.cal)
