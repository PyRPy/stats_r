
# Chapter02 Simple Comparative Experiments --------------------------------
# https://github.com/ehassler/MontgomeryDAE/tree/master

library(MASS)
library(MontgomeryDAE)
head(Table2.1)


# Two sample comparison ---------------------------------------------------

# boxplot
bp.returned <- boxplot(
  Table2.1,
  main="Box plots for Table 2.1",
  xlab='Mortar Formulation',
  ylab='Strength'
)

# uset t.test in R base
attach(Table2.1)
# reformat the data into 'long form'
df <- data.frame(
  'Strength'=c(ModifiedMortar, UnmodifiedMortar),
  'Mortar'=factor(c(rep('Modified', 10), rep('Unmodified', 10)))
)
head(df)
tail(df)
t.test(Strength ~ Mortar, data = df, paired = FALSE, var.equal = TRUE)
# t = -2.1869, df = 18, p-value = 0.0422

# plot distribution
qqnorm(ModifiedMortar, main="Modified Mortar")
qqline(ModifiedMortar, col='orange')

qqnorm(UnmodifiedMortar, main="Unmodified Mortar")
qqline(UnmodifiedMortar, col='blue')

detach(Table2.1)


# Power and Sample Size ---------------------------------------------------
library(pwr)
pwr.t.test(n=10,
           d=2,
           sig.level=0.05,
           type='two.sample',
           alternative='two.sided')

# power = 0.988179

# how about 90% power
pwr.t.test(power=0.9,
           d=2,
           sig.level=0.05,
           type='two.sample',
           alternative='two.sided')
# n = 7, need 7 samples


# Unequal Variance --------------------------------------------------------

df <- data.frame(
  'Flourescence'=c(Table2.3$Muscle, Table2.3$Nerve),
  'Tissue'=factor(c(rep('Muscle', 12), rep('Nerve', 12)))
)

t.test(Flourescence ~ Tissue, data=df,
       paired=FALSE,
       var.equal=FALSE,
       alternative='greater')
# p-value = 0.9927


# Paired Comparisons ------------------------------------------------------
df <- data.frame(
  'Depth'=c(Table2.6$Tip1, Table2.6$Tip2),
  'Tip'=factor(c(rep('Tip1', 10), rep('Tip2', 10)))
)
head(df)

t.test(Depth ~ Tip, data=df, paired=TRUE)
# p-value = 0.7976

