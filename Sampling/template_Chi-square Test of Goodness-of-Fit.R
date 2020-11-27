# Chi-square Test of Goodness-of-Fit --------------------------------------

# https://rcompanion.org/rcompanion/b_03.html
observed = c(1752, 1895)    # observed frequencies
expected = c(0.5, 0.5)      # expected proportions

chisq.test(x = observed, p = expected)

### --------------------------------------------------------------
### Rice example, Chi-square goodness-of-fit, p. 47
### --------------------------------------------------------------

observed = c(772, 1611, 737)
expected = c(0.25, 0.50, 0.25)

chisq.test(x = observed,
           p = expected)

### --------------------------------------------------------------
### Bird foraging example, Chi-square goodness-of-fit, pp. 47–48
### --------------------------------------------------------------

observed = c(70, 79, 3, 4)
expected = c(0.54, 0.40, 0.05, 0.01)

chisq.test(x = observed,
           p = expected)
### --------------------------------------------------------------
### Intrinsic example, Chi-square goodness-of-fit, p. 48
### --------------------------------------------------------------

observed       = c(1203,  2919,  1678)
expected.prop  = c(0.211, 0.497, 0.293)

expected.count = sum(observed)*expected.prop

chi2 = sum((observed- expected.count)^2/ expected.count)

chi2
pchisq(chi2,
       df=1,
       lower.tail=FALSE)   

# Graphing the results ----------------------------------------------------
### --------------------------------------------------------------
### Simple bar plot of proportions, p. 49
###      Uses data in a matrix format
### --------------------------------------------------------------

observed = c(70, 79, 3, 4)

expected = c(0.54, 0.40, 0.05, 0.01)

total = sum(observed)

observed.prop = observed / total

observed.prop

### Re-enter data as a matrix

Input =("
Value     Douglas.fir  Ponderosa.pine  Grand.fir   Western.larch
Observed  0.4487179    0.5064103       0.01923077  0.02564103
Expected  0.5400000    0.4000000       0.05000000  0.01000000   
")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE, 
                              row.names=1))

Matriz
barplot(Matriz, 
        beside=TRUE, 
        legend=TRUE, 
        ylim=c(0, 0.6),
        xlab="Tree species",
        ylab="Foraging proportion")

### --------------------------------------------------------------
### Graph example, Chi-square goodness-of-fit, p. 49
###    Using ggplot2
###    Plot adapted from:
###       shinyapps.stat.ubc.ca/r-graph-catalog/   
### --------------------------------------------------------------

Input =("
Tree              Value      Count   Total Proportion  Expected
'Douglas fir'     Observed   70      156   0.4487      0.54
'Douglas fir'     Expected   54      100   0.54        0.54
'Ponderosa pine'  Observed   79      156   0.5064      0.40
'Ponderosa pine'  Expected   40      100   0.40        0.40
'Grand fir'       Observed    3      156   0.0192      0.05
'Grand fir'       Expected    5      100   0.05        0.05
'Western larch'   Observed    4      156   0.0256      0.01
'Western larch'   Expected    1      100   0.01        0.01
")

Forage = read.table(textConnection(Input),header=TRUE)
### Specify the order of factor levels. Otherwise R will alphabetize them.

library(dplyr)

Forage = 
  mutate(Forage,
         Tree = factor(Tree, levels=unique(Tree)),
         Value = factor(Value, levels=unique(Value))
  )


### Add confidence intervals

Forage = 
  mutate(Forage,       
         low.ci = apply(Forage[c("Count", "Total", "Expected")], 
                        1, 
                        function(x) 
                          binom.test(x["Count"], x["Total"], x["Expected"]
                          )$ conf.int[1]),
         
         upper.ci = apply(Forage[c("Count", "Total", "Expected")],
                          1,
                          function(x) 
                            binom.test(x["Count"], x["Total"], x["Expected"]
                            )$ conf.int[2])
  )

Forage$ low.ci [Forage$ Value == "Expected"] = 0
Forage$ upper.ci [Forage$ Value == "Expected"] = 0

Forage

### Plot adapted from:
###   shinyapps.stat.ubc.ca/r-graph-catalog/

library(ggplot2)
library(grid)

ggplot(Forage, 
       aes(x = Tree, y = Proportion, fill = Value, 
           ymax=upper.ci, ymin=low.ci))  +
  geom_bar(stat="identity", position = "dodge", width = 0.7) +
  geom_bar(stat="identity", position = "dodge", 
           colour = "black", width = 0.7, 
           show_guide = FALSE)  +
  scale_y_continuous(breaks = seq(0, 0.60, 0.1), 
                     limits = c(0, 0.60), 
                     expand = c(0, 0))  +
  scale_fill_manual(name = "Count type" , 
                    values = c('grey80', 'grey30'), 
                    labels = c("Observed value", 
                               "Expected value"))  +
  geom_errorbar(position=position_dodge(width=0.7), 
                width=0.0, size=0.5, color="black")  +
  labs(x = "Tree species", 
       y = "Foraging proportion")  +
  ## ggtitle("Main title") + 
  theme_bw()  +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size = rel(1.5), 
                                  face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        legend.key = element_rect(fill = "black"),
        axis.title.y = element_text(vjust= 1.8),
        axis.title.x = element_text(vjust= -0.5)
  )

### --------------------------------------------------------------
### Power analysis, Chi-square goodness-of-fit, snapdragons, p. 51
### --------------------------------------------------------------

library(pwr)

P0      = c(0.25,  0.50, 0.25)
P1      = c(0.225, 0.55, 0.225) 

effect.size = ES.w1(P0, P1)  

degrees = length(P0) - 1

pwr.chisq.test(
  w=effect.size, 
  N=NULL,            # Total number of observations
  df=degrees, 
  power=0.80,        # 1 minus Type II probability
  sig.level=0.05)    # Type I probability


# A dice rolling example --------------------------------------------------
# test 1
observed <- c(10, 7, 8, 8, 8, 9) # rolling 50 times, count for each side
expected = rep(1/6, 6)
chisq.test(x = observed,
           p = expected)

# test 2
observed2 = c(6, 16, 7, 3, 13, 5)
expected = rep(1/6, 6)
chisq.test(x = observed2,
           p = expected)
