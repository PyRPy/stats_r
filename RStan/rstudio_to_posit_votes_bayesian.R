
# Opinions and polls from RStudio name change -----------------------------
# https://www.r-bloggers.com/2022/08/robservations-36-opinions-on-rstudios-name-change-a-bayesian-approach-with-stan/

# Most of the respondents to the poll voted that they hated the name 
# change (31), followed by individuals who were "somewhere in between" 
# (24). Only 11 individuals said voted that they loved the name change.

library(rstan)

# Setting seed for reproducibility
set.seed(1234)

# Make a vector of 0's representing the individual voters
y_love_it<-rep(0,66)

# Randomly select impressions and assign them as votes- the same number as the known data. 
y_love_it[sample(c(1:66),11)] <- 1

# Now lets do it for the rest of the data
y_hate_it<-rep(0,66)
y_hate_it[sample(c(1:66),31)]<-1

y_in_between<-rep(0,66)
y_in_between[sample(c(1:66),24)]<-1

# Set up the data in list form
data <- list(n=66,
             y1=y_love_it,
             y2=y_hate_it,
             y3=y_in_between)

fit <- stan(file ="rstudio_to_posit_votes.stan", 
            data=data) 

# point estimates for three parameters
library(broom.mixed)
tidyMCMC(fit)

# plot intervals
library(tidyverse) 
library(reshape2)
# library(ggdark)
# library(ggpubr)

rstan::stan_plot(fit)+
  ggtitle("Density Of Posterior Distribution By Voter Choice")

# 
# params<- rstan::extract(fit) %>% 
#   as.list() %>% 
#   as_tibble() %>% 
#   select(!lp__) %>% 
#   transmute("Love it (theta1)"= theta1,
#             "Hate it (theta2)" = theta2,
#             "Somewhere in between (theta3)"= theta3) %>% 
#   melt() %>% 
#   mutate(value=c(value))
# 
# ggplot(data=params, 
#        mapping=aes(x=value,fill=variable,color=variable))+
#   geom_density(alpha=0.8)+
#   dark_theme_gray(base_size = 14) + 
#   theme(plot.background = element_rect(fill = "grey10"),
#         panel.background = element_blank(),
#         panel.grid.major = element_line(color = "grey30", size = 0.2),
#         panel.grid.minor = element_line(color = "grey30", size = 0.2),
#         legend.background = element_blank(),
#         axis.ticks = element_blank(),
#         legend.key = element_blank(),
#         legend.position = "bottom",
#         legend.title=element_blank())+
#   scale_fill_manual(values = c("#FDE725",
#                                "#22908C",
#                                "#450D54"))+
#   scale_color_manual(values = c("#FDE725",
#                                 "#22908C",
#                                 "#450D54"))+
#   ggtitle("Thoughts On RStudio's Name Change To Posit \n(Posterior Distributions)")
