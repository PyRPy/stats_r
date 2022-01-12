# Bayes idea introduction
# flip coins again, you flipped 20 times, you got 14 heads

# assume there are two piles of 50k coins
fair = rbinom(50000, 20, 0.5)
sum(fair == 14) # 1790

biased = rbinom(50000, 20, 0.75)
sum(biased == 14) # 8296

# conditional probability Prob(Biased | 14 heads) 

sum(biased == 14)/(sum(biased == 14) + sum(fair == 14)) # 0.8225263
# results may vary with different 'random seed'

# more simulations
# assume there are two piles of 50k coins
fair = rbinom(90000, 20, 0.5)
sum(fair == 14) 

biased = rbinom(10000, 20, 0.75)
sum(biased == 14) 

# conditional probability Prob(Biased | 14 heads) 

sum(biased == 14)/(sum(biased == 14) + sum(fair == 14)) # 0.339194


# Bayes theory ------------------------------------------------------------
fair = rbinom(90000, 20, 0.5)
sum(fair == 14)
dbinom(14, 20, 0.5) * 0.9

biased = rbinom(10000, 20, 0.75)
sum(biased == 14) 
dbinom(14, 20, 0.75) * 0.1

# conditional probability: Prob(Biased | 14 heads)
prob_14_fair = dbinom(14, 20, 0.5) * 0.9
prob_14_biased = dbinom(14, 20, 0.75) * 0.1

prob_14_biased/(prob_14_fair + prob_14_biased) # Prob(Biased | 14 heads)
