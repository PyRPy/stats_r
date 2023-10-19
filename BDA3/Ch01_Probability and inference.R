
# Exercise 1.3 - eyes color -----------------------------------------------

set.seed(2023)

N <- 1000000
p <- 0.2

alleles <- c('x', 'X')
weights <- c(p, 1 - p)

df <- tibble(id = 1:N %>% as.character()) %>%
  mutate(
    allele1 = sample(alleles, N, prob = weights, replace = TRUE),
    allele2 = sample(alleles, N, prob = weights, replace = TRUE),
    genotype = if_else(allele1 == allele2, 'homozygote', 'heterozygote'),
    eye_colour = if_else(allele1 == 'x' & allele2 == 'x', 'blue', 'brown')
  )
head(df)

allele_distribution <- df %>%
  group_by(allele1, allele2) %>%
  tally() %>%
  mutate(frac = n / sum(n))

eye_colour_distribution <- df %>%
  group_by(eye_colour) %>%
  tally() %>%
  mutate(frac = n / sum(n))

genotype_distribution <- df %>%
  group_by(genotype) %>%
  tally() %>%
  mutate(frac = n / sum(n))


# Waiting time for seeing a doctor ----------------------------------------
# 1.9
arrivals <- function(lambda, t, n=100) {
  rexp(n, lambda) %>%
    tibble(
      delay = .,
      time = cumsum(delay)
    ) %>%
    filter(time <= t) %>%
    pull(time) %>%
    return()
}

lambda <- 1 / 10
t <- (16 - 9) * 60

waiting_time <- arrivals(lambda, t)
plot(waiting_time)
hist(waiting_time)
hist(diff(waiting_time))
