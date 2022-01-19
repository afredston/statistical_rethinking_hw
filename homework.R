library(rethinking)
library(brms)
library(tidyverse)

# Homework 1 ------------

#1
p_grid <- seq(from=0, to=1, length.out=20)
prior <- rep(1, 20)
likelihood <- dbinom(4, size=15, prob=p_grid)
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior / sum(unstd.posterior)
plot(p_grid, posterior)

#2
p_grid <- seq(from=0, to=1, length.out=20)
prior <- ifelse(p_grid < 0.5, 0, 1)
likelihood <- dbinom(4, size=6, prob=p_grid)
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior / sum(unstd.posterior)
plot(p_grid, posterior)

#3
samples <- sample(p_grid, size=1e4, replace=TRUE, prob=posterior)
PI(samples, prob=0.89)
HPDI(samples, prob=0.89) # narrower -- less susceptible to skewness 


#4
water <- 0.7
draws <- 20
true_waters <- rbinom(1, draws, prob=c(water))
p_water <- true_waters / draws
number_wrong <- rbinom(1, true_waters, .2)
sampled_waters <- true_waters - number_wrong
perceived_water <- sampled_waters / draws

sample <- rbinom(1, 20, 0.7)
p_sample <- sample / draws

posterior <- dbinom(true_waters, draws, seq(0, 1, length.out=1000))
plot(posterior)

# Homework 2 ------------

data(Howell1)

dat <- Howell1[Howell1$age >= 18,]

m1 <- brm(formula = weight ~ height, 
          data = dat,
          family = gaussian(),
          prior = c(prior(lognormal(0, 1), "b"),
                    prior(normal(0, 20), "Intercept")))
summary(m1)
plot(m1)

post_m1 <- posterior_predict(m1, tribble(
  ~ "height", 140, 160, 175
))
hist(post_m1[,1])
hist(post_m1[,2])
hist(post_m1[,3])

median(post_m1[,1])
median(post_m1[,2])
median(post_m1[,3])

quantile(post_m1[,1], 0.11)
quantile(post_m1[,2], 0.89)

dat2 <- Howell1[Howell1$age < 13,]

# form2 <- bf(weight ~ height + age, 
#             height ~ age, 
#             nl = TRUE)

m2 <- brm(formula = weight ~ 1 + height + age, 
            data = dat2
            )

summary(m2)
marginal_effects(m2)

m2.1 <- brm(formula = weight ~ age, 
          data = dat2
)
summary(m2.1)
plot(m2.1)

# weight ~ age:male

m3 <- brm(formula = weight ~ 1 + height + age + 1|male, 
          data = dat2
)
summary(m3)

dat_g <- dat2[dat2$male==1,]
dat_b <- dat2[dat2$male==1,]

m3_g <- brm(formula = weight ~ age, 
            data = dat_g
)

m3_b <- brm(formula = weight ~  age, 
            data = dat_b
)

m4 <- brm(formula = height ~ age*male,
          data = dat2)
summary(m4)

data("Oxboys")

head(Oxboys)
Oxboys <- Oxboys %>% 
  group_by(Subject) %>% 
  arrange(Occasion) %>% 
  mutate(growth = height - lag(height),
         Occasion = as.factor(Occasion)) %>% 
  filter(!is.na(growth))

m5 <- brm(formula = growth ~ Occasion,
          data = Oxboys, 
          prior = c(prior(lognormal(0,1), "b")))
summary(m5)


m6 <- brm(formula = log(growth) ~ Occasion,
          data = Oxboys)
summary(m6)
plot(m6)
