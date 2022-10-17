# █▀█ █▀▀ █▀ █▀▀ ▀█▀ █▀
# █▀▄ ██▄ ▄█ ██▄ ░█░ ▄█
# clear console
cat("\014")
# clear global environment
rm(list = ls())
# clear plots
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE)
# clear packages
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE)
# disables scientific notion for entire R session
options(scipen = 100)

# █▀█ ▄▀█ █▀▀ █▄▀ ▄▀█ █▀▀ █▀▀ █▀
# █▀▀ █▀█ █▄▄ █░█ █▀█ █▄█ ██▄ ▄█
library(pacman)
p_load(tidyverse)
p_load(ggthemes)
p_load(glue)
p_load(onewaytests)
p_load(skimr)
p_load(knitr)

# █░█ █▀▀ █░░ █▀█ █▀▀ █▀█ █▀
# █▀█ ██▄ █▄▄ █▀▀ ██▄ █▀▄ ▄█
# helper function for generating tibble with important values 
report_values <- function(test, alpha) {
  test.method <- ifelse(
    str_detect(test$method, "Chi-squared"), "chi.squared", "anova")
  switch(test.method,
         "chi.squared" = tibble(
           alpha = alpha,
           degrees = unname(test$parameter),
           cv = round(qchisq(p = alpha, df = degrees, 
                             lower.tail = FALSE), 3),
           test.value = round(unname(test$statistic), 3)),
         "anova" = tibble(
           alpha = alpha,
           df.N = test$parameter[1],
           df.D = test$parameter[2],
           cv = round(qf(p = alpha, 
                         df1 = df.N, df2 = df.D, 
                         lower.tail = FALSE), 2),
           test.value = round(test$statistic, 2)))}

# helper function for making the decision
decision <- function(values) {
  significant <- values$test.value > values$cv
  ifelse(significant == TRUE, 
         glue("Reject the null hypothesis. 
              The test value of {values$test.value} 
              is greater than the critical value of {values$cv}"), 
         glue("Do not reject the null hypothesis. 
              The test value of {values$test.value}
              is less than the critical value of {values$cv}"))}

# helper function for summarizing the result
results <- function(values, hypotheses) {
  significant <- values$test.value > values$cv
  claim <- gsub(" (claim)", "", hypotheses$h1, fixed = TRUE)
  ifelse(significant == TRUE, 
         glue("There is sufficient evidence to support 
              the claim that {claim}."), 
         glue("There is not enough evidence to support 
              the claim that {claim}."))}

# █▀ █▀▀ █▀▀ ▀█▀ █ █▀█ █▄░█   ▄█ ▄█ ▄▄ ▄█
# ▄█ ██▄ █▄▄ ░█░ █ █▄█ █░▀█   ░█ ░█ ░░ ░█
# 6. Blood Types
###### Log the known information
data <- tibble(
  E = c(0.20, 0.28, 0.36, 0.16),
  O = c(12, 8, 24, 6))

###### State the hypotheses and identify the claim.
hypotheses <- 
  tibble(
    h0 = "the distribution for the general population is: 
    type A, 20%; type B, 28%; type O, 36%; and type AB = 16%",
    h1 = "the distribution differs from 
    that of the general population (claim)")

###### Find the critical value and test value.
chi.sq <- chisq.test(x = data$O, p = data$E)
values <- report_values(test = chi.sq, alpha = 0.10)
kable(values)

###### Make the decision.
writeLines(decision(values))

###### Summarize the results.
writeLines(results(values, hypotheses))

# 8. On-Time Performance by Airline
###### Log the known information
data <- tibble(
  E = c(.708, .082, .09, .12),
  O = c(125, 10, 25, 40))

###### State the hypotheses and identify the claim.
hypotheses <- 
  tibble(
    h0 = "the performance of airlines is that 70.8% were on time, 
    12% were delayed for various reasons, 8.2% were delayed by the 
    National Aviation System, and 9% were delayed by other 
    aircraft arriving late",
    h1 = "the proportions of delays are different 
    from those stated in the null hypothesis (claim)")

###### Find the critical value and test value.
chi.sq <- chisq.test(x = data$O, p = data$E)
values <- report_values(test = chi.sq, alpha = 0.05)
kable(values)

###### Make the decision.
writeLines(decision(values))

###### Summarize the results.
writeLines(results(values, hypotheses))

# █▀ █▀▀ █▀▀ ▀█▀ █ █▀█ █▄░█   ▄█ ▄█ ▄▄ ▀█
# ▄█ ██▄ █▄▄ ░█░ █ █▄█ █░▀█   ░█ ░█ ░░ █▄
# 8. Ethnicity and Movie Admissions
###### Log the known information
data <- data.frame(
  Caucasion = c(724, 370),
  Hispanic = c(335, 292),
  African_American = c(174, 152),
  Other = c(107, 140))
rownames(data) <- c(2013, 2014)

###### State the hypotheses and identify the claim.
hypotheses <- 
  tibble(
    h0 = "movie attendance by year is independent 
    of the ethnicity of the movie goers",
    h1 = "movie attendance by year is dependent 
    upon the ethnicity of the movie goers (claim)")

###### Find the critical value and test value.
chi.sq <- chisq.test(data)
values <- report_values(test = chi.sq, alpha = 0.05)
kable(values)

###### Make the decision.
writeLines(decision(values))

###### Summarize the results.
writeLines(results(values, hypotheses))

# 10. Women in the Military
###### Log the known information
data <- data.frame(
  Officers = c(10791, 7816, 932, 11819),
  Enlisted = c(62491, 42750, 9525, 54344)
)
rownames(data) <- c("Army", "Navy", "Marine Corps", "Air Force")

###### State the hypotheses and identify the claim.
hypotheses <- 
  tibble(
    h0 = "the rank of women personnel is 
    independent of the military branch of service",
    h1 = "the rank of women personnel is 
    dependent upon the military branch of service (claim)")

###### Find the critical value and test value.
chi.sq <- chisq.test(data)
values <- report_values(test = chi.sq, alpha = 0.05)
kable(values)

###### Make the decision.
writeLines(decision(values))

###### Summarize the results.
writeLines(results(values, hypotheses))

# █▀ █▀▀ █▀▀ ▀█▀ █ █▀█ █▄░█   ▄█ ▀█ ▄▄ ▄█
# ▄█ ██▄ █▄▄ ░█░ █ █▄█ █░▀█   ░█ █▄ ░░ ░█
# 8. Sodium Contents of Foods
###### Log the known information
data <- bind_rows(
  tibble(Type = "Condiments", Sodium = c(270, 130, 230, 180, 80, 70, 200, NA)),
  tibble(Type = "Cereals", Sodium = c(260, 220, 290, 290, 200, 320, 140, NA)),
  tibble(Type = "Desserts", Sodium = c(100, 180, 250, 250, 300, 360, 300, 160)))

###### State the hypotheses and identify the claim.
hypotheses <- 
  tibble(
    h0 = "the mean amount of sodium in condiments, 
    cereals, and desserts is the same",
    h1 = "at least one mean is different from the others (claim)")

###### Find the critical value and test value.
one.anova <- aov.test(Sodium ~ Type, data = data, alpha = 0.05)
values <- report_values(test = one.anova, alpha = one.anova$alpha)
kable(values)

###### Make the decision.
writeLines(decision(values))

###### Summarize the results.
writeLines(results(values, hypotheses))

# █▀ █▀▀ █▀▀ ▀█▀ █ █▀█ █▄░█   ▄█ ▀█ ▄▄ ▀█
# ▄█ ██▄ █▄▄ ░█░ █ █▄█ █░▀█   ░█ █▄ ░░ █▄
# 10. Sales for Leading Companies
###### Log the known information
data <- bind_rows(
  tibble(Type = "Cereal", Sales.in.millions = c(578, 320, 264, 249, 237)),
  tibble(Type = "Chocolate Candy", Sales.in.millions = c(311, 106, 109, 125, 173)),
  tibble(Type = "Coffee", Sales.in.millions = c(261, 185, 302, 689, NA)))

###### State the hypotheses and identify the claim.
hypotheses <- 
  tibble(
    h0 = "the mean amount of sales for cereal, 
    chocolate candy, and coffee is the same",
    h1 = "at least one mean is different from the others (claim)")

###### Find the critical value and test value.
one.anova <- aov.test(Sales.in.millions ~ Type, data = data, alpha = 0.01)
values <- report_values(test = one.anova, alpha = one.anova$alpha)
kable(values)

###### Make the decision.
writeLines(decision(values))

###### Summarize the results.
writeLines(results(values, hypotheses))

## 12. Per-Pupil Expenditures
###### Log the known information
data <- bind_rows(
  tibble(Type = "Eastern third", Expenditures = c(4946, 5953, 6202, 7243, 6113)),
  tibble(Type = "Middle third", Expenditures = c(6149, 7451, 6000, 6479, NA)),
  tibble(Type = "Western third", Expenditures = c(5282, 8605, 6528, 6911, NA)))

###### State the hypotheses and identify the claim.
hypotheses <- 
  tibble(
    h0 = "the mean expenditures (in dollars) per student 
    for states in three sections of the country are the same",
    h1 = "at least one mean is different from the others (claim)")

###### Find the critical value and test value.
one.anova <- aov.test(Expenditures ~ Type, data = data, alpha = 0.05)
values <- report_values(test = one.anova, alpha = one.anova$alpha)
kable(values)

###### Make the decision.
writeLines(decision(values))

###### Summarize the results.
writeLines(results(values, hypotheses))

# █▀ █▀▀ █▀▀ ▀█▀ █ █▀█ █▄░█   ▄█ ▀█ ▄▄  THREE
# ▄█ ██▄ █▄▄ ░█░ █ █▄█ █░▀█   ░█ █▄ ░░
# 10. Increasing Plant Growth
###### Log the known information
data <- bind_rows(
  tibble(Plant.food = "A", Grow.light = c(9.2, 9.4, 8.9), Type = c("GL1")),
  tibble(Plant.food = "A", Grow.light = c(8.5, 9.2, 8.9), Type = c("GL2")),
  tibble(Plant.food = "B", Grow.light = c(7.1, 7.2, 8.5), Type = c("GL1")),
  tibble(Plant.food = "B", Grow.light = c(5.5, 5.8, 7.6), Type = c("GL2")))
data$Plant.food <- factor(data$Plant.food)
data$Type <- factor(data$Type)

###### State the hypotheses and identify the claim.
hypotheses <- 
  tibble(
    h0.interaction = "There is no interaction between the strength 
    of the grow light and the plant food supplement",
    h1.interaction = "There is an interaction between the strength 
    of the grow light and the plant food supplement (claim)",
    h0.plant.food = "There is no difference between mean growth 
    with respect to the type of plant food supplement",
    h1.plant.food = "There is a difference between mean growth 
    with respect to the type of plant food supplement (claim)",
    h0.grow.light = "There is no difference between mean growth 
    with respect to the strength of the grow light",
    h1.grow.light = "There is a difference between mean growth 
    with respect to the strength of the grow light (claim)",
  )

###### Find the critical value and test value.
two.anova <- aov(Grow.light ~ Plant.food * Type, data = data)
values <- tibble(
  alpha = 0.05,
  df.N = summary(two.anova)[[1]][[1]][1],
  df.D = summary(two.anova)[[1]][[1]][4],
  Plant.food.ss = summary(two.anova)[[1]][[2]][1],
  Plant.food.F = summary(two.anova)[[1]][[4]][1],
  Grow.light.ss = summary(two.anova)[[1]][[2]][2],
  Grow.light.F = summary(two.anova)[[1]][[4]][2],
  Interaction.ss = summary(two.anova)[[1]][[2]][3],
  Interaction.F = summary(two.anova)[[1]][[4]][3],
  cv = round(qf(p = alpha, df1 = df.N, df2 = df.D, lower.tail = FALSE), 2))
kable(values)

###### Make the decision and summarize the results
writeLines("Since the F test value for plant food (24.56) is greater 
than the critical value (5.32), reject the null hypothesis for plant food. 
There is sufficient evidence to support the claim that there is a difference 
between mean growth with respect to the type of plant food supplement.
")
writeLines("Since the F test value for grow light (3.68) is lower 
than the critical value (5.32) and since the F test value for an interaction 
between plant food and grow light (1.44) is lower than the 
critical value (5.32), do not reject the null hypotheses. 
There is not sufficient evidence to support that there is a difference 
between mean growth with respect to the strength of the grow light or 
that there is an interaction between the strength of the grow light 
and the plant food supplement.")

# █▄▄ ▄▀█ █▀ █▀▀ █▄▄ ▄▀█ █░░ █░░
# █▄█ █▀█ ▄█ ██▄ █▄█ █▀█ █▄▄ █▄▄
baseball <- read_csv("/Users/justin/Desktop/ALY 6015/Assignments/M2/chi-square-testing-and-anova/baseball.csv")

# EDA
skim(baseball) %>% select(-starts_with('numeric.p'))
baseball %>% 
  ggplot +
  aes(Year, W) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5, fullrange = TRUE) +
  geom_line(size = 0.8) +
  facet_wrap(~ Team) +
  labs(title = "Plot 1: Wins per year by team with line of best fit",
       y = "Count of wins") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Extract decade from year
baseball$Decade <- baseball$Year - (baseball$Year %% 10)

###### Log the known information
wins.per.decade <- baseball %>%
  select(Decade, W) %>%
  group_by(Decade) %>%
  summarize(O = sum(W)) %>%
  ungroup() %>%
  mutate(E = O / sum(O)) %>%
  t()

###### State the hypotheses and identify the claim.
hypotheses <- 
  tibble(
    h0 = "there is no difference in the number of wins by decade",
    h1 = "there is a difference in the number of wins by decade (claim)")

###### Find the critical value and test value.
chi.sq <- chisq.test(x = wins.per.decade["O",], 
                     p = wins.per.decade["E",])
values <- report_values(test = chi.sq, alpha = 0.05)
kable(values)

###### Make the decision.
writeLines(decision(values))

###### Summarize the results.
writeLines(results(values, hypotheses))

# █▀▀ █▀█ █▀█ █▀█ █▀
# █▄▄ █▀▄ █▄█ █▀▀ ▄█
crops <- read_csv("/Users/justin/Desktop/ALY 6015/Assignments/M2/chi-square-testing-and-anova/crop_data.csv")
# The block variable is superfluous -- it can  be dropped.
crops <- crops %>% select(-block)
# Setting as factors
crops$fertilizer <- factor(crops$fertilizer)
crops$density <- factor(crops$density)

###### State the hypotheses and identify the claim.
hypotheses <- 
  tibble(
    h0.interaction = "There is no interaction between the 
    fertilizer used and plant density",
    h1.interaction = "There is an interaction between the 
    fertilizer used and plant density (claim)",
    h0.fertilizer = "There is no difference between the 
    fertilizer used with respect to the grain yield",
    h1.fertilizer = "There is a difference between the 
    fertilizer used with respect to the grain yield (claim)",
    h0.density = "There is no difference between the 
    plant density with respect to the grain yield",
    h1.density = "There is a difference between the 
    plant density with respect to the grain yield (claim)")

###### Find the critical value and test value.
two.anova <- aov(yield ~ fertilizer * density, data = crops)
values <- tibble(
  alpha = 0.05,
  df.N = summary(two.anova)[[1]][[1]][1],
  df.D = summary(two.anova)[[1]][[1]][4],
  fertilizer.ss = summary(two.anova)[[1]][[2]][1],
  fertilizer.F = summary(two.anova)[[1]][[4]][1],
  density.ss = summary(two.anova)[[1]][[2]][2],
  density.F = summary(two.anova)[[1]][[4]][2],
  Interaction.ss = summary(two.anova)[[1]][[2]][3],
  Interaction.F = summary(two.anova)[[1]][[4]][3],
  cv = round(qf(p = alpha, df1 = df.N, df2 = df.D, lower.tail = FALSE), 2))
kable(values)

###### Make the decision and summarize the results
writeLines("Since the F test value for fertilizer used (9.001) is greater 
than the critical value (3.1), reject the null hypothesis for fertilizer. 
There is sufficient evidence to support the claim that there is a 
difference between the fertilizer used with respect to the grain yield.")
writeLines("Since the F test value for plant density (15.19) is greater than 
the critical value (3.1), reject the null hypothesis for density. 
There is sufficient evidence to support the claim that there is a difference 
between the plant density with respect to the grain yield.")
writeLines("Since the F test value for an interaction between fertilizer used 
and plant density (0.63) is lower than the critical value (3.1), do not 
reject the null hypothesis. There is not sufficient evidence to support the 
claim that there is an interaction between the fertilizer used 
and plant density.")