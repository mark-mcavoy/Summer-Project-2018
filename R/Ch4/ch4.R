# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Replication program for Chapter 4 in SW3U;
# Mark McAvoy
# Summer 2018
# R code for chapter as log file
# ch4_replication
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# install.packages("tidyverse") # run this the first time (only need to once)
library(tidyverse) # includes ggplot2 and many more useful libraries

# read in california school data: caschool.csv
# change working directory to where caschool.csv is found
caschool <- read.csv("caschool.csv")

# ********************************************************
#                                               Table 4.1
# ********************************************************

avg_str <- mean(caschool$str)
std_str <- sd(caschool$str)

avg_tsc <- mean(caschool$testscr)
std_tsc <- sd(caschool$testscr)

quantile_str <- as.matrix(quantile(caschool$str, c(seq(0.05:1, by = 0.05)))) # quantile finds values in str based by percentages from 5-100%
quantile_str2 <- as.tibble(quantile(caschool$str, c(seq(0.05:1, by = 0.05)))) 
names(quantile_str2) <- "str"

quantile_tsc <- as.matrix(quantile(caschool$testscr, c(seq(0.05:1, by = 0.05))))
quantile_tsc2 <- as.tibble(quantile(caschool$testscr, c(seq(0.05:1, by = 0.05)))) # if tibble then need install tidyverse
names(quantile_tsc2) <- "tsc"


table_4.1 <- cbind(quantile_str2, quantile_tsc2)
# bind together same as in book

# ========================================================
#                                               Figure 4.2
# ========================================================

fig_4.2 <- ggplot(caschool, aes(x = str, y = testscr)) + 
           geom_point() +
           labs(x = "Student-teacher ratio", y = "Test score")
fig_4.2
# scale same to as in book

cor(caschool$testscr, caschool$str)

# ........................................................
#                                            Equation 4.11
# ........................................................

# regression are: lm(y ~ x)
m <- lm(caschool$testscr ~ caschool$str)

summary(m) # plain std errors
# rename variable just str
# save as log file so can copy paste 

# ========================================================
#                                               Figure 4.3
# ========================================================

# obtain y_hat
predict_tsc <- predict(m)

fig_4.3 <- fig_4.2 +
  geom_line(aes(y=predict_tsc))
fig_4.3
# format same size book

# If want HAC std errors
# http://data.princeton.edu/wws509/r/robust.html
# install.packages("sandwich")
# install.packages("lmtest")
library(sandwich)
library(lmtest)

coeftest(m, vcov = vcovHC(m, type = "HC1"))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                  Empirical Exercise E4.1
# separate file EE4_1, also log file EE4_1.log (and open as text file) 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# read in data
growth <- read.csv("Growth.csv")

# --- a ---
plot_4.1_a <- ggplot(growth, aes(x = tradeshare, y =growth)) +
              geom_point()
plot_4.1_a

# --- b ---
# subset data to only Malta
malta <- subset(growth, country_name == "Malta") 
plot_4.1_b <- plot_4.1_a +
              geom_point(data=malta, colour="orange")
plot_4.1_b
# add arrow to outlier

# --- c ---
reg_4.1_c <- lm(growth$growth ~ growth$tradeshare)
reg_4.1_c
# intercept = 0.6403, slope = 2.3064

reg_4.1_c$coefficients[1] + reg_4.1_c$coefficients[2]*0.5

reg_4.1_c$coefficients[1] + reg_4.1_c$coefficients[2]*1.0

# --- d ---
# remove malta from data
growth_no_malta <- subset(growth, country_name != "Malta")

reg_4.1_d <- lm(growth_no_malta$growth ~ growth_no_malta$tradeshare)
reg_4.1_d
# intercept = 0.9574, slope = 1.6809

reg_4.1_d$coefficients[1] + reg_4.1_d$coefficients[2]*0.5


reg_4.1_d$coefficients[1] + reg_4.1_d$coefficients[2]*1.0

# --- e ---

predict_4.1_c <- predict(reg_4.1_c)
predict_4.1_d <- predict(reg_4.1_d); predict_4.1_d <- c(predict_4.1_d, NA) # include an empty value to match length

plot_4.1_e <- plot_4.1_a +
              geom_line(aes(y = predict_4.1_c), colour = "blue") +
              geom_line(aes(y = predict_4.1_d), colour = "green")
plot_4.1_e



