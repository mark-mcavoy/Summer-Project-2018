# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Replication program for Chapter 4 in SW3U;
# Mark McAvoy
# Summer 2018
# R code for chapter as log file
# ch4_replication
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# sink("log-file-ch4.txt) # uncomment if want to create a log file [of output]
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

quantile_str <- as.tibble(quantile(caschool$str, c(seq(0.05:1, by = 0.05)))) # quantile finds values in str based by percentages from 5-100%
names(quantile_str) <- "str"

quantile_tsc <- as.tibble(quantile(caschool$testscr, c(seq(0.05:1, by = 0.05)))) # if tibble then need install tidyverse
names(quantile_tsc) <- "tsc"

percentiles <- c("10%", "25%", "40%", "50%", "60%", "75%", "90%") # choose only the percentiles listed in the book
table_4.1 <- as.tibble(cbind(rbind(avg_str,avg_tsc), rbind(std_str,std_tsc), t(cbind(quantile_str, quantile_tsc)[percentiles,]))) # bring all together
names(table_4.1) <- c("Average", "Standard Deviation", percentiles)
table_4.1

# ========================================================
#                                               Figure 4.2
# ========================================================

fig_4.2 <- ggplot(caschool, aes(x = str, y = testscr)) + 
           geom_point() +
           labs(x = "Student-teacher ratio", y = "Test score") + 
           xlim(xmin = 10, xmax = 30) + ylim(ymin=600, ymax = 720)
fig_4.2


cor(caschool$testscr, caschool$str)

# ........................................................
#                                            Equation 4.11
# ........................................................

# regression are: lm(y ~ x) 
m <- lm(caschool$testscr ~ caschool$str)
names(m$coefficients) <- c("(Intercept)", "str")
summary(m) # plain std errors
 

# ========================================================
#                                               Figure 4.3
# ========================================================

# obtain y_hat
predict_tsc <- predict(m)

fig_4.3 <- fig_4.2 +
  geom_line(aes(y=predict_tsc), color = "blue", size = 1.5) +
  stat_smooth(method = "lm", fullrange = TRUE, size = 1.5, level=0) +
  xlim(xmin = 10, xmax = 30) + ylim(ymin=600, ymax = 720)
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
# sink()

