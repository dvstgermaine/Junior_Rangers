pacman::p_load(gapminder,
               tidyverse,
               tidyr,
               ggplot2,
               car,
               plyr,
               dplyr,
               Rmisc)

#Question 1

set.seed(14)
DF <- rnorm(10000, mean = 5, sd = sqrt(1.6))
            
conf1 <- (mean(DF)-(1.96*sqrt(1.6)))
conf2 <- (mean(DF)+(1.96*sqrt(1.6)))
conf3 <- quantile(DF, probs = 0.975)
conf4 <- quantile(DF, probs = 0.025)

data.frame(x = DF) %>% 
ggplot(aes(x = x)) +
geom_density(fill = "blue", size = 1,
               alpha = 0.5) +
               ggtitle("Density Probability Plot") +
              geom_vline(xintercept = conf1, color = "black", size = 0.5) +
              geom_vline(xintercept = conf2, color = "black", size = 0.5) +
              geom_vline(xintercept = 5, color = "red", linetype = "dotted") +
              geom_vline(xintercept = conf3, color = "black", size = 0.5)
              geom_vline(xintercept = conf4, color = "black", size = 0.5)



#Q1.1.2 ADD LABELS AND COLOURS
set.seed(114)
data.frame(x = rnorm(10000, mean = 5, sd = sqrt(1.6))) %>% 
  ggplot(aes(x = x)) +
  stat_function(fun = pnorm, args = list(mean = 5, sd = sqrt(1.6)), aes(col = "pnorm"), size = 2)

#Q2 ADD LABELS

# DONT USE THIS: plot(c(0:10), dbinom(c(0:10), size = 10, prob = 0.5), type = "h")

binomial <- dbinom(0:10, 10, 0.5)

newDF <- data.frame(numcount = 0:10, prob = binomial)

figureDF <- ggplot(data = newDF, mapping = aes(x = numcount, y = binomial)) +
  geom_point() +
  geom_linerange(data = newDF, mapping = aes(x = numcount, ymin = 0, ymax = prob), 
                 size = 1, color = "blue")
figureDF

#Q1.3

fair <- pbinom(12,25,0.5)
unfair <- pbinom(12, 25, 0.3)

fair
unfair

#Q2.1 ADD labels and STUFF to this plot
set.seed(105)

bike_sample <- rnorm(100, mean = 15, sd = 4)

bike_speed_plot <- data.frame(x = bike_sample) %>%
  ggplot(aes(x = x)) +
  geom_histogram(binwidth = 2, col = "black", fill = "blue")
bike_speed_plot
  
#Q2.2
cyclists_over_limit <- bike_sample %>%
  data.frame(x = bike_sample) %>%
  filter(x > 20) %>%
  nrow()

proportion_over_limit <- cyclists_over_limit/100
proportion_over_limit

#Q2.3

less_than <- quantile(bike_sample, probs = 0.75)
less_than

#Q2.4
more_than <- quantile(bike_sample, probs = 0.9)
more_than

#Q2.5

set.seed(110)

#Create 100 samples x 100 cyclists
one_hundred_samples <- replicate(100, rnorm(100, mean = 15, sd = 4))
one_hundred_samples

#Find the means of each of the 100 sample sets
col_means <- colMeans(one_hundred_samples)
col_sd <- apply(one_hundred_samples, 2, sd)

#Plot of speed means
hundred_bike_speed_plot <- data.frame(x = col_means) %>%
  ggplot(aes(x = x)) +
  geom_histogram(col = "black", fill = "blue")
hundred_bike_speed_plot

#Plot of standard deviation
hundred_bike_speed_plot2 <- data.frame(x = col_sd) %>%
  ggplot(aes(x = x)) +
  geom_histogram(col = "black", fill = "blue")
hundred_bike_speed_plot2

#Visualization of how different

