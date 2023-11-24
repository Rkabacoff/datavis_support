
# load data
data(Salaries, package="carData")

# get means, standard deviations, and
# 95% confidence intervals for
# assistant professor salary by sex 
library(dplyr)
df <- Salaries %>%
  filter(rank == "AsstProf") %>%
  group_by(sex) %>%
  summarize(n = n(),
            mean = mean(salary), 
            sd = sd(salary),
            se = sd / sqrt(n),
            ci = qt(0.975, df = n - 1) * se)

df

# create and save the plot
library(ggplot2)
p <- ggplot(df, 
            aes(x = sex, y = mean, group=1)) +
  geom_point(size = 4) +
  geom_line() +
  scale_y_continuous(limits = c(77000, 82000),
                     label = scales::dollar) +
  labs(title = "Mean salary differences by gender",
       subtitle = "9-mo academic salary in 2007-2008",
       caption = paste("source: Fox J. and Weisberg, S. (2011)",
                       "An R Companion to Applied Regression,", 
                       "Second Edition Sage"),
       x = "Gender",
       y = "Salary") +
  scale_y_continuous(labels = scales::dollar)


# plot in a narrow range of y
p + scale_y_continuous(limits=c(77000, 82000))
        

# plot in a wide range of y
p + scale_y_continuous(limits = c(0, 125000))
        

# plot with confidence limits
p +  geom_errorbar(aes(ymin = mean - ci, 
                       ymax = mean + ci), 
                       width = .1) +
  ggplot2::annotate("text", 
           label = "I-bars are 95% \nconfidence intervals", 
           x=2, 
           y=73500,
           fontface = "italic",
           size = 3)

