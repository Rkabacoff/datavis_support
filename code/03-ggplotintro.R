
# load the data
url <- "https://tinyurl.com/mtktm8e5"
insurance <- read.csv(url)

# create an obesity variable
insurance$obese <- ifelse(insurance$bmi >= 30, 
                          "obese", "not obese")

# specify dataset and mapping
library(ggplot2)
ggplot(data = insurance,
       mapping = aes(x = age, y = expenses))

# add points
ggplot(data = insurance,
       mapping = aes(x = age, y = expenses)) +
  geom_point()

# make points blue, larger, and semi-transparent
ggplot(data = insurance,
       mapping = aes(x = age, y = expenses)) +
  geom_point(color = "cornflowerblue",
             alpha = .7,
             size = 2)

# add a line of best fit.
ggplot(data = insurance,
       mapping = aes(x = age, y = expenses)) +
  geom_point(color = "cornflowerblue",
             alpha = .5,
             size = 2) +
  geom_smooth(method = "lm")

# indicate sex using color
ggplot(data = insurance,
       mapping = aes(x = age, 
                     y = expenses,
                     color = smoker)) +
  geom_point(alpha = .5,
             size = 2) +
  geom_smooth(method = "lm", 
              se = FALSE, 
              size = 1.5)

# modify the x and y axes and specify the colors to be used
ggplot(data = insurance,
       mapping = aes(x = age, 
                     y = expenses,
                     color = smoker)) +
  geom_point(alpha = .5,
             size = 2) +
  geom_smooth(method = "lm", 
              se = FALSE, 
              size = 1.5) +
  scale_x_continuous(breaks = seq(0, 70, 10)) +
  scale_y_continuous(breaks = seq(0, 60000, 20000),
                     label = scales::dollar) +
  scale_color_manual(values = c("indianred3", 
                                "cornflowerblue"))

# reproduce plot for each obsese and non-obese individuals
ggplot(data = insurance,
       mapping = aes(x = age, 
                     y = expenses,
                     color = smoker)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm", 
              se = FALSE) +
  scale_x_continuous(breaks = seq(0, 70, 10)) +
  scale_y_continuous(breaks = seq(0, 60000, 20000),
                     label = scales::dollar) +
  scale_color_manual(values = c("indianred3", 
                                "cornflowerblue")) +
  facet_wrap(~obese)

# add informative labels
ggplot(data = insurance,
       mapping = aes(x = age, 
                     y = expenses,
                     color = smoker)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm", 
              se = FALSE) +
  scale_x_continuous(breaks = seq(0, 70, 10)) +
  scale_y_continuous(breaks = seq(0, 60000, 20000),
                     label = scales::dollar) +
  scale_color_manual(values = c("indianred3", 
                                "cornflowerblue")) +
  facet_wrap(~obese) +
  labs(title = "Relationship between patient demographics and medical costs",
       subtitle = "US Census Bureau 2013",
       caption = "source: http://mosaic-web.org/",
       x = " Age (years)",
       y = "Annual expenses",
       color = "Smoker?")

# use a minimalist theme
ggplot(data = insurance,
       mapping = aes(x = age, 
                     y = expenses,
                     color = smoker)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm", 
              se = FALSE) +
  scale_x_continuous(breaks = seq(0, 70, 10)) +
  scale_y_continuous(breaks = seq(0, 60000, 20000),
                     label = scales::dollar) +
  scale_color_manual(values = c("indianred3", 
                                "cornflowerblue")) +
  facet_wrap(~obese) +
  labs(title = "Relationship between age and medical expenses",
       subtitle = "US Census Data 2013",
       caption = "source: https://github.com/dataspelunking/MLwR",
       x = " Age (years)",
       y = "Medical Expenses",
       color = "Smoker?") +
  theme_minimal()

# placing color mapping in the ggplot function
ggplot(insurance,
       aes(x = age, 
           y = expenses,
           color = smoker)) +
  geom_point(alpha = .5,
             size = 2) +
  geom_smooth(method = "lm",
              se = FALSE, 
              size = 1.5)

# placing color mapping in the geom_point function
ggplot(insurance,
       aes(x = age, 
           y = expenses)) +
  geom_point(aes(color = smoker),
             alpha = .5,
             size = 2) +
  geom_smooth(method = "lm",
              se = FALSE, 
              size = 1.5)


# create scatterplot and save it
myplot <- ggplot(data = insurance,
                  aes(x = age, y = expenses)) +
             geom_point()

# plot the graph
myplot

# make the points larger and blue
# then print the graph
myplot <- myplot + geom_point(size = 2, color = "blue")
myplot

# print the graph with a title and line of best fit
# but don't save those changes
myplot + geom_smooth(method = "lm") +
  labs(title = "Mildly interesting graph")

# print the graph with a black and white theme
# but don't save those changes
myplot + theme_bw()


