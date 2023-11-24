
library(ggplot2)

# stacked bar chart
ggplot(mpg, aes(x = class, fill = drv)) + 
  geom_bar(position = "stack")


library(ggplot2)

# grouped bar plot
ggplot(mpg, aes(x = class, fill = drv)) + 
  geom_bar(position = "dodge")


library(ggplot2)

# grouped bar plot preserving zero count bars
ggplot(mpg, aes(x = class, fill = drv)) + 
  geom_bar(position = position_dodge(preserve = "single"))


library(ggplot2)

# bar plot, with each bar representing 100%
ggplot(mpg, aes(x = class, fill = drv)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")


library(ggplot2)

# bar plot, with each bar representing 100%, 
# reordered bars, and better labels and colors
library(scales)
ggplot(mpg, 
       aes(x = factor(class,
                      levels = c("2seater", "subcompact", 
                                "compact", "midsize", 
                                "minivan", "suv", "pickup")),
           fill = factor(drv, 
                         levels = c("f", "r", "4"),
                         labels = c("front-wheel", 
                                    "rear-wheel", 
                                    "4-wheel")))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Percent", 
       fill="Drive Train",
       x = "Class",
       title = "Automobile Drive by Class") +
  theme_minimal()


# create a summary dataset
library(dplyr)
plotdata <- mpg %>%
  group_by(class, drv) %>%
  summarize(n = n()) %>% 
  mutate(pct = n/sum(n),
         lbl = scales::percent(pct))
plotdata


# create segmented bar chart
# adding labels to each segment

ggplot(plotdata, 
       aes(x = factor(class,
                      levels = c("2seater", "subcompact", 
                                 "compact", "midsize", 
                                 "minivan", "suv", "pickup")),
           y = pct,
          fill = factor(drv, 
                        levels = c("f", "r", "4"),
                        labels = c("front-wheel", 
                                   "rear-wheel", 
                                   "4-wheel")))) + 
  geom_bar(stat = "identity",
           position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  geom_text(aes(label = lbl), 
            size = 3, 
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Percent", 
       fill="Drive Train",
       x = "Class",
       title = "Automobile Drive by Class") +
  theme_minimal()


library(ggplot2)
data(Salaries, package="carData")

# simple scatterplot
ggplot(Salaries, 
       aes(x = yrs.since.phd, y = salary)) +
  geom_point()


# enhanced scatter plot
ggplot(Salaries, 
       aes(x = yrs.since.phd, y = salary)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha=.8) +
  scale_y_continuous(label = scales::dollar, 
                     limits = c(50000, 250000)) +
  scale_x_continuous(breaks = seq(0, 60, 10), 
                     limits=c(0, 60)) + 
  labs(x = "Years Since PhD",
       y = "",
       title = "Experience vs. Salary",
       subtitle = "9-month salary for 2008-2009")


# scatterplot with linear fit line
ggplot(Salaries, aes(x = yrs.since.phd, y = salary)) +
  geom_point(color= "steelblue") +
  geom_smooth(method = "lm")



# scatterplot with quadratic line of best fit
ggplot(Salaries, aes(x = yrs.since.phd, y = salary)) +
  geom_point(color= "steelblue") +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 2), 
              color = "indianred3")


# scatterplot with loess smoothed line
ggplot(Salaries, aes(x = yrs.since.phd, y = salary)) +
  geom_point(color= "steelblue") +
  geom_smooth(color = "tomato")



# scatterplot with loess smoothed line 
# and better labeling and color
ggplot(Salaries, 
       aes(x = yrs.since.phd, y = salary)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha=.6) +
  geom_smooth(size = 1.5,
              color = "darkgrey") +
  scale_y_continuous(label = scales::dollar, 
                     limits=c(50000, 250000)) +
  scale_x_continuous(breaks = seq(0, 60, 10), 
                     limits=c(0, 60)) + 
  labs(x = "Years Since PhD",
       y = "",
       title = "Experience vs. Salary",
       subtitle = "9-month salary for 2008-2009") +
  theme_minimal()


data(gapminder, package="gapminder")

# Select US cases
library(dplyr)
plotdata <- filter(gapminder, country == "United States")

# simple line plot
ggplot(plotdata, aes(x = year, y = lifeExp)) +
  geom_line() 


# line plot with points
# and improved labeling
ggplot(plotdata, aes(x = year, y = lifeExp)) +
  geom_line(size = 1.5, 
            color = "lightgrey") +
  geom_point(size = 3, 
             color = "steelblue") +
  labs(y = "Life Expectancy (years)", 
       x = "Year",
       title = "Life expectancy changes over time",
       subtitle = "United States (1952-2007)",
       caption = "Source: http://www.gapminder.org/data/")


data(Salaries, package="carData")

# calculate mean salary for each rank
library(dplyr)
plotdata <- Salaries %>%
  group_by(rank) %>%
  summarize(mean_salary = mean(salary))

# plot mean salaries
ggplot(plotdata, aes(x = rank, y = mean_salary)) +
  geom_bar(stat = "identity")


# plot mean salaries in a more attractive fashion
library(scales)
ggplot(plotdata, 
       aes(x = factor(rank,
                      labels = c("Assistant\nProfessor",
                                 "Associate\nProfessor",
                                 "Full\nProfessor")), 
                      y = mean_salary)) +
  geom_bar(stat = "identity", 
           fill = "cornflowerblue") +
  geom_text(aes(label = dollar(mean_salary)), 
            vjust = -0.25) +
  scale_y_continuous(breaks = seq(0, 130000, 20000), 
                     label = dollar) +
  labs(title = "Mean Salary by Rank", 
       subtitle = "9-month academic salary for 2008-2009",
       x = "",
       y = "")


# plot the distribution of salaries 
# by rank using kernel density plots
ggplot(Salaries, aes(x = salary, fill = rank)) +
  geom_density(alpha = 0.4) +
  labs(title = "Salary distribution by rank")


# plot the distribution of salaries by rank using boxplots
ggplot(Salaries, aes(x = rank, y = salary)) +
  geom_boxplot() +
  labs(title = "Salary distribution by rank")


# plot the distribution of salaries by rank using boxplots
ggplot(Salaries, aes(x = rank, y = salary)) +
  geom_boxplot(notch = TRUE, 
               fill = "cornflowerblue", 
               alpha = .7) +
  labs(title = "Salary distribution by rank")


# plot the distribution of salaries 
# by rank using violin plots
ggplot(Salaries, aes(x = rank, y = salary)) +
  geom_violin() +
  labs(title = "Salary distribution by rank")


# plot the distribution using violin and boxplots
ggplot(Salaries, aes(x = rank, y = salary)) +
  geom_violin(fill = "cornflowerblue") +
  geom_boxplot(width = .15, 
               fill = "orange",
               outlier.color = "orange",
               outlier.size = 2) + 
  labs(title = "Salary distribution by rank")


# create ridgeline graph
library(ggplot2)
library(ggridges)

ggplot(mpg, 
       aes(x = cty, y = class, fill = class)) +
  geom_density_ridges() + 
  theme_ridges() +
  labs("Highway mileage by auto class") +
  theme(legend.position = "none")


# calculate means, standard deviations,
# standard errors, and 95% confidence 
# intervals by rank
library(dplyr)
plotdata <- Salaries %>%
  group_by(rank) %>%
  summarize(n = n(),
         mean = mean(salary),
         sd = sd(salary),
         se = sd / sqrt(n),
         ci = qt(0.975, df = n - 1) * sd / sqrt(n))


knitr::kable(plotdata, caption = "Plot data")

# plot the means and standard errors
ggplot(plotdata, 
       aes(x = rank, 
           y = mean, 
           group = 1)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se), 
                width = .1)


# calculate means and standard errors by rank and sex
plotdata <- Salaries %>%
  group_by(rank, sex) %>%
  summarize(n = n(),
            mean = mean(salary),
            sd = sd(salary),
            se = sd/sqrt(n))

# plot the means and standard errors by sex
ggplot(plotdata, aes(x = rank,
                     y = mean, 
                     group=sex, 
                     color=sex)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin  =mean - se, 
                    ymax = mean+se), 
                width = .1)


# plot the means and standard errors by sex (dodged)
pd <- position_dodge(0.2)
ggplot(plotdata, 
       aes(x = rank, 
           y = mean, 
           group=sex, 
           color=sex)) +
  geom_point(position = pd, 
             size = 3) +
  geom_line(position = pd,
            size = 1) +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se), 
                width = .1, 
                position= pd)


# improved means/standard error plot
pd <- position_dodge(0.2)
ggplot(plotdata, 
       aes(x = factor(rank, 
                      labels = c("Assistant\nProfessor",
                                 "Associate\nProfessor",
                                 "Full\nProfessor")), 
                      y = mean, group=sex, color=sex)) +
  geom_point(position=pd, 
             size=3) +
  geom_line(position=pd, 
            size = 1) +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se), 
                width = .1, 
                position=pd, 
                size=1) +
  scale_y_continuous(label = scales::dollar) +
  scale_color_brewer(palette="Set1") +
  theme_minimal() +
  labs(title = "Mean salary by rank and sex",
       subtitle = "(mean +/- standard error)",
       x = "", 
       y = "",
       color = "Gender")



# plot the distribution of salaries 
# by rank using strip plots
ggplot(Salaries, aes(y = rank, x = salary)) +
  geom_point() + 
  labs(title = "Salary distribution by rank")


# plot the distribution of salaries
# by rank using jittering
ggplot(Salaries, aes(y = rank, x = salary)) +
  geom_jitter() + 
  labs(title = "Salary distribution by rank")


# plot the distribution of salaries 
# by rank using jittering
library(scales)
ggplot(Salaries, 
       aes(y = factor(rank,
                      labels = c("Assistant\nProfessor",
                                 "Associate\nProfessor",
                                 "Full\nProfessor")), 
           x = salary, color = rank)) +
  geom_jitter(alpha = 0.7) + 
  scale_x_continuous(label = dollar) +
  labs(title = "Academic Salary by Rank", 
       subtitle = "9-month salary for 2008-2009",
       x = "",
       y = "") +
  theme_minimal() +
  theme(legend.position = "none")


# plot the distribution of salaries 
# by rank using jittering
library(scales)
ggplot(Salaries, 
       aes(x = factor(rank,
                      labels = c("Assistant\nProfessor",
                                 "Associate\nProfessor",
                                 "Full\nProfessor")), 
           y = salary, color = rank)) +
  geom_boxplot(size=1,
               outlier.shape = 1,
               outlier.color = "black",
               outlier.size  = 3) +
  geom_jitter(alpha = 0.5, 
              width=.2) + 
  scale_y_continuous(label = dollar) +
  labs(title = "Academic Salary by Rank", 
       subtitle = "9-month salary for 2008-2009",
       x = "",
       y = "") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()


# plot the distribution of salaries 
# by rank using jittering
library(ggpol)
library(scales)
ggplot(Salaries, 
       aes(x = factor(rank,
                      labels = c("Assistant\nProfessor",
                                 "Associate\nProfessor",
                                 "Full\nProfessor")), 
           y = salary, 
           fill=rank)) +
  geom_boxjitter(color="black",
                 jitter.color = "darkgrey",
                 errorbar.draw = TRUE) +
  scale_y_continuous(label = dollar) +
  labs(title = "Academic Salary by Rank", 
       subtitle = "9-month salary for 2008-2009",
       x = "",
       y = "") +
  theme_minimal() +
  theme(legend.position = "none")


data(gapminder, package="gapminder")

# subset Asian countries in 2007
library(dplyr)
plotdata <- gapminder %>%
  filter(continent == "Asia" & 
           year == 2007)

# basic Cleveland plot of life expectancy by country
ggplot(plotdata, 
       aes(x= lifeExp, y = country)) +
  geom_point()


# Sorted Cleveland plot
ggplot(plotdata, aes(x=lifeExp, 
                     y=reorder(country, lifeExp))) +
  geom_point()


## ----cleveland3, fig.cap="Fancy Cleveland plot", message=FALSE, warning=FALSE----

# Fancy Cleveland plot
ggplot(plotdata, aes(x=lifeExp, 
                     y=reorder(country, lifeExp))) +
  geom_point(color="blue", size = 2) +
  geom_segment(aes(x = 40, 
               xend = lifeExp, 
               y = reorder(country, lifeExp), 
               yend = reorder(country, lifeExp)),
               color = "lightgrey") +
  labs (x = "Life Expectancy (years)",
        y = "",
        title = "Life Expectancy by Country",
        subtitle = "GapMinder data for Asia - 2007") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

