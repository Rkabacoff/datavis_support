
library(ggplot2)
data(Salaries, package="carData")

# plot experience vs. salary
ggplot(Salaries, 
       aes(x = yrs.since.phd, y = salary)) +
  geom_point() + 
  labs(title = "Academic salary by years since degree")

# plot experience vs. salary (color represents rank)
ggplot(Salaries, aes(x = yrs.since.phd, 
                     y = salary, 
                     color=rank)) +
  geom_point() +
  labs(title = "Academic salary by rank and years since degree")


# plot experience vs. salary 
# (color represents rank, shape represents sex)
ggplot(Salaries, aes(x = yrs.since.phd, 
                     y = salary, 
                     color = rank, 
                     shape = sex)) +
  geom_point(size = 3, alpha = .6) +
  labs(title = "Academic salary by rank, sex, and years since degree")


library(ggplot2)
data(Salaries, package="carData")

# plot experience vs. salary 
# (color represents rank and size represents service)
ggplot(Salaries, aes(x = yrs.since.phd, 
                     y = salary, 
                     color = rank, 
                     size = yrs.service)) +
  geom_point(alpha = .6) +
  labs(title = paste0("Academic salary by rank, years of service, ",
                      "and years since degree"))


# plot experience vs. salary with 
# fit lines (color represents sex)
ggplot(Salaries, 
       aes(x = yrs.since.phd, 
           y = salary, 
           color = sex)) +
  geom_point(alpha = .4, 
             size=3) +
  geom_smooth(se=FALSE, 
              method="lm", 
              formula=y~poly(x,2), 
              size = 1.5) +
  labs(x = "Years Since Ph.D.",
       title = "Academic Salary by Sex and Years Experience",
       subtitle = "9-month salary for 2008-2009",
       y = "",
       color = "Sex") +
  scale_y_continuous(label = scales::dollar) +
  scale_color_brewer(palette="Set1") +
  theme_minimal()


# plot salary histograms by rank
ggplot(Salaries, aes(x = salary)) +
  geom_histogram() +
  facet_wrap(~rank, ncol = 1) +
  labs(title = "Salary histograms by rank")


# plot salary histograms by rank and sex
ggplot(Salaries, aes(x = salary/1000)) +
  geom_histogram() +
  facet_grid(sex ~ rank) +
  labs(title = "Salary histograms by sex and rank",
       x = "Salary ($1000)")


# plot salary by years of experience by sex and discipline
ggplot(Salaries, 
       aes(x=yrs.since.phd, y = salary, color=sex)) +
  geom_point() +
  geom_smooth(method="lm", 
              se=FALSE) +
  facet_wrap(~discipline, 
             ncol = 1) 


# plot salary by years of experience by sex and discipline
ggplot(Salaries, aes(x=yrs.since.phd, 
                     y = salary, 
                     color=sex)) +
  geom_point(size = 2, 
             alpha=.5) +
  geom_smooth(method="lm", 
              se=FALSE,
              size = 1.5) +
  facet_wrap(~factor(discipline, 
                     labels = c("Theoretical", "Applied")), 
             ncol = 1) +
  scale_y_continuous(labels = scales::dollar) + 
  theme_minimal() +
  scale_color_brewer(palette="Set1") +
  labs(title = paste0("Relationship of salary and years ",
                      "since degree by sex and discipline"),
       subtitle = "9-month salary for 2008-2009",
       color = "Gender",
       x = "Years since Ph.D.",
       y = "Academic Salary")


# plot life expectancy by year separately 
# for each country in the Americas
data(gapminder, package = "gapminder")

# Select the Americas data
plotdata <- dplyr::filter(gapminder, 
                          continent == "Americas")

# plot life expectancy by year, for each country
ggplot(plotdata, aes(x=year, y = lifeExp)) +
  geom_line(color="grey") +
  geom_point(color="blue") +
  facet_wrap(~country) + 
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1)) +
  labs(title = "Changes in Life Expectancy",
       x = "Year",
       y = "Life Expectancy") 

