library(ggplot2)
ggplot(economics, aes(x = date, y = psavert)) +
  geom_line() +
  labs(title = "Personal Savings Rate",
       x = "Date",
       y = "Personal Savings Rate")

library(ggplot2)
library(scales)
ggplot(economics, aes(x = date, y = psavert)) +
  geom_line(color = "indianred3", 
            size=1 ) +
  geom_smooth() +
  scale_x_date(date_breaks = '5 years', 
               labels = date_format("%b-%y")) +
  labs(title = "Personal Savings Rate",
       subtitle = "1967 to 2015",
       x = "",
       y = "Personal Savings Rate") +
  theme_minimal()

# multivariate time series

# one time install
# install.packages("quantmod")

library(quantmod)
library(dplyr)

# get apple (AAPL) closing prices
apple <- getSymbols("AAPL", 
                    return.class = "data.frame", 
                    from="2023-01-01")

apple <- AAPL %>% 
  mutate(Date = as.Date(row.names(.))) %>%
  select(Date, AAPL.Close) %>%
  rename(Close = AAPL.Close) %>%
  mutate(Company = "Apple")

# get Meta (META) closing prices
meta <- getSymbols("META", 
                   return.class = "data.frame", 
                   from="2023-01-01")

meta <- META %>% 
  mutate(Date = as.Date(row.names(.))) %>%
  select(Date, META.Close) %>%
  rename(Close = META.Close) %>%
  mutate(Company = "Meta")

# combine data for both companies
mseries <- rbind(apple, meta)

# plot data
library(ggplot2)
ggplot(mseries, 
       aes(x=Date, y= Close, color=Company)) + 
  geom_line(size=1) +
  scale_x_date(date_breaks = '1 month', 
               labels = scales::date_format("%b")) +
  scale_y_continuous(limits = c(120, 280), 
                     breaks = seq(120, 280, 20),
                     labels = scales::dollar) +
  labs(title = "NASDAQ Closing Prices",
       subtitle = "Jan - June 2023",
       caption = "source: Yahoo Finance",
       y = "Closing Price") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2")


library(ggalt)
library(tidyr)
library(dplyr)

# load data
data(gapminder, package = "gapminder")

# subset data
plotdata_long <- filter(gapminder,
                        continent == "Americas" &
                        year %in% c(1952, 2007)) %>%
  select(country, year, lifeExp)

# convert data to wide format
plotdata_wide <- pivot_wider(plotdata_long, 
                             names_from = year, 
                             values_from = lifeExp)
names(plotdata_wide) <- c("country", "y1952", "y2007")

# create dumbbell plot
ggplot(plotdata_wide, aes(y = country,
                          x = y1952,
                          xend = y2007)) +  
  geom_dumbbell()


# create dumbbell plot
ggplot(plotdata_wide, 
       aes(y = reorder(country, y1952),
           x = y1952,
           xend = y2007)) +  
  geom_dumbbell(size = 1.2,
                size_x = 3, 
                size_xend = 3,
                colour = "grey", 
                colour_x = "red", 
                colour_xend = "blue") +
  theme_minimal() + 
  labs(title = "Change in Life Expectancy",
       subtitle = "1952 to 2007",
       x = "Life Expectancy (years)",
       y = "")

library(CGPfunctions)

# Select Central American countries data 
# for 1992, 1997, 2002, and 2007

df <- gapminder %>%
  filter(year %in% c(1992, 1997, 2002, 2007) &
           country %in% c("Panama", "Costa Rica", 
                          "Nicaragua", "Honduras", 
                          "El Salvador", "Guatemala",
                          "Belize")) %>%
  mutate(year = factor(year),
         lifeExp = round(lifeExp)) 

# create slope graph

newggslopegraph(df, year, lifeExp, country) +
  labs(title="Life Expectancy by Country", 
       subtitle="Central America", 
       caption="source: gapminder")

# basic area chart
ggplot(economics, aes(x = date, y = psavert)) +
  geom_area(fill="lightblue", color="black") +
  labs(title = "Personal Savings Rate",
       x = "Date",
       y = "Personal Savings Rate")

# stacked area chart
data(uspopage, package = "gcookbook")
ggplot(uspopage, aes(x = Year,
                     y = Thousands, 
                     fill = AgeGroup)) +
  geom_area() +
  labs(title = "US Population by age",
       x = "Year",
       y = "Population in Thousands")


# stacked area chart
data(uspopage, package = "gcookbook")
ggplot(uspopage, aes(x = Year,
                     y = Thousands/1000, 
                     fill = forcats::fct_rev(AgeGroup))) +
  geom_area(color = "black") +
  labs(title = "US Population by age",
       subtitle = "1900 to 2002",
       caption = "source: U.S. Census Bureau, 2003, HS-3",
       x = "Year",
       y = "Population in Millions",
       fill = "Age Group") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

# basic stream graph
data(uspopage, package = "gcookbook")
library(ggstream)
ggplot(uspopage, aes(x = Year,
                     y = Thousands/1000, 
                     fill = forcats::fct_rev(AgeGroup))) +
  geom_stream() +
  labs(title = "US Population by age",
       subtitle = "1900 to 2002",
       caption = "source: U.S. Census Bureau, 2003, HS-3",
       x = "Year",
       y = "",
       fill = "Age Group") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank())

# basic stream graph
data(uspopage, package = "gcookbook")
library(ggstream)
ggplot(uspopage, aes(x = Year,
                     y = Thousands/1000, 
                     fill = forcats::fct_rev(AgeGroup))) +
  geom_stream(type="proportional") +
  labs(title = "US Population by age",
       subtitle = "1900 to 2002",
       caption = "source: U.S. Census Bureau, 2003, HS-3",
       x = "Year",
       y = "Proportion",
       fill = "Age Group") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() 

