# customize numerical x and y axes
library(ggplot2)
ggplot(mpg, aes(x=displ, y=hwy)) +
  geom_point() +
  scale_x_continuous(breaks = seq(1, 7, 1), 
                     limits=c(1, 7)) +
  scale_y_continuous(breaks = seq(10, 45, 5), 
                     limits=c(10, 45))

# create some data
set.seed(1234)
df <- data.frame(xaxis = rnorm(50, 100000, 50000),
                 yaxis = runif(50, 0, 1),
                 pointsize = rnorm(50, 1000, 1000))
library(ggplot2)

# plot the axes and legend with formats
ggplot(df, aes(x = xaxis, 
               y = yaxis, 
               size=pointsize)) + 
  geom_point(color = "cornflowerblue",
             alpha = .6) +
  scale_x_continuous(label = scales::comma) +
  scale_y_continuous(label = scales::percent) +
  scale_size(range = c(1,10), # point size range
             label = scales::dollar)


library(ggplot2)
# customize categorical x axis
ggplot(mpg, aes(x = class)) +
  geom_bar(fill = "steelblue") +
  scale_x_discrete(limits = c("pickup", "suv", "minivan", 
                             "midsize", "compact", "subcompact", 
                             "2seater"),
                   labels = c("Pickup\nTruck", 
                              "Sport Utility\nVehicle", 
                              "Minivan", "Mid-size", "Compact", 
                              "Subcompact", "2-Seater"))
                    

library(ggplot2)
# customize date scale on x axis
ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line(color="darkgreen") +
  scale_x_date(date_breaks = "5 years",
               date_labels = "%b-%y")
  

# specify fill color manually
library(ggplot2)
ggplot(diamonds, aes(x = cut, fill = clarity)) +
  geom_bar() +
  scale_fill_manual(values = c("darkred", "steelblue", 
                               "darkgreen", "gold",
                               "brown", "purple", 
                               "grey", "khaki4"))


library(RColorBrewer)
display.brewer.all()


# use an ColorBrewer fill palette
ggplot(diamonds, aes(x = cut, fill = clarity)) +
  geom_bar() +
  scale_fill_brewer(palette = "Dark2") 


# Use a viridis fill palette
ggplot(diamonds, aes(x = cut, fill = clarity)) +
  geom_bar() + 
  scale_fill_viridis_d()



# one time install
# install.packages("extrafont")
# library(extrafont)
# font_import()

# see what fonts are now available
fonts()


# specify new font
library(extrafont)
ggplot(mpg, aes(x = displ, y=hwy)) +
  geom_point() +
  labs(title = "Diplacement by Highway Mileage",
       subtitle = "MPG dataset") +
  theme(text = element_text(size = 16, family = "Comic Sans MS"))

# place legend on top
ggplot(mpg, 
       aes(x = displ, y=hwy, color = class)) +
  geom_point(size = 4) +
  labs(title = "Diplacement by Highway Mileage") + 
  theme_minimal() +
  theme(legend.position = "top") 

# change the default legend title
ggplot(mpg, 
       aes(x = displ, y=hwy, color = class)) +
  geom_point(size = 4) +
  labs(title = "Diplacement by Highway Mileage",
       color = "Automobile\nClass") + 
  theme_minimal() +
  theme(legend.title.align=0.5)

# add plot labels
ggplot(mpg, 
       aes(x = displ, y=hwy, 
           color = class,
           shape = factor(year))) +
  geom_point(size = 3, 
             alpha = .5) +
  labs(title = "Mileage by engine displacement",
       subtitle = "Data from 1999 and 2008",
       caption = "Source: EPA (http://fueleconomy.gov)",
       x = "Engine displacement (litres)",
       y = "Highway miles per gallon",
       color = "Car Class",
       shape = "Year") + 
  theme_minimal()

# basic scatterplot
data(mtcars)
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()


# scatterplot with labels
data(mtcars)
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_text(label = row.names(mtcars))


# scatterplot with non-overlapping labels
data(mtcars)
library(ggrepel)
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_text_repel(label = row.names(mtcars), 
                  size=3)



# scatterplot with explanatory text
data(mtcars)
library(ggrepel)
txt <- paste("The relationship between car weight",
              "and mileage appears to be roughly linear",
              sep = "\n")
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "red") +
  geom_text_repel(label = row.names(mtcars), 
                  size=3) +
  ggplot2::annotate("text", 
                    6, 30, 
                    label=txt,
                    color = "red",
                    hjust = 1) +
  theme_bw()


# add annotation line and text label
min_cty <- min(mpg$cty)
mean_hwy <- mean(mpg$hwy)
ggplot(mpg, 
       aes(x = cty, y=hwy, color=drv)) +
  geom_point(size = 3) +
  geom_hline(yintercept = mean_hwy,
             color = "darkred",
             linetype = "dashed") +
  ggplot2::annotate("text", 
           min_cty, 
           mean_hwy + 1, 
           label = "Mean",
           color = "darkred") +
  labs(title = "Mileage by drive type",
       x = "City miles per gallon",
       y = "Highway miles per gallon",
       color = "Drive")
    

# highlight a set of points
library(ggplot2)
library(gghighlight)
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point(color = "red",
             size=2) +
  gghighlight(class == "midsize")


# highlight a single bar
library(gghighlight)
ggplot(mpg, aes(x = class)) +
  geom_bar(fill = "red") +
  gghighlight(class == "midsize")


# create graph
data(Salaries, package = "carData")
p <- ggplot(Salaries, aes(x = rank, fill = sex)) +
  geom_bar() +
  facet_wrap(~discipline) +
  labs(title = "Academic Rank by Gender and Discipline",
       x = "Rank",
       y = "Frequency",
       fill = "Gender")
p

p +
  theme(text = element_text(color = "navy"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.y = element_line(color = "grey", 
                                          linetype = "dashed"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(fill = "white", color="grey"))


# create basic plot
library(ggplot2)
p <- ggplot(mpg, 
            aes(x = displ, y=hwy, 
                color = class)) +
  geom_point(size = 3, 
             alpha = .5) +
  labs(title = "Mileage by engine displacement",
       subtitle = "Data from 1999 and 2008",
       caption = "Source: EPA (http://fueleconomy.gov)",
       x = "Engine displacement (litres)",
       y = "Highway miles per gallon",
       color = "Car Class") 

# display graph
p


# add economist theme
library(ggthemes)
p + theme_economist() 


# add fivethirtyeight theme
p + theme_fivethirtyeight()


# add wsj theme
p + theme_wsj(base_size=8)


# add few theme
p + theme_few() + scale_color_few()


# add few theme
library(hrbrthemes)
p + theme_ipsum()


# one time install
# install.packages("remotes")
# remotes::install_github('cttobin/ggthemr')


# set graphs to the flat dark theme
library(ggthemr)
ggthemr("flat dark")
p
ggthemr_reset()


data(Salaries, package = "carData")
library(ggplot2)
library(patchwork)

# boxplot of salary by sex
p1 <- ggplot(Salaries, aes(x = sex, y = salary, fill=sex)) +
  geom_boxplot()

# scatterplot of salary by experience and sex
p2 <- ggplot(Salaries, 
             aes(x = yrs.since.phd, y = salary, color=sex)) +
  geom_point()

# barchart of rank and sex
p3 <- ggplot(Salaries, aes(x = rank, fill = sex)) +
    geom_bar()

# combine the graphs and tweak the theme and colors
(p1 | p2)/p3 +
  plot_annotation(title = "Salaries for college professors") &
  theme_minimal() &
  scale_fill_viridis_d() &
  scale_color_viridis_d()

