# create plotly graph.
library(ggplot2)
library(plotly)

p <- ggplot(mpg, aes(x=displ, 
                     y=hwy, 
                     color=class)) +
  geom_point(size=3) +
  labs(x = "Engine displacement",
       y = "Highway Mileage",
       color = "Car Class") +
  theme_bw()

ggplotly(p)


# create plotly graph.
library(ggplot2)
library(plotly)

p <- ggplot(mpg, aes(x=displ, 
                     y=hwy, 
                     color=class,
                     label1 = manufacturer,
                     label2 = model,
                     label3 = year)) +
  geom_point(size=3) +
  labs(x = "Engine displacement",
       y = "Highway Mileage",
       color = "Car Class") +
  theme_bw()

ggplotly(p, tooltip = c("label1", "label2", "label3"))


# create plotly graph.
library(ggplot2)
library(plotly)
library(dplyr)

mpg <- mpg %>%
  mutate(mylabel = paste("This is a", manufacturer, model, "\n",
                         "released in", year, "."))

p <- ggplot(mpg, aes(x=displ, 
                     y=hwy, 
                     color=class,
                     text = mylabel)) +
  geom_point(size=3) +
  labs(x = "Engine displacement",
       y = "Highway Mileage",
       color = "Car Class") +
  theme_bw()

ggplotly(p, tooltip = c("mylabel"))


library(ggplot2)
library(ggiraph)


p <- ggplot(mpg, aes(x=displ, 
                y=hwy, 
                color=class,
                tooltip = manufacturer)) +
  geom_point_interactive()

girafe(ggobj = p)


library(ggplot2)
library(ggiraph)
library(patchwork)

library(dplyr)
mpg <- mpg %>%
  mutate(tooltip = paste(manufacturer, model, class))

p <- ggplot(mpg, aes(x=displ, 
                y=hwy, 
                color=class,
                tooltip = tooltip)) +
  geom_point_interactive()

girafe(ggobj = p, options = list(opts_tooltip(use_fill = TRUE)))


library(patchwork)
p1 <- ggplot(mtcars, aes(x=wt, 
                      y=mpg, 
                      tooltip = rownames(mtcars),
                      data_id = rownames(mtcars))) +
  geom_point_interactive(size=3, alpha =.6) 

p2 <- ggplot(mtcars, aes(x=drat, 
                         y=qsec, 
                         tooltip = rownames(mtcars),
                         data_id = rownames(mtcars))) +
  geom_point_interactive(size = 3, alpha = .6) 

p3 <- ggplot(mtcars, aes(x=cyl,
                         data_id = rownames(mtcars))) +
  geom_bar_interactive() 

p3 <- (p1 | p2)/p3
girafe(code = print (p3)) 


data(gapminder, package="gapminder")

# subset Asian countries
asia <- gapminder %>%
  filter(continent == "Asia") %>%
  select(year, country, lifeExp)

p1 <- ggplot(asia[asia$year == 1982,], 
       aes(y = reorder(country, lifeExp), 
           x=lifeExp,
           tooltip = lifeExp,
           data_id = country)) +
  geom_bar_interactive(stat="identity", 
                       fill="steelblue") +
  labs(y="", x="1982") + 
  theme_minimal()

p2 <- ggplot(asia[asia$year == 2007,], 
             aes(y = reorder(country, lifeExp), 
                 x=lifeExp,
                 tooltip = lifeExp,
                 data_id = country)) +
  geom_bar_interactive(stat="identity", 
                       fill="steelblue") +
  labs(y="", x="2007") + 
  theme_minimal()

p3 <- (p1 | p2) +
  plot_annotation(title = "Life Expectancy in Asia")
girafe(code = print (p3)) 


# create rbokeh graph

# prepare data
data(mtcars)
mtcars$name <- row.names(mtcars)
mtcars$cyl <- factor(mtcars$cyl)

# graph it
library(rbokeh)
figure() %>%
  ly_points(disp, mpg, data=mtcars,
            color = cyl, glyph = cyl,
            hover = list(name, mpg, wt))


# create interactive bar chart
library(rCharts)
hair_eye_male = subset(as.data.frame(HairEyeColor), 
                       Sex == "Male")
n1 <- nPlot(Freq ~ Hair, 
            group = 'Eye', 
            data = hair_eye_male, 
            type = 'multiBarChart'
)
n1$set(width = 600)
n1$show('iframesrc', cdn=TRUE)


# create interactive line chart
library(highcharter)

# prepare data
data(gapminder, package = "gapminder")
library(dplyr)
asia <- gapminder %>%
  filter(continent == "Asia") %>%
  select(year, country, lifeExp)

# convert to long to wide format
library(tidyr)
plotdata <- spread(asia, country, lifeExp)

# generate graph
h <- highchart() %>% 
  hc_xAxis(categories = plotdata$year) %>% 
  hc_add_series(name = "Afghanistan", 
                data = plotdata$Afghanistan) %>% 
  hc_add_series(name = "Bahrain", 
                data = plotdata$Bahrain) %>%
  hc_add_series(name = "Cambodia", 
                data = plotdata$Cambodia) %>%
  hc_add_series(name = "China", 
                data = plotdata$China) %>%
  hc_add_series(name = "India", 
                data = plotdata$India) %>%
  hc_add_series(name = "Iran", 
                data = plotdata$Iran)

h


# customize interactive line chart
h <- h %>%
  hc_title(text = "Life Expectancy by Country",
           margin = 20, 
           align = "left",
           style = list(color = "steelblue")) %>% 
  hc_subtitle(text = "1952 to 2007",
              align = "left",
              style = list(color = "#2b908f", 
                           fontWeight = "bold")) %>% 
  hc_credits(enabled = TRUE, # add credits
             text = "Gapminder Data",
             href = "http://gapminder.com") %>% 
  hc_legend(align = "left", 
            verticalAlign = "top",
            layout = "vertical", 
            x = 0, 
            y = 100) %>%
  hc_tooltip(crosshairs = TRUE, 
             backgroundColor = "#FCFFC5",
             shared = TRUE, 
             borderWidth = 4) %>% 
  hc_exporting(enabled = TRUE)

h

