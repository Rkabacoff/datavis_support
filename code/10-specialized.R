# basic 3-D scatterplot
library(scatterplot3d)
with(mtcars, {
   scatterplot3d(x = disp,
                 y = wt, 
                 z = mpg,
                 main="3-D Scatterplot Example 1")
})


library(scatterplot3d)
with(mtcars, {
  scatterplot3d(x = disp, 
                y = wt, 
                z = mpg, 
                # filled blue circles
                color="blue", 
                pch = 19, 
                # lines to the horizontal plane
                type = "h",
                main = "3-D Scatterplot Example 2",
                xlab = "Displacement (cu. in.)",
                ylab = "Weight (lb/1000)",
                zlab = "Miles/(US) Gallon")
})


library(scatterplot3d)
with(mtcars, {
  s3d <- scatterplot3d(
    x = disp, 
    y = wt, 
    z = mpg,
    color = "blue", 
    pch = 19,      
    type = "h",
    main = "3-D Scatterplot Example 3",
    xlab = "Displacement (cu. in.)",
    ylab = "Weight (lb/1000)",
    zlab = "Miles/(US) Gallon")
  
  # convert 3-D coords to 2D projection
  s3d.coords <- s3d$xyz.convert(disp, wt, mpg) 
  
  # plot text with 50% shrink and place to right of points
  text(s3d.coords$x, 
       s3d.coords$y,   
       labels = row.names(mtcars),  
       cex = .5, 
       pos = 4)
})


library(scatterplot3d)

# create column indicating point color
mtcars$pcolor[mtcars$cyl == 4] <- "red"
mtcars$pcolor[mtcars$cyl == 6] <- "blue"
mtcars$pcolor[mtcars$cyl == 8] <- "darkgreen"

with(mtcars, {
    s3d <- scatterplot3d(
      x = disp, 
      y = wt, 
      z = mpg,
      color = pcolor, 
      pch = 19, 
      type = "h", 
      lty.hplot = 2, 
      scale.y = .75,
      main = "3-D Scatterplot Example 4",
      xlab = "Displacement (cu. in.)",
      ylab = "Weight (lb/1000)",
      zlab = "Miles/(US) Gallon")
    
     s3d.coords <- s3d$xyz.convert(disp, wt, mpg)
     text(s3d.coords$x, 
          s3d.coords$y, 
          labels = row.names(mtcars), 
          pos = 4, 
          cex = .5)  
     
# add the legend
legend(# top left and indented
       "topleft", inset=.05,
       # suppress legend box, shrink text 50%
       bty="n", cex=.5, 
       title="Number of Cylinders",
       c("4", "6", "8"), 
       fill=c("red", "blue", "darkgreen"))
})


library(car)
with(mtcars,
     scatter3d(disp, wt, mpg))

library(car)
with(mtcars,
     scatter3d(disp, wt, mpg,
               axis.col = c("black", "black", "black"),
               groups = factor(am),
               surface.col = c("red", "blue"),
               col = c("red", "blue"),
               text.col = "grey",
               id = list(n=nrow(mtcars),
                         labels=rownames(mtcars)),
               surface = FALSE,
               xlab = "Displacement",
               ylab = "Weight",
               zlab = "Miles Per Gallon"))


# create a bubble plot
data(mtcars)
library(ggplot2)
ggplot(mtcars, 
       aes(x = wt, y = mpg, size = hp)) +
  geom_point()

# create a bubble plot with modifications
ggplot(mtcars, 
       aes(x = wt, y = mpg, size = hp)) +
  geom_point(alpha = .5, 
             fill="cornflowerblue", 
             color="black", 
             shape=21) +
  scale_size_continuous(range = c(1, 14)) +
  labs(title = "Auto mileage by weight and horsepower",
       subtitle = "Motor Trend US Magazine (1973-74 models)",
       x = "Weight (1000 lbs)",
       y = "Miles/(US) gallon",
       size = "Gross horsepower") 

# create a biplot
# load data
data(mtcars)

# fit a principal components model
fit <- prcomp(x = mtcars, 
              center = TRUE, 
              scale = TRUE)

# plot the results
library(factoextra)
fviz_pca(fit, 
         repel = TRUE, 
         labelsize = 3) + 
  theme_bw() +
  labs(title = "Biplot of mtcars data")


# input data
library(readr)
titanic <- read_csv("titanic.csv")

# summarize data
library(dplyr)
titanic_table <- titanic %>%
  group_by(Class, Sex, Survived) %>%
  count()

# convert survived to a factor with labels
titanic_table$Survived <- factor(titanic_table$Survived, 
                                 levels = c("Yes", "No"))
# view the first 6 cases
head(titanic_table)

library(ggalluvial)
ggplot(titanic_table,
       aes(axis1 = Class,
           axis2 = Sex,
           axis3 = Survived,
           y = n)) +
  geom_alluvium(aes(fill = Class)) +
  geom_stratum() +
  geom_text(stat = "stratum", 
            aes(label = after_stat(stratum)))

library(dplyr)
data(mtcars)
mtcars_table <- mtcars %>%
  mutate(am = factor(am, labels = c("Auto", "Man")),
         cyl = factor(cyl),
         gear = factor(gear),
         carb = factor(carb)) %>%
  group_by(cyl, gear, carb, am) %>%
  count()

head(mtcars_table)

ggplot(mtcars_table,
       aes(axis1 = carb,
           axis2 = cyl,
           axis3 = gear,
           axis4 = am,
           y = n)) +
  geom_alluvium(aes(fill = carb), color="black") +
  geom_stratum(alpha=.8) +
  geom_text(stat = "stratum", 
            aes(label = after_stat(stratum))) + 
  scale_x_discrete(limits = c("Carburetors", "Cylinders", 
                              "Gears", "Transmission"),
                   expand = c(.1, .1)) +
  # scale_fill_brewer(palette="Paired") +
  labs(title = "Mtcars data",
       subtitle = "stratified by carb, cyl, gear, and am",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

# create a heatmap
data(mtcars)
library(superheat)
superheat(mtcars, scale = TRUE)

# sorted heat map
superheat(mtcars,
          scale = TRUE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          row.dendrogram = TRUE )

# create heatmap for gapminder data (Asia)
library(tidyr)
library(dplyr)

# load data
data(gapminder, package="gapminder")

# subset Asian countries
asia <- gapminder %>%
  filter(continent == "Asia") %>%
  select(year, country, lifeExp)

# convert to long to wide format
plotdata <- pivot_wider(asia, names_from = year, 
                        values_from = lifeExp)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$country
plotdata$country <- NULL

# row order
sort.order <- order(plotdata$"2007")

# color scheme
library(RColorBrewer)
colors <- rev(brewer.pal(5, "Blues"))


# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          heat.pal = colors,
          order.rows = sort.order,
          title = "Life Expectancy in Asia")


# create a radar chart

# prepare data
data(msleep, package = "ggplot2")
library(ggplot2)
library(ggradar)
library(scales)
library(dplyr)

plotdata <- msleep %>%
  filter(name %in% c("Cow", "Dog", "Pig")) %>%
  select(name, sleep_total, sleep_rem, 
         sleep_cycle, brainwt, bodywt) %>%
  rename(group = name) %>%
  mutate_at(vars(-group),
            funs(rescale))
plotdata

# generate radar chart
ggradar(plotdata, 
        grid.label.size = 4,
        axis.label.size = 4, 
        group.point.size = 5,
        group.line.width = 1.5,
        legend.text.size= 10) +
  labs(title = "Mammals, size, and sleep")


library(GGally)

# prepare data
data(msleep, package="ggplot2")
library(dplyr)
df <- msleep %>% 
  mutate(log_brainwt = log(brainwt),
         log_bodywt = log(bodywt)) %>%
  select(log_brainwt, log_bodywt, sleep_total, sleep_rem)
 

# create a scatterplot matrix
ggpairs(df)

# custom function for density plot
my_density <- function(data, mapping, ...){
  ggplot(data = data, mapping = mapping) + 
    geom_density(alpha = 0.5,
                 fill = "cornflowerblue", ...)
}

# custom function for scatterplot
my_scatter <- function(data, mapping, ...){
  ggplot(data = data, mapping = mapping) + 
    geom_point(alpha = 0.5,
               color = "cornflowerblue") + 
    geom_smooth(method=lm, 
                se=FALSE, ...)
}


# create scatterplot matrix
ggpairs(df, 
        lower=list(continuous = my_scatter), 
        diag = list(continuous = my_density)) +
  labs(title = "Mammal size and sleep characteristics") +
  theme_bw()


# create company income statement
category <- c("Sales", "Services", "Fixed Costs", 
              "Variable Costs", "Taxes")
amount <- c(101000, 52000, -23000, -15000, -10000)
income <- data.frame(category, amount) 

# create waterfall chart
library(ggplot2)
library(waterfalls)
waterfall(income) 

# create waterfall chart with total column
waterfall(income, 
          calc_total=TRUE, 
          total_axis_text = "Net",
          total_rect_text_color="black",
          total_rect_color="goldenrod1") +
  scale_y_continuous(label=scales::dollar) +
  labs(title = "West Coast Profit and Loss", 
       subtitle = "Year 2017",
       y="", 
       x="") +
  theme_minimal() 


# install packages for text mining
# install.packages(c("tm", "SnowballC",
#                    "wordcloud", "RColorBrewer",
#                    "RCurl", "XML"))


# create a word cloud
script <- "http://www.sthda.com/upload/rquery_wordcloud.r"
source(script)
res<-rquery.wordcloud("JFKspeech.txt",
                      type ="file",
                      lang = "english",
                      textStemming=FALSE,
                      min.freq=3,
                      max.words=200)


# create a word cloud
script <- "rquery_wordcloud.r"
source(script)
res<-rquery.wordcloud("JFKspeech.txt", 
                      type ="file", 
                      lang = "english",
                      textStemming=FALSE,
                      min.freq=3, 
                      max.words=200)

