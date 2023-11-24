
# simple bar chart
library(ggplot2)
data(Marriage, package = "mosaicData")

# plot the distribution of race
ggplot(Marriage, aes(x = race)) + 
  geom_bar()


# plot the distribution of race with modified colors and labels
ggplot(Marriage, aes(x=race)) + 
  geom_bar(fill = "cornflowerblue", 
           color="black") +
  labs(x = "Race", 
       y = "Frequency", 
       title = "Participants by race")


# plot the distribution as percentages
ggplot(Marriage, 
       aes(x = race, y = after_stat(count/sum(count)))) + 
  geom_bar() +
  labs(x = "Race", 
       y = "Percent", 
       title  = "Participants by race") +
  scale_y_continuous(labels = scales::percent)


# calculate number of participants in each race category
library(dplyr)
plotdata <- Marriage %>%
 count(race)


knitr::kable(plotdata, caption = "plotdata")


# plot the bars in ascending order
ggplot(plotdata, 
       aes(x = reorder(race, n), y = n)) + 
  geom_bar(stat="identity") +
  labs(x = "Race", 
       y = "Frequency", 
       title  = "Participants by race")


# plot the bars with numeric labels
ggplot(plotdata, 
       aes(x = race, y = n)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label = n), vjust=-0.5) +
  labs(x = "Race", 
       y = "Frequency", 
       title  = "Participants by race")


library(dplyr)
library(scales)
plotdata <- Marriage %>%
  count(race) %>%
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%"))

# plot the bars as percentages, 
# in decending order with bar labels
ggplot(plotdata, 
       aes(x = reorder(race, -pct), y = pct)) + 
  geom_bar(stat="identity", fill="indianred3", color="black") +
  geom_text(aes(label = pctlabel), vjust=-0.25) +
  scale_y_continuous(labels = percent) +
  labs(x = "Race", 
       y = "Percent", 
       title  = "Participants by race")

# basic bar chart with overlapping labels
ggplot(Marriage, aes(x=officialTitle)) + 
  geom_bar() +
  labs(x = "Officiate",
       y = "Frequency",
       title = "Marriages by officiate")


# horizontal bar chart
ggplot(Marriage, aes(x = officialTitle)) + 
  geom_bar() +
  labs(x = "",
       y = "Frequency",
       title = "Marriages by officiate") +
  coord_flip()


# bar chart with rotated labels
ggplot(Marriage, aes(x=officialTitle)) + 
  geom_bar() +
  labs(x = "",
       y = "Frequency",
       title = "Marriages by officiate") +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1))
  

# bar chart with staggered labels
lbls <- paste0(c("","\n"), levels(Marriage$officialTitle))
ggplot(Marriage, 
       aes(x=factor(officialTitle, 
                    labels = lbls))) + 
  geom_bar() +
  labs(x = "",
       y = "Frequency",
       title = "Marriages by officiate")
  

# create a basic ggplot2 pie chart
library(ggpie)
ggpie(Marriage, race)

# create a pie chart with slice labels within figure
ggpie(Marriage, race, legend = FALSE, title = "Participants by race")

library(treemapify)

# create a treemap of marriage officials
plotdata <- Marriage %>%
  count(officialTitle)

ggplot(plotdata, 
       aes(fill = officialTitle, area = n)) +
  geom_treemap() + 
  labs(title = "Marriages by officiate")


# create a treemap with tile labels
ggplot(plotdata, 
       aes(fill = officialTitle, 
           area = n, 
           label = officialTitle)) +
  geom_treemap() + 
  geom_treemap_text(colour = "white", 
                    place = "centre") +
  labs(title = "Marriages by officiate") +
  theme(legend.position = "none")


library(dplyr)
plotdata <- Marriage %>%
  count(officialTitle)


# create a basic waffle chart
library(waffle)
ggplot(plotdata, aes(fill = officialTitle, values=n)) +
  geom_waffle(na.rm=TRUE)


# Create a customized caption
cap <- paste0("1 square = ", ceiling(sum(plotdata$n)/100), 
              " case(s).")
library(waffle)
ggplot(plotdata, aes(fill = officialTitle, values=n)) +
  geom_waffle(na.rm=TRUE,
              n_rows = 10,
              size = .4,
              color = "white") + 
  scale_fill_brewer(palette = "Spectral") +
  coord_equal() +
  theme_minimal() + 
  theme_enhance_waffle() +
  theme(legend.title = element_blank()) +
  labs(title = "Proportion of Wedding Officials",
       caption = cap)


library(ggplot2)

# plot the age distribution using a histogram
ggplot(Marriage, aes(x = age)) +
  geom_histogram() + 
  labs(title = "Participants by age",
       x = "Age")


# plot the histogram with blue bars and white borders
ggplot(Marriage, aes(x = age)) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white") + 
  labs(title="Participants by age",
       x = "Age")


# plot the histogram with 20 bins
ggplot(Marriage, aes(x = age)) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white", 
                 bins = 20) + 
  labs(title="Participants by age", 
       subtitle = "number of bins = 20",
       x = "Age")


# plot the histogram with a binwidth of 5
ggplot(Marriage, aes(x = age)) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white", 
                 binwidth = 5) + 
  labs(title="Participants by age", 
       subtitle = "binwidth = 5 years",
       x = "Age")


library(scales)
ggplot(Marriage, 
       aes(x = age, y= after_stat(count/sum(count)))) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white", 
                 binwidth = 5) + 
  labs(title="Participants by age", 
       y = "Percent",
       x = "Age") +
  scale_y_continuous(labels = percent)



# Create a kernel density plot of age
ggplot(Marriage, aes(x = age)) +
  geom_density() + 
  labs(title = "Participants by age")



# Create a kernel density plot of age
ggplot(Marriage, aes(x = age)) +
  geom_density(fill = "indianred3") + 
  labs(title = "Participants by age")


# default bandwidth for the age variable
bw.nrd0(Marriage$age)

# Create a kernel density plot of age
ggplot(Marriage, aes(x = age)) +
  geom_density(fill = "deepskyblue", 
               bw = 1) + 
  labs(title = "Participants by age",
       subtitle = "bandwidth = 1")



# plot the age distribution using a dotplot
ggplot(Marriage, aes(x = age)) +
  geom_dotplot() + 
  labs(title = "Participants by age",
       y = "Proportion",
       x = "Age")


# Plot ages as a dot plot using 
# gold dots with black borders
ggplot(Marriage, aes(x = age)) +
  geom_dotplot(fill = "gold", 
               color="black") + 
  labs(title = "Participants by age",
       y = "Proportion",
       x = "Age")

