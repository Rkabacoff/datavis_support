
# save a graph
library(ggplot2)
p <- ggplot(mtcars,
            aes(x = wt , y = mpg)) +
  geom_point()
ggsave(p, filename = "mygraph.png")
