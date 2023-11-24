CRAN_pkgs <- c("ggplot2", "dplyr", "tidyr", "mosaicData",
               "carData", "VIM", "scales", "treemapify",
               "gapminder","sf", "tidygeocoder",  "mapview",
               "ggmap", "osmdata", "choroplethr",
               "choroplethrMaps", "lubridate", "CGPfunctions",
               "ggcorrplot", "visreg", "gcookbook", "forcats",
               "survival", "survminer", "car", "rgl",
               "ggalluvial", "ggridges", "GGally", "superheat",
               "waterfalls", "factoextra","networkD3",
               "ggthemes", "patchwork", "hrbrthemes", "ggpol",
               "quantmod", "gghighlight", "leaflet", "ggiraph",
               "rbokeh", "ggalt")
install.packages(CRAN_pkgs)

install.packages("remotes")

github_pkgs <- c("rkabacoff/ggpie", "hrbrmstr/waffle",
                 "ricardo-bion/ggradar", "ramnathv/rCharts",
                 "Mikata-Project/ggthemr")

remotes::install_github(github_pkgs, dependencies = TRUE)

