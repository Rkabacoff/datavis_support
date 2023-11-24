library(readr)

# import data from a comma delimited file
Salaries <- read_csv("salaries.csv")

# import data from a tab delimited file
Salaries <- read_tsv("salaries.txt")

library(readxl)

# import data from an Excel workbook
Salaries <- read_excel("salaries.xlsx", sheet=1)

library(haven)

# import data from Stata
Salaries <- read_dta("salaries.dta")

# import data from SPSS
Salaries <- read_sav("salaries.sav")

# import data from SAS
Salaries <- read_sas("salaries.sas7bdat")

library(dplyr)

# keep the variables name, height, and gender
newdata <- select(starwars, name, height, gender)

# keep the variables name and all variables
# between mass and species inclusive
newdata <- select(starwars, name, mass:species)

# keep all variables except birth_year and gender
newdata <- select(starwars, -birth_year, -gender)

library(dplyr)

# select females
newdata <- filter(starwars,
                  gender == "female")

# select females that are from Alderaan
newdata <- select(starwars,
                  gender == "female" &
                  homeworld == "Alderaan")


# select individuals that are from Alderaan, Coruscant, or Endor
newdata <- select(starwars,
                  homeworld == "Alderaan" |
                  homeworld == "Coruscant" |
                  homeworld == "Endor")

# this can be written more succinctly as
newdata <- select(starwars,
                  homeworld %in%
                    c("Alderaan", "Coruscant", "Endor"))

library(dplyr)

# convert height in centimeters to inches,
# and mass in kilograms to pounds
newdata <- mutate(starwars,
                  height = height * 0.394,
                  mass   = mass   * 2.205)

library(dplyr)

# if height is greater than 180 then heightcat = "tall",
# otherwise heightcat = "short"

newdata <- mutate(starwars,
                  heightcat = ifelse(height > 180,
                                     "tall",
                                     "short"))

# convert any eye color that is not black, blue or brown, to other.
newdata <- mutate(starwars,
                  eye_color = ifelse(eye_color %in%
                                     c("black", "blue", "brown"),
                                     eye_color,
                                     "other"))

# set heights greater than 200 or less than 75 to missing
newdata <- mutate(starwars,
                  height = ifelse(height < 75 | height > 200,
                                     NA,
                                     height))


library(dplyr)

# calculate mean height and mass
newdata <- summarize(starwars, 
                    mean_ht = mean(height, na.rm=TRUE), 
                    mean_mass = mean(mass, na.rm=TRUE))
newdata
                 
# calculate mean height and weight by gender
newdata <- group_by(starwars, gender)
newdata <- summarize(newdata, 
                    mean_ht = mean(height, na.rm=TRUE), 
                    mean_wt = mean(mass, na.rm=TRUE))
newdata


library(dplyr)

# calculate the mean height for women by species
newdata <- filter(starwars,
                  gender == "female")
newdata <- group_by(species)
newdata <- summarize(newdata,
                     mean_ht = mean(height, na.rm = TRUE))

# this can be written as more succinctly as
newdata <- starwars %>%
  filter(gender == "female") %>%
  group_by(species) %>%
  summarize(mean_ht = mean(height, na.rm = TRUE))


df <- data.frame(
 dob = c("11/10/1963", "Jan-23-91", "12:1:2001")
)
# view struction of data frame
str(df) 

library(lubridate)
# convert dob from character to date
df$dob <- mdy(df$dob)
str(df)

wide_data <- data.frame(id = c("01", "02", "03"),
                       name = c("Bill", "Bob", "Mary"),
                       sex = c("Male", "Male", "Female"),
                       height = c(70, 72, 62),
                       weight = c(180, 195, 130))
knitr::kable(wide_data, caption = "Wide data")

# convert wide dataset to long dataset
library(tidyr)
long_data <- pivot_longer(wide_data,
                          cols = c("height", "weight"),
                          names_to = "variable",
                          values_to ="value")


long_data <- tidyr::pivot_longer(wide_data,
                         cols = c("height", "weight"),
                         names_to = "variable", 
                         values_to ="value")

knitr::kable(long_data, caption = "Long data")


# convert long dataset to wide dataset
library(tidyr)
wide_data <- pivot_wider(long_data,
                         names_from = "variable",
                         values_from = "value")


data(msleep, package="ggplot2")

# what is the proportion of missing data for each variable?
pctmiss <- colSums(is.na(msleep))/nrow(msleep)
round(pctmiss, 2)

# Create a dataset containing genus, vore, and conservation.
# Delete any rows containing missing data.
newdata <- select(msleep, genus, vore, conservation)
newdata <- na.omit(newdata)

# Impute missing values using the 5 nearest neighbors
library(VIM)
newdata <- kNN(msleep, k=5)

