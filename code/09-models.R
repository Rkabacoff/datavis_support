data(SaratogaHouses, package="mosaicData")

# select numeric variables
df <- dplyr::select_if(SaratogaHouses, is.numeric)

# calulate the correlations
r <- cor(df, use="complete.obs")
round(r,2)

library(ggplot2)
library(ggcorrplot)
ggcorrplot(r)

ggcorrplot(r, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE)

data(SaratogaHouses, package="mosaicData")
houses_lm <- lm(price ~ lotSize + age + landValue +
                  livingArea + bedrooms + bathrooms +
                  waterfront, 
                data = SaratogaHouses)

result <- broom::tidy(houses_lm)
result <- dplyr::mutate_if(result, is.numeric, round, 2)
knitr::kable(result, caption = "Linear Regression results")

# conditional plot of price vs. living area
library(ggplot2)
library(visreg)
visreg(houses_lm, "livingArea", gg = TRUE) 

# conditional plot of price vs. waterfront location
visreg(houses_lm, "waterfront", gg = TRUE) +
  scale_y_continuous(label = scales::dollar) +
  labs(title = "Relationship between price and location",
       subtitle = paste0("controlling for lot size, age, ",
                         "land value, bedrooms and bathrooms"),
       caption = "source: Saratoga Housing Data (2006)",
       y = "Home Price",
       x = "Waterfront")


# fit logistic model for predicting
# marital status: married/single
data(CPS85, package = "mosaicData")
cps85_glm <- glm(married ~ sex + age + sex*age + race + sector, 
                 family="binomial", 
                 data=CPS85)

# plot results
library(ggplot2)
library(visreg)
visreg(cps85_glm, "age", 
       gg = TRUE, 
       scale="response") +
  labs(y = "Prob(Married)", 
       x = "Age",
       title = "Relationship of age and marital status",
       subtitle = "controlling for sex, race, and job sector",
       caption = "source: Current Population Survey 1985")


# plot results
library(ggplot2)
library(visreg)
visreg(cps85_glm, "age",
       by = "sex",
       gg = TRUE, 
       scale="response") +
  labs(y = "Prob(Married)", 
       x = "Age",
       title = "Relationship of age and marital status",
       subtitle = "controlling for race and job sector",
       caption = "source: Current Population Survey 1985")


# plot survival curve
library(survival)
library(survminer)

data(lung)
sfit <- survfit(Surv(time, status) ~  1, data=lung)
ggsurvplot(sfit,
            title="Kaplan-Meier curve for lung cancer survival") 


# plot survival curve for men and women
sfit <- survfit(Surv(time, status) ~  sex, data=lung)
ggsurvplot(sfit, 
           conf.int=TRUE, 
           pval=TRUE,
           legend.labs=c("Male", "Female"), 
           legend.title="Sex",  
           palette=c("cornflowerblue", "indianred3"), 
           title="Kaplan-Meier Curve for lung cancer survival",
           xlab = "Time (days)")


# input data
library(readr)
titanic <- read_csv("titanic.csv")

# create a table
tbl <- xtabs(~Survived + Class + Sex, titanic)
ftable(tbl)

# create a mosaic plot from the table
library(vcd)
mosaic(tbl, main = "Titanic data")

mosaic(tbl, 
       shade = TRUE,
       labeling_args = 
         list(set_varnames = c(Sex = "Gender",
                               Survived = "Survived",
                               Class = "Passenger Class")),
       set_labels = 
         list(Survived = c("No", "Yes"),
                         Class = c("1st", "2nd", "3rd", "Crew"),
                         Sex = c("F", "M")),
       main = "Titanic data")

