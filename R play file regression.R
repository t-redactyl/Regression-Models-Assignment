rm(list = ls())
data(mtcars)
help(mtcars)

# Exploratory analyses
colnames(mtcars)
dim(mtcars)
str(mtcars)

summary(mtcars$mpg)
table(as.factor(mtcars$am))

# Look for variables with a strong relationship with both the outcome and the predictor
#mtcars$cyl <- as.ordered(mtcars$cyl)
#mtcars$vs <- as.factor(mtcars$vs)
#mtcars$am <- as.factor(mtcars$am)
#mtcars$gear <- as.ordered(mtcars$gear)
#mtcars$carb <- as.ordered(mtcars$carb)

# Reorder data frame so am is second variable
mtcars <- mtcars[ , c(1, 9, 2:8, 10:11)]

require(ggplot2); require(GGally)
g = ggpairs(mtcars, lower = list(continuous = "smooth", params = c(method = "loess")))
g

spec.cor <- function (dat, r, ...) { 
    x <- cor(dat, ...) 
    x[upper.tri(x, TRUE)] <- NA 
    i <- which(abs(x) >= r, arr.ind = TRUE) 
    data.frame(matrix(colnames(x)[as.vector(i)], ncol = 2), value = x[i]) 
} 

spec.cor(mtcars[ , 2:11], 0.8)

cor.list <- c()
outcome.cor <- function(predictor, ...) {
    x <- cor(mtcars$mpg, predictor)
    cor.list <- c(cor.list, x)
}
cor.list <- sapply(mtcars[ , 2:11], outcome.cor)
sort(abs(cor.list), decreasing = TRUE)


# cyl, disp and wt are collinear with outcome, indicating they may be interchangeable with the outcome
# drat, wt and gear are highly correlated with am

# am is positive related to mpg
# drat and gear are positively related to am
# drat is strongly positively associated with outcome
# gear is moderately positively associated with outcome
# wt is negatively related to gear, and strongly postively related to mpg

# Good extra predictors have the ability to clarify the relationship between transmission and MPG:
    # Is there something that is highly associated with tranmission AND MPG (e.g., real axle ratio (drat), 
    # weight (wt))? While there is an MPG difference for transmission, these variables may explain it.
    # Are there variables that clarify the relationship between transmission and MPG? I.e. are strongly
    # related to the outcome, but not to AM?
# Should try model with just MPG and AM
# Then model with one of the highly related variables, then the other (plus likelihood tests to assess fit)

# Box plot of AM versus MPG
library(ggplot2)
mtcars$am2 <- as.factor(mtcars$am)
levels(mtcars$am2) <- c("Automatic", "Manual")

p <- ggplot(mtcars, aes(am2, mpg)) +
        geom_boxplot(aes(fill = am2)) +
        geom_jitter() +
        ylab("Miles per gallon") +
        xlab("\nTransmission type") +
        theme_bw()

# Initial model fitting tranmission type to MPG
model1 <- lm(mpg ~ am, data = mtcars)

