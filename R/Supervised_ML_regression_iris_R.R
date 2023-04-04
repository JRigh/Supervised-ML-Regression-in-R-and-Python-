#---------------------------------
# Machine Learning algorithms in R
# Regression algorithms
#---------------------------------

library(tidyverse)     # dpylr, ggplot2,...
library(xtable)        # to export tables in LaTeX
library(cowplot)       # for visualization
data(iris)             # to load the data

# 1. splitting the dataset into training and test sets
set.seed(2023)
ind <- sample(2, nrow(iris),replace=TRUE,prob=c(0.7,0.3))
training <- iris[ind==1,]
testing <- iris[ind==2,]

write.csv(iris, 
          "C:/Users/julia/OneDrive/Desktop/github/30. Regressions/iris.csv",
          row.names = FALSE)
write.csv(training, 
          "C:/Users/julia/OneDrive/Desktop/github/30. Regressions/iris_training.csv",
          row.names = FALSE)
write.csv(testing, 
          "C:/Users/julia/OneDrive/Desktop/github/30. Regressions/iris_testing.csv",
          row.names = FALSE)

# visualization

#---------------------------------------------------------
# Scatterplot with regression lines and marginal densities
#---------------------------------------------------------

# regression model
mod1 <- lm(Petal.Width ~ Sepal.Length, data = iris)
iris$predictions <- predict(mod1, type = 'response')

# plot
p <- ggplot(data = iris, aes(x = Sepal.Length,y = Petal.Width, colour=Species)) + 
  geom_smooth(method='lm', se = FALSE) + 
  geom_line(color='black', size = 1.2, data = iris, aes(x=Sepal.Length, y = predictions)) +
  geom_point() +
  labs(title = 'Scatterplot with different  regression lines and marginal densities',
       subtitle = 'Sepal Width x Sepal Length from Iris dataset',
       y="Sepal Width", x="Sepal Length") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# 2. Create marginal Boxplots
xbp <- axis_canvas(p, axis = "x") +
  geom_density(data = iris, aes(x = Sepal.Length, fill = Species),
               alpha = 0.4, size = 0.2)
ybp <- axis_canvas(p, axis = "y", coord_flip = TRUE)+
  geom_density(data = iris, aes(x = Petal.Width, fill = Species),
               alpha = 0.4, size = 0.2) + coord_flip()

p1 <- insert_xaxis_grob(p, xbp, grid::unit(.2, "null"), position = "top")
p2 <- insert_yaxis_grob(p1, ybp, grid::unit(.2, "null"), position = "right")

# 3. Create complete plot
ggdraw(p2)

#----
# end
#----

#-----------------------
# 1.1. linear regression
#-----------------------

# 1. Build a linear regression model
lr.model <- lm(Petal.Width ~ Sepal.Length, data = training)
summary(lr.model)

training$lr.model <- predict(lr.model, type = 'response', newdata = training)
testing$lr.model<- predict(lr.model, type = 'response', newdata = testing)

intercept <- round(lr.model$coefficients[1],3)
slope <- round(lr.model$coefficients[2],3)

# 2. Plot the regression line on the testing set

ggplot(testing, aes(x = Sepal.Length, y = Petal.Width, group = Species)) + 
  geom_point(aes(shape = Species, color = Species), size = 1.8) +
  geom_line(color='darkred', size = 1.2, data = testing, aes(x=Sepal.Length, y = lr.model)) +
  annotate('text', label = paste("Intercept = ", intercept, ", Slope =", slope), x = 7, y = 0.35, size = 3) + 
  labs(title = 'Scatterplot - Linear Regression model',
       subtitle = 'Sepal.Length x Petal.Width on Iris testing dataset',
       y="Petal.Width", x="Sepal.Length") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#---------------------------
# 1.2. log-linear regression
#---------------------------

# 1. Build a log-linear regression model
log.lin.model <- lm(log(Petal.Width) ~ Sepal.Length, data = training)
summary(log.lin.model)

training$log.lin.model <- predict(log.lin.model, type = 'response', newdata = training)
testing$log.lin.model <- predict(log.lin.model, type = 'response', newdata = testing)

intercept <- round(log.lin.model$coefficients[1],3)
slope <- round(log.lin.model$coefficients[2],3)

# 2. Plot the regression line on the testing set

ggplot(testing, aes(x = Sepal.Length, y = log(Petal.Width), group = Species)) + 
  geom_point(aes(shape = Species, color = Species), size = 1.8) +
  geom_line(color='darkred', size = 1.2, data = testing, aes(x=Sepal.Length, y = log.lin.model)) +
  annotate('text', label = paste("Intercept = ", intercept, ", Slope ="), x = 7, y = -1.75, size = 3) + 
  labs(title = 'Scatterplot - Log-Linear Regression model',
       subtitle = 'Sepal.Length x Petal.Width on Iris testing dataset',
       y="log(Petal.Width)", x="Sepal.Length") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#------------------------
# 1.3. Poisson regression
#------------------------

# 1. Build a Poisson regression model
pois.model <- glm(round(Petal.Width) ~ Sepal.Length, data = training, 
                  family=poisson(link="log"))
summary(pois.model)

training$pois.model <- predict(pois.model, type = 'response', newdata = training)
testing$pois.model <- predict(pois.model, type = 'response', newdata = testing)

intercept <- round(pois.model$coefficients[1],3)
slope <- round(pois.model$coefficients[2],3)

# 2. Plot the regression line on the testing set

ggplot(testing, aes(x = Sepal.Length, y = Petal.Width, group = Species)) + 
  geom_point(aes(shape = Species, color = Species), size = 1.8) +
  geom_line(color='darkred', size = 1.2, data = testing, aes(x=Sepal.Length[order(testing$Sepal.Length)], y = pois.model[order(testing$Sepal.Length)])) +
  annotate('text', label = paste("Intercept = ", intercept, ", Slope =", slope), x = 7, y = 0.35, size = 3) + 
  labs(title = 'Scatterplot - Poisson Regression model',
       subtitle = 'Sepal.Length x Petal.Width on Iris testing dataset',
       y="Petal.Width", x="Sepal.Length") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))


#----------------------------------
# 1.4. Negative Binomial regression
#----------------------------------

library(MASS)

# 1. Build a Negative binomial regression model
nb.model <- glm.nb(round(Petal.Width) ~ Sepal.Length, data = training)

summary(nb.model)

training$nb.model <- predict(nb.model, type = 'response', newdata = training)
testing$nb.model <- predict(nb.model, type = 'response', newdata = testing)

intercept <- round(nb.model$coefficients[1],3)
slope <- round(nb.model$coefficients[2],3)

# 2. Plot the regression line on the testing set

ggplot(testing, aes(x = Sepal.Length, y = Petal.Width, group = Species)) + 
  geom_point(aes(shape = Species, color = Species), size = 1.8) +
  geom_line(color='darkred', size = 1.2, data = testing, aes(x=Sepal.Length[order(testing$Sepal.Length)], y = nb.model[order(testing$Sepal.Length)])) +
  annotate('text', label = paste("Intercept = ", intercept, ", Slope =", slope), x = 7, y = 0.35, size = 3) + 
  labs(title = 'Scatterplot - Negative Binomial Regression model',
       subtitle = 'Sepal.Length x Petal.Width on Iris testing dataset',
       y="Petal.Width", x="Sepal.Length") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))


#----------------------
# 1.5. Gamma regression
#----------------------

# 1. Build a log-linear regression model
gam.model <- glm(Petal.Width ~ Sepal.Length, data = training, 
                 family=Gamma(link = "log"))

training$gam.model <- predict(gam.model, type = 'response', newdata = training)
testing$gam.model <- predict(gam.model, type = 'response', newdata = testing)

intercept <- round(gam.model$coefficients[1],3)
slope <- round(gam.model$coefficients[2],3)

# 2. Plot the regression line on the testing set

ggplot(testing, aes(x = Sepal.Length, y = Petal.Width, group = Species)) + 
  geom_point(aes(shape = Species, color = Species), size = 1.8) +
  geom_line(color='darkred', size = 1.2, data = testing, aes(x=Sepal.Length[order(testing$Sepal.Length)], 
                                                             y = gam.model[order(testing$Sepal.Length)])) +
  annotate('text', label = paste("Intercept = ", intercept, ", Slope =", slope), x = 7, y = 0.35, size = 3) + 
  labs(title = 'Scatterplot - Gamma Regression model',
       subtitle = 'Sepal.Length x Petal.Width on Iris testing dataset',
       y="Petal.Width", x="Sepal.Length") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))


#-----------------------------------------
# 1.6 Comparison of all RMSE across models
#-----------------------------------------

RMSE_lin <- sqrt(mean((testing$Petal.Width - testing$lr.model)^2))
RMSE_lol <- sqrt(mean((testing$Petal.Width - testing$log.lin.model)^2))
RMSE_poi <- sqrt(mean((testing$Petal.Width - testing$pois.model)^2))
RMSE_nbi <- sqrt(mean((testing$Petal.Width - testing$nb.model)^2))
RMSE_gam <- sqrt(mean((testing$Petal.Width - testing$gam.model)^2))

RMSE <- matrix(c(RMSE_lin, RMSE_lol, RMSE_poi, RMSE_nbi, RMSE_gam ), ncol = 5)
colnames(RMSE) <- c("Linear", "Log-Linear",  "Poisson", "Negative Binomial", "Gamma")
rownames(RMSE) <- 'RMSE'
RMSE

# export the results in LaTex document
print(xtable(RMSE, type = "latex"), file = "C:/Users/julia/OneDrive/Desktop/github/30. Regressions/tablesreg.tex")

#------------------------------
# 2.1 Nonparametric regressions
#------------------------------

# 2.2 Kernel regression

Kreg = ksmooth(x = iris$Sepal.Length, y = iris$Sepal.Width,
               kernel = "normal", bandwidth = 1)

# 2.3 Ploting the regression
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, group = Species)) + 
  geom_point(aes(shape = Species, color = Species), size = 1.8) +
  geom_line(color='darkred', size = 1.2, data = iris, aes(x = Kreg$x,
                                                          y = Kreg$y)) +
  labs(title = 'Scatterplot - Nonparametric Kernel regression model',
       subtitle = 'Sepal.Length x Sepal.Width on Iris testing dataset',
       y="Sepal.Width", x="Sepal.Length") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))


# 2.4 smoothing splines

# 2.5 Ploting the regression
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point(aes(shape = Species, color = Species), size = 1.8) +
  geom_smooth(method = 'loess' , color='darkred', size = 1.2, se = FALSE) + 
  labs(title = 'Scatterplot - Nonparametric Smoothing Splines regression model',
       subtitle = 'Sepal.Length x Sepal.Width on Iris testing dataset',
       y="Sepal.Width", x="Sepal.Length") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#----
# end
#----
