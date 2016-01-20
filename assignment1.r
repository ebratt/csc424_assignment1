# setup
# clear the environment
rm(list=ls())

DATA_DIR <- './data'
IMAGES_DIR <- './images'
OUTPUT_DIR <- './output'

make_dir <- function(d) {
    if (file.exists(d)) unlink(d, recursive=TRUE, force=TRUE)
    dir.create(d)
}
lapply(c(IMAGES_DIR, OUTPUT_DIR),make_dir)


## function that concatenates strings (useful for directory paths)
concat <- function(x1,x2) {
    result <- paste(x1,x2,sep="")
    return(result)
}

## function that checks to see if a package is installed and,if not,installs it
## portions of this code came from http://stackoverflow.com/questions/9341635/how-can-i-check-for-installed-r-packages-before-running-install-packages
load_package <- function(x) {
    if (x %in% rownames(installed.packages())) { 
        print(concat("package already installed: ", x))
    }
    else { 
        install.packages(x) 
    }
    library(x, character.only=TRUE)
}
lapply(c("car"), load_package)

#######################################################
# PROBLEM 1                                           #
#######################################################
# import the olympics.csv file
data <- read.csv(concat(DATA_DIR,'/olympics.csv'))
colnames(data) <- c("iso_country_code", 
                    "country_name",
                    "gdp_2011",
                    "pop_2010",
                    "count_f",
                    "count_m",
                    "medals_gold",
                    "medals_silver",
                    "medals_bronze")

# create new variables
data["medals_total"] <- data["medals_gold"] + 
    data["medals_silver"] + 
    data["medals_bronze"]
data["pct_f"] <- data["count_f"] / (data["count_f"] + data["count_m"])

# drop the first two label/id columns, the counts of colored medals, 
# and the counts of female/male now that we have pct female
data_plot <- data[c('medals_total','gdp_2011','pop_2010','pct_f')]

# plot the relationships between the variables
png(concat(IMAGES_DIR,'/problem1_scatterplot_matrix.png'), width = 1024, height = 1024)
scatterplotMatrix(data_plot, diagonal="density")
dev.off()

#######################################################
# PROBLEM 2                                           #
#######################################################
rm(list=ls())

DATA_DIR <- './data'
IMAGES_DIR <- './images'
OUTPUT_DIR <- './output'

## function that checks to see if a package is installed and,if not,installs it
## portions of this code came from http://stackoverflow.com/questions/9341635/how-can-i-check-for-installed-r-packages-before-running-install-packages
load_package <- function(x) {
    if (x %in% rownames(installed.packages())) { 
        print(concat("package already installed: ", x))
    }
    else { 
        install.packages(x) 
    }
    library(x, character.only=TRUE)
}

## function that concatenates strings (useful for directory paths)
concat <- function(x1,x2) {
    result <- paste(x1,x2,sep="")
    return(result)
}

# import the maple.txt file
lapply(c("data.table"), load_package)
data <- read.table(concat(DATA_DIR,'/maple.txt'), header=TRUE)
colnames(data) <- c("location", 
                    "latitude",
                    "july_temp",
                    "leaf_index")

# drop the first categorical column
data_plot <- data[c('latitude','july_temp','leaf_index')]

# plot the relationships between the variables
png(concat(IMAGES_DIR,'/problem2_scatterplot_matrix.png'), width = 1024, height = 1024)
scatterplotMatrix(data_plot, diagonal="density")
dev.off()

# a) regression of leaf_index on latitude
leaf_index <- data$leaf_index
latitude <- data$latitude
a <- lm(leaf_index ~ latitude)
summary(a)
# Call:
#   lm(formula = leaf_index ~ latitude)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.2348 -0.8488  0.0773  1.0074  3.3305 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.66716    3.05202  -0.546    0.589    
# latitude     0.45369    0.07427   6.108 1.03e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.673 on 30 degrees of freedom
# Multiple R-squared:  0.5543,	Adjusted R-squared:  0.5394 
# F-statistic: 37.31 on 1 and 30 DF,  p-value: 1.031e-06
july_temp <- data$july_temp
b <- lm(leaf_index ~ july_temp)
summary(b)
# Call:
#   lm(formula = leaf_index ~ july_temp)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -3.254 -1.288  0.096  1.245  3.212 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) 40.74297    4.45498   9.145 3.51e-10 ***
#   july_temp   -0.33318    0.06206  -5.368 8.23e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.789 on 30 degrees of freedom
# Multiple R-squared:   0.49,	Adjusted R-squared:  0.473 
# F-statistic: 28.82 on 1 and 30 DF,  p-value: 8.233e-06
c <- lm(leaf_index ~ july_temp + latitude)
summary(c)
# Call:
#   lm(formula = leaf_index ~ july_temp + latitude)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.2082 -1.2363  0.1613  1.0551  3.2445 
# 
# Coefficients:
#               Estimate Std. Error   t value Pr(>|t|)  
#   (Intercept) 13.73184   11.42026   1.202   0.2389  
#   july_temp   -0.13524    0.09676  -1.398   0.1728  
#   latitude     0.31393    0.12388   2.534   0.0169 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.647 on 29 degrees of freedom
# Multiple R-squared:  0.5824,	Adjusted R-squared:  0.5536 
# F-statistic: 20.22 on 2 and 29 DF,  p-value: 3.167e-06

#######################################################
# PROBLEM 3                                           #
#######################################################
rm(list=ls())

DATA_DIR <- './data'
IMAGES_DIR <- './images'
OUTPUT_DIR <- './output'

## function that checks to see if a package is installed and,if not,installs it
## portions of this code came from http://stackoverflow.com/questions/9341635/how-can-i-check-for-installed-r-packages-before-running-install-packages
load_package <- function(x) {
    if (x %in% rownames(installed.packages())) { 
        print(concat("package already installed: ", x))
    }
    else { 
        install.packages(x) 
    }
    library(x, character.only=TRUE)
}

## function that concatenates strings (useful for directory paths)
concat <- function(x1,x2) {
    result <- paste(x1,x2,sep="")
    return(result)
}

# import the chicinsur.txt file
data <- read.table(concat(DATA_DIR,'/chicinsur.txt'), header=TRUE)
summary(data)
data_plot <- data[c('newpol', 'pctmin', 'fires', 'thefts', 'pctold', 'income')]

# plot the relationships between the variables
png(concat(IMAGES_DIR,'/problem3_scatterplot_matrix.png'), width = 1024, height = 1024)
scatterplotMatrix(data_plot, diagonal="density")
dev.off()

# check the correlation matrix
cor(data_plot)
#            newpol     pctmin      fires     thefts     pctold     income
# newpol  1.0000000 -0.7594196 -0.6864766 -0.3116183 -0.6057428  0.7509780
# pctmin -0.7594196  1.0000000  0.5927956  0.2550647  0.2505118 -0.7037328
# fires  -0.6864766  0.5927956  1.0000000  0.5562105  0.4122225 -0.6104481
# thefts -0.3116183  0.2550647  0.5562105  1.0000000  0.3176308 -0.1729226
# pctold -0.6057428  0.2505118  0.4122225  0.3176308  1.0000000 -0.5286695
# income  0.7509780 -0.7037328 -0.6104481 -0.1729226 -0.5286695  1.0000000

# define the variable vectors
newpol <- data$newpol
pctmin <- data$pctmin
fires <- data$fires
thefts <- data$thefts
pctold <- data$pctold
income <- data$income

# what does the histogram of newpol look like?
load_package('ggplot2')
png(concat(IMAGES_DIR,'/problem3_histogram_newpol.png'), width = 512, height = 512)
ggplot(data_plot, aes(x=newpol)) + 
    geom_histogram(binwidth=1, colour="black", fill="white") + 
    geom_vline(data=data_plot, aes(xintercept=mean(newpol)),
               linetype="dashed", size=1, colour="red") + 
    labs(title="Histogram for New Policies per 100 Households") +
    labs(x="New Policies per 100 Households", y="Frequency")
dev.off()

# what is the skew of the dependent variable?
load_package('psych')
describe(newpol)$skew

# build the full first-order linear model
a <- lm(newpol ~ pctmin + fires + thefts + pctold + income)
summary(a)
# Call:
#   lm(formula = newpol ~ pctmin + fires + thefts + pctold + income)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.5235 -1.2134 -0.1544  1.0181  3.8096 
# 
# Coefficients:
#               Estimate  Std. Error  t value Pr(>|t|)    
#   (Intercept) 12.0610686  2.8187818   4.279 0.000110 ***
#   pctmin      -0.0594738  0.0131806  -4.512  5.3e-05 ***
#   fires       -0.1018544  0.0480110  -2.121 0.039972 *  
#   thefts       0.0135616  0.0162371   0.835 0.408436    
#   pctold      -0.0643711  0.0158312  -4.066 0.000211 ***
#   income       0.0001164  0.0001804   0.645 0.522525    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.907 on 41 degrees of freedom
# Multiple R-squared:  0.7939,	Adjusted R-squared:  0.7688 
# F-statistic: 31.59 on 5 and 41 DF,  p-value: 4.773e-13

# look at collinearity
write.table(cor(data_plot), file=concat(OUTPUT_DIR,'/problem3_model_correlations.csv'), sep=",")
write.table(summary(a)$coefficients, file=concat(OUTPUT_DIR,'/problem3_model_coefficients.csv'), sep=",")
write.table(anova(a), file=concat(OUTPUT_DIR,'/problem3_model_anova.csv'), sep=",")
load_package('lm.beta')
lm.a.beta <- lm.beta(a)
write.table(coef(lm.a.beta), file=concat(OUTPUT_DIR,'/prblem3_model_coefficients_standard.csv'), sep=",")
load_package('DAAG')
vif(a)
write("VIF", file=concat(OUTPUT_DIR,'/problem3_model_vif.csv'))
write.table(vif(a), file=concat(OUTPUT_DIR,'/problem3_model_vif.csv'), sep=",", append=TRUE)
write("", file=concat(OUTPUT_DIR,'/problem3_model_vif.csv'), append=TRUE)
write("", file=concat(OUTPUT_DIR,'/problem3_model_vif.csv'), append=TRUE)
sqrt(vif(a))
write("sqrt(VIF)", file=concat(OUTPUT_DIR,'/problem3_model_vif.csv'), append=TRUE)
write.table(sqrt(vif(a)), file=concat(OUTPUT_DIR,'/problem3_model_vif.csv'), sep=",", append=TRUE)
# create corrgram
load_package('corrgram')
png(concat(IMAGES_DIR,'/problem3_corrgram.png'), height=1024, width=1024)
corrgram(data_plot, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt, main="Quantitative Variables in PC2/PC1 Order")
dev.off()


# plot of deleted studentized residuals vs hat values
png(concat(IMAGES_DIR,'/problem3_model_influentials.png'))
plot(hatvalues(a), rstudent(a))
abline(a=0,b=0, col="red")
# add labels to points
text(hatvalues(a), rstudent(a), cex=0.7, pos=2)
dev.off()

# create a plot of residuals versus predicted values
png(concat(IMAGES_DIR,'/problem3_residuals_vs_predicted.png'), width = 512, height = 512)
plot(fitted(a), rstandard(a), main="Predicted vs. Residuals Plot")
abline(a=0, b=0, col="red")
text(fitted(a), rstandard(a), cex=0.7, pos=2)
dev.off()

# Assessing Outliers
png(concat(IMAGES_DIR,'/problem3_model_qqplot.png'), width = 1024, height = 1024)
qqPlot(a, main="QQ Plot") #qq plot for studentized resid 
dev.off()
png(concat(IMAGES_DIR,'/problem3_model_leverage_plots.png'), width = 1024, height = 1024)
leveragePlots(a) # leverage plots
dev.off()

# print out only observations that may be influential
write.table(summary(influence.measures(a)), 
            file=concat(OUTPUT_DIR,'/problem3_model_influentials.csv'), sep=",")
# Influential Observations
# added variable plots 
png(concat(IMAGES_DIR,'/problem3_added_value_plots.png'), width = 1024, height = 1024)
avPlots(a)
dev.off()
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(data_plot)-length(a$coefficients)-2)) 
png(concat(IMAGES_DIR,'/problem3_cook_levels.png'), width = 1024, height = 1024)
plot(a, which=4, cook.levels=cutoff)
dev.off()
# Influence Plot 
png(concat(IMAGES_DIR,'/problem3_influence_plot.png'), width = 1024, height = 1024)
influencePlot(a, id.method="identify", 
              main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance" )
dev.off()

# residuals histogram
x = rstudent(a)
png(concat(IMAGES_DIR,'/problem3_model_residuals_hist.png'))
hist(x, breaks=100, col="red", xlab="New Policies per 100 Households", 
     main="Histogram of Residuals with Normal Curve")
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
lines(xfit, yfit, col="blue", lwd=2)
dev.off()

