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


## function that checks to see if a package is installed and,if not,installs it
## portions of this code came from http://stackoverflow.com/questions/9341635/how-can-i-check-for-installed-r-packages-before-running-install-packages
load_package <- function(x) {
    if (x %in% rownames(installed.packages())) { 
        print(c("package already installed: ", x))
    }
    else { 
        install.packages(x) 
        library(x)
    }
}
## function that concatenates strings (useful for directory paths)
concat <- function(x1,x2) {
    result <- paste(x1,x2,sep="")
    return(result)
}
# lapply(c("car",
#          "psych",
#          "corrgram",
#          "gclus",
#          "lm.beta",
#          "DAAG",
#          "RColorBrewer"), 
#        load_package)
lapply(c("car"), load_package)

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

# drop the first two label/id colums, the counts of colored medals, 
# and the counts of female/male now that we have pct female
data_plot <- data[c('medals_total','gdp_2011','pop_2010','pct_f')]

# plot the relationships between the variables
png(concat(IMAGES_DIR,'/scatterplot_matrix.png'), width = 1024, height = 1024)
scatterplotMatrix(data_plot)
dev.off()

