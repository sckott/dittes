setwd("~/Mac/consulting/dittes")
library('ggplot2')

dat <- read.csv("dittes_data.csv")
dat$date <- as.Date(paste(as.character(dat$year), "-01-01", sep = ""), "%Y-%m-%d")
dat <- dat[ !dat$habitat == 2, ] # drop habitat=32
head(dat); str(dat)

m1 <- lm(log10(rdm+1) ~ landform + soil + slope_class + habitat, data = dat)
m1_dat <- fortify(m1)
ggplot(m1_dat, aes(, )) +
  geom_point()

# Summary data
setwd("~/Mac/consulting/dittes")
library('ggplot2')

dat <- read.csv("dittes_data.csv")
dat$date <- as.Date(paste(as.character(dat$year), "-01-01", sep = ""), "%Y-%m-%d")
dat <- dat[ !dat$habitat == 2, ] # drop habitat=32
head(dat); str(dat)

library('dplyr')
dat_df <- tbl_df(dat)

avg_by <- function(...){
  dat_df %>% 
    group_by(...) %>%
    summarise(rdm_mean=mean(rdm, na.rm = TRUE))
}

# all combinators of factors
avg_by(transect, year, soil, slope_class, landform, habitat)

# by transect
avg_by(transect)

# by year
avg_by(year)

# by soil type
avg_by(soil)

# by slope_class
avg_by(slope_class)

# by landform
avg_by(landform)

# by landform
avg_by(landform)

# by habitat
avg_by(habitat)


######
# Some stats
library('car')
summary(m1)
Anova(m1, type = 3)
