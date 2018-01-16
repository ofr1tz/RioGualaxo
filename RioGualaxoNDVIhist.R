# Install tidyverse package if necessary
if (!require(tidyverse)) {
      install.packages("tidyverse")
      library(tidyverse)
}

# Load ENVI ASCII output data
oct15 <- read.table("2015-10-11 NDVI Maske Rio Gualaxo ASCII output.txt", skip=5)
nov15 <- read.table("2015-11-12 NDVI Maske Rio Gualaxo ASCII output.txt", skip=5)
aug17 <- read.table("2017-08-29 NDVI Maske Rio Gualaxo ASCII output.txt", skip=5)

# Transform and consolidate data
oct15 <- oct15 %>% mutate(Scene=ymd("2015-10-11")) %>% gather(-Scene, key="Position", value="NDVI") %>% filter(NDVI != 999)
nov15 <- nov15 %>% mutate(Scene=ymd("2015-11-12")) %>% gather(-Scene, key="Position", value="NDVI") %>% filter(NDVI != 999)
aug17 <- aug17 %>% mutate(Scene=ymd("2017-08-29")) %>% gather(-Scene, key="Position", value="NDVI") %>% filter(NDVI != 999)
dat <- bind_rows(oct15, nov15, aug17)

# Plot histogram
p <- ggplot(dat, aes(NDVI))+
      geom_histogram(binwidth=.005, col="darkgreen",fill="darkgreen")+
      facet_grid(Scene~.)+
      coord_cartesian(xlim=c(0,1))+
      theme_bw()

print(p)

# Write data
# write.table(dat, "dat.txt")