# Install tidyverse package if necessary
if (!require(tidyverse)) {
      install.packages("tidyverse")
      library(tidyverse)
}


# This function reads an ENVI Standard ASCII output file and
# transforms it into a tidy long format data frame:

tidy_ENVI_ASCII <- function(
      file,
      scene_id=file, 
      value="value",
      mask=FALSE,
      sort_by=FALSE,
      decreasing=FALSE,
      tibble=TRUE) {
      
      # Requires tidyverse
      
      # Read ENVI Standard ASCII output file
      d <- read.table(file, skip=5)
      
      # Transform data frame
      d <- d %>% 
            setNames(substring(names(d), first=2)) %>%
            rowid_to_column(var="y") %>%
            mutate(scene_id=scene_id) %>%
            gather(-scene_id, -y, key="x", value="value", convert=TRUE) %>%
            filter(value != mask) %>%
            rename(!!value:=value) %>%
            select(scene_id, x, y, !!value)
      
      # Sort data frame if indicated
      if(sort_by=="position") {
            d <- arrange(d, x, y)
      }
      else if(sort_by=="value") {
            d <- d[order(d[value], decreasing=decreasing),]
            
      }
      
      # Return tidy data as tibble or data frame 
      if(tibble) return(as.tibble(d)) else return(d)
}


# Read and transform Rio Gualaxo masked NDVI data
oct15 <- tidy_ENVI_ASCII(
      file="2015-10-11 NDVI Maske Rio Gualaxo ASCII output.txt",
      scene_id="11.10.2015",
      value="NDVI",
      mask=999
      )
nov15 <- tidy_ENVI_ASCII(
      file="2015-11-12 NDVI Maske Rio Gualaxo ASCII output.txt",
      scene_id="12.11.2015",
      value="NDVI",
      mask=999
      )
aug17 <- tidy_ENVI_ASCII(
      file="2017-08-29 NDVI Maske Rio Gualaxo ASCII output.txt",
      scene_id="29.08.2017",
      value="NDVI",
      mask=999
)
dat <- bind_rows(oct15, nov15, aug17)

# Construct histogram
g <- ggplot(dat, aes(NDVI))+
      geom_histogram(binwidth=.005, col="darkgreen",fill="darkgreen")+
      facet_grid(scene_id~.)+
      coord_cartesian(xlim=c(0,1))+
      ylab("Anzahl Pixel")+
      theme_bw()

# Add median line
m <- dat %>%
      group_by(scene_id) %>%
      summarise(NDVI=round(median(NDVI),3)) %>%
      mutate(text=paste("Median =", NDVI))
m <- as.data.frame(m)

p <- g+
      geom_vline(mapping=aes(xintercept = NDVI), data=m, lty=5, lwd=1, col="grey35")+
      geom_text(mapping=aes(label=text,x=NDVI+.02), y=200, hjust=0, size=3.75, col="grey35", data=m)
      

# Print
print(p)