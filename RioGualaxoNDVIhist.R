# Install tidyverse package if necessary
if (!require(tidyverse)) {
      install.packages("tidyverse")
      library(tidyverse)
}


# This function reads an ENVI Standard ASCII output file and
# transforms it to a tidy, long format data frame:

tidy_ENVI_ASCII <- function(
      file,
      id=file, 
      value="value",
      mask=FALSE,
      sort_by=FALSE,
      decreasing=FALSE,
      tibble=TRUE) {
      
      # Requires tidyverse
      
      # Read ENVI Standard ASCII output file
      d <- read.table(file, skip=5)
      
      # Transform data frame
      names(d) <- substring(names(d), first=2)
      d <- d %>% 
            rowid_to_column(var="y") %>%
            mutate(id=id) %>%
            gather(-id, -y, key="x", value="value", convert=TRUE) %>%
            filter(value != mask) %>%
            rename(!!value:=value) %>%
            select(id, x, y, !!value)
      
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
      id="11.10.2015",
      value="NDVI",
      mask=999
      )
nov15 <- tidy_ENVI_ASCII(
      file="2015-11-12 NDVI Maske Rio Gualaxo ASCII output.txt",
      id="12.11.2015",
      value="NDVI",
      mask=999
      )
aug17 <- tidy_ENVI_ASCII(
      file="2017-08-29 NDVI Maske Rio Gualaxo ASCII output.txt",
      id="29.08.2017",
      value="NDVI",
      mask=999
)
dat <- bind_rows(oct15, nov15, aug17)

# Plot histogram
p <- ggplot(dat, aes(NDVI))+
      geom_histogram(binwidth=.005, col="darkgreen",fill="darkgreen")+
      facet_grid(id~.)+
      coord_cartesian(xlim=c(0,1))+
      ylab("Anzahl Pixel")
      theme_bw()

print(p)