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

# 
scenes <- c("11.10.2015", "12.11.2015","29.08.2017")
ratios <- c("Fe2O", "Fe3O")
filter_classes <- c(0,7)


# Load classification file
classes <- tidy_ENVI_ASCII(
      file=file.path(path, "2015-10-11 classes.txt"),
      scene_id=scenes[1],
      value="class"
      )

classesNov15 <- classes %>% mutate(scene_id=scenes[2])
classesAug17 <- classes %>% mutate(scene_id=scenes[3])
classes <- bind_rows(classes, classesNov15, classesAug17)

# Load ratio files
fe2O <- list()
for (s in 1:length(scenes)) {
    fe2O[[s]] <- tidy_ENVI_ASCII(
          file=file.path(path, paste0(scenes[s], " Fe2O.txt")),
          scene_id=scenes[s],
          value="Fe2O"
    )
}
fe2O_merged <- bind_rows(fe2O[[1]], fe2O[[2]], fe2O[[3]])

fe3O <- list()
for (s in 1:length(scenes)) {
    fe3O[[s]] <- tidy_ENVI_ASCII(
        file=file.path(path, paste0(scenes[s], " Fe3O.txt")),
        scene_id=scenes[s],
        value="Fe3O"
    )
}
fe3O_merged <- bind_rows(fe3O[[1]], fe3O[[2]], fe3O[[3]])

# Assemble
dat <- inner_join(fe2O_merged, fe3O_merged, by=c("scene_id","x", "y")) %>%
      inner_join(classes, by=c("scene_id","x", "y")) %>%
      filter(!(class %in% filter_classes)) %>%
      mutate(class = recode_factor(class,
                                   `1`="Fels",
                                   `2`="Siedlung",
                                   `3`="Wald",
                                   `4`="Abraum", 
                                   `5`="Tagebau",
                                   `6`="Wiese"                                  
                                   ))

# Remove extreme outliers
dat <- dat %>% filter(between(Fe3O, quantile(dat$Fe3O, .01), quantile(dat$Fe3O, .99)))
dat <- dat %>% filter(between(Fe2O, quantile(dat$Fe2O, .01), quantile(dat$Fe2O, .99)))

# Construct plot
g <- ggplot(dat)+
      geom_density(aes(x=Fe2O), fill="blue", alpha=.3)+
      geom_density(aes(x=Fe3O), fill="orange", alpha=.3)+
      facet_grid(scene_id~class)+
      ylab("Verteilungsdichte")+
      xlab("Eisen(II)oxid- (blau) und Eisen(III)oxid-Ratio (orange), gestrichelte Linien: Median")+
      theme_bw()

# Add median lines
fe2Om <- dat %>%
    group_by(scene_id, class) %>%
    summarise(Fe2O=round(median(Fe2O),3)) %>%
    mutate(text=paste("Median =", Fe2O))
fe2Om <- as.data.frame(fe2Om)

fe3Om <- dat %>%
    group_by(scene_id, class) %>%
    summarise(Fe3O=round(median(Fe3O),3)) %>%
    mutate(text=paste("Median =", Fe3O))
fe3Om <- as.data.frame(fe3Om)

p <- g +
    geom_vline(mapping=aes(xintercept = Fe2O), data=fe2Om, lty=5, lwd=1, col="blue")+
    geom_vline(mapping=aes(xintercept = Fe3O), data=fe3Om, lty=5, lwd=1, col="orange")

# Print
print(p)
