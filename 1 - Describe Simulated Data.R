#' 1 - Describe Simulated Data
#' 
#' Problem: A country has identified signs of widespread impact to 
#' child development, possibly a disease or toxic substance outbreak.
#' 
#' Describe:
#'  number of children by city, state, and age
#'  typical severity by city, state, and age


if( !require(ggplot2) ){ 
  stop("You need to install ggplot2 to continue")
   # Code you need 
   # install.packages("ggplot2") # once per computer 
   # library(ggplot2) # once per R session
} 

child_outbreak_tbl <- read.csv("child_outbreak_simulation.csv", 
                               row.names = NULL)

# 1000 children 
nrow(child_outbreak_tbl)

# table of severity
table( child_outbreak_tbl$severity )


# plot of severity across age
ggplot(child_outbreak_tbl,
       aes(x = 1, 
           y = age)) + facet_grid(cols = vars(severity)) +
  geom_violin(position = position_dodge(width = 0.4)) + 
  geom_boxplot(width = 0.2, outlier.alpha = 0, 
               position = position_dodge(width = 0.4)) +
  theme_classic() + scale_y_continuous(breaks = 1:12) + 
  theme(strip.background = element_rect(fill = 'black'),
        strip.text = element_text(colour = 'white'),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = rel(1.5)),
        legend.position = "none")
