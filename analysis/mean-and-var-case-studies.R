#-----------------------------------------
# This script analyses select case studies
# for mean and variance
#
# NOTE: This script requires setup.R and
# analysis/compute-mean-and-var.R to 
# have been run first
#-----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 9 February 2023
#-----------------------------------------

# Define vector of 2 problems that got 100% and 1 that got close

the_probs <- c("InsectEPGRegularTrain", "GunPointOldVersusYoung", "InsectEPGSmallTrain")

# Filter feature dataframe to problems of interest

mean_sd_test_filt <- mean_sd_test %>%
  filter(problem %in% the_probs) %>%
  pivot_wider(id_cols = c("id", "problem", "group", "method"), names_from = "names", value_from = "values")

#----------------------- Draw graphics ------------------

#' Draw scatterplot of mean against variance for a given problem
#' 
#' @param data \code{data.frame} containing feature values
#' @param theproblem \code{string} specifying the problem to draw the plot for
#' @return object of class \code{ggplot}
#' @author Trent Henderson
#' 

plot_mean_var <- function(data, theproblem){
  
  p <- data %>%
    filter(problem == theproblem) %>%
    mutate(group = as.factor(group)) %>%
    ggplot(aes(x = DN_Mean, y = DN_Spread_Std)) +
    stat_ellipse(aes(x = DN_Mean, y = DN_Spread_Std, fill = group), geom = "polygon", alpha = 0.2) +
    guides(fill = "none") +
    geom_point(size = 2.25, aes(colour = group)) +
    labs(x = "Mean",
         y = "Standard deviation",
         colour = NULL) +
    scale_fill_brewer(palette = "Dark2") +
    scale_colour_brewer(palette = "Dark2") +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          legend.position = "bottom")
  
  return(p)
}

# Run the function

plots <- list()

for(i in 1:length(the_probs)){
  plots[[i]] <- plot_mean_var(data = mean_sd_test_filt, theproblem = the_probs[i])
}

# Merge the graphics

p2 <- patchwork::wrap_plots(plots, ncol = length(the_probs), nrow = 1)
print(p2)
ggsave("output/mean-and-var-case-studies.pdf", plot = p2, units = "in", height = 8, width = 11)
