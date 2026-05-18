#------------------------------------------
# This script sets out to plot FTM results
#------------------------------------------

#--------------------------------------
# Author: Trent Henderson, 18 May 2026
#--------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(Hmisc)

#--------------- Get chance probabilities ---------------

#' Function to load all UEA/UCR datasets that has been saved from `aeon` in Python and get the chance probability
#' 
#' @return \code{data.frame} containing the summary statistics
#' @author Trent Henderson
#' 

get_chance_probability <- function(){
  
  the_files <- gsub("\\.Rda", "\\1", list.files("feature-calculations/features/"))
  storage <- vector(mode = "list", length = length(the_files))
  
  for(i in 1:length(the_files)){
    
    problem <- the_files[i]
    
    # Pull labels from saved format
    
    train_y <- read.csv(paste0("data/", problem, "/", problem, "_train_y.csv"))
    
    # Calculate chance probability
    
    storage[[i]] <- data.frame(problem = problem, chance = 1 / length(unique(train_y$target)))
  }
  
  storage <- do.call("rbind", storage)
  return(storage)
}

chances <- get_chance_probability()

#--------------- Get classification results ---------------

results_files <- list.files("classification-models/results/")
results <- vector(mode = "list", length = length(results_files))

for(i in 1:length(results_files)){
  tmp <- read.csv(paste0("classification-models/results/", results_files[i]))
  results[[i]] <- tmp
}

results <- do.call("rbind", results) |> filter(feature_set == "User")
rm(results_files, i, tmp)

#------------- Find problems where FTM outperforms chance --------------

benchmark_keepers <- results |>
  reframe(mu = mean(accuracy, na.rm = TRUE),
          sigma = sd(accuracy, na.rm = TRUE),
          .by = "problem") |>
  left_join(chances, by = c("problem" = "problem")) |>
  mutate(p.value = pnorm(chance, 
                         mean = mu,
                         sd = sigma,
                         lower.tail = FALSE),
         p.value = 1 - p.value) |>
  mutate(category = ifelse(p.value <= 0.05, "Significant", "Non-significant")) |>
  dplyr::select(problem, p.value, category)

#------------- Results visualisation --------------

means <- results |>
  reframe(.mean = mean(accuracy), .by = "problem")

p <- results |>
  mutate(accuracy = accuracy * 100) |>
  inner_join(means, by = c("problem" = "problem")) |>
  inner_join(chances, by = c("problem" = "problem")) |>
  inner_join(benchmark_keepers, by = c("problem" = "problem")) |>
  mutate(chance = chance * 100) |>
  filter(category == "Significant") |>
  mutate(colour1 = "Mean accuracy",
         colour2 = "Chance probability",
         shape1 = "Mean accuracy",
         shape2 = "Chance probability") |>
  ggplot(aes(x = reorder(problem, .mean))) +
  geom_point(aes(y = chance, colour = colour2, shape = shape2), size = 1) +
  stat_summary(aes(y = accuracy, colour = colour1), geom = "errorbar",
               fun.data = mean_cl_normal, fun.args = list(conf.int = 0.95)) +
  stat_summary(aes(y = accuracy, colour = colour1, shape = shape1), geom = "point", fun = mean) +
  labs(x = "Problem",
       y = "Classification accuracy (%)",
       colour = NULL) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(from = 0, to = 100, by = 20),
                     labels = function(x)paste0(x, "%")) + 
  scale_color_manual(name = "Type", labels = c("Chance probability", "Mean accuracy of FTM with 95% CI"), values = c("black", RColorBrewer::brewer.pal(6, "Dark2")[1])) +
  scale_shape_manual(name = "Type", labels = c("Chance probability", "Mean accuracy of FTM with 95% CI"), values = c(3, 16)) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

print(p)
ggsave("output/mean-and-sd-resamples.pdf", plot = p, units = "in", height = 17, width = 11)

#------------- Summary statistics for main text --------------

# Total significant problems

benchmark_keepers |>
  reframe(counter = n(), .by = "category") |>
  mutate(props = counter / sum(counter))
