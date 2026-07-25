#------------------------------------------
# This script sets out to plot FTM and FFM
# results against chance
#------------------------------------------

#--------------------------------------
# Author: Trent Henderson, 18 May 2026
#--------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(Hmisc)
library(latex2exp)

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

results <- do.call("rbind", results) |> filter(feature_set != "catch22")
rm(results_files, i, tmp)

#------------- Results visualisation --------------

# Order problems by mean accuracy

means <- results |>
  filter(feature_set == "Moments 1,2") |>
  reframe(.mean = mean(accuracy), .by = "problem")

# Draw plot

dark2 <- RColorBrewer::brewer.pal(8, "Dark2")

p <- results |>
  filter(feature_set == "Moments 1,2") |>
  mutate(accuracy = accuracy * 100) |>
  inner_join(means, by = c("problem" = "problem")) |>
  inner_join(chances, by = c("problem" = "problem")) |>
  mutate(chance = chance * 100) |>
  mutate(feature_set = "Moments 1 and 2") |>
  ggplot(aes(x = reorder(problem, .mean))) +
  geom_point(aes(y = chance, colour = "Chance probability", shape = "Chance probability"), size = 2) +
  stat_summary(aes(y = accuracy, colour = feature_set, group = feature_set), geom = "errorbar",
               fun.data = mean_cl_normal, fun.args = list(conf.int = 0.95)) +
  stat_summary(aes(y = accuracy, colour = feature_set, shape = feature_set, group = feature_set),
               geom = "point", fun = mean) +
  labs(x = "Problem",
       y = "Classification accuracy (%)",
       colour = NULL) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(from = 0, to = 100, by = 20),
                     labels = function(x)paste0(x, "%")) +
  scale_color_manual(name = "Type",
                     breaks = c("Chance probability", "Moments 1 and 2"),
                     labels = c("Chance probability", "Mean accuracy of Moments 1 and 2"),
                     values = c("Chance probability" = "black", "Moments 1 and 2" = dark2[1])) +
  scale_shape_manual(name = "Type",
                     breaks = c("Chance probability", "Moments 1 and 2"),
                     labels = c("Chance probability", "Mean accuracy of Moments 1 and 2"),
                     values = c("Chance probability" = 3, "Moments 1 and 2" = 16)) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

print(p)
ggsave("output/moments-vs-chance.pdf", plot = p, units = "in", height = 20, width = 14)

# Draw boxplot

p1 <- results |>
  mutate(accuracy = accuracy * 100) |> 
  inner_join(chances, by = c("problem" = "problem")) |>
  mutate(chance = chance * 100) |>
  mutate(delta = accuracy - chance) |>
  mutate(feature_set = case_when(
          feature_set == "Moment 1"        ~ "Mean",
          feature_set == "Moments 1,2"     ~ "Mean + variance",
          feature_set == "Moments 1,2,3"   ~ "Mean + variance +\nskewness",
          feature_set == "Moments 1,2,3,4" ~ "Mean + variance +\nskewness +\nkurtosis")) |>
  ggplot(aes(x = feature_set, y = delta, fill = feature_set)) + 
  geom_boxplot(alpha = 0.9, colour = "black") +
  geom_hline(aes(yintercept = 0), colour = "black", linewidth = 0.9, linetype = "dashed") +
  labs(x = "Feature set", 
       y = "Difference in raw classification accuracy (%) relative to chance",
       #y = TeX(r"($\Delta_{Raw \, accuracy}$)"),
       fill = NULL) +
  scale_y_continuous(labels = function(x)paste0(x, "%")) +
  scale_fill_manual(values = c("Mean" = dark2[1], "Mean + variance" = dark2[2],
                                 "Mean + variance +\nskewness" = dark2[3], 
                               "Mean + variance +\nskewness +\nkurtosis" = dark2[4])) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

print(p1)
ggsave("output/moments-dists.pdf", plot = p1, units = "in", height = 6, width = 11)

#------------- Summary statistics for main text --------------

# Get train-test split sizes

get_split_sizes <- function(){
  the_files <- gsub("\\.Rda", "\\1", list.files("feature-calculations/features/"))
  storage <- vector(mode = "list", length = length(the_files))
  
  for(i in 1:length(the_files)){
    problem <- the_files[i]
    storage[[i]] <- data.frame(
      problem = problem,
      n_train = nrow(read.csv(paste0("data/", problem, "/", problem, "_train_y.csv"))),
      n_test  = nrow(read.csv(paste0("data/", problem, "/", problem, "_test_y.csv")))
    )
  }
  
  do.call("rbind", storage)
}

split_sizes <- get_split_sizes()

# Compute statistical tests

benchmark_keepers <- results |>
  filter(feature_set == "Moments 1,2,3,4") |>
  left_join(chances, by = c("problem" = "problem")) |>
  left_join(split_sizes, by = c("problem" = "problem")) |>
  reframe(
    mu = mean(accuracy, na.rm = TRUE),
    sigma2 = var(accuracy, na.rm = TRUE), # Unbiased, denominator = n-1
    n = sum(!is.na(accuracy)),
    chance = unique(chance),
    rho = unique(n_test) / unique(n_train), # n2 / n1
    .by = "problem"
  ) |>
  mutate(
    se = sqrt(sigma2 * (1 / n + rho)), # Nadeau & Bengio (2003) correction
    t.stat = (mu - chance) / se,
    p.value = pt(t.stat, df = n - 1, lower.tail = FALSE) # One-sided test: H1 accuracy > chance
  ) |>
  mutate(category = ifelse(p.value <= 0.05, "Significant", "Non-significant")) |>
  dplyr::select(problem, p.value, category)


# Total significant problems (FTM vs chance)

benchmark_keepers |>
  reframe(counter = n(), .by = "category") |>
  mutate(props = counter / sum(counter))
