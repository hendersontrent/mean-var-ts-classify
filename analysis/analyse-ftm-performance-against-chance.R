#------------------------------------------
# This script sets out to plot FTM results
#
# NOTE: This script requires setup.R and
# analysis/compute-mean-and-var.R to have 
# been run first
#-----------------------------------------

#--------------------------------------
# Author: Trent Henderson, 3 March 2023
#--------------------------------------

# Load data

load("data/ftm.Rda")
load("data/TimeSeriesData.Rda")

# Get chance probabilities

num_classes <- TimeSeriesData %>%
  dplyr::select(c(target, problem)) %>%
  distinct() %>%
  group_by(problem) %>%
  summarise(classes = n()) %>%
  ungroup() %>%
  mutate(chance = 1 / classes)

rm(TimeSeriesData)

#------------- Final list of problems --------------

# Find out for which problems mean and SD significantly outperformed chance

benchmark_keepers <- ftm %>%
  group_by(problem) %>%
  summarise(mu = mean(balanced_accuracy, na.rm = TRUE),
            sigma = sd(balanced_accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(num_classes, by = c("problem" = "problem")) %>%
  mutate(p.value = pnorm(chance, 
                         mean = mu,
                         sd = sigma,
                         lower.tail = FALSE),
         p.value = 1 - p.value) %>%
  mutate(category = ifelse(p.value <= 0.05, "Significant", "Non-significant")) %>%
  dplyr::select(problem, p.value, category, mu)

save(benchmark_keepers, file = "data/benchmark_keepers.Rda")

#------------- Results visualisation --------------

p <- ftm %>%
  mutate(balanced_accuracy = balanced_accuracy * 100) %>%
  group_by(problem) %>%
  summarise(mu = mean(balanced_accuracy, na.rm = TRUE),
            lower = mean(balanced_accuracy, na.rm = TRUE) - 1 * sd(balanced_accuracy, na.rm = TRUE),
            upper = mean(balanced_accuracy, na.rm = TRUE) + 1 * sd(balanced_accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(num_classes, by = c("problem" = "problem")) %>%
  left_join(benchmark_keepers, by = c("problem" = "problem")) %>%
  mutate(chance = chance * 100) %>%
  filter(category == "Significant") %>%
  ggplot() +
  geom_errorbar(aes(ymin = lower, ymax = upper, x = reorder(problem, mu), y = mu), colour = RColorBrewer::brewer.pal(6, "Dark2")[1]) +
  geom_point(aes(x = reorder(problem, mu), y = mu), colour = RColorBrewer::brewer.pal(6, "Dark2")[1]) +
  geom_point(aes(x = reorder(problem, mu), y = chance), colour = "black", shape = 3, size = 1) +
  labs(x = "Problem",
       y = "Balanced classification accuracy (%)",
       colour = NULL) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(from = 0, to = 100, by = 20),
                     labels = function(x)paste0(x, "%")) + 
  scale_colour_brewer(palette = "Dark2") +
  coord_flip() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

print(p)
ggsave("output/mean-and-sd-resamples.pdf", plot = p, units = "in", height = 16, width = 11)

#------------- Summary statistics for main text --------------

# Total significant problems

benchmark_keepers %>%
  group_by(category) %>%
  summarise(counter = n()) %>%
  ungroup() %>%
  mutate(props = counter / sum(counter))
