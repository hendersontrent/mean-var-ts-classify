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

# Load data

load("data/mean_sd_test.Rda")

#----------------------- Draw graphics ------------------

# Filter feature dataframe to problems of interest and draw plot

p <- mean_sd_test %>%
  filter(problem == "GunPointOldVersusYoung") %>%
  mutate(group = ifelse(group == 1, "Young", "Old")) %>%
  pivot_wider(id_cols = c("id", "problem", "group", "method", "set_split"), names_from = "names", values_from = "values") %>%
  mutate(group = as.factor(group)) %>%
  ggplot(aes(x = mu, y = sigma)) +
  stat_ellipse(aes(x = mu, y = sigma, fill = group), geom = "polygon", alpha = 0.2) +
  guides(fill = "none") +
  geom_point(size = 2.25, aes(colour = group)) +
  labs(x = "Mean",
       y = "Standard deviation",
       colour = "Group") +
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank()) +
  facet_wrap(~problem, scales = "free", nrow = 1, ncol = 1)

print(p)

p1 <- mean_sd_test %>%
  filter(problem %in% c("InsectEPGRegularTrain", "InsectEPGSmallTrain")) %>%
  pivot_wider(id_cols = c("id", "problem", "group", "method", "set_split"), names_from = "names", values_from = "values") %>%
  mutate(group = as.factor(group)) %>%
  ggplot(aes(x = mu, y = ..density.., fill = group)) +
  geom_histogram() +
  labs(x = "Mean",
       y = "Density",
       fill = "Group") +
  scale_fill_manual(values = c("#7570B3", "#E7298A", "#E6AB02")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank()) +
  facet_wrap(~problem, scales = "free", nrow = 2, ncol = 1)

print(p1)

p2 <- patchwork::wrap_plots(list(p, p1), nrow = 1, ncol = 2)
ggsave("output/mean-and-var-case-studies.pdf", plot = p2, units = "in", height = 8, width = 11)
