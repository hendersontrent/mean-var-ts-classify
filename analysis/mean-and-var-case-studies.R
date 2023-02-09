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
  filter(problem %in% the_probs) %>%
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
  facet_wrap(~problem, nrow = 1, ncol = 3, scales = "free")

print(p)
ggsave("output/mean-and-var-case-studies.pdf", plot = p, units = "in", height = 8, width = 11)
