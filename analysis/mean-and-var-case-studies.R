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
  annotate("text", x = 1125, y = 225, label = "bold(A)", parse = TRUE, size = 7) +
  labs(subtitle = "GunPointOldVersusYoung",
       x = "Mean",
       y = "Standard deviation",
       colour = "Group") +
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.subtitle = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 14))

print(p)

text_df <- data.frame(x = c(2.5),
                      y = c(8.07),
                      label = c("B"))

p1 <- mean_sd_test %>%
  filter(problem == "InsectEPGRegularTrain") %>%
  pivot_wider(id_cols = c("id", "problem", "group", "method", "set_split"), names_from = "names", values_from = "values") %>%
  mutate(group = as.factor(group)) %>%
  ggplot() +
  geom_histogram(aes(x = mu, y = ..density.., fill = group)) +
  geom_text(data = text_df, aes(x = x, y = y, label = label), fontface = "bold", size = 7) +
  labs(subtitle = "InsectEPGRegularTrain",
       x = "Mean",
       y = "Probability density",
       fill = "Group") +
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(6, "Dark2")[3], RColorBrewer::brewer.pal(6, "Dark2")[4], 
                               RColorBrewer::brewer.pal(6, "Dark2")[6])) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.text = element_text(size = 14),
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 14))

print(p1)

p2 <- patchwork::wrap_plots(list(p, p1), nrow = 1, ncol = 2)
ggsave("output/mean-and-var-case-studies.pdf", plot = p2, units = "in", height = 5.5, width = 11)
