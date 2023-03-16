#------------------------------------------
# This script sets out to compare catch2
# against catch24
#
# NOTE: This script requires setup.R,
# analysis/summarise-problems.R,
# analysis/compute-mean-and-var.R,
# analysis/compute-catch24.R and 
# analysis/fit-catch24-classifiers.R to 
# have been run first
#------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 10 February 2023
#------------------------------------------

# Load results

load("data/ftm.Rda")
load("data/catch24.Rda")

# Load problem metadata

load("data/problem_summaries.Rda")

# Merge datasets

ftm <- ftm %>%
  mutate(method = "FTM")

catch24 <- catch24 %>%
  mutate(method = "FTM + catch22")

both <- bind_rows(ftm, catch24) %>%
  filter(problem %in% unique(catch24$problem))

# Calculate summary statistics for plotting later

aggregated <- list()

for(i in c("FTM", "FTM + catch22")){
  tmp <- both %>%
    filter(method == i) %>%
    mutate(accuracy = accuracy * 100) %>%
    group_by(problem) %>%
    summarise(accuracy_mean = mean(accuracy, na.rm = TRUE),
              accuracy_sd = sd(accuracy, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(lower = accuracy_mean - 1 * accuracy_sd,
           upper = accuracy_mean + 1 * accuracy_sd)
  
  if(i == "FTM"){
    colnames(tmp) <- c("problem", "mean_and_var_bal_acc_mean", "mean_and_var_bal_acc_sd", "lower_x", "upper_x")
  } else{
    colnames(tmp) <- c("problem", "catch24_bal_acc_mean", "catch24_bal_acc_sd", "lower_y", "upper_y")
  }
  aggregated[[match(i, c("FTM", "FTM + catch22"))]] <- tmp
}

aggregated <- do.call("left_join", aggregated)

#-------------- Do statistical comparisons ---------------

p_values <- unique(both$problem) %>%
  purrr::map_df(~ calculate_p_values(data = both, theproblem = .x, problem_data = problem_summaries))

aggregated2 <- aggregated %>%
  inner_join(p_values, by = c("problem" = "problem")) %>%
  mutate(p.value.adj = p.adjust(p.value, method = "holm")) %>%
  mutate(significant = ifelse(p.value < 0.05, "Significant difference", "Non-significant difference"),
         top_performer = case_when(
           significant == "Significant difference" & catch24_bal_acc_mean > mean_and_var_bal_acc_mean ~ "FTM + catch22",
           significant == "Significant difference" & catch24_bal_acc_mean < mean_and_var_bal_acc_mean ~ "Mean and variance",
           significant == "Non-significant difference"                                                ~ "Non-significant difference")) %>%
  mutate(significant = ifelse(catch24_bal_acc_sd == 0 | mean_and_var_bal_acc_sd == 0, "Zero variance for one/more sets", significant),
         top_performer = ifelse(catch24_bal_acc_sd == 0 | mean_and_var_bal_acc_sd == 0, "Zero variance for one/more sets", top_performer))

#-------------- Draw summary graphic ---------------

# Create palette for whoever is top performer

mypal <- c("Non-significant difference" = "grey80",
           "Zero variance for one/more sets" = "grey50",
           "Mean and variance" = RColorBrewer::brewer.pal(6, "Dark2")[2],
           "FTM + catch22" = RColorBrewer::brewer.pal(6, "Dark2")[1])

# Define coordinates for upper triangle to shade

upper_tri <- data.frame(x = c(0, 0, 100), y = c(0, 100, 100))

# Separate into significant and non-significant for point sizing

ns <- aggregated2 %>%
  filter(top_performer %in% c("Non-significant difference", "Zero variance for one/more sets"))

sig <- aggregated2 %>%
  filter(top_performer %in% c("Mean and variance", "FTM + catch22"))

# Draw scatterplot

p <- ns %>%
  ggplot(aes(x = mean_and_var_bal_acc_mean, y = catch24_bal_acc_mean)) +
  geom_polygon(data = upper_tri, aes(x = x, y = y), fill = "steelblue2", alpha = 0.1) +
  geom_abline(intercept = 0, slope = 1, colour = "grey50", lty = "dashed") +
  geom_errorbar(aes(ymin = lower_y, ymax = upper_y, colour = top_performer)) +
  geom_errorbarh(aes(xmin = lower_x, xmax = upper_x, colour = top_performer)) +
  geom_point(aes(colour = top_performer), size = 2) +
  geom_linerange(data = sig, aes(ymin = lower_y, ymax = upper_y, colour = top_performer), size = 0.7) +
  geom_linerange(data = sig, aes(xmin = lower_x, xmax = upper_x, colour = top_performer), size = 0.7) +
  geom_point(data = sig, aes(colour = top_performer), size = 4) +
  annotate("text", x = 75, y = 10, label = "FTM", size = 5) +
  annotate("text", x = 25, y = 90, label = "FTM + catch22", size = 5) +
  labs(x = "Classification accuracy FTM (%)",
       y = "Classification accuracy FTM + catch22 (%)",
       colour = NULL) +
  scale_x_continuous(labels = function(x)paste0(x, "%"),
                     breaks = seq(from = 0, to = 100, by = 20)) + 
  scale_y_continuous(labels = function(x)paste0(x, "%"),
                     breaks = seq(from = 0, to = 100, by = 20)) + 
  scale_colour_manual(values = mypal) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 14))

print(p)
ggsave("output/catch2-vs-catch24.pdf", p, units = "in", height = 9, width = 9)

#-------------- Calculate average increase ---------------

catch24_mu <- catch24 %>%
  group_by(problem) %>%
  summarise(catch24_avg = mean(accuracy, na.rm = TRUE)) %>%
  ungroup()

ftm_mu <- ftm %>%
  filter(problem %in% unique(catch24_mu$problem)) %>%
  group_by(problem) %>%
  summarise(ftm_avg = mean(accuracy, na.rm = TRUE)) %>%
  ungroup()

ftm_mu %>%
  left_join(catch24_mu) %>%
  mutate(diff = (catch24_avg - ftm_avg) * 100) %>%
  summarise(avg_diff = mean(diff, na.rm = TRUE))
