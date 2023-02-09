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

load("data/mean_sd_outputs.Rda")
load("data/catch24.Rda")

# Load problem metadata

load("data/problem_summaries.Rda")

# Merge datasets

mean_sd_outputs <- mean_sd_outputs %>%
  mutate(method = "Mean and variance")

catch24 <- catch24 %>%
  mutate(method = "catch24")

both <- bind_rows(mean_sd_outputs, catch24)

# Calculate summary statistics for plotting later

aggregated <- list()

for(i in c("Mean and variance", "catch24")){
  tmp <- both %>%
    filter(method == i) %>%
    mutate(balanced_accuracy = balanced_accuracy * 100) %>%
    group_by(problem) %>%
    summarise(balanced_accuracy_mean = mean(balanced_accuracy, na.rm = TRUE),
              balanced_accuracy_sd = sd(balanced_accuracy, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(lower = balanced_accuracy_mean - 1 * balanced_accuracy_sd,
           upper = balanced_accuracy_mean + 1 * balanced_accuracy_sd)
  
  if(i == "Mean and variance"){
    colnames(tmp) <- c("problem", "mean_and_var_bal_acc_mean", "mean_and_var_bal_acc_sd", "lower_x", "upper_x")
  } else{
    colnames(tmp) <- c("problem", "catch24_bal_acc_mean", "catch24_bal_acc_sd", "lower_y", "upper_y")
  }
  aggregated[[match(i, c("Mean and variance", "catch24"))]] <- tmp
}

aggregated <- do.call("left_join", aggregated)

#-------------- Do statistical comparisons ---------------

p_values <- unique(both$problem) %>%
  purrr::map_df(~ calculate_p_values(data = both, theproblem = .x, problem_data = problem_summaries))

aggregated2 <- aggregated %>%
  inner_join(p_values, by = c("problem" = "problem")) %>%
  mutate(p.value.adj = p.adjust(p.value, method = "holm")) %>%
  mutate(significant = ifelse(p.value.adj < 0.05, "Significant difference", "Non-significant difference"),
         top_performer = case_when(
           significant == "Significant difference" & catch24_bal_acc_mean > mean_and_var_bal_acc_mean ~ "catch24",
           significant == "Significant difference" & catch24_bal_acc_mean < mean_and_var_bal_acc_mean ~ "Mean and variance",
           significant == "Non-significant difference"                                                ~ "Non-significant difference")) %>%
  mutate(significant = ifelse(catch24_bal_acc_sd == 0 | mean_and_var_bal_acc_sd == 0, "Zero variance for one/more sets", significant),
         top_performer = ifelse(catch24_bal_acc_sd == 0 | mean_and_var_bal_acc_sd == 0, "Zero variance for one/more sets", top_performer))

#-------------- Draw summary graphic ---------------

# Create palette for whoever is top performer

mypal <- c("Non-significant difference" = "grey80",
           "Zero variance for one/more sets" = "grey50",
           "Mean and variance" = "#377EB8",
           "catch24" = "#FF0029") # Same first 2 colours as my palette from big upcoming classification paper

# Define coordinates for upper triangle to shade

upper_tri <- data.frame(x = c(0, 0, 100), y = c(0, 100, 100))

# Separate into significant and non-significant for point sizing

ns <- aggregated2 %>%
  filter(top_performer %in% c("Non-significant difference", "Zero variance for one/more sets"))

sig <- aggregated2 %>%
  filter(top_performer %in% c("Mean and variance", "catch24"))

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
  geom_point(data = sig, aes(colour = top_performer), size = 3) +
  annotate("text", x = 75, y = 10, label = "Mean and variance", size = 4) +
  annotate("text", x = 25, y = 90, label = "catch24", size = 4) +
  labs(x = "Balanced classification accuracy mean and variance (%)",
       y = "Balanced classification accuracy catch24 (%)",
       colour = NULL) +
  scale_x_continuous(labels = function(x)paste0(x, "%"),
                     breaks = seq(from = 0, to = 100, by = 20)) + 
  scale_y_continuous(labels = function(x)paste0(x, "%"),
                     breaks = seq(from = 0, to = 100, by = 20)) + 
  scale_colour_manual(values = mypal) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11))

print(p)
ggsave("output/catch2-vs-catch24.pdf", p, units = "in", height = 9, width = 9)
