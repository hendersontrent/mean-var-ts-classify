################################################################################
# Load libraries and data
################################################################################
library(tidyverse)
library(glue)
library(cowplot)
library(ggseg)
library(RColorBrewer)
library(correctR)
library(broom)
library(colorspace)
library(patchwork)
library(scales)
library(ggpubr)
theme_set(theme_cowplot())

# Load in metadata 
load("fMRI_analysis_data/metadata.Rda")

# Load catch2 and catch22 feature values for data
load("fMRI_analysis_data/all_feature_values.Rda")

# Load balanced accuracy results from 10-resample 10-fold CV linear SVM
load("fMRI_analysis_data/balanced_accuracy_by_repeats.Rda")

# Load null balanced accuracy results from 10-resample 10-fold CV linear SVM
load("fMRI_analysis_data/Null_SVM_balanced_accuracy.Rda")

# Load in univariate time-series feature info
TS_feature_info <- read.csv("info/catch24_info.csv")

################################################################################
# Helper functions
################################################################################

# Functions to calculate empirical p-value
compare_main_and_null <- function(main_df_iter, null_distribution_df) {
  # Filter null to this data -- keep all grouping vars in this analysis type
  null_distribution_df <- null_distribution_df %>%
    mutate(Null_Balanced_Accuracy = 100*Null_Balanced_Accuracy) %>%
    dplyr::select(-index, -group_var) %>%
    semi_join(., main_df_iter)
  
  # Compare main balanced accuracy with that of the empirical null distribution
  main_balanced_accuracy_value <- main_df_iter$Balanced_Accuracy_Across_Repeats
  null_balanced_accuracy_values <- null_distribution_df$Null_Balanced_Accuracy
  
  # Find proportion of iterations for which the main balanced accuracy is greater
  prop_main_greater <- sum(main_balanced_accuracy_value > null_balanced_accuracy_values)/length(null_balanced_accuracy_values)
  
  # Find p-value
  p_value <- 1 - prop_main_greater
  
  # Organise into dataframe
  main_df_iter$p_value <- p_value
  return(main_df_iter)
}

################################################################################
# Compare catch2 vs. catch22 vs. catch22+FTM classification performance
################################################################################

# Take the average balanced accuracy per model across ten repeats 
# for a more robust estimate of balanced accuracy
mean_balanced_accuracy <- balanced_accuracy_by_repeats %>%
  group_by(Univariate_Feature_Set, Analysis_Type, group_var) %>%
  summarise(Balanced_Accuracy_Across_Repeats = 100*mean(Repeat_Balanced_Accuracy, na.rm=T))

# Split the dataframe and calculate empirical p-values
balanced_accuracy_split <- mean_balanced_accuracy %>%
  group_by(Analysis_Type, Univariate_Feature_Set, group_var) %>%
  group_split()

p_values <- balanced_accuracy_split %>%
  purrr::map_df(~ compare_main_and_null(main_df_iter = .x,
                                        null_distribution_df = null_balanced_accuracy_all_folds)) %>%
  filter(!(Univariate_Feature_Set == "catch24" & Analysis_Type == "Univariate_TS_Feature" & group_var %in% c("DN_Mean", "DN_Spread_Std"))) %>%
  group_by(Analysis_Type, Univariate_Feature_Set) %>%
  mutate(p_value_BH = p.adjust(p_value, method="BH"),
         p_value_Bonferroni = p.adjust(p_value, method="bonferroni"))

# TS Feature-wise
mean_balanced_accuracy %>%
  filter(Analysis_Type == "Univariate_TS_Feature",
         Univariate_Feature_Set != "catch24") %>%
  left_join(., p_values) %>%
  filter(p_value_Bonferroni < 0.05) %>%
  mutate(Univariate_Feature_Set = factor(Univariate_Feature_Set, levels=c("FTM", "catch22"))) %>%
  ggplot(data=., mapping=aes(x = Balanced_Accuracy_Across_Repeats, color = Univariate_Feature_Set)) +
  geom_vline(aes(xintercept = Balanced_Accuracy_Across_Repeats, color = Univariate_Feature_Set), linewidth=1.5) +
  labs(color = "Feature Set") +
  scale_color_manual(values = brewer.pal(3, "Dark2")[c(1,3)]) +
  scale_x_continuous(limits=c(55, 66), breaks = c(55, 60, 65)) +
  xlab("Mean Balanced Accuracy (%)") +
  theme(legend.position = "bottom",
        axis.title.y = element_blank())
ggsave("output/Schizophrenia_BalAcc_TSFeature_Feature_Set.png",
       width = 6, height = 1.8, units="in", dpi=300)

# Use correctR to test for difference across resamples
num_samples <- length(unique(metadata$Sample_ID))
training_size <- ceiling(0.9*num_samples)
test_size <- floor(0.1*num_samples)

# First do FTM vs. catch22
data_for_correctR_catch22 <- balanced_accuracy_by_repeats %>%
  filter(Analysis_Type == "Univariate_Combo",
         Univariate_Feature_Set %in% c("FTM", "catch22")) %>% 
  pivot_wider(id_cols = c(Repeat_Number, group_var), 
              names_from = Univariate_Feature_Set,
              values_from = Repeat_Balanced_Accuracy) %>%
  dplyr::rename("x" = "FTM", "y" = "catch22")
resampled_ttest(x=data_for_correctR_catch22$x, y=data_for_correctR_catch22$y, n=10, n1=training_size, n2=test_size)

# Then do FTM vs. catch24
data_for_correctR_catch24 <- balanced_accuracy_by_repeats %>%
  filter(Analysis_Type == "Univariate_Combo",
         Univariate_Feature_Set %in% c("FTM", "catch24")) %>% 
  pivot_wider(id_cols = c(Repeat_Number, group_var), 
              names_from = Univariate_Feature_Set,
              values_from = Repeat_Balanced_Accuracy) %>%
  dplyr::rename("x" = "FTM", "y" = "catch24")
resampled_ttest(x=data_for_correctR_catch24$x, y=data_for_correctR_catch24$y, n=10, n1=training_size, n2=test_size)

# Our transformation function
scaleFUN <- function(x) sprintf("%.0f", x)

# Combo-wise
balanced_accuracy_by_repeats_combo <- balanced_accuracy_by_repeats %>%
  filter(Analysis_Type == "Univariate_Combo") %>%
  mutate(Univariate_Feature_Set = factor(Univariate_Feature_Set, levels=c("FTM", "catch24", "catch22")),
         Repeat_Balanced_Accuracy = 100*Repeat_Balanced_Accuracy) 

balanced_accuracy_by_repeats_combo %>%
  ggplot(data=., ) +
  geom_boxplot(mapping=aes(x = Univariate_Feature_Set, 
                           y = Repeat_Balanced_Accuracy,
                           color = Univariate_Feature_Set),
               fill=NA) +
  geom_line(aes(x = Univariate_Feature_Set, 
                y = Repeat_Balanced_Accuracy,
                group = Repeat_Number), alpha=0.3) +
  geom_bracket(xmin = "FTM", xmax = "catch22", y.position = 74, 
               label = "***", label.size = 9) +
  geom_bracket(xmin = "FTM", xmax = "catch24", y.position = 72, 
               label = "**", label.size = 9) +
  scale_color_brewer(palette = "Dark2") +

  ylab("Resample Balanced Accuracy (%)") +
  scale_x_discrete(labels = c("FTM", "catch22+FTM", "catch22")) +
  scale_y_continuous(expand=c(0,0,0.1,0),
                     labels = scaleFUN) +
  xlab("Feature Set") +
  theme(legend.position = "none",
        axis.text = element_text(size=14),
        axis.title = element_text(size=16),
        plot.title = element_text(hjust=0.5, size=13))
ggsave("output/Schizophrenia_BalAcc_Combo_Feature_Set.png",
       width = 4, height = 4.25, units="in", dpi=300)

################################################################################
# Print summary statistics
################################################################################

# Mean + SD individually
balanced_accuracy_by_repeats %>%
  filter(Analysis_Type == "Univariate_TS_Feature", 
         Univariate_Feature_Set == "FTM") %>%
  group_by(group_var) %>%
  summarise(value_mean = round(mean(Repeat_Balanced_Accuracy)*100, 1),
            value_SD = round(sd(Repeat_Balanced_Accuracy)*100, 1)) %>%
  left_join(., p_values)


# combo wise results
balanced_accuracy_by_repeats %>%
  filter(Analysis_Type == "Univariate_Combo") %>%
  group_by(Univariate_Feature_Set, group_var) %>%
  summarise(value_mean = round(mean(Repeat_Balanced_Accuracy)*100, 1),
            value_SD = round(sd(Repeat_Balanced_Accuracy)*100, 1)) %>%
  left_join(., p_values)

# PD_PeriodicityWang_th0_01
balanced_accuracy_by_repeats %>%
  filter(Analysis_Type == "Univariate_TS_Feature", 
         Univariate_Feature_Set == "catch22",
         group_var == "PD_PeriodicityWang_th0_01") %>%
  group_by(Univariate_Feature_Set, group_var) %>%
  summarise(value_mean = round(mean(Repeat_Balanced_Accuracy)*100, 1),
            value_SD = round(sd(Repeat_Balanced_Accuracy)*100, 1)) %>%
  left_join(., p_values)


################################################################################
# Visualize standard deviation in the brain
################################################################################

# Helper function to run t-test for given statistic
run_t_test_for_feature <- function(all_feature_values, input_feature) {
  results <- all_feature_values %>%
    filter(names==input_feature) %>%
    dplyr::select(Brain_Region, Diagnosis, values) %>%
    mutate(Diagnosis = factor(Diagnosis, levels = c("Schizophrenia", "Control"))) %>%
    group_by(Brain_Region) %>%
    nest() %>%
    mutate(
      fit = map(data, ~ t.test(values ~ Diagnosis, data = .x)),
      tidied = map(fit, tidy)
    ) %>% 
    unnest(tidied) %>%
    dplyr::select(-data, -fit) %>%
    arrange(p.value) %>%
    ungroup() %>%
    mutate(label = ifelse(str_detect(Brain_Region, "ctx-"),
                          gsub("-", "_", Brain_Region),
                          as.character(Brain_Region))) %>%
    mutate(label = gsub("ctx_", "", label)) %>%
    dplyr::select(label, statistic)
  return(results)
}

# Run t-test for SD
SD_Tdata_for_ggseg <- run_t_test_for_feature(all_feature_values = all_feature_values,
                                             input_feature = "DN_Spread_Std")


# Helper function to plot cortex data
plot_data_in_cortex <- function(results_data, min_value, 
                                max_value, 
                                label_title) {
  cortex_plot <- results_data %>%
    ggseg(atlas = dk, mapping = aes(fill = statistic),
          position = "stacked", colour = "gray40") +
    scale_fill_continuous_divergingx(palette = 'RdBu', 
                                     mid = 0, 
                                     rev=TRUE, 
                                     na.value="gray90",
                                     limits = c(min_value,
                                                max_value)) +
    theme_void() +
    labs(fill=label_title) +
    guides(fill = guide_colorbar(title.position = "top", 
                                 nrow = 1,
                                 barwidth = 0.75, 
                                 barheight = 7,
                                 title.hjust = 0.5))  +
    theme(plot.title = element_blank()) 
  
  return(cortex_plot)
}

plot_data_in_subcortex <- function(results_data, min_value, max_value, label_title) {
  subcortex_plot <- results_data %>%
    ggplot() +
    geom_brain(atlas = aseg, mapping = aes(fill=statistic), 
               side = "coronal", colour = "gray40") +
    scale_fill_continuous_divergingx(palette = 'RdBu', 
                                     mid = 0, 
                                     rev=TRUE, 
                                     na.value="gray90",
                                     limits = c(min_value,
                                                max_value)) +
    theme_void() +
    labs(fill=label_title) +
    guides(fill = guide_colorbar(title.position = "top", 
                                 nrow = 1,
                                 barwidth = 0.75, 
                                 barheight = 7,
                                 title.hjust = 0.5))  +
    theme(plot.title = element_blank()) 
  
  
  return(subcortex_plot)
}

# Plot SD T-statistics in the brain
SD_T_cortex_plot <- plot_data_in_cortex(results_data = SD_Tdata_for_ggseg,
                                        min_value = min(SD_Tdata_for_ggseg$statistic),
                                        max_value = max(SD_Tdata_for_ggseg$statistic),
                                        label_title = "SD\nT-statistic")

SD_T_subcortex_plot <- plot_data_in_subcortex(results_data = SD_Tdata_for_ggseg,
                                              min_value = min(SD_Tdata_for_ggseg$statistic),
                                              max_value = max(SD_Tdata_for_ggseg$statistic),
                                              label_title = "SD\nT-statistic")

# Wrap the two plots and combine legends
wrap_plots(list(SD_T_cortex_plot, SD_T_subcortex_plot),
           nrow = 1, widths = c(0.7,0.3)) + 
  plot_layout(guides = "collect") & 
  theme(legend.title = element_text(size=9))
ggsave("output/Schizophrenia_SD_Tstats_Cortex_Subcortex.png",
       width = 4, height = 2.5, units="in", dpi=300)
