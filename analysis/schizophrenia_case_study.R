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
load("fMRI_analysis_data/schizophrenia_case_study/metadata.Rda")

# Load catch2 and catch22 feature values for data
load("fMRI_analysis_data/schizophrenia_case_study/all_feature_values.Rda")

# Load balanced accuracy results from 10-resample 10-fold CV linear SVM
load("fMRI_analysis_data/schizophrenia_case_study/balanced_accuracy_by_repeats.Rda")

# Load SVM coefficients
load("fMRI_analysis_data/schizophrenia_case_study/SVM_coefficients.Rda")

# Load null balanced accuracy results from 10-resample 10-fold CV linear SVM
load("fMRI_analysis_data/schizophrenia_case_study/Null_SVM_balanced_accuracy.Rda")

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
# Compare catch2 vs. catch22 classification performance
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
  group_by(Analysis_Type, Univariate_Feature_Set) %>%
  mutate(p_value_BH = p.adjust(p_value, method="BH"),
         p_value_Bonferroni = p.adjust(p_value, method="bonferroni"))

# TS Feature-wise
mean_balanced_accuracy %>%
  filter(Analysis_Type == "Univariate_TS_Feature") %>%
  left_join(., p_values) %>%
  filter(p_value_Bonferroni < 0.05) %>%
  mutate(Univariate_Feature_Set = factor(Univariate_Feature_Set, levels=c("FTM", "catch22"))) %>%
  ggplot(data=., mapping=aes(x = Balanced_Accuracy_Across_Repeats, color = Univariate_Feature_Set)) +
  geom_vline(aes(xintercept = Balanced_Accuracy_Across_Repeats, color = Univariate_Feature_Set), linewidth=1.5) +
  labs(color = "Feature Set") +
  scale_colour_brewer(palette = "Dark2") +
  ggtitle("Schizophrenia vs. Control Classification within TS Features by Feature Set") +
  scale_x_continuous(limits=c(55, 70)) +
  xlab("Mean Balanced Accuracy (%)") +
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        plot.title = element_text(hjust=0.5,margin=margin(0,0,20,0),
                                  size=11))
ggsave("output/Schizophrenia_BalAcc_TSFeature_Feature_Set.png",
       width = 6, height = 2, units="in", dpi=300)

# Use correctR to test for difference across resamples
num_samples <- length(unique(metadata$Sample_ID))
training_size <- ceiling(0.9*num_samples)
test_size <- floor(0.1*num_samples)

data_for_correctR <- balanced_accuracy_by_repeats %>%
  filter(Analysis_Type == "Univariate_Combo") %>% 
  pivot_wider(id_cols = c(Repeat_Number, group_var), 
              names_from = Univariate_Feature_Set,
              values_from = Repeat_Balanced_Accuracy) %>%
  dplyr::rename("x" = "FTM", "y" = "catch22")

resampled_ttest(x=data_for_correctR$x, y=data_for_correctR$y, n=10, n1=training_size, n2=test_size)

# Our transformation function
scaleFUN <- function(x) sprintf("%.0f", x)

# Combo-wise
balanced_accuracy_by_repeats %>%
  filter(Analysis_Type == "Univariate_Combo") %>%
  mutate(Univariate_Feature_Set = factor(Univariate_Feature_Set, levels=c("FTM", "catch22")),
         Repeat_Balanced_Accuracy = 100*Repeat_Balanced_Accuracy) %>%
  ggplot(data=., ) +
  geom_bracket(xmin = "FTM", xmax = "catch22", y.position = 75, 
               label = "**", label.size = 10) +
  ggtitle("Schizophrenia vs. Control\nClassification with\nRegion-by-Feature\nCombinations") +
  scale_fill_brewer(palette = "Dark2") +
  labs(fill = "Feature Set") +
  geom_violin(mapping=aes(x = Univariate_Feature_Set, 
                          y = Repeat_Balanced_Accuracy,
                          fill = Univariate_Feature_Set)) +
  geom_line(aes(x = Univariate_Feature_Set, 
                y = Repeat_Balanced_Accuracy,
                group = Repeat_Number), alpha=0.3) +
  geom_boxplot(aes(x = Univariate_Feature_Set, 
                   y = Repeat_Balanced_Accuracy),
               fill=NA, width=0.1) +
  ylab("Resample Balanced Accuracy (%)") +
  scale_y_continuous(expand=c(0,0,0.1,0),
                     labels = scaleFUN) +
  xlab("Feature Set") +
  theme(legend.position = "none",
        axis.text = element_text(size=16),
        axis.title = element_text(size=18),
        plot.title = element_text(hjust=0.5, size=13))
ggsave("output/Schizophrenia_BalAcc_Combo_Feature_Set.png",
       width = 4, height = 6, units="in", dpi=300)



################################################################################
# Visualize standard deviation + mean in the brain
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

extract_coefs_for_feature <- function(SVM_coefficients, input_feature) {
  feature_coefs <- SVM_coefficients %>%
    filter(Analysis_Type=="Univariate_TS_Feature",
           group_var==input_feature) %>%
    arrange(desc(abs(coef))) %>%
    mutate(label = ifelse(str_detect(`Feature Name`, "ctx-"),
                          gsub("-", "_", `Feature Name`),
                          as.character(`Feature Name`))) %>%
    mutate(label = gsub("ctx_", "", label)) %>%
    dplyr::select(label, coef) %>%
    dplyr::rename("statistic" = "coef")
  
  return(feature_coefs)
}

# Run t-test for SD
SD_Tdata_for_ggseg <- run_t_test_for_feature(all_feature_values = all_feature_values,
                                             input_feature = "DN_Spread_Std")

# Run t-test for Mean
Mean_Tdata_for_ggseg <- run_t_test_for_feature(all_feature_values = all_feature_values,
                                               input_feature = "DN_Mean")

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
                                 barwidth = 10, 
                                 barheight = 0.75,
                                 title.hjust = 0.5,
                                 label.position = "bottom"))  +
    theme(plot.title = element_blank(),
          legend.position = "bottom") 
  
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
                                 barwidth = 10, 
                                 barheight = 0.75,
                                 title.hjust = 0.5,
                                 label.position = "bottom"))  +
    theme(plot.title = element_blank(),
          legend.position = "bottom") 
  
  
  return(subcortex_plot)
}

# Plot SD T-statistics in the brain
SD_T_cortex_plot <- plot_data_in_cortex(results_data = SD_Tdata_for_ggseg,
                                        min_value = min(SD_Tdata_for_ggseg$statistic),
                                        max_value = max(SD_Tdata_for_ggseg$statistic),
                                        label_title = "Region T-statistic for SD")

SD_T_subcortex_plot <- plot_data_in_subcortex(results_data = SD_Tdata_for_ggseg,
                                              min_value = min(SD_Tdata_for_ggseg$statistic),
                                              max_value = max(SD_Tdata_for_ggseg$statistic),
                                              label_title = "Region T-statistic for SD")

# Wrap the two plots and combine legends
wrap_plots(list(SD_T_cortex_plot, SD_T_subcortex_plot),
           nrow = 2, heights = c(0.65,0.35)) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom',
        legend.title = element_text(size=9),
        legend.justification = "center")
ggsave("output/Schizophrenia_SD_Tstats_Cortex_Subcortex.png",
       width = 3, height = 4, units="in", dpi=300)


# Plot SD SVM coefficients in the brain
SD_coefs_for_ggseg <- extract_coefs_for_feature(SVM_coefficients, input_feature="DN_Spread_Std")

SD_coefs_cortex_plot <- plot_data_in_cortex(results_data = SD_coefs_for_ggseg,
                                            min_value = min(SD_coefs_for_ggseg$statistic),
                                            max_value = max(SD_coefs_for_ggseg$statistic),
                                            label_title = "Region SVM Coefficients for SD")

SD_coefs_subcortex_plot <- plot_data_in_subcortex(results_data = SD_coefs_for_ggseg,
                                                  min_value = min(SD_coefs_for_ggseg$statistic),
                                                  max_value = max(SD_coefs_for_ggseg$statistic),
                                                  label_title = "Region SVM Coefficients for SD")
# Wrap the two plots and combine legends
wrap_plots(list(SD_coefs_cortex_plot, SD_coefs_subcortex_plot),
           nrow = 2, heights = c(0.65,0.35)) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom',
        legend.title = element_text(size=9),
        legend.justification = "center")
ggsave("output/Schizophrenia_SD_Coefs_Cortex_Subcortex.png",
       width = 3, height = 4, units="in", dpi=300)


# Plot t-statistic for Mean in schizophrenia vs controls by region
Mean_T_cortex_plot <- plot_data_in_cortex(results_data = Mean_Tdata_for_ggseg,
                                          min_value = min(Mean_Tdata_for_ggseg$statistic),
                                          max_value = max(Mean_Tdata_for_ggseg$statistic),
                                          label_title = "Region T-statistic for Mean")

Mean_T_subcortex_plot <- plot_data_in_subcortex(results_data = Mean_Tdata_for_ggseg,
                                                min_value = min(Mean_Tdata_for_ggseg$statistic),
                                                max_value = max(Mean_Tdata_for_ggseg$statistic),
                                                label_title = "Region T-statistic for Mean")
# Wrap the two plots and combine legends
wrap_plots(list(Mean_T_cortex_plot, Mean_T_subcortex_plot),
           nrow = 1, widths = c(0.7,0.3)) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom',
        legend.justification = "center")
ggsave("output/Schizophrenia_Mean_Tstats_Cortex_Subcortex.png",
       width = 4, height = 3, units="in", dpi=300)


# SVM Coefficients for Mean
Mean_coefs_for_ggseg <- extract_coefs_for_feature(SVM_coefficients, input_feature="DN_Mean")

Mean_coefs_cortex_plot <- plot_data_in_cortex(results_data = Mean_coefs_for_ggseg,
                                              min_value = min(Mean_coefs_for_ggseg$statistic),
                                              max_value = max(Mean_coefs_for_ggseg$statistic),
                                              label_title = "Region SVM Coefficients for Mean")

Mean_coefs_subcortex_plot <- plot_data_in_subcortex(results_data = Mean_coefs_for_ggseg,
                                                    min_value = min(Mean_coefs_for_ggseg$statistic),
                                                    max_value = max(Mean_coefs_for_ggseg$statistic),
                                                    label_title = "Region SVM Coefficients for Mean")
# Wrap the two plots and combine legends
wrap_plots(list(Mean_coefs_cortex_plot, Mean_coefs_subcortex_plot),
           nrow = 1, widths = c(0.7,0.3)) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom',
        legend.justification = "center")
ggsave("output/Schizophrenia_Mean_Coefs_Cortex_Subcortex.png",
       width = 4, height = 3, units="in", dpi=300)


# Wrap the two plots and combine legends
wrap_plots(list(Mean_coefs_cortex_plot, Mean_coefs_subcortex_plot),
           nrow = 1, widths = c(0.7,0.3)) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom',
        legend.justification = "center")
ggsave("output/Schizophrenia_Mean_Coefs_Cortex_Subcortex.png",
       width = 4, height = 3, units="in", dpi=300)
