################################################################################
# Load libraries and data
#
# Author: Annie G. Bryant, March 2023
################################################################################

theme_set(theme_cowplot())

# Load in metadata 
load("fMRI_analysis_data/metadata.Rda")

# Load balanced accuracy results from 10-resample 10-fold CV linear SVM
load("fMRI_analysis_data/balanced_accuracy_by_repeats.Rda")
balanced_accuracy_by_repeats <- balanced_accuracy_by_repeats %>%
  mutate(Univariate_Feature_Set = factor(Univariate_Feature_Set, levels=c("FTM", "catch24")),
         Repeat_Balanced_Accuracy = 100*Repeat_Balanced_Accuracy) 

# Load in univariate time-series feature info
TS_feature_info <- read.csv("info/catch24_info.csv")

################################################################################
# Compare FTM vs. catch22+FTM classification performance
################################################################################

# Use correctR to test for difference across resamples for FTM vs catch22+FTM
num_samples <- length(unique(metadata$Sample_ID))
training_size <- ceiling(0.9*num_samples)
test_size <- floor(0.1*num_samples)

# Then do FTM vs. catch22+FTM
data_for_correctR_catch24 <- balanced_accuracy_by_repeats %>%
  filter(Analysis_Type == "Univariate_Combo",
         Univariate_Feature_Set %in% c("FTM", "catch24")) %>% 
  pivot_wider(id_cols = c(Repeat_Number, group_var), 
              names_from = Univariate_Feature_Set,
              values_from = Repeat_Balanced_Accuracy) %>%
  dplyr::rename("x" = "FTM", "y" = "catch24")
resampled_ttest(x=data_for_correctR_catch24$x, y=data_for_correctR_catch24$y, n=10, n1=training_size, n2=test_size)

# Function to specify number of decimal points in axis text
scaleFUN <- function(x) sprintf("%.0f", x)

balanced_accuracy_by_repeats %>%
  ggplot(data=., ) +
  geom_boxplot(mapping=aes(x = Univariate_Feature_Set, 
                           y = Repeat_Balanced_Accuracy,
                           color = Univariate_Feature_Set),
               fill=NA) +
  geom_line(aes(x = Univariate_Feature_Set, 
                y = Repeat_Balanced_Accuracy,
                group = Repeat_Number), alpha=0.3) +
  geom_bracket(xmin = "FTM", xmax = "catch24", y.position = 72, 
               label = "**", label.size = 9) +
  scale_color_brewer(palette = "Dark2") +
  
  ylab("Resample Balanced Accuracy (%)") +
  scale_x_discrete(labels = c("FTM", "catch22+FTM")) +
  scale_y_continuous(expand=c(0.05,0,0.1,0),
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

# combo wise results
balanced_accuracy_by_repeats %>%
  filter(Analysis_Type == "Univariate_Combo") %>%
  group_by(Univariate_Feature_Set, group_var) %>%
  summarise(value_mean = round(mean(Repeat_Balanced_Accuracy)*100, 1),
            value_SD = round(sd(Repeat_Balanced_Accuracy)*100, 1)) %>%
  left_join(., p_values)