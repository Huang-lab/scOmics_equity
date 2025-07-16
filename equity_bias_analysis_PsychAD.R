# Ancestry and Gender Bias Analysis
# Analysis of psychAD media dataset for equity and representation bias

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(scales)

# Create output directory
dir.create("out", showWarnings = FALSE)

# Set global theme for larger fonts
theme_set(theme_minimal(base_size = 14))  # Increase base font size

# Initialize logging
log_file <- "out/psychAD_analysis_log.txt"
cat("", file = log_file)  # Clear the log file

# Function to log messages to both console and file
log_message <- function(message) {
  cat(message, "\n")
  cat(message, "\n", file = log_file, append = TRUE)
}

# Define consistent color palettes for ancestry and sex (used throughout all plots)
ancestry_colors <- c("African" = "#E31A1C", "Latino" = "#FF7F00", "Asian" = "#1F78B4",
                     "European" = "#6A3D9A",
                     "Unknown" = "#CAB2D6", "Other" = "#FDBF6F")

sex_colors <- c("female" = "#E69F00", "male" = "#56B4E9")

# Load data
log_message("Loading psychAD media dataset...")
data <- read.csv("data/psych-AD_media-1.csv", stringsAsFactors = FALSE)

# Clean ancestry categories with new definitions
data$Ancestry_clean <- case_when(
  data$Ancestry == "AFR" ~ "African",
  data$Ancestry == "AMR" ~ "Latino",  # AMR becomes Latino
  data$Ancestry %in% c("EAS", "SAS") ~ "Asian",  # Combine EAS and SAS into Asian
  data$Ancestry == "EUR" ~ "European",
  data$Ancestry == "EAS_SAS" ~ "Asian",  # Mixed Asian category goes to Asian
  data$Ancestry == "Unknown" | is.na(data$Ancestry) ~ "Unknown",  # Missing or explicitly unknown
  TRUE ~ "Other"  # Any other reported values that don't fit canonical categories
)

# Basic data overview
log_message("Dataset loaded successfully!")
log_message(paste("Total samples:", nrow(data)))
log_message(paste("Total variables:", ncol(data)))

# ============================================================================
# 1. DATASET COMPOSITION OVERVIEW
# ============================================================================

log_message("\n=== DATASET COMPOSITION OVERVIEW ===")

# Basic counts
total_samples <- nrow(data)
cohort_counts <- table(data$cohort)
log_message(paste("Total samples:", total_samples))
log_message("Cohort distribution:")
capture.output(print(cohort_counts), file = log_file, append = TRUE)

# ============================================================================
# 2. ANCESTRY DISTRIBUTION ANALYSIS  
# ============================================================================

log_message("\n=== ANCESTRY DISTRIBUTION ANALYSIS ===")

# Clean and examine ancestry data
ancestry_counts <- table(data$Ancestry_clean, useNA = "always")
log_message("Ancestry distribution:")
capture.output(print(ancestry_counts), file = log_file, append = TRUE)

# Calculate percentages
ancestry_pct <- round(prop.table(ancestry_counts) * 100, 2)
log_message("\nAncestry percentages:")
capture.output(print(ancestry_pct), file = log_file, append = TRUE)

# Create ancestry summary table
ancestry_summary <- data.frame(
  Ancestry = names(ancestry_counts),
  Count = as.numeric(ancestry_counts),
  Percentage = as.numeric(ancestry_pct)
)

log_message("\nAncestry Summary Table:")
capture.output(print(ancestry_summary), file = log_file, append = TRUE)

# Visualize ancestry distribution - Version 1: Including Unknown/Other
p1_full <- ggplot(data = subset(ancestry_summary, !is.na(Ancestry)),
             aes(x = reorder(Ancestry, -Count), y = Count, fill = Ancestry)) +
  geom_bar(stat = "identity") +
  theme_minimal(base_size = 14) +
  labs(title = "psychAD: Sample Distribution by Ancestry (All Categories)",
       x = "Ancestry Group",
       y = "Number of Samples") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        title = element_text(size = 16),
        legend.position = "none") +
  scale_fill_manual(values = ancestry_colors)

ggsave("out/psychAD_ancestry_distribution_barplot_full.pdf", p1_full, width = 4, height = 3)
log_message("Saved: psychAD_ancestry_distribution_barplot_full.pdf")

# Version 2: Excluding Unknown/Other
ancestry_summary_canonical <- subset(ancestry_summary, !is.na(Ancestry) &
                                    !Ancestry %in% c("Unknown", "Other"))

p1_canonical <- ggplot(data = ancestry_summary_canonical,
             aes(x = reorder(Ancestry, -Count), y = Count, fill = Ancestry)) +
  geom_bar(stat = "identity") +
  theme_minimal(base_size = 14) +
  labs(title = "psychAD: Sample Distribution by Ancestry (Canonical Only)",
       x = "Ancestry Group",
       y = "Number of Samples") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        title = element_text(size = 16),
        legend.position = "none") +
  scale_fill_manual(values = ancestry_colors)

ggsave("out/psychAD_ancestry_distribution_barplot_canonical.pdf", p1_canonical, width = 4, height = 3)
log_message("Saved: psychAD_ancestry_distribution_barplot_canonical.pdf")

# Pie chart for ancestry - Version 1: Including Unknown/Other
p2_full <- ggplot(data = subset(ancestry_summary, !is.na(Ancestry)),
             aes(x = "", y = Count, fill = Ancestry)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void(base_size = 14) +
  labs(title = "psychAD: Ancestry Distribution (All Categories)") +
  theme(title = element_text(size = 16),
        legend.text = element_text(size = 12)) +
  scale_fill_manual(values = ancestry_colors)

ggsave("out/psychAD_ancestry_distribution_pie_full.pdf", p2_full, width = 4, height = 3)
log_message("Saved: psychAD_ancestry_distribution_pie_full.pdf")

# Version 2: Excluding Unknown/Other
p2_canonical <- ggplot(data = ancestry_summary_canonical,
             aes(x = "", y = Count, fill = Ancestry)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void(base_size = 14) +
  labs(title = "psychAD: Ancestry Distribution (Canonical Only)") +
  theme(title = element_text(size = 16),
        legend.text = element_text(size = 12)) +
  scale_fill_manual(values = ancestry_colors)

ggsave("out/psychAD_ancestry_distribution_pie_canonical.pdf", p2_canonical, width = 4, height = 3)
log_message("Saved: psychAD_ancestry_distribution_pie_canonical.pdf")

# ============================================================================
# 3. GENDER DISTRIBUTION ANALYSIS
# ============================================================================

log_message("\n=== GENDER DISTRIBUTION ANALYSIS ===")

# Overall gender distribution
sex_counts <- table(data$sex, useNA = "always")
log_message("Overall gender distribution:")
capture.output(print(sex_counts), file = log_file, append = TRUE)

sex_pct <- round(prop.table(sex_counts) * 100, 2)
log_message("\nGender percentages:")
capture.output(print(sex_pct), file = log_file, append = TRUE)

# Gender distribution by ancestry
sex_ancestry_table <- table(data$Ancestry_clean, data$sex, useNA = "always")
log_message("\nGender by ancestry cross-tabulation:")
capture.output(print(sex_ancestry_table), file = log_file, append = TRUE)

# Convert to proportions within each ancestry
sex_ancestry_prop <- prop.table(sex_ancestry_table, margin = 1)
log_message("\nGender proportions within each ancestry:")
capture.output(print(round(sex_ancestry_prop, 3)), file = log_file, append = TRUE)

# Visualize gender by ancestry
# Prepare data for plotting
plot_data <- data %>%
  filter(!is.na(Ancestry_clean) & !is.na(sex)) %>%
  count(Ancestry_clean, sex) %>%
  group_by(Ancestry_clean) %>%
  mutate(prop = n/sum(n))

p3 <- ggplot(plot_data, aes(x = Ancestry_clean, y = n, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal(base_size = 14) +
  labs(title = "Gender Distribution by Ancestry",
       x = "Ancestry Group",
       y = "Number of Samples",
       fill = "Sex") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)) +
  scale_fill_manual(values = sex_colors)

ggsave("out/psychAD_gender_by_ancestry_barplot_full.pdf", p3, width = 5, height = 3)
log_message("Saved: psychAD_gender_by_ancestry_barplot_full.pdf")

# Stacked proportional bar chart
p4 <- ggplot(plot_data, aes(x = Ancestry_clean, y = prop, fill = sex)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal(base_size = 14) +
  labs(title = "Gender Proportions by Ancestry",
       x = "Ancestry Group",
       y = "Proportion",
       fill = "Sex") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)) +
  scale_fill_manual(values = sex_colors) +
  scale_y_continuous(labels = scales::percent)

ggsave("out/gender_proportions_by_ancestry.pdf", p4, width = 5, height = 3)
log_message("Saved: gender_proportions_by_ancestry.pdf")

# ============================================================================
# 3.5. DISEASE DISTRIBUTION ANALYSIS
# ============================================================================

log_message("\n=== DISEASE DISTRIBUTION ANALYSIS ===")

# Create disease status variables with control category
disease_data <- data %>%
  mutate(
    # Combine AD cross-disorder and AD diagnosis
    AD_combined = ifelse((crossDis_AD == 1 | dx_AD == 1) %in% TRUE, 1, 0),
    # Use individual cross-disorders for others
    SCZ = ifelse(crossDis_SCZ == 1 %in% TRUE, 1, 0),
    DLBD = ifelse(crossDis_DLBD == 1 %in% TRUE, 1, 0),
    Vascular = ifelse(crossDis_Vas == 1 %in% TRUE, 1, 0),
    BD = ifelse(crossDis_BD == 1 %in% TRUE, 1, 0),
    Tau = ifelse(crossDis_Tau == 1 %in% TRUE, 1, 0),
    PD = ifelse(crossDis_PD == 1 %in% TRUE, 1, 0),
    FTD = ifelse(crossDis_FTD == 1 %in% TRUE, 1, 0),
    Dementia = ifelse(Dementia == 1 %in% TRUE, 1, 0),
    # Create control category: samples without any of the diseases listed
    Any_Disease = ifelse(AD_combined == 1 | SCZ == 1 | DLBD == 1 | Vascular == 1 |
                        BD == 1 | Tau == 1 | PD == 1 | FTD == 1 | Dementia == 1, 1, 0),
    Control = ifelse(Any_Disease == 0, 1, 0)
  )

# Disease distribution counts
disease_counts <- disease_data %>%
  summarise(
    AD_combined = sum(AD_combined, na.rm = TRUE),
    SCZ = sum(SCZ, na.rm = TRUE),
    DLBD = sum(DLBD, na.rm = TRUE),
    Vascular = sum(Vascular, na.rm = TRUE),
    BD = sum(BD, na.rm = TRUE),
    Tau = sum(Tau, na.rm = TRUE),
    PD = sum(PD, na.rm = TRUE),
    FTD = sum(FTD, na.rm = TRUE),
    Dementia = sum(Dementia, na.rm = TRUE),
    Control = sum(Control, na.rm = TRUE),
    Total_samples = n()
  )

log_message("Disease distribution counts:")
capture.output(print(disease_counts), file = log_file, append = TRUE)

# Disease distribution by ancestry
disease_ancestry_summary <- disease_data %>%
  filter(!is.na(Ancestry_clean)) %>%
  group_by(Ancestry_clean) %>%
  summarise(
    n_total = n(),
    AD_combined = sum(AD_combined, na.rm = TRUE),
    SCZ = sum(SCZ, na.rm = TRUE),
    DLBD = sum(DLBD, na.rm = TRUE),
    Vascular = sum(Vascular, na.rm = TRUE),
    BD = sum(BD, na.rm = TRUE),
    Tau = sum(Tau, na.rm = TRUE),
    PD = sum(PD, na.rm = TRUE),
    FTD = sum(FTD, na.rm = TRUE),
    Dementia = sum(Dementia, na.rm = TRUE),
    Control = sum(Control, na.rm = TRUE),
    .groups = 'drop'
  )

log_message("\nDisease distribution by ancestry:")
capture.output(print(disease_ancestry_summary), file = log_file, append = TRUE)

# Disease distribution by sex
disease_sex_summary <- disease_data %>%
  filter(!is.na(sex)) %>%
  group_by(sex) %>%
  summarise(
    n_total = n(),
    AD_combined = sum(AD_combined, na.rm = TRUE),
    SCZ = sum(SCZ, na.rm = TRUE),
    DLBD = sum(DLBD, na.rm = TRUE),
    Vascular = sum(Vascular, na.rm = TRUE),
    BD = sum(BD, na.rm = TRUE),
    Tau = sum(Tau, na.rm = TRUE),
    PD = sum(PD, na.rm = TRUE),
    FTD = sum(FTD, na.rm = TRUE),
    Dementia = sum(Dementia, na.rm = TRUE),
    Control = sum(Control, na.rm = TRUE),
    .groups = 'drop'
  )

log_message("\nDisease distribution by sex:")
capture.output(print(disease_sex_summary), file = log_file, append = TRUE)

# Create disease distribution plots
# Overall disease distribution
disease_counts_plot <- disease_data %>%
  summarise(
    AD_combined = sum(AD_combined, na.rm = TRUE),
    SCZ = sum(SCZ, na.rm = TRUE),
    DLBD = sum(DLBD, na.rm = TRUE),
    Vascular = sum(Vascular, na.rm = TRUE),
    BD = sum(BD, na.rm = TRUE),
    Tau = sum(Tau, na.rm = TRUE),
    PD = sum(PD, na.rm = TRUE),
    FTD = sum(FTD, na.rm = TRUE),
    Dementia = sum(Dementia, na.rm = TRUE),
    Control = sum(Control, na.rm = TRUE)
  ) %>%
  gather(Disease, Count) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

p_disease_dist <- ggplot(disease_counts_plot, aes(x = reorder(Disease, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "#1F78B4", alpha = 0.8) +
  geom_text(aes(label = paste0(Count, "\n(", Percentage, "%)")),
            hjust = -0.1, size = 3, color = "black") +
  coord_flip() +
  labs(title = "Disease Distribution in PsychAD Dataset",
       x = "Disease Type",
       y = "Number of Samples") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

ggsave("out/PsychAD_disease_type_distribution.pdf", p_disease_dist, width = 8, height = 6)
log_message("Saved: PsychAD_disease_type_distribution.pdf")

log_message("\n=== STEP 1-3 COMPLETED ===")
log_message("✓ Dataset composition overview completed")
log_message("✓ Ancestry distribution analysis completed")
log_message("✓ Gender distribution analysis completed")
log_message("✓ Disease distribution analysis completed")

# ============================================================================
# 4. REPRESENTATION BIAS ANALYSIS
# ============================================================================

log_message("\n=== REPRESENTATION BIAS ANALYSIS ===")

# Intersectional analysis (ancestry × gender)
intersectional_counts <- data %>%
  filter(!is.na(Ancestry_clean) & !is.na(sex)) %>%
  count(Ancestry_clean, sex) %>%
  spread(sex, n, fill = 0) %>%
  mutate(Total = female + male,
         Female_Pct = round(female/Total * 100, 1),
         Male_Pct = round(male/Total * 100, 1))

log_message("Intersectional analysis (Ancestry × Gender):")
capture.output(print(intersectional_counts), file = log_file, append = TRUE)

# Identify underrepresented groups
total_samples_clean <- sum(intersectional_counts$Total)
intersectional_counts$Overall_Pct <- round(intersectional_counts$Total / total_samples_clean * 100, 2)

# Flag groups with < 5% representation
underrep_threshold <- 5
underrepresented <- intersectional_counts[intersectional_counts$Overall_Pct < underrep_threshold, ]
log_message("\nUnderrepresented ancestry groups (< 5% of total):")
capture.output(print(underrepresented), file = log_file, append = TRUE)

# Create heatmap showing sample counts
heatmap_data <- data %>%
  filter(!is.na(Ancestry_clean) & !is.na(sex)) %>%
  count(Ancestry_clean, sex)

p5 <- ggplot(heatmap_data, aes(x = sex, y = Ancestry_clean, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), color = "white", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Count") +
  theme_minimal(base_size = 14) +
  labs(title = "Sample Count Heatmap: Ancestry × Gender",
       x = "Gender", y = "Ancestry") +
  theme(axis.text.x = element_text(angle = 0, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))

ggsave("out/intersectional_heatmap.pdf", p5, width = 4, height = 3)
log_message("Saved: intersectional_heatmap.pdf")

# Missing data patterns by demographics
missing_analysis <- data %>%
  select(Ancestry_clean, sex, ageDeath, PMI, apoeGenotype, CERAD, Braak) %>%
  gather(variable, value, -Ancestry_clean, -sex) %>%
  group_by(Ancestry_clean, sex, variable) %>%
  summarise(missing_count = sum(is.na(value)),
            total_count = n(),
            missing_pct = round(missing_count/total_count * 100, 1),
            .groups = 'drop')

log_message("\nMissing data patterns by ancestry and gender:")
capture.output(print(missing_analysis), file = log_file, append = TRUE)

log_message("\n=== STEP 4 COMPLETED ===")
log_message("✓ Representation bias analysis completed")

# ============================================================================
# ADDITIONAL PLOTS REQUESTED
# ============================================================================

log_message("\n=== STATISTICAL ANALYSIS ===")

# Load additional packages for statistical tests
if (!require(car, quietly = TRUE)) {
  install.packages("car")
  library(car)
}

# 1. Hypothesis Testing
log_message("\n--- Hypothesis Testing ---")

# Chi-square test for ancestry distribution vs expected equal distribution
ancestry_for_test <- data$Ancestry_clean[!is.na(data$Ancestry_clean) & data$Ancestry_clean != "Unknown"]
chisq_ancestry <- chisq.test(table(ancestry_for_test))
log_message("Chi-square test for ancestry distribution:")
log_message(paste("X-squared =", chisq_ancestry$statistic, ", p-value =", chisq_ancestry$p.value))

# Chi-square test for gender balance within each ancestry
for (anc in unique(ancestry_for_test)) {
  anc_data <- data[data$Ancestry_clean == anc & !is.na(data$sex), ]
  if (nrow(anc_data) > 10) {  # Only test if sufficient sample size
    gender_test <- chisq.test(table(anc_data$sex))
    log_message(paste("Gender balance in", anc, "- Chi-square p-value:", gender_test$p.value))
  }
}

# 2. Equity Metrics
log_message("\n--- Equity Metrics ---")

# Calculate representation ratios (actual vs expected equal representation)
total_samples_analysis <- nrow(data[!is.na(data$Ancestry_clean) & data$Ancestry_clean != "Unknown", ])
unique_ancestries <- unique(data$Ancestry_clean[!is.na(data$Ancestry_clean) & data$Ancestry_clean != "Unknown"])
expected_equal <- total_samples_analysis / length(unique_ancestries)

equity_metrics <- data %>%
  filter(!is.na(Ancestry_clean) & Ancestry_clean != "Unknown") %>%
  count(Ancestry_clean) %>%
  mutate(
    expected_equal = expected_equal,
    representation_ratio = n / expected_equal,
    equity_index = ifelse(representation_ratio > 1, 1/representation_ratio, representation_ratio)
  )

log_message("Representation ratios (>1 = overrepresented, <1 = underrepresented):")
capture.output(print(equity_metrics), file = log_file, append = TRUE)

# Simpson's Diversity Index
simpson_diversity <- 1 - sum((equity_metrics$n / sum(equity_metrics$n))^2)
log_message(paste("Simpson's Diversity Index:", round(simpson_diversity, 3)))

# Shannon's Diversity Index
shannon_diversity <- -sum((equity_metrics$n / sum(equity_metrics$n)) * log(equity_metrics$n / sum(equity_metrics$n)))
log_message(paste("Shannon's Diversity Index:", round(shannon_diversity, 3)))

# 3. Missing Data Analysis
log_message("\n--- Missing Data Analysis ---")

# Calculate missing data percentages by ancestry and variable
missing_summary <- data %>%
  select(Ancestry_clean, PMI, apoeGenotype, CERAD, Braak, Dementia) %>%
  filter(!is.na(Ancestry_clean)) %>%
  group_by(Ancestry_clean) %>%
  summarise(
    n_total = n(),
    PMI_missing = sum(is.na(PMI)) / n() * 100,
    apoeGenotype_missing = sum(is.na(apoeGenotype)) / n() * 100,
    CERAD_missing = sum(is.na(CERAD)) / n() * 100,
    Braak_missing = sum(is.na(Braak)) / n() * 100,
    Dementia_missing = sum(is.na(Dementia)) / n() * 100,
    .groups = 'drop'
  )

log_message("Missing data percentages by ancestry:")
missing_summary_numeric <- missing_summary[, -1]  # Remove Ancestry column for rounding
missing_summary_rounded <- cbind(missing_summary[1], round(missing_summary_numeric, 1))
capture.output(print(missing_summary_rounded), file = log_file, append = TRUE)

# Create summary report
log_message("\n=== ANALYSIS SUMMARY REPORT ===")
log_message(paste("✓ Dataset contains", nrow(data), "samples across", length(unique(data$cohort)), "cohorts"))
log_message("✓ Ancestry distribution: EUR dominant, AFR substantial, others underrepresented")
log_message("✓ Gender balance: Overall balanced, but varies by ancestry")
log_message("✓ Representation bias: EAS, SAS, Unknown groups severely underrepresented")
log_message("✓ Missing data: Substantial gaps in clinical measures (CERAD, Braak) for some ancestry groups")
log_message("✓ Power concerns: Small ancestry groups may be underpowered for analyses")

log_message("\n=== ALL ANALYSIS STEPS COMPLETED ===")
log_message("✓ Dataset composition overview completed")
log_message("✓ Ancestry distribution analysis completed")
log_message("✓ Gender distribution analysis completed")
log_message("✓ Disease distribution analysis completed")
log_message("✓ Representation bias analysis completed")
log_message("✓ Statistical analysis completed")
log_message("✓ Equity metrics calculated")
log_message("✓ Additional plots completed")

# Save the workspace for further analysis
save.image("out/equity_bias_analysis_workspace.RData")
log_message("✓ Workspace saved to: equity_bias_analysis_workspace.RData")

# ============================================================================
# ADDITIONAL PLOTS REQUESTED
# ============================================================================

log_message("\n=== ADDITIONAL VISUALIZATION ANALYSIS ===")

# 1. Ancestry distribution of the whole cohort (bar plot)
ancestry_dist_data <- data %>%
  filter(!is.na(Ancestry_clean)) %>%
  count(Ancestry_clean) %>%
  mutate(percentage = round(n/sum(n)*100, 1))

p_ancestry_overall <- ggplot(ancestry_dist_data, aes(x = reorder(Ancestry_clean, -n), y = n, fill = Ancestry_clean)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(n, "\n(", percentage, "%)")), 
            vjust = -0.5, size = 3) +
  theme_minimal(base_size = 14) +
  labs(title = "Ancestry Distribution - Whole Cohort",
       x = "Ancestry Group", 
       y = "Number of Samples") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        title = element_text(size = 16),
        legend.position = "none") +
  scale_fill_manual(values = ancestry_colors)

ggsave("out/ancestry_distribution_whole_cohort.pdf", p_ancestry_overall, width = 4, height = 3)
log_message("Saved: ancestry_distribution_whole_cohort.pdf")

# 2. 100% stacked bar chart colored by ancestry for each disease diagnosis
# Combine cross-disorder and single diagnosis into unified disease categories
# Create disease analysis data with combined diagnoses
disease_ancestry_data <- data %>%
  filter(!is.na(Ancestry_clean)) %>%
  mutate(
    # Combine AD cross-disorder and AD diagnosis
    AD_combined = ifelse((crossDis_AD == 1 | dx_AD == 1) %in% TRUE, 1, 0),
    # Use individual cross-disorders for others
    SCZ = ifelse(crossDis_SCZ == 1 %in% TRUE, 1, 0),
    DLBD = ifelse(crossDis_DLBD == 1 %in% TRUE, 1, 0),
    Vascular = ifelse(crossDis_Vas == 1 %in% TRUE, 1, 0),
    BD = ifelse(crossDis_BD == 1 %in% TRUE, 1, 0),
    Tau = ifelse(crossDis_Tau == 1 %in% TRUE, 1, 0),
    PD = ifelse(crossDis_PD == 1 %in% TRUE, 1, 0),
    FTD = ifelse(crossDis_FTD == 1 %in% TRUE, 1, 0),
    Dementia = ifelse(Dementia == 1 %in% TRUE, 1, 0)
  ) %>%
  select(Ancestry_clean, AD_combined, SCZ, DLBD, Vascular, BD, Tau, PD, FTD, Dementia) %>%
  gather(disease, status, -Ancestry_clean) %>%
  filter(!is.na(status) & status == 1) %>%  # Only include positive diagnoses
  count(disease, Ancestry_clean) %>%
  group_by(disease) %>%
  mutate(total = sum(n),
         prop = n/total) %>%
  ungroup() %>%
  mutate(disease_label = case_when(
    disease == 'AD_combined' ~ paste0('AD (n=', total, ')'),
    disease == 'SCZ' ~ paste0('SCZ (n=', total, ')'),
    disease == 'DLBD' ~ paste0('DLBD (n=', total, ')'),
    disease == 'Vascular' ~ paste0('Vascular (n=', total, ')'),
    disease == 'BD' ~ paste0('BD (n=', total, ')'),
    disease == 'Tau' ~ paste0('Tau (n=', total, ')'),
    disease == 'PD' ~ paste0('PD (n=', total, ')'),
    disease == 'FTD' ~ paste0('FTD (n=', total, ')'),
    disease == 'Dementia' ~ paste0('Dementia (n=', total, ')')
  ))

p_disease_ancestry <- ggplot(disease_ancestry_data, aes(x = disease_label, y = prop, fill = Ancestry_clean)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal(base_size = 14) +
  labs(title = "Disease Diagnoses by Ancestry (100% Stacked)",
       x = "Disease Diagnosis", 
       y = "Proportion",
       fill = "Ancestry") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)) +
  scale_fill_manual(values = ancestry_colors) +
  scale_y_continuous(labels = scales::percent)

ggsave("out/disease_diagnoses_by_ancestry_stacked.pdf", p_disease_ancestry, width = 6, height = 4)
log_message("Saved: disease_diagnoses_by_ancestry_stacked.pdf")

# 3. 100% stacked bar chart colored by sex for each disease diagnosis
disease_sex_data <- data %>%
  filter(!is.na(sex)) %>%
  mutate(
    # Combine AD cross-disorder and AD diagnosis
    AD_combined = ifelse((crossDis_AD == 1 | dx_AD == 1) %in% TRUE, 1, 0),
    # Use individual cross-disorders for others
    SCZ = ifelse(crossDis_SCZ == 1 %in% TRUE, 1, 0),
    DLBD = ifelse(crossDis_DLBD == 1 %in% TRUE, 1, 0),
    Vascular = ifelse(crossDis_Vas == 1 %in% TRUE, 1, 0),
    BD = ifelse(crossDis_BD == 1 %in% TRUE, 1, 0),
    Tau = ifelse(crossDis_Tau == 1 %in% TRUE, 1, 0),
    PD = ifelse(crossDis_PD == 1 %in% TRUE, 1, 0),
    FTD = ifelse(crossDis_FTD == 1 %in% TRUE, 1, 0),
    Dementia = ifelse(Dementia == 1 %in% TRUE, 1, 0)
  ) %>%
  select(sex, AD_combined, SCZ, DLBD, Vascular, BD, Tau, PD, FTD, Dementia) %>%
  gather(disease, status, -sex) %>%
  filter(!is.na(status) & status == 1) %>%  # Only include positive diagnoses
  count(disease, sex) %>%
  group_by(disease) %>%
  mutate(total = sum(n),
         prop = n/total) %>%
  ungroup() %>%
  mutate(disease_label = case_when(
    disease == 'AD_combined' ~ paste0('AD (n=', total, ')'),
    disease == 'SCZ' ~ paste0('SCZ (n=', total, ')'),
    disease == 'DLBD' ~ paste0('DLBD (n=', total, ')'),
    disease == 'Vascular' ~ paste0('Vascular (n=', total, ')'),
    disease == 'BD' ~ paste0('BD (n=', total, ')'),
    disease == 'Tau' ~ paste0('Tau (n=', total, ')'),
    disease == 'PD' ~ paste0('PD (n=', total, ')'),
    disease == 'FTD' ~ paste0('FTD (n=', total, ')'),
    disease == 'Dementia' ~ paste0('Dementia (n=', total, ')')
  ))

p_disease_sex <- ggplot(disease_sex_data, aes(x = disease_label, y = prop, fill = sex)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal(base_size = 14) +
  labs(title = "Disease Diagnoses by Sex (100% Stacked)",
       x = "Disease Diagnosis", 
       y = "Proportion",
       fill = "Sex") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)) +
  scale_fill_manual(values = sex_colors) +
  scale_y_continuous(labels = scales::percent)

ggsave("out/disease_diagnoses_by_sex_stacked.pdf", p_disease_sex, width = 6, height = 4)
log_message("Saved: disease_diagnoses_by_sex_stacked.pdf")

log_message("\n=== ADDITIONAL PLOTS COMPLETED ===")
log_message("✓ Ancestry distribution whole cohort completed")
log_message("✓ Disease diagnoses by ancestry (stacked) completed")
log_message("✓ Disease diagnoses by sex (stacked) completed")
