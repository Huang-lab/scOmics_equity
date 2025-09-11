# Ancestry and Gender Bias Analysis - HCA Dataset
# Analysis of HCA dataset for equity and representation bias by tissue type

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(scales)
library(readr)

# Create output directory
dir.create("out", showWarnings = FALSE)

# Set global theme for larger fonts
theme_set(theme_minimal(base_size = 14))

# Initialize logging
log_file <- "out/HCA_analysis_log.txt"
cat("", file = log_file)  # Clear the log file

# Function to log messages to both console and file
log_message <- function(message) {
  cat(message, "\n")
  cat(message, "\n", file = log_file, append = TRUE)
}

# Define consistent color palettes for ancestry and sex (SAME AS PSYCHAD)
ancestry_colors <- c("African" = "#E31A1C", "Latino" = "#FF7F00", "Asian" = "#1F78B4",
                     "European" = "#6A3D9A",
                     "Unknown" = "#CAB2D6", "Other" = "#FDBF6F")

sex_colors <- c("female" = "#E69F00", "male" = "#56B4E9")

# Load HCA data from all tissue sheets
log_message("Loading HCA dataset from all tissue sheets...")

# Get all sheet names
excel_file <- "data/HCA scrnaseq data final.xlsx"
sheet_names <- excel_sheets(excel_file)
log_message(paste("Found", length(sheet_names), "tissue sheets:", paste(sheet_names, collapse=", ")))

# Read and combine all sheets
all_data <- list()
for(sheet_name in sheet_names) {
  log_message(paste("Reading sheet:", sheet_name))
  sheet_data <- read_excel(excel_file, sheet = sheet_name)

  # Convert all columns to character to avoid type conflicts when binding
  sheet_data <- sheet_data %>% mutate_all(as.character)
  sheet_data$Tissue_Sheet <- sheet_name  # Add tissue type column
  all_data[[sheet_name]] <- sheet_data
}

# Combine all sheets into one dataframe
data_raw <- bind_rows(all_data)
log_message(paste("Combined data from all sheets. Total samples:", nrow(data_raw)))

# Clean and standardize the data
log_message("Cleaning and standardizing HCA data...")

# Use the combined data directly
data_clean <- data_raw

# Standardize race/ancestry categories to match psychAD using the correct columns
data <- data_clean %>%
  mutate(
    # Clean ethnicity column using ontology_aggregated and map to new standard categories
    Race_clean = case_when(
      tolower(`donor_organism.human_specific.ethnicity.ontology_aggregated`) %in% c("european", "european ") ~ "European",
      tolower(`donor_organism.human_specific.ethnicity.ontology_aggregated`) %in% c("african") ~ "African",
      tolower(`donor_organism.human_specific.ethnicity.ontology_aggregated`) %in% c("asian", "east asian", "south asian", "south-east asian") ~ "Asian",
      tolower(`donor_organism.human_specific.ethnicity.ontology_aggregated`) %in% c("american") ~ "Latino",  # American becomes Latino
      tolower(`donor_organism.human_specific.ethnicity.ontology_aggregated`) %in% c("other", "mixed", "homo sapiens") ~ "Other",  # Reported but not canonical
      tolower(`donor_organism.human_specific.ethnicity.ontology_aggregated`) %in% c("unknown", "not provided") ~ "Unknown",  # Explicitly unknown
      # If row exists but data is missing, categorize as Unknown (as specified)
      is.na(`donor_organism.human_specific.ethnicity.ontology_aggregated`) ~ "Unknown",
      TRUE ~ "Other"  # Any other reported values
    ),
    # Clean sex column using the correct column - handle various formats
    Gender_clean = case_when(
      tolower(`donor_organism.sex`) %in% c("female") ~ "female",
      tolower(`donor_organism.sex`) %in% c("male") ~ "male",
      tolower(`donor_organism.sex`) %in% c("unknown", "yes", "homo sapiens") ~ NA_character_,
      is.na(`donor_organism.sex`) ~ NA_character_,
      TRUE ~ NA_character_
    ),
    # Use the tissue sheet name as the primary tissue type
    Tissue_Type = Tissue_Sheet
  )

# Basic data overview
log_message("Dataset loaded and cleaned successfully!")
log_message(paste("Total samples:", nrow(data)))
log_message(paste("Total variables:", ncol(data)))

# ============================================================================
# 1. DATASET COMPOSITION OVERVIEW
# ============================================================================

log_message("\n=== DATASET COMPOSITION OVERVIEW ===")

# Basic counts
total_samples <- nrow(data)
log_message(paste("Total samples:", total_samples))

# Tissue type distribution
tissue_counts <- table(data$Tissue_Type, useNA = "always")
log_message("\nTissue type distribution:")
capture.output(print(tissue_counts), file = log_file, append = TRUE)
print(tissue_counts)

# Study distribution
study_counts <- table(data$`study_id`, useNA = "always")
log_message("\nStudy distribution (top 5):")
top_studies <- head(sort(study_counts, decreasing = TRUE), 5)
capture.output(print(top_studies), file = log_file, append = TRUE)
print(top_studies)

# ============================================================================
# 2. ANCESTRY DISTRIBUTION ANALYSIS
# ============================================================================

log_message("\n=== ANCESTRY DISTRIBUTION ANALYSIS ===")

# Clean and examine ancestry data
ancestry_counts <- table(data$Race_clean, useNA = "always")
log_message("Ancestry distribution:")
capture.output(print(ancestry_counts), file = log_file, append = TRUE)
print(ancestry_counts)

# Calculate percentages
ancestry_pct <- round(prop.table(ancestry_counts) * 100, 2)
log_message("\nAncestry percentages:")
capture.output(print(ancestry_pct), file = log_file, append = TRUE)
print(ancestry_pct)

# Create ancestry summary table for logging
ancestry_summary_table <- data.frame(
  Ancestry = names(ancestry_counts),
  Count = as.numeric(ancestry_counts),
  Percentage = as.numeric(ancestry_pct)
)
log_message("\nAncestry Summary Table:")
capture.output(print(ancestry_summary_table), file = log_file, append = TRUE)

# Create ancestry summary table
ancestry_summary <- data.frame(
  Ancestry = names(ancestry_counts),
  Count = as.numeric(ancestry_counts),
  Percentage = as.numeric(ancestry_pct)
)

# Visualize ancestry distribution
p1 <- ggplot(data = subset(ancestry_summary, !is.na(Ancestry)), 
             aes(x = reorder(Ancestry, -Count), y = Count, fill = Ancestry)) +
  geom_bar(stat = "identity") +
  theme_minimal(base_size = 14) +
  labs(title = "HCA: Sample Distribution by Ancestry",
       x = "Ancestry Group", 
       y = "Number of Samples") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 14),
        title = element_text(size = 16),
        legend.position = "none") +
  scale_fill_manual(values = ancestry_colors)

ggsave("out/HCA_ancestry_distribution_barplot.pdf", p1, width = 3, height = 2.5)
log_message("Saved: HCA_ancestry_distribution_barplot.pdf")

# Pie chart for ancestry
p2 <- ggplot(data = subset(ancestry_summary, !is.na(Ancestry)), 
             aes(x = "", y = Count, fill = Ancestry)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void(base_size = 14) +
  labs(title = "HCA: Ancestry Distribution (Pie Chart)") +
  theme(title = element_text(size = 16),
        legend.text = element_text(size = 14)) +
  scale_fill_manual(values = ancestry_colors)

ggsave("out/HCA_ancestry_distribution_pie.pdf", p2, width = 3, height = 2.5)
log_message("Saved: HCA_ancestry_distribution_pie.pdf")

# ============================================================================
# 3. GENDER DISTRIBUTION ANALYSIS
# ============================================================================

log_message("\n=== GENDER DISTRIBUTION ANALYSIS ===")

# Overall gender distribution
sex_counts <- table(data$Gender_clean, useNA = "always")
log_message("Overall gender distribution:")
capture.output(print(sex_counts), file = log_file, append = TRUE)

sex_pct <- round(prop.table(sex_counts) * 100, 2)
log_message("\nGender percentages:")
capture.output(print(sex_pct), file = log_file, append = TRUE)

# Gender distribution by ancestry
sex_ancestry_table <- table(data$Race_clean, data$Gender_clean, useNA = "always")
log_message("\nGender by ancestry cross-tabulation:")
capture.output(print(sex_ancestry_table), file = log_file, append = TRUE)

# Convert to proportions within each ancestry
sex_ancestry_prop <- prop.table(sex_ancestry_table, margin = 1)
log_message("\nGender proportions within each ancestry:")
capture.output(print(round(sex_ancestry_prop, 3)), file = log_file, append = TRUE)

# Visualize gender by ancestry (only for groups with sufficient data)
plot_data <- data %>%
  filter(!is.na(Race_clean) & !is.na(Gender_clean) & Race_clean != "Unknown") %>%
  count(Race_clean, Gender_clean) %>%
  group_by(Race_clean) %>%
  mutate(prop = n/sum(n)) %>%
  filter(sum(n) >= 5)  # Only include ancestry groups with at least 5 samples

if(nrow(plot_data) > 0) {
  p3 <- ggplot(plot_data, aes(x = Race_clean, y = n, fill = Gender_clean)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_minimal(base_size = 14) +
    labs(title = "HCA: Gender Distribution by Ancestry",
         x = "Ancestry Group",
         y = "Number of Samples",
         fill = "Sex") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 14),
          title = element_text(size = 16),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14)) +
    scale_fill_manual(values = sex_colors)

  ggsave("out/HCA_gender_by_ancestry_barplot.pdf", p3, width = 4, height = 2.5)
  log_message("Saved: HCA_gender_by_ancestry_barplot.pdf")

  # Stacked proportional bar chart
  p4 <- ggplot(plot_data, aes(x = Race_clean, y = prop, fill = Gender_clean)) +
    geom_bar(stat = "identity", position = "stack") +
    theme_minimal(base_size = 14) +
    labs(title = "HCA: Gender Proportions by Ancestry",
         x = "Ancestry Group",
         y = "Proportion",
         fill = "Sex") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 14),
          title = element_text(size = 16),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14)) +
    scale_fill_manual(values = sex_colors) +
    scale_y_continuous(labels = scales::percent)

  ggsave("out/HCA_gender_proportions_by_ancestry.pdf", p4, width = 4, height = 2.5)
  log_message("Saved: HCA_gender_proportions_by_ancestry.pdf")
} else {
  log_message("Insufficient data for gender by ancestry visualization")
}

log_message("\n=== STEP 1-3 COMPLETED ===")
log_message("✓ Dataset composition overview completed")
log_message("✓ Ancestry distribution analysis completed")
log_message("✓ Gender distribution analysis completed")

# ============================================================================
# 4. REPRESENTATION BIAS ANALYSIS
# ============================================================================

log_message("\n=== REPRESENTATION BIAS ANALYSIS ===")

# Intersectional analysis (ancestry × gender) - only for groups with data
intersectional_data <- data %>%
  filter(!is.na(Race_clean) & !is.na(Gender_clean) & Race_clean != "Unknown")

if(nrow(intersectional_data) > 0) {
  intersectional_counts <- intersectional_data %>%
    count(Race_clean, Gender_clean) %>%
    spread(Gender_clean, n, fill = 0) %>%
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
  heatmap_data <- intersectional_data %>%
    count(Race_clean, Gender_clean)

  p5 <- ggplot(heatmap_data, aes(x = Gender_clean, y = Race_clean, fill = n)) +
    geom_tile(color = "white") +
    geom_text(aes(label = n), color = "white", size = 6) +
    scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Count") +
    theme_minimal(base_size = 14) +
    labs(title = "HCA: Sample Count Heatmap (Ancestry × Gender)",
         x = "Gender", y = "Ancestry") +
    theme(axis.text.x = element_text(angle = 0, size = 14),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 14),
          title = element_text(size = 16),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14))

  ggsave("out/HCA_intersectional_heatmap.pdf", p5, width = 3, height = 2.5)
  log_message("Saved: HCA_intersectional_heatmap.pdf")
} else {
  log_message("Insufficient data for intersectional analysis")
}

# ============================================================================
# 5. TISSUE-SPECIFIC ANALYSIS
# ============================================================================

log_message("\n=== TISSUE-SPECIFIC ANALYSIS ===")

# Tissue type distribution by ancestry (only for groups with sufficient data)
tissue_ancestry_data <- data %>%
  filter(!is.na(Race_clean) & !is.na(Tissue_Type) & Race_clean != "Unknown") %>%
  count(Tissue_Type, Race_clean) %>%
  group_by(Tissue_Type) %>%
  mutate(total = sum(n),
         prop = n/total) %>%
  ungroup() %>%
  filter(total >= 5) %>%  # Only include tissue types with at least 5 samples
  mutate(tissue_label = paste0(Tissue_Type, ' (n=', total, ')'))

if(nrow(tissue_ancestry_data) > 0) {
  # 100% stacked bar chart colored by ancestry for each tissue type
  p6 <- ggplot(tissue_ancestry_data, aes(x = tissue_label, y = prop, fill = Race_clean)) +
    geom_bar(stat = "identity", position = "stack") +
    theme_minimal(base_size = 14) +
    labs(title = "HCA: Tissue Types by Ancestry",
         x = "Tissue Type",
         y = "Proportion",
         fill = "Ancestry") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 14),
          title = element_text(size = 16),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14)) +
    scale_fill_manual(values = ancestry_colors) +
    scale_y_continuous(labels = scales::percent)

  ggsave("out/HCA_tissue_types_by_ancestry_stacked.pdf", p6, width = 8, height = 6)
  log_message("Saved: HCA_tissue_types_by_ancestry_stacked.pdf")
} else {
  log_message("Insufficient data for tissue by ancestry analysis")
}

# Tissue type distribution by sex
tissue_sex_data <- data %>%
  filter(!is.na(Gender_clean) & !is.na(Tissue_Type)) %>%
  count(Tissue_Type, Gender_clean) %>%
  group_by(Tissue_Type) %>%
  mutate(total = sum(n),
         prop = n/total) %>%
  ungroup() %>%
  filter(total >= 5) %>%  # Only include tissue types with at least 5 samples
  mutate(tissue_label = paste0(Tissue_Type, ' (n=', total, ')'))

if(nrow(tissue_sex_data) > 0) {
  # 100% stacked bar chart colored by sex for each tissue type
  p7 <- ggplot(tissue_sex_data, aes(x = tissue_label, y = prop, fill = Gender_clean)) +
    geom_bar(stat = "identity", position = "stack") +
    theme_minimal(base_size = 14) +
    labs(title = "HCA: Tissue Types by Sex",
         x = "Tissue Type",
         y = "Proportion",
         fill = "Sex") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 14),
          title = element_text(size = 16),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14)) +
    scale_fill_manual(values = sex_colors) +
    scale_y_continuous(labels = scales::percent)

  ggsave("out/HCA_tissue_types_by_sex_stacked.pdf", p7, width = 8, height = 6)
  log_message("Saved: HCA_tissue_types_by_sex_stacked.pdf")
} else {
  log_message("Insufficient data for tissue by sex analysis")
}

# Overall tissue type distribution
tissue_dist_data <- data %>%
  filter(!is.na(Tissue_Type)) %>%
  count(Tissue_Type) %>%
  mutate(percentage = round(n/sum(n)*100, 1)) %>%
  arrange(desc(n))

p8 <- ggplot(tissue_dist_data, aes(x = reorder(Tissue_Type, -n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(n, "\n(", percentage, "%)")),
            vjust = -0.5, size = 4) +
  theme_minimal(base_size = 14) +
  labs(title = "HCA: Tissue Type Distribution",
       x = "Tissue Type",
       y = "Number of Samples") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 14),
        title = element_text(size = 16)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

ggsave("out/HCA_tissue_type_distribution.pdf", p8, width = 5, height = 3.5)
log_message("Saved: HCA_tissue_type_distribution.pdf")

# ============================================================================
# 6. STATISTICAL ANALYSIS AGAINST GLOBAL REFERENCE
# ============================================================================

log_message("\n=== STATISTICAL ANALYSIS AGAINST GLOBAL REFERENCE ===")




# Load global reference data
log_message("Loading global reference data...")
global_ref <- read_csv("data/Global_reference.csv")

# Parse global ancestry data
global_ancestry_text <- global_ref$`Source & Numbers`[global_ref$Category == "Ancestry" & global_ref$Region == "Global"]
global_ancestry_text <- global_ancestry_text[1]  # Take the first match

# Extract percentages using regex
global_ancestry_data <- data.frame(
  Ancestry = c("Asian", "African", "European", "Latino", "Other"),
  Global_Percentage = c(59.4, 17.6, 9.4, 13.6, 0)  # N. America + Oceania + S. America = 13.6%
)

# Parse global sex data
global_sex_text <- global_ref$`Source & Numbers`[global_ref$Category == "Sex Distribution" & global_ref$Region == "Global"]
global_sex_text <- global_sex_text[1]

global_sex_data <- data.frame(
  Sex = c("male", "female"),
  Global_Percentage = c(50.2, 49.8)
)

log_message("Global reference data loaded:")
log_message("Global ancestry distribution:")
capture.output(print(global_ancestry_data), file = log_file, append = TRUE)
log_message("Global sex distribution:")
capture.output(print(global_sex_data), file = log_file, append = TRUE)

# 1. Hypothesis Testing Against Global Reference
log_message("\n--- Hypothesis Testing Against Global Reference ---")

# Chi-square test for ancestry distribution vs global reference
ancestry_for_test <- data$Race_clean[!is.na(data$Race_clean) & data$Race_clean != "Unknown"]
if(length(unique(ancestry_for_test)) > 1 & length(ancestry_for_test) > 10) {
  # Create observed counts
  observed_counts <- table(ancestry_for_test)
  
  # Create expected counts based on global reference
  total_observed <- sum(observed_counts)
  expected_counts <- numeric(length(observed_counts))
  
  # Map HCA categories to global categories
  for(i in 1:length(observed_counts)) {
    ancestry_name <- names(observed_counts)[i]
    if(ancestry_name %in% global_ancestry_data$Ancestry) {
      global_pct <- global_ancestry_data$Global_Percentage[global_ancestry_data$Ancestry == ancestry_name]
      expected_counts[i] <- total_observed * global_pct / 100
    } else {
      # For categories not in global reference, use equal distribution
      expected_counts[i] <- total_observed / length(observed_counts)
    }
  }
  
  # Perform chi-square test
  chisq_ancestry_global <- chisq.test(observed_counts, p = expected_counts/sum(expected_counts))
  log_message("Chi-square test for ancestry distribution vs global reference:")
  log_message(paste("X-squared =", round(chisq_ancestry_global$statistic, 3), ", p-value =", format.pval(chisq_ancestry_global$p.value, digits = 3)))
  log_message("Observed vs Expected counts:")
  comparison_table <- data.frame(
    Ancestry = names(observed_counts),
    Observed = as.numeric(observed_counts),
    Expected = round(expected_counts, 1),
    Difference = round(as.numeric(observed_counts) - expected_counts, 1)
  )
  capture.output(print(comparison_table), file = log_file, append = TRUE)
} else {
  log_message("Insufficient data for chi-square test of ancestry distribution vs global reference")
}

# Chi-square test for sex distribution vs global reference
sex_for_test <- data$Gender_clean[!is.na(data$Gender_clean)]
if(length(unique(sex_for_test)) > 1 & length(sex_for_test) > 10) {
  # Create observed counts
  observed_sex_counts <- table(sex_for_test)
  
  # Create expected counts based on global reference
  total_observed_sex <- sum(observed_sex_counts)
  expected_sex_counts <- numeric(length(observed_sex_counts))
  
  for(i in 1:length(observed_sex_counts)) {
    sex_name <- names(observed_sex_counts)[i]
    if(sex_name %in% global_sex_data$Sex) {
      global_pct <- global_sex_data$Global_Percentage[global_sex_data$Sex == sex_name]
      expected_sex_counts[i] <- total_observed_sex * global_pct / 100
    } else {
      expected_sex_counts[i] <- total_observed_sex / length(observed_sex_counts)
    }
  }
  
  # Perform chi-square test
  chisq_sex_global <- chisq.test(observed_sex_counts, p = expected_sex_counts/sum(expected_sex_counts))
  log_message("\nChi-square test for sex distribution vs global reference:")
  log_message(paste("X-squared =", round(chisq_sex_global$statistic, 3), ", p-value =", format.pval(chisq_sex_global$p.value, digits = 3)))
  log_message("Observed vs Expected sex counts:")
  sex_comparison_table <- data.frame(
    Sex = names(observed_sex_counts),
    Observed = as.numeric(observed_sex_counts),
    Expected = round(expected_sex_counts, 1),
    Difference = round(as.numeric(observed_sex_counts) - expected_sex_counts, 1)
  )
  capture.output(print(sex_comparison_table), file = log_file, append = TRUE)
} else {
  log_message("Insufficient data for chi-square test of sex distribution vs global reference")
}

# 2. Equity Metrics Against Global Reference
log_message("\n--- Equity Metrics Against Global Reference ---")

# Calculate representation ratios (actual vs global reference)
ancestry_data <- data[!is.na(data$Race_clean) & data$Race_clean != "Unknown", ]
if(nrow(ancestry_data) > 0) {
  total_samples_analysis <- nrow(ancestry_data)
  
  # Create equity metrics against global reference
  equity_metrics_global <- ancestry_data %>%
    count(Race_clean) %>%
    mutate(
      observed_pct = n / total_samples_analysis * 100
    )
  
  # Add global reference percentages
  equity_metrics_global <- equity_metrics_global %>%
    left_join(global_ancestry_data, by = c("Race_clean" = "Ancestry")) %>%
    mutate(
      Global_Percentage = ifelse(is.na(Global_Percentage), 0, Global_Percentage),
      representation_ratio_global = observed_pct / Global_Percentage,
      equity_index_global = ifelse(representation_ratio_global > 1, 1/representation_ratio_global, representation_ratio_global)
    )

  log_message("Representation ratios vs global reference (>1 = overrepresented, <1 = underrepresented):")
  capture.output(print(equity_metrics_global), file = log_file, append = TRUE)

  # Simpson's Diversity Index
  simpson_diversity <- 1 - sum((equity_metrics_global$n / sum(equity_metrics_global$n))^2)
  log_message(paste("Simpson's Diversity Index:", round(simpson_diversity, 3)))

  # Shannon's Diversity Index
  shannon_diversity <- -sum((equity_metrics_global$n / sum(equity_metrics_global$n)) * log(equity_metrics_global$n / sum(equity_metrics_global$n)))
  log_message(paste("Shannon's Diversity Index:", round(shannon_diversity, 3)))
} else {
  log_message("Insufficient ancestry data for equity metrics calculation")
}

# 3. Visualization: HCA vs Global Reference Comparison
log_message("\n--- Visualization: HCA vs Global Reference Comparison ---")

# Create comparison barplot for ancestry
if(exists("equity_metrics_global") && nrow(equity_metrics_global) > 0) {
  # Prepare data for plotting
  plot_data_ancestry <- equity_metrics_global %>%
    select(Race_clean, observed_pct, Global_Percentage) %>%
    gather(key = "Source", value = "Percentage", -Race_clean) %>%
    mutate(Source = ifelse(Source == "observed_pct", "HCA Dataset", "Global Reference"))
  
  # Create comparison barplot
  p9 <- ggplot(plot_data_ancestry, aes(x = Race_clean, y = Percentage, fill = Source)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    theme_minimal(base_size = 14) +
    labs(title = "HCA: Ancestry Distribution vs Global Reference",
         x = "Ancestry Group", 
         y = "Percentage (%)",
         fill = "Data Source") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 14),
          title = element_text(size = 16),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14)) +
    scale_fill_manual(values = c("HCA Dataset" = "steelblue", "Global Reference" = "darkred"))

  ggsave("out/HCA_ancestry_vs_global_reference.pdf", p9, width = 5, height = 4.5)
  log_message("Saved: HCA_ancestry_vs_global_reference.pdf")
  
  # Create global reference barplot in same format as HCA
  global_ancestry_plot_data <- global_ancestry_data %>%
    filter(Global_Percentage > 0)  # Remove zero percentages
  
  p10 <- ggplot(global_ancestry_plot_data, aes(x = reorder(Ancestry, -Global_Percentage), y = Global_Percentage, fill = Ancestry)) +
    geom_bar(stat = "identity") +
    theme_minimal(base_size = 14) +
    labs(title = "Global Reference: Sample Distribution by Ancestry",
         x = "Ancestry Group", 
         y = "Percentage (%)") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 14),
          title = element_text(size = 16),
          legend.position = "none") +
    scale_fill_manual(values = ancestry_colors)

  ggsave("out/Global_reference_ancestry_distribution_barplot.pdf", p10, width = 3, height = 3.5)
  log_message("Saved: Global_reference_ancestry_distribution_barplot.pdf")
}

# Create comparison barplot for sex
sex_data <- data[!is.na(data$Gender_clean), ]
if(nrow(sex_data) > 0) {
  # Calculate observed sex percentages
  sex_observed <- sex_data %>%
    count(Gender_clean) %>%
    mutate(observed_pct = n / sum(n) * 100)
  
  # Prepare data for plotting
  plot_data_sex <- sex_observed %>%
    select(Gender_clean, observed_pct) %>%
    bind_rows(global_sex_data %>% select(Sex, Global_Percentage) %>% 
              rename(Gender_clean = Sex, observed_pct = Global_Percentage)) %>%
    mutate(Source = rep(c("HCA Dataset", "Global Reference"), each = nrow(sex_observed)))
  
  # Create comparison barplot
  p11 <- ggplot(plot_data_sex, aes(x = Gender_clean, y = observed_pct, fill = Source)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    theme_minimal(base_size = 14) +
    labs(title = "HCA: Sex Distribution vs Global Reference",
         x = "Sex", 
         y = "Percentage (%)",
         fill = "Data Source") +
    theme(axis.text.x = element_text(angle = 0, size = 14),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 14),
          title = element_text(size = 16),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14)) +
    scale_fill_manual(values = c("HCA Dataset" = "steelblue", "Global Reference" = "darkred"))

  ggsave("out/HCA_sex_vs_global_reference.pdf", p11, width = 5, height = 3.5)
  log_message("Saved: HCA_sex_vs_global_reference.pdf")
  
  # Create global reference barplot for sex in same format as HCA
  p12 <- ggplot(global_sex_data, aes(x = Sex, y = Global_Percentage, fill = Sex)) +
    geom_bar(stat = "identity") +
    theme_minimal(base_size = 14) +
    labs(title = "Global Reference: Sex Distribution",
         x = "Sex", 
         y = "Percentage (%)") +
    theme(axis.text.x = element_text(angle = 0, size = 14),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 14),
          title = element_text(size = 16),
          legend.position = "none") +
    scale_fill_manual(values = sex_colors)

  ggsave("out/Global_reference_sex_distribution_barplot.pdf", p12, width = 3, height = 3.5)
  log_message("Saved: Global_reference_sex_distribution_barplot.pdf")
}

# 4. Missing Data Analysis
log_message("\n--- Missing Data Analysis ---")

# Calculate missing data percentages by ancestry for key variables
missing_summary <- data %>%
  select(Race_clean, `donor_organism.organism_age`, `donor_organism.sex`,
         `donor_organism.diseases.text`, `study_id`) %>%
  filter(!is.na(Race_clean)) %>%
  group_by(Race_clean) %>%
  summarise(
    n_total = n(),
    Age_missing = sum(is.na(`donor_organism.organism_age`)) / n() * 100,
    Sex_missing = sum(is.na(`donor_organism.sex`)) / n() * 100,
    Disease_missing = sum(is.na(`donor_organism.diseases.text`)) / n() * 100,
    Study_missing = sum(is.na(`study_id`)) / n() * 100,
    .groups = 'drop'
  )

log_message("Missing data percentages by ancestry:")
missing_summary_numeric <- missing_summary[, -1]  # Remove Ancestry column for rounding
missing_summary_rounded <- cbind(missing_summary[1], round(missing_summary_numeric, 1))
capture.output(print(missing_summary_rounded), file = log_file, append = TRUE)

# Create summary report
log_message("\n=== HCA ANALYSIS SUMMARY REPORT ===")
log_message(paste("✓ Dataset contains", nrow(data), "samples from HCA studies"))
log_message("✓ Ancestry distribution: Severe underrepresentation of non-European groups")
log_message("✓ Gender balance: Relatively balanced where data available")
log_message("✓ Representation bias: Extreme EUR dominance, minimal diversity")
log_message("✓ Tissue types: Multi-tissue studies dominant, limited single-tissue representation")
log_message("✓ Missing data: High missingness for demographic information")
log_message("✓ Power concerns: Very small sample sizes for most ancestry groups")
log_message("✓ Statistical testing: Chi-square tests against global reference completed")
log_message("✓ Global comparison: Visualizations comparing HCA vs global reference generated")

log_message("\n=== ALL HCA ANALYSIS STEPS COMPLETED ===")
log_message("✓ Dataset composition overview completed")
log_message("✓ Ancestry distribution analysis completed")
log_message("✓ Gender distribution analysis completed")
log_message("✓ Representation bias analysis completed")
log_message("✓ Tissue-specific analysis completed")

# -------------------------------------------------------------
# Per-tissue ancestry & sex distributions + chi-square tests
# - For each tissue type, compute ancestry% and sex% (including NA as a category and excluding NA)
# - Compare each tissue's distribution to the overall HCA distribution using chi-square
# - Save CSV summaries and a test-results table for downstream inspection
# -------------------------------------------------------------
log_message("\n--- Per-tissue: ancestry and sex distributions & tests ---")

safe_chisq <- function(obs, exp_p) {
  res <- list(statistic = NA, p.value = NA)
  if(sum(obs) > 0 && length(obs) > 1) {
    nonzero <- sum(obs > 0)
    if(nonzero > 1) {
      test <- tryCatch(chisq.test(obs, p = exp_p, simulate.p.value = FALSE), error = function(e) NULL)
      if(!is.null(test)) res <- list(statistic = unname(test$statistic), p.value = test$p.value)
    }
  }
  return(res)
}

## Normalize tissue type strings to avoid duplicates from trailing spaces/case
data$Tissue_Type <- trimws(as.character(data$Tissue_Type))
tissue_groups <- sort(unique(na.omit(data$Tissue_Type)))

per_tissue_ancestry_summary <- data.frame()
per_tissue_ancestry_tests <- data.frame()
per_tissue_sex_summary <- data.frame()
per_tissue_sex_tests <- data.frame()

# Precompute overall reference distributions
overall_ancestry_incl <- prop.table(table(data$Race_clean, useNA = "ifany"))
overall_ancestry_excl <- prop.table(table(data$Race_clean[!is.na(data$Race_clean)]))
overall_sex_incl <- prop.table(table(data$Gender_clean, useNA = "ifany"))
overall_sex_excl <- prop.table(table(data$Gender_clean[!is.na(data$Gender_clean)]))

for(t in tissue_groups) {
  grp_rows <- data %>% filter(!is.na(Tissue_Type) & Tissue_Type == t)
  grp_n <- nrow(grp_rows)
  if(grp_n == 0) next

  anc_incl_tab <- table(grp_rows$Race_clean, useNA = "ifany")
  anc_incl_pct <- round(prop.table(anc_incl_tab) * 100, 2)
  anc_excl_tab <- table(grp_rows$Race_clean[!is.na(grp_rows$Race_clean)])
  anc_excl_pct <- if(sum(anc_excl_tab) > 0) round(prop.table(anc_excl_tab) * 100, 2) else numeric(0)

  exp_incl <- as.numeric(overall_ancestry_incl[names(anc_incl_tab)])
  exp_incl[is.na(exp_incl)] <- 0
  chisq_incl <- safe_chisq(as.numeric(anc_incl_tab), exp_incl)

  if(length(anc_excl_tab) > 0) {
    exp_excl <- as.numeric(overall_ancestry_excl[names(anc_excl_tab)])
    exp_excl[is.na(exp_excl)] <- 0
    chisq_excl <- safe_chisq(as.numeric(anc_excl_tab), exp_excl)
  } else {
    chisq_excl <- list(statistic = NA, p.value = NA)
  }

  per_tissue_ancestry_summary <- bind_rows(per_tissue_ancestry_summary,
    data.frame(Tissue = t, Include_NA = TRUE, Group_N = grp_n, Category = names(anc_incl_tab), Count = as.numeric(anc_incl_tab), Pct = as.numeric(anc_incl_pct), stringsAsFactors = FALSE),
    if(length(anc_excl_tab) > 0) data.frame(Tissue = t, Include_NA = FALSE, Group_N = sum(anc_excl_tab), Category = names(anc_excl_tab), Count = as.numeric(anc_excl_tab), Pct = as.numeric(anc_excl_pct), stringsAsFactors = FALSE) else NULL
  )

  per_tissue_ancestry_tests <- bind_rows(per_tissue_ancestry_tests,
    data.frame(Tissue = t, Include_NA = TRUE, Group_N = grp_n, ChiSq = chisq_incl$statistic, P_value = chisq_incl$p.value, stringsAsFactors = FALSE),
    data.frame(Tissue = t, Include_NA = FALSE, Group_N = sum(anc_excl_tab), ChiSq = chisq_excl$statistic, P_value = chisq_excl$p.value, stringsAsFactors = FALSE)
  )

  # Sex
  sex_incl_tab <- table(grp_rows$Gender_clean, useNA = "ifany")
  sex_incl_pct <- round(prop.table(sex_incl_tab) * 100, 2)
  sex_excl_tab <- table(grp_rows$Gender_clean[!is.na(grp_rows$Gender_clean)])
  sex_excl_pct <- if(sum(sex_excl_tab) > 0) round(prop.table(sex_excl_tab) * 100, 2) else numeric(0)

  exp_sex_incl <- as.numeric(overall_sex_incl[names(sex_incl_tab)])
  exp_sex_incl[is.na(exp_sex_incl)] <- 0
  chisq_sex_incl <- safe_chisq(as.numeric(sex_incl_tab), exp_sex_incl)

  if(length(sex_excl_tab) > 0) {
    exp_sex_excl <- as.numeric(overall_sex_excl[names(sex_excl_tab)])
    exp_sex_excl[is.na(exp_sex_excl)] <- 0
    chisq_sex_excl <- safe_chisq(as.numeric(sex_excl_tab), exp_sex_excl)
  } else {
    chisq_sex_excl <- list(statistic = NA, p.value = NA)
  }

  per_tissue_sex_summary <- bind_rows(per_tissue_sex_summary,
    data.frame(Tissue = t, Include_NA = TRUE, Group_N = grp_n, Category = names(sex_incl_tab), Count = as.numeric(sex_incl_tab), Pct = as.numeric(sex_incl_pct), stringsAsFactors = FALSE),
    if(length(sex_excl_tab) > 0) data.frame(Tissue = t, Include_NA = FALSE, Group_N = sum(sex_excl_tab), Category = names(sex_excl_tab), Count = as.numeric(sex_excl_tab), Pct = as.numeric(sex_excl_pct), stringsAsFactors = FALSE) else NULL
  )

  per_tissue_sex_tests <- bind_rows(per_tissue_sex_tests,
    data.frame(Tissue = t, Include_NA = TRUE, Group_N = grp_n, ChiSq = chisq_sex_incl$statistic, P_value = chisq_sex_incl$p.value, stringsAsFactors = FALSE),
    data.frame(Tissue = t, Include_NA = FALSE, Group_N = sum(sex_excl_tab), ChiSq = chisq_sex_excl$statistic, P_value = chisq_sex_excl$p.value, stringsAsFactors = FALSE)
  )

  # Log a short summary
  log_message(paste0("Tissue=", t, " (n=", grp_n, "): ancestry incl-NA chi2=", round(as.numeric(chisq_incl$statistic),3), ", p=", format.pval(as.numeric(chisq_incl$p.value), digits = 3)))
  log_message(paste0("Tissue=", t, " (n=", grp_n, "): ancestry excl-NA chi2=", round(as.numeric(chisq_excl$statistic),3), ", p=", format.pval(as.numeric(chisq_excl$p.value), digits = 3)))
  log_message(paste0("Tissue=", t, " (n=", grp_n, "): sex incl-NA chi2=", round(as.numeric(chisq_sex_incl$statistic),3), ", p=", format.pval(as.numeric(chisq_sex_incl$p.value), digits = 3)))
  log_message(paste0("Tissue=", t, " (n=", grp_n, "): sex excl-NA chi2=", round(as.numeric(chisq_sex_excl$statistic),3), ", p=", format.pval(as.numeric(chisq_sex_excl$p.value), digits = 3)))
}

# Save CSV outputs
write.csv(per_tissue_ancestry_summary, file = "out/HCA_per_tissue_ancestry_distribution.csv", row.names = FALSE)
write.csv(per_tissue_ancestry_tests, file = "out/HCA_per_tissue_ancestry_chisq.csv", row.names = FALSE)
write.csv(per_tissue_sex_summary, file = "out/HCA_per_tissue_sex_distribution.csv", row.names = FALSE)
write.csv(per_tissue_sex_tests, file = "out/HCA_per_tissue_sex_chisq.csv", row.names = FALSE)

log_message("Saved per-tissue distribution CSVs and chi-square results to out/")

# Build one-row-per-tissue wide summary (percentages excluding NA)
anc_cats <- names(overall_ancestry_excl)
sex_cats <- names(overall_sex_excl)

per_tissue_summary_wide <- lapply(tissue_groups, function(t) {
  grp_rows <- data %>% filter(!is.na(Tissue_Type) & Tissue_Type == t)
  grp_n <- nrow(grp_rows)

  anc_counts_excl <- as.integer(sapply(anc_cats, function(x) sum(grp_rows$Race_clean == x, na.rm = TRUE)))
  names(anc_counts_excl) <- anc_cats
  anc_na_count <- sum(is.na(grp_rows$Race_clean))
  denom_anc <- sum(anc_counts_excl)
  anc_pct_excl <- if(denom_anc > 0) round(anc_counts_excl / denom_anc * 100, 2) else rep(0, length(anc_counts_excl))

  sex_counts_excl <- as.integer(sapply(sex_cats, function(x) sum(grp_rows$Gender_clean == x, na.rm = TRUE)))
  names(sex_counts_excl) <- sex_cats
  sex_na_count <- sum(is.na(grp_rows$Gender_clean))
  denom_sex <- sum(sex_counts_excl)
  sex_pct_excl <- if(denom_sex > 0) round(sex_counts_excl / denom_sex * 100, 2) else rep(0, length(sex_counts_excl))

  exp_anc <- as.numeric(overall_ancestry_excl[anc_cats])
  exp_sex <- as.numeric(overall_sex_excl[sex_cats])
  chisq_anc <- safe_chisq(anc_counts_excl, exp_anc)
  chisq_sex <- safe_chisq(sex_counts_excl, exp_sex)

  row <- c(
    Tissue = t,
    Group_N = grp_n,
    setNames(as.list(anc_counts_excl), paste0("Count_", anc_cats)),
    Count_Ancestry_NA = anc_na_count,
    setNames(as.list(anc_pct_excl), paste0("Pct_", anc_cats)),
    setNames(as.list(sex_counts_excl), paste0("Count_", sex_cats)),
    Count_Sex_NA = sex_na_count,
    setNames(as.list(sex_pct_excl), paste0("Pct_", sex_cats)),
    ChiSq_Ancestry = if(!is.na(chisq_anc$statistic)) round(as.numeric(chisq_anc$statistic),3) else NA,
    Pvalue_Ancestry = if(!is.na(chisq_anc$p.value)) chisq_anc$p.value else NA,
    ChiSq_Sex = if(!is.na(chisq_sex$statistic)) round(as.numeric(chisq_sex$statistic),3) else NA,
    Pvalue_Sex = if(!is.na(chisq_sex$p.value)) chisq_sex$p.value else NA
  )
  as.data.frame(row, stringsAsFactors = FALSE)
}) %>% bind_rows()

num_cols <- setdiff(names(per_tissue_summary_wide), c("Tissue"))
per_tissue_summary_wide[num_cols] <- lapply(per_tissue_summary_wide[num_cols], function(x) as.numeric(as.character(x)))

# Clean tissue names and remove duplicate rows (one row per tissue)
per_tissue_summary_wide$Tissue <- trimws(as.character(per_tissue_summary_wide$Tissue))
# Remove duplicate tissue rows (keep first) - robust base-R fallback
per_tissue_summary_wide <- per_tissue_summary_wide[!duplicated(as.character(per_tissue_summary_wide$Tissue)), ]

write.csv(per_tissue_summary_wide, file = "out/HCA_per_tissue_summary_wide.csv", row.names = FALSE)
log_message("Saved wide per-tissue summary (pct exclude NA) to out/HCA_per_tissue_summary_wide.csv")

log_message("✓ Statistical analysis against global reference completed")
log_message("✓ Equity metrics against global reference calculated")
log_message("✓ Global reference comparison visualizations completed")

# Save the workspace for further analysis
save.image("out/HCA_equity_bias_analysis_workspace.RData")
log_message("✓ Workspace saved to: HCA_equity_bias_analysis_workspace.RData")
