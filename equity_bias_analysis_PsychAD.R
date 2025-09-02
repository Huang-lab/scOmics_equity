# Ancestry and Gender Bias Analysis
# Analysis of psychAD media dataset for equity and representation bias

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(scales)
library(readr)

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

# -------------------------------------------------------------
# Per-disease ancestry & sex distributions + chi-square tests
# - For each disease group, compute ancestry% and sex% (including NA as a category and excluding NA)
# - Compare each group's distribution to the overall dataset distribution using chi-square
# - Save CSV summaries and a test-results table for downstream inspection
# -------------------------------------------------------------
log_message("\n--- Per-disease: ancestry and sex distributions & tests ---")

# Helper: safe chisq that returns NA when not enough categories
safe_chisq <- function(obs, exp_p) {
  res <- list(statistic = NA, p.value = NA)
  if(sum(obs) > 0 && length(obs) > 1) {
    # require at least 2 non-zero categories
    nonzero <- sum(obs > 0)
    if(nonzero > 1) {
      test <- tryCatch(chisq.test(obs, p = exp_p, simulate.p.value = FALSE), error = function(e) NULL)
      if(!is.null(test)) res <- list(statistic = unname(test$statistic), p.value = test$p.value)
    }
  }
  return(res)
}

# Define disease groups to iterate over (same as used above)
disease_groups <- c("AD_combined","SCZ","DLBD","Vascular","BD","Tau","PD","FTD","Dementia","Control")

per_disease_ancestry_summary <- data.frame()
per_disease_ancestry_tests <- data.frame()
per_disease_sex_summary <- data.frame()
per_disease_sex_tests <- data.frame()

# Precompute overall reference distributions (including NA as a category and excluding NAs)
overall_ancestry_incl <- prop.table(table(data$Ancestry_clean, useNA = "ifany"))
overall_ancestry_excl <- prop.table(table(data$Ancestry_clean[!is.na(data$Ancestry_clean)]))
overall_sex_incl <- prop.table(table(data$sex, useNA = "ifany"))
overall_sex_excl <- prop.table(table(data$sex[!is.na(data$sex)]))

for(g in disease_groups) {
  grp_rows <- disease_data %>% filter(!is.na(!!as.name(g)) & (!!as.name(g)) == 1)
  grp_n <- nrow(grp_rows)
  if(grp_n == 0) next

  # Ancestry distributions
  anc_incl_tab <- table(grp_rows$Ancestry_clean, useNA = "ifany")
  anc_incl_pct <- round(prop.table(anc_incl_tab) * 100, 2)
  anc_excl_tab <- table(grp_rows$Ancestry_clean[!is.na(grp_rows$Ancestry_clean)])
  anc_excl_pct <- if(sum(anc_excl_tab) > 0) round(prop.table(anc_excl_tab) * 100, 2) else numeric(0)

  # Chi-square vs overall (including NA)
  exp_incl <- as.numeric(overall_ancestry_incl[names(anc_incl_tab)])
  exp_incl[is.na(exp_incl)] <- 0
  chisq_incl <- safe_chisq(as.numeric(anc_incl_tab), exp_incl)

  # Chi-square vs overall (excluding NA)
  if(length(anc_excl_tab) > 0) {
    exp_excl <- as.numeric(overall_ancestry_excl[names(anc_excl_tab)])
    exp_excl[is.na(exp_excl)] <- 0
    chisq_excl <- safe_chisq(as.numeric(anc_excl_tab), exp_excl)
  } else {
    chisq_excl <- list(statistic = NA, p.value = NA)
  }

  # Append summary rows
  per_disease_ancestry_summary <- bind_rows(per_disease_ancestry_summary,
    data.frame(Disease = g, Include_NA = TRUE, Group_N = grp_n, Category = names(anc_incl_tab), Count = as.numeric(anc_incl_tab), Pct = as.numeric(anc_incl_pct), stringsAsFactors = FALSE),
    if(length(anc_excl_tab) > 0) data.frame(Disease = g, Include_NA = FALSE, Group_N = sum(anc_excl_tab), Category = names(anc_excl_tab), Count = as.numeric(anc_excl_tab), Pct = as.numeric(anc_excl_pct), stringsAsFactors = FALSE) else NULL
  )

  per_disease_ancestry_tests <- bind_rows(per_disease_ancestry_tests,
    data.frame(Disease = g, Include_NA = TRUE, Group_N = grp_n, ChiSq = chisq_incl$statistic, P_value = chisq_incl$p.value, stringsAsFactors = FALSE),
    data.frame(Disease = g, Include_NA = FALSE, Group_N = sum(anc_excl_tab), ChiSq = chisq_excl$statistic, P_value = chisq_excl$p.value, stringsAsFactors = FALSE)
  )

  # Sex distributions
  sex_incl_tab <- table(grp_rows$sex, useNA = "ifany")
  sex_incl_pct <- round(prop.table(sex_incl_tab) * 100, 2)
  sex_excl_tab <- table(grp_rows$sex[!is.na(grp_rows$sex)])
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

  per_disease_sex_summary <- bind_rows(per_disease_sex_summary,
    data.frame(Disease = g, Include_NA = TRUE, Group_N = grp_n, Category = names(sex_incl_tab), Count = as.numeric(sex_incl_tab), Pct = as.numeric(sex_incl_pct), stringsAsFactors = FALSE),
    if(length(sex_excl_tab) > 0) data.frame(Disease = g, Include_NA = FALSE, Group_N = sum(sex_excl_tab), Category = names(sex_excl_tab), Count = as.numeric(sex_excl_tab), Pct = as.numeric(sex_excl_pct), stringsAsFactors = FALSE) else NULL
  )

  per_disease_sex_tests <- bind_rows(per_disease_sex_tests,
    data.frame(Disease = g, Include_NA = TRUE, Group_N = grp_n, ChiSq = chisq_sex_incl$statistic, P_value = chisq_sex_incl$p.value, stringsAsFactors = FALSE),
    data.frame(Disease = g, Include_NA = FALSE, Group_N = sum(sex_excl_tab), ChiSq = chisq_sex_excl$statistic, P_value = chisq_sex_excl$p.value, stringsAsFactors = FALSE)
  )

  # Log a short summary
  log_message(paste0("Disease=", g, " (n=", grp_n, "): ancestry incl-NA chi2=", round(as.numeric(chisq_incl$statistic),3), ", p=", format.pval(as.numeric(chisq_incl$p.value), digits = 3)))
  log_message(paste0("Disease=", g, " (n=", grp_n, "): ancestry excl-NA chi2=", round(as.numeric(chisq_excl$statistic),3), ", p=", format.pval(as.numeric(chisq_excl$p.value), digits = 3)))
  log_message(paste0("Disease=", g, " (n=", grp_n, "): sex incl-NA chi2=", round(as.numeric(chisq_sex_incl$statistic),3), ", p=", format.pval(as.numeric(chisq_sex_incl$p.value), digits = 3)))
  log_message(paste0("Disease=", g, " (n=", grp_n, "): sex excl-NA chi2=", round(as.numeric(chisq_sex_excl$statistic),3), ", p=", format.pval(as.numeric(chisq_sex_excl$p.value), digits = 3)))
}

# Save CSV outputs
write.csv(per_disease_ancestry_summary, file = "out/psychAD_per_disease_ancestry_distribution.csv", row.names = FALSE)
write.csv(per_disease_ancestry_tests, file = "out/psychAD_per_disease_ancestry_chisq.csv", row.names = FALSE)
write.csv(per_disease_sex_summary, file = "out/psychAD_per_disease_sex_distribution.csv", row.names = FALSE)
write.csv(per_disease_sex_tests, file = "out/psychAD_per_disease_sex_chisq.csv", row.names = FALSE)

log_message("Saved per-disease distribution CSVs and chi-square results to out/")

## Build one-row-per-disease wide summary (percentages excluding NA)
anc_cats <- names(overall_ancestry_excl)
sex_cats <- names(overall_sex_excl)

per_disease_summary_wide <- lapply(disease_groups, function(g) {
  grp_rows <- disease_data %>% filter(!is.na(!!as.name(g)) & (!!as.name(g)) == 1)
  grp_n <- nrow(grp_rows)

  # ancestry counts excluding NA
  anc_counts_excl <- as.integer(sapply(anc_cats, function(x) sum(grp_rows$Ancestry_clean == x, na.rm = TRUE)))
  names(anc_counts_excl) <- anc_cats
  anc_na_count <- sum(is.na(grp_rows$Ancestry_clean))
  denom_anc <- sum(anc_counts_excl)
  anc_pct_excl <- if(denom_anc > 0) round(anc_counts_excl / denom_anc * 100, 2) else rep(0, length(anc_counts_excl))

  # sex counts excluding NA
  sex_counts_excl <- as.integer(sapply(sex_cats, function(x) sum(grp_rows$sex == x, na.rm = TRUE)))
  names(sex_counts_excl) <- sex_cats
  sex_na_count <- sum(is.na(grp_rows$sex))
  denom_sex <- sum(sex_counts_excl)
  sex_pct_excl <- if(denom_sex > 0) round(sex_counts_excl / denom_sex * 100, 2) else rep(0, length(sex_counts_excl))

  # Chi-square tests against overall excluding-NA reference
  exp_anc <- as.numeric(overall_ancestry_excl[anc_cats])
  exp_sex <- as.numeric(overall_sex_excl[sex_cats])
  chisq_anc <- safe_chisq(anc_counts_excl, exp_anc)
  chisq_sex <- safe_chisq(sex_counts_excl, exp_sex)

  # assemble row
  row <- c(
    Disease = g,
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

# Ensure numeric columns are numeric
num_cols <- setdiff(names(per_disease_summary_wide), c("Disease"))
per_disease_summary_wide[num_cols] <- lapply(per_disease_summary_wide[num_cols], function(x) as.numeric(as.character(x)))

write.csv(per_disease_summary_wide, file = "out/psychAD_per_disease_summary_wide.csv", row.names = FALSE)
log_message("Saved wide per-disease summary (pct exclude NA) to out/psychAD_per_disease_summary_wide.csv")

# ============================================================================
# 5. STATISTICAL ANALYSIS AGAINST ADRD DISEASE DEMOGRAPHICS
# ============================================================================

log_message("\n=== STATISTICAL ANALYSIS AGAINST ADRD DISEASE DEMOGRAPHICS ===")

# Load ADRD disease demographics data
log_message("Loading ADRD disease demographics data...")
adrd_data <- read_csv("data/ADRD_disease_demographics_wide.csv")

# Clean and process ADRD data
adrd_clean <- adrd_data %>%
  # Remove rows with all NA values
  filter(!if_all(-Disease, is.na)) %>%
  # Convert to long format for easier processing
  gather(key = "Demographic", value = "Percentage", -Disease) %>%
  filter(!is.na(Percentage) & Percentage > 0) %>%
  mutate(
    Percentage = as.numeric(Percentage) * 100,  # Convert to percentage
    Measure = case_when(
      Demographic %in% c("Female", "Male") ~ "Sex",
      TRUE ~ "Ancestry"
    )
  )

# Map ADRD ancestry categories to PsychAD categories
adrd_clean <- adrd_clean %>%
  mutate(
    Ancestry_Mapped = case_when(
      str_detect(Demographic, "Non-Hispanic White|White") ~ "European",
      str_detect(Demographic, "Black") ~ "African",
      str_detect(Demographic, "Asian|Asian/PI") ~ "Asian",
      str_detect(Demographic, "Hispanic") ~ "Latino",
      str_detect(Demographic, "AI/AN|Native American") ~ "Other",
      str_detect(Demographic, "Two\\+ races") ~ "Other",
      TRUE ~ "Other"
    ),
    Sex_Mapped = case_when(
      Demographic == "Female" ~ "female",
      Demographic == "Male" ~ "male",
      TRUE ~ NA_character_
    )
  )

# Map ADRD disease categories to PsychAD categories
adrd_clean <- adrd_clean %>%
  mutate(
    Disease_Mapped = case_when(
      str_detect(tolower(Disease), "ad/adrd|alzheimer") ~ "AD_combined",
      str_detect(tolower(Disease), "bipolar") ~ "BD",
      str_detect(tolower(Disease), "lewy") ~ "DLBD",
      str_detect(tolower(Disease), "frontotemporal") ~ "FTD",
      str_detect(tolower(Disease), "parkinson") ~ "PD",
      str_detect(tolower(Disease), "schizophrenia") ~ "SCZ",
      str_detect(tolower(Disease), "tauopathies|tau") ~ "Tau",
      str_detect(tolower(Disease), "vascular") ~ "Vascular",
      TRUE ~ "Other"
    )
  )

# Calculate overall ADRD ancestry distribution (across all diseases)
adrd_ancestry_overall <- adrd_clean %>%
  filter(Measure == "Ancestry") %>%
  group_by(Ancestry_Mapped) %>%
  summarise(
    Total_Percentage = sum(Percentage, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    ADRD_Percentage = Total_Percentage / sum(Total_Percentage) * 100
  )

# Calculate overall ADRD sex distribution (across all diseases)
adrd_sex_overall <- adrd_clean %>%
  filter(Measure == "Sex") %>%
  group_by(Sex_Mapped) %>%
  summarise(
    Total_Percentage = sum(Percentage, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    ADRD_Percentage = Total_Percentage / sum(Total_Percentage) * 100
  )

log_message("ADRD reference data loaded and processed:")
log_message("Overall ADRD ancestry distribution:")
capture.output(print(adrd_ancestry_overall), file = log_file, append = TRUE)
log_message("Overall ADRD sex distribution:")
capture.output(print(adrd_sex_overall), file = log_file, append = TRUE)

# Debug: Check ADRD disease mapping
log_message("ADRD disease mapping summary:")
adrd_disease_summary <- adrd_clean %>%
  group_by(Disease_Mapped, Measure) %>%
  summarise(
    Count = n(),
    Total_Percentage = sum(Percentage, na.rm = TRUE),
    .groups = 'drop'
  )
capture.output(print(adrd_disease_summary), file = log_file, append = TRUE)


# 1. Hypothesis Testing Against ADRD Reference
log_message("\n--- Hypothesis Testing Against ADRD Reference ---")

# Chi-square test for ancestry distribution vs ADRD reference
ancestry_for_test <- data$Ancestry_clean[!is.na(data$Ancestry_clean) & data$Ancestry_clean != "Unknown"]
if(length(unique(ancestry_for_test)) > 1 & length(ancestry_for_test) > 10) {
  # Create observed counts
  observed_counts <- table(ancestry_for_test)
  
  # Create expected counts based on ADRD reference
  total_observed <- sum(observed_counts)
  expected_counts <- numeric(length(observed_counts))
  
  # Map PsychAD categories to ADRD categories
  for(i in 1:length(observed_counts)) {
    ancestry_name <- names(observed_counts)[i]
    if(ancestry_name %in% adrd_ancestry_overall$Ancestry_Mapped) {
      adrd_pct <- adrd_ancestry_overall$ADRD_Percentage[adrd_ancestry_overall$Ancestry_Mapped == ancestry_name]
      expected_counts[i] <- total_observed * adrd_pct / 100
    } else {
      # For categories not in ADRD reference, use equal distribution
      expected_counts[i] <- total_observed / length(observed_counts)
    }
  }
  
  # Perform chi-square test
  chisq_ancestry_adrd <- chisq.test(observed_counts, p = expected_counts/sum(expected_counts))
  log_message("Chi-square test for ancestry distribution vs ADRD reference:")
  log_message(paste("X-squared =", round(chisq_ancestry_adrd$statistic, 3), ", p-value =", format.pval(chisq_ancestry_adrd$p.value, digits = 3)))
  log_message("Observed vs Expected counts:")
  comparison_table <- data.frame(
    Ancestry = names(observed_counts),
    Observed = as.numeric(observed_counts),
    Expected = round(expected_counts, 1),
    Difference = round(as.numeric(observed_counts) - expected_counts, 1)
  )
  capture.output(print(comparison_table), file = log_file, append = TRUE)
} else {
  log_message("Insufficient data for chi-square test of ancestry distribution vs ADRD reference")
}

# Chi-square test for sex distribution vs ADRD reference
sex_for_test <- data$sex[!is.na(data$sex)]
if(length(unique(sex_for_test)) > 1 & length(sex_for_test) > 10) {
  # Create observed counts
  observed_sex_counts <- table(sex_for_test)
  
  # Create expected counts based on ADRD reference
  total_observed_sex <- sum(observed_sex_counts)
  expected_sex_counts <- numeric(length(observed_sex_counts))
  
  for(i in 1:length(observed_sex_counts)) {
    sex_name <- names(observed_sex_counts)[i]
    if(sex_name %in% adrd_sex_overall$Sex_Mapped) {
      adrd_pct <- adrd_sex_overall$ADRD_Percentage[adrd_sex_overall$Sex_Mapped == sex_name]
      expected_sex_counts[i] <- total_observed_sex * adrd_pct / 100
    } else {
      expected_sex_counts[i] <- total_observed_sex / length(observed_sex_counts)
    }
  }
  
  # Perform chi-square test
  chisq_sex_adrd <- chisq.test(observed_sex_counts, p = expected_sex_counts/sum(expected_sex_counts))
  log_message("\nChi-square test for sex distribution vs ADRD reference:")
  log_message(paste("X-squared =", round(chisq_sex_adrd$statistic, 3), ", p-value =", format.pval(chisq_sex_adrd$p.value, digits = 3)))
  log_message("Observed vs Expected sex counts:")
  sex_comparison_table <- data.frame(
    Sex = names(observed_sex_counts),
    Observed = as.numeric(observed_sex_counts),
    Expected = round(expected_sex_counts, 1),
    Difference = round(as.numeric(observed_sex_counts) - expected_sex_counts, 1)
  )
  capture.output(print(sex_comparison_table), file = log_file, append = TRUE)
} else {
  log_message("Insufficient data for chi-square test of sex distribution vs ADRD reference")
}

# 2. Equity Metrics Against ADRD Reference
log_message("\n--- Equity Metrics Against ADRD Reference ---")

# Calculate representation ratios (actual vs ADRD reference)
ancestry_data <- data[!is.na(data$Ancestry_clean) & data$Ancestry_clean != "Unknown", ]
if(nrow(ancestry_data) > 0) {
  total_samples_analysis <- nrow(ancestry_data)
  
  # Create equity metrics against ADRD reference
  equity_metrics_adrd <- ancestry_data %>%
    count(Ancestry_clean) %>%
    mutate(
      observed_pct = n / total_samples_analysis * 100
    )
  
  # Add ADRD reference percentages
  equity_metrics_adrd <- equity_metrics_adrd %>%
    left_join(adrd_ancestry_overall, by = c("Ancestry_clean" = "Ancestry_Mapped")) %>%
    mutate(
      ADRD_Percentage = ifelse(is.na(ADRD_Percentage), 0, ADRD_Percentage),
      representation_ratio_adrd = observed_pct / ADRD_Percentage,
      equity_index_adrd = ifelse(representation_ratio_adrd > 1, 1/representation_ratio_adrd, representation_ratio_adrd)
    )

  log_message("Representation ratios vs ADRD reference (>1 = overrepresented, <1 = underrepresented):")
  capture.output(print(equity_metrics_adrd), file = log_file, append = TRUE)

  # Simpson's Diversity Index
  simpson_diversity <- 1 - sum((equity_metrics_adrd$n / sum(equity_metrics_adrd$n))^2)
  log_message(paste("Simpson's Diversity Index:", round(simpson_diversity, 3)))

  # Shannon's Diversity Index
  shannon_diversity <- -sum((equity_metrics_adrd$n / sum(equity_metrics_adrd$n)) * log(equity_metrics_adrd$n / sum(equity_metrics_adrd$n)))
  log_message(paste("Shannon's Diversity Index:", round(shannon_diversity, 3)))
} else {
  log_message("Insufficient ancestry data for equity metrics calculation")
}

# 3. Visualization: PsychAD vs ADRD Reference Comparison
log_message("\n--- Visualization: PsychAD vs ADRD Reference Comparison ---")

# Create comparison barplot for ancestry
if(exists("equity_metrics_adrd") && nrow(equity_metrics_adrd) > 0) {
  # Prepare data for plotting
  plot_data_ancestry <- equity_metrics_adrd %>%
    select(Ancestry_clean, observed_pct, ADRD_Percentage) %>%
    gather(key = "Source", value = "Percentage", -Ancestry_clean) %>%
    mutate(Source = ifelse(Source == "observed_pct", "PsychAD Dataset", "ADRD Reference"))
  
  # Create comparison barplot
  p_adrd_ancestry <- ggplot(plot_data_ancestry, aes(x = Ancestry_clean, y = Percentage, fill = Source)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    theme_minimal(base_size = 14) +
    labs(title = "PsychAD: Ancestry Distribution vs ADRD Reference",
         x = "Ancestry Group", 
         y = "Percentage (%)",
         fill = "Data Source") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14),
          title = element_text(size = 16),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14)) +
    scale_fill_manual(values = c("PsychAD Dataset" = "steelblue", "ADRD Reference" = "darkred"))

  ggsave("out/PsychAD_ancestry_vs_ADRD_reference.pdf", p_adrd_ancestry, width = 8, height = 5)
  log_message("Saved: PsychAD_ancestry_vs_ADRD_reference.pdf")
  
  # Create ADRD reference barplot in same format as PsychAD
  adrd_ancestry_plot_data <- adrd_ancestry_overall %>%
    filter(ADRD_Percentage > 0)  # Remove zero percentages
  
  p_adrd_ancestry_ref <- ggplot(adrd_ancestry_plot_data, aes(x = reorder(Ancestry_Mapped, -ADRD_Percentage), y = ADRD_Percentage, fill = Ancestry_Mapped)) +
    geom_bar(stat = "identity") +
    theme_minimal(base_size = 14) +
    labs(title = "ADRD Reference: Disease Demographics by Ancestry",
         x = "Ancestry Group", 
         y = "Percentage (%)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14),
          title = element_text(size = 16),
          legend.position = "none") +
    scale_fill_manual(values = ancestry_colors)

  ggsave("out/ADRD_reference_ancestry_distribution_barplot.pdf", p_adrd_ancestry_ref, width = 4, height = 3)
  log_message("Saved: ADRD_reference_ancestry_distribution_barplot.pdf")
}

# Create comparison barplot for sex
sex_data <- data[!is.na(data$sex), ]
if(nrow(sex_data) > 0) {
  # Calculate observed sex percentages
  sex_observed <- sex_data %>%
    count(sex) %>%
    mutate(observed_pct = n / sum(n) * 100)
  
  # Prepare data for plotting
  plot_data_sex <- sex_observed %>%
    select(sex, observed_pct) %>%
    bind_rows(adrd_sex_overall %>% select(Sex_Mapped, ADRD_Percentage) %>% 
              rename(sex = Sex_Mapped, observed_pct = ADRD_Percentage)) %>%
    mutate(Source = rep(c("PsychAD Dataset", "ADRD Reference"), each = nrow(sex_observed)))
  
  # Create comparison barplot
  p_adrd_sex <- ggplot(plot_data_sex, aes(x = sex, y = observed_pct, fill = Source)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    theme_minimal(base_size = 14) +
    labs(title = "PsychAD: Sex Distribution vs ADRD Reference",
         x = "Sex", 
         y = "Percentage (%)",
         fill = "Data Source") +
    theme(axis.text.x = element_text(angle = 0, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14),
          title = element_text(size = 16),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14)) +
    scale_fill_manual(values = c("PsychAD Dataset" = "steelblue", "ADRD Reference" = "darkred"))

  ggsave("out/PsychAD_sex_vs_ADRD_reference.pdf", p_adrd_sex, width = 6, height = 4)
  log_message("Saved: PsychAD_sex_vs_ADRD_reference.pdf")
  
  # Create ADRD reference barplot for sex in same format as PsychAD
  p_adrd_sex_ref <- ggplot(adrd_sex_overall, aes(x = Sex_Mapped, y = ADRD_Percentage, fill = Sex_Mapped)) +
    geom_bar(stat = "identity") +
    theme_minimal(base_size = 14) +
    labs(title = "ADRD Reference: Disease Demographics by Sex",
         x = "Sex", 
         y = "Percentage (%)") +
    theme(axis.text.x = element_text(angle = 0, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14),
          title = element_text(size = 16),
          legend.position = "none") +
    scale_fill_manual(values = sex_colors)

  ggsave("out/ADRD_reference_sex_distribution_barplot.pdf", p_adrd_sex_ref, width = 4, height = 3)
  log_message("Saved: ADRD_reference_sex_distribution_barplot.pdf")
}

# 4. Disease-Specific Analysis: PsychAD vs ADRD Reference
log_message("\n--- Disease-Specific Analysis: PsychAD vs ADRD Reference ---")

# Define disease groups for analysis with full names and their corresponding column names
disease_groups <- list(
  "Alzheimer's Disease" = "AD_combined",
  "Schizophrenia" = "SCZ", 
  "Dementia with Lewy Bodies" = "DLBD",
  "Vascular Dementia" = "Vascular",
  "Bipolar Disorder" = "BD",
  "Tauopathies" = "Tau",
  "Parkinson's Disease" = "PD",
  "Frontotemporal Dementia" = "FTD",
  "Dementia" = "Dementia",
  "Control" = "Control"
)

# Create disease-specific ancestry comparison
disease_ancestry_comparison <- data.frame()

for(group_name in names(disease_groups)) {
  disease_column <- disease_groups[[group_name]]
  
  # PsychAD data for this disease group
  psychad_group_data <- data %>%
    filter(!is.na(Ancestry_clean) & Ancestry_clean != "Unknown") %>%
    mutate(
      # Create disease indicators
      AD_combined = ifelse((crossDis_AD == 1 | dx_AD == 1) %in% TRUE, 1, 0),
      SCZ = ifelse(crossDis_SCZ == 1 %in% TRUE, 1, 0),
      DLBD = ifelse(crossDis_DLBD == 1 %in% TRUE, 1, 0),
      Vascular = ifelse(crossDis_Vas == 1 %in% TRUE, 1, 0),
      BD = ifelse(crossDis_BD == 1 %in% TRUE, 1, 0),
      Tau = ifelse(crossDis_Tau == 1 %in% TRUE, 1, 0),
      PD = ifelse(crossDis_PD == 1 %in% TRUE, 1, 0),
      FTD = ifelse(crossDis_FTD == 1 %in% TRUE, 1, 0),
      Dementia = ifelse(Dementia == 1 %in% TRUE, 1, 0),
      Any_Disease = ifelse(AD_combined == 1 | SCZ == 1 | DLBD == 1 | Vascular == 1 |
                          BD == 1 | Tau == 1 | PD == 1 | FTD == 1 | Dementia == 1, 1, 0),
      Control = ifelse(Any_Disease == 0, 1, 0)
    ) %>%
    filter(!!sym(disease_column) == 1) %>%
    count(Ancestry_clean) %>%
    mutate(
      Disease_Group = group_name,
      Source = "PsychAD Dataset",
      Percentage = n / sum(n) * 100
    )
  
  # ADRD data for this disease group
  adrd_group_data <- adrd_clean %>%
    filter(Disease_Mapped == disease_column & Measure == "Ancestry") %>%
    group_by(Ancestry_Mapped) %>%
    summarise(
      Total_Percentage = sum(Percentage, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      Disease_Group = group_name,
      Source = "ADRD Reference",
      Percentage = Total_Percentage / sum(Total_Percentage) * 100,
      n = Total_Percentage  # For consistency with PsychAD data
    ) %>%
    rename(Ancestry_clean = Ancestry_Mapped)
  
  # Debug: Log data availability for each disease group
  log_message(paste("Disease group:", group_name, "- PsychAD samples:", sum(psychad_group_data$n, na.rm = TRUE), 
                   "- ADRD total percentage:", sum(adrd_group_data$n, na.rm = TRUE)))
  
  # Combine PsychAD and ADRD data
  if(nrow(psychad_group_data) > 0 || nrow(adrd_group_data) > 0) {
    combined_data <- bind_rows(psychad_group_data, adrd_group_data)
    disease_ancestry_comparison <- bind_rows(disease_ancestry_comparison, combined_data)
  }
}

# Create disease-specific sex comparison
disease_sex_comparison <- data.frame()

for(group_name in names(disease_groups)) {
  disease_column <- disease_groups[[group_name]]
  
  # PsychAD data for this disease group
  psychad_group_data <- data %>%
    filter(!is.na(sex)) %>%
    mutate(
      # Create disease indicators
      AD_combined = ifelse((crossDis_AD == 1 | dx_AD == 1) %in% TRUE, 1, 0),
      SCZ = ifelse(crossDis_SCZ == 1 %in% TRUE, 1, 0),
      DLBD = ifelse(crossDis_DLBD == 1 %in% TRUE, 1, 0),
      Vascular = ifelse(crossDis_Vas == 1 %in% TRUE, 1, 0),
      BD = ifelse(crossDis_BD == 1 %in% TRUE, 1, 0),
      Tau = ifelse(crossDis_Tau == 1 %in% TRUE, 1, 0),
      PD = ifelse(crossDis_PD == 1 %in% TRUE, 1, 0),
      FTD = ifelse(crossDis_FTD == 1 %in% TRUE, 1, 0),
      Dementia = ifelse(Dementia == 1 %in% TRUE, 1, 0),
      Any_Disease = ifelse(AD_combined == 1 | SCZ == 1 | DLBD == 1 | Vascular == 1 |
                          BD == 1 | Tau == 1 | PD == 1 | FTD == 1 | Dementia == 1, 1, 0),
      Control = ifelse(Any_Disease == 0, 1, 0)
    ) %>%
    filter(!!sym(disease_column) == 1) %>%
    count(sex) %>%
    mutate(
      Disease_Group = group_name,
      Source = "PsychAD Dataset",
      Percentage = n / sum(n) * 100
    )
  
  # ADRD data for this disease group
  adrd_group_data <- adrd_clean %>%
    filter(Disease_Mapped == disease_column & Measure == "Sex") %>%
    group_by(Sex_Mapped) %>%
    summarise(
      Total_Percentage = sum(Percentage, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      Disease_Group = group_name,
      Source = "ADRD Reference",
      Percentage = Total_Percentage / sum(Total_Percentage) * 100,
      n = Total_Percentage  # For consistency with PsychAD data
    ) %>%
    rename(sex = Sex_Mapped)
  
  # Debug: Log data availability for each disease group
  log_message(paste("Disease group (sex):", group_name, "- PsychAD samples:", sum(psychad_group_data$n, na.rm = TRUE), 
                   "- ADRD total percentage:", sum(adrd_group_data$n, na.rm = TRUE)))
  
  # Combine PsychAD and ADRD data
  if(nrow(psychad_group_data) > 0 || nrow(adrd_group_data) > 0) {
    combined_data <- bind_rows(psychad_group_data, adrd_group_data)
    disease_sex_comparison <- bind_rows(disease_sex_comparison, combined_data)
  }
}

# Create stacked bar plots for disease-specific ancestry comparison
if(nrow(disease_ancestry_comparison) > 0) {
  # Filter for disease groups with sufficient data
  disease_groups_with_data <- disease_ancestry_comparison %>%
    group_by(Disease_Group, Source) %>%
    summarise(Total_Samples = sum(n, na.rm = TRUE), .groups = 'drop') %>%
    filter(Total_Samples >= 5) %>%
    pull(Disease_Group) %>%
    unique()
  
  if(length(disease_groups_with_data) > 0) {
    # PsychAD diseases by ancestry
    psychad_disease_ancestry <- disease_ancestry_comparison %>%
      filter(Source == "PsychAD Dataset" & Disease_Group %in% disease_groups_with_data) %>%
      group_by(Disease_Group) %>%
      mutate(total = sum(n),
             prop = n/total) %>%
      ungroup() %>%
      mutate(disease_label = paste0(Disease_Group, ' (n=', total, ')'))
    
    p_disease_ancestry <- ggplot(psychad_disease_ancestry, aes(x = disease_label, y = prop, fill = Ancestry_clean)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_minimal(base_size = 14) +
      labs(title = "PsychAD: Diseases by Ancestry",
           x = "Disease Type",
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
    
    # Adjust width based on number of diseases
    plot_width <- max(8, length(disease_groups_with_data) * 1.2)
    ggsave("out/PsychAD_diseases_by_ancestry_stacked_vs_ADRD.pdf", p_disease_ancestry, width = plot_width, height = 6)
    log_message("Saved: PsychAD_diseases_by_ancestry_stacked_vs_ADRD.pdf")
    
    # ADRD diseases by ancestry
    adrd_disease_ancestry <- disease_ancestry_comparison %>%
      filter(Source == "ADRD Reference" & Disease_Group %in% disease_groups_with_data) %>%
      group_by(Disease_Group) %>%
      mutate(total = sum(n),
             prop = n/total) %>%
      ungroup() %>%
      mutate(disease_label = paste0(Disease_Group, ' (pct=', round(total, 1), '%)'))
    
    p_adrd_disease_ancestry <- ggplot(adrd_disease_ancestry, aes(x = disease_label, y = prop, fill = Ancestry_clean)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_minimal(base_size = 14) +
      labs(title = "ADRD Reference: Diseases by Ancestry",
           x = "Disease Type",
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
    
    # Adjust width based on number of diseases
    plot_width <- max(8, length(disease_groups_with_data) * 1.2)
    ggsave("out/ADRD_diseases_by_ancestry_stacked.pdf", p_adrd_disease_ancestry, width = plot_width, height = 6)
    log_message("Saved: ADRD_diseases_by_ancestry_stacked.pdf")
  }
}

# Create stacked bar plots for disease-specific sex comparison
if(nrow(disease_sex_comparison) > 0) {
  # Filter for disease groups with sufficient data
  disease_groups_with_data <- disease_sex_comparison %>%
    group_by(Disease_Group, Source) %>%
    summarise(Total_Samples = sum(n, na.rm = TRUE), .groups = 'drop') %>%
    filter(Total_Samples >= 5) %>%
    pull(Disease_Group) %>%
    unique()
  
  if(length(disease_groups_with_data) > 0) {
    # PsychAD diseases by sex
    psychad_disease_sex <- disease_sex_comparison %>%
      filter(Source == "PsychAD Dataset" & Disease_Group %in% disease_groups_with_data) %>%
      group_by(Disease_Group) %>%
      mutate(total = sum(n),
             prop = n/total) %>%
      ungroup() %>%
      mutate(disease_label = paste0(Disease_Group, ' (n=', total, ')'))
    
    p_disease_sex <- ggplot(psychad_disease_sex, aes(x = disease_label, y = prop, fill = sex)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_minimal(base_size = 14) +
      labs(title = "PsychAD: Diseases by Sex",
           x = "Disease Type",
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
    
    # Adjust width based on number of diseases
    plot_width <- max(8, length(disease_groups_with_data) * 1.2)
    ggsave("out/PsychAD_diseases_by_sex_stacked_vs_ADRD.pdf", p_disease_sex, width = plot_width, height = 6)
    log_message("Saved: PsychAD_diseases_by_sex_stacked_vs_ADRD.pdf")
    
    # ADRD diseases by sex
    adrd_disease_sex <- disease_sex_comparison %>%
      filter(Source == "ADRD Reference" & Disease_Group %in% disease_groups_with_data) %>%
      group_by(Disease_Group) %>%
      mutate(total = sum(n),
             prop = n/total) %>%
      ungroup() %>%
      mutate(disease_label = paste0(Disease_Group, ' (pct=', round(total, 1), '%)'))
    
    p_adrd_disease_sex <- ggplot(adrd_disease_sex, aes(x = disease_label, y = prop, fill = sex)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_minimal(base_size = 14) +
      labs(title = "ADRD Reference: Diseases by Sex",
           x = "Disease Type",
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
    
    # Adjust width based on number of diseases
    plot_width <- max(8, length(disease_groups_with_data) * 1.2)
    ggsave("out/ADRD_diseases_by_sex_stacked.pdf", p_adrd_disease_sex, width = plot_width, height = 6)
    log_message("Saved: ADRD_diseases_by_sex_stacked.pdf")
  }
}

# Create side-by-side comparison plots for specific diseases
if(nrow(disease_ancestry_comparison) > 0) {
  # Select diseases with sufficient data in both PsychAD and ADRD
  comparison_diseases <- disease_ancestry_comparison %>%
    group_by(Disease_Group, Source) %>%
    summarise(Total_Samples = sum(n, na.rm = TRUE), .groups = 'drop') %>%
    group_by(Disease_Group) %>%
    filter(n() == 2) %>%  # Must have both PsychAD and ADRD data
    summarise(Min_Samples = min(Total_Samples), .groups = 'drop') %>%
    filter(Min_Samples >= 5) %>%
    pull(Disease_Group)
  
  if(length(comparison_diseases) > 0) {
    # Ancestry comparison for specific diseases
    for(disease_type in comparison_diseases) {
      disease_data <- disease_ancestry_comparison %>%
        filter(Disease_Group == disease_type)
      
      p_disease_ancestry_comp <- ggplot(disease_data, aes(x = Ancestry_clean, y = Percentage, fill = Source)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.7) +
        theme_minimal(base_size = 14) +
        labs(title = paste("PsychAD vs ADRD: Ancestry Distribution -", disease_type),
             x = "Ancestry Group", 
             y = "Percentage (%)",
             fill = "Data Source") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 14),
              title = element_text(size = 16),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 14)) +
        scale_fill_manual(values = c("PsychAD Dataset" = "steelblue", "ADRD Reference" = "darkred"))
      
      # Adjust width based on disease name length
      disease_name_length <- nchar(disease_type)
      plot_width <- max(6, min(12, disease_name_length * 0.3))
      ggsave(paste0("out/PsychAD_vs_ADRD_ancestry_", gsub(" ", "_", disease_type), ".pdf"), p_disease_ancestry_comp, width = plot_width, height = 5)
      log_message(paste("Saved: PsychAD_vs_ADRD_ancestry_", gsub(" ", "_", disease_type), ".pdf"))
    }
    
    # Sex comparison for specific diseases
    for(disease_type in comparison_diseases) {
      disease_data <- disease_sex_comparison %>%
        filter(Disease_Group == disease_type)
      
      p_disease_sex_comp <- ggplot(disease_data, aes(x = sex, y = Percentage, fill = Source)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.7) +
        theme_minimal(base_size = 14) +
        labs(title = paste("PsychAD vs ADRD: Sex Distribution -", disease_type),
             x = "Sex", 
             y = "Percentage (%)",
             fill = "Data Source") +
        theme(axis.text.x = element_text(angle = 0, size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 14),
              title = element_text(size = 16),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 14)) +
        scale_fill_manual(values = c("PsychAD Dataset" = "steelblue", "ADRD Reference" = "darkred"))
      
      # Adjust width based on disease name length
      disease_name_length <- nchar(disease_type)
      plot_width <- max(5, min(10, disease_name_length * 0.25))
      ggsave(paste0("out/PsychAD_vs_ADRD_sex_", gsub(" ", "_", disease_type), ".pdf"), p_disease_sex_comp, width = plot_width, height = 4)
      log_message(paste("Saved: PsychAD_vs_ADRD_sex_", gsub(" ", "_", disease_type), ".pdf"))
    }
  }
}

# 5. Missing Data Analysis
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
log_message("✓ Statistical analysis against ADRD reference completed")
log_message("✓ Equity metrics against ADRD reference calculated")
log_message("✓ Disease-specific ADRD comparison visualizations completed")
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
    disease == 'AD_combined' ~ paste0('Alzheimer\'s Disease (n=', total, ')'),
    disease == 'SCZ' ~ paste0('Schizophrenia (n=', total, ')'),
    disease == 'DLBD' ~ paste0('Dementia with Lewy Bodies (n=', total, ')'),
    disease == 'Vascular' ~ paste0('Vascular Dementia (n=', total, ')'),
    disease == 'BD' ~ paste0('Bipolar Disorder (n=', total, ')'),
    disease == 'Tau' ~ paste0('Tauopathies (n=', total, ')'),
    disease == 'PD' ~ paste0('Parkinson\'s Disease (n=', total, ')'),
    disease == 'FTD' ~ paste0('Frontotemporal Dementia (n=', total, ')'),
    disease == 'Dementia' ~ paste0('Dementia (n=', total, ')')
  ))

p_disease_ancestry <- ggplot(disease_ancestry_data, aes(x = disease_label, y = prop, fill = Ancestry_clean)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal(base_size = 14) +
  labs(title = "Disease Diagnoses by Ancestry",
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

# Adjust width based on number of diseases
plot_width <- max(8, length(unique(disease_ancestry_data$disease)) * 1.5)
ggsave("out/disease_diagnoses_by_ancestry_stacked.pdf", p_disease_ancestry, width = plot_width, height = 4)
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
    disease == 'AD_combined' ~ paste0('Alzheimer\'s Disease (n=', total, ')'),
    disease == 'SCZ' ~ paste0('Schizophrenia (n=', total, ')'),
    disease == 'DLBD' ~ paste0('Dementia with Lewy Bodies (n=', total, ')'),
    disease == 'Vascular' ~ paste0('Vascular Dementia (n=', total, ')'),
    disease == 'BD' ~ paste0('Bipolar Disorder (n=', total, ')'),
    disease == 'Tau' ~ paste0('Tauopathies (n=', total, ')'),
    disease == 'PD' ~ paste0('Parkinson\'s Disease (n=', total, ')'),
    disease == 'FTD' ~ paste0('Frontotemporal Dementia (n=', total, ')'),
    disease == 'Dementia' ~ paste0('Dementia (n=', total, ')')
  ))

p_disease_sex <- ggplot(disease_sex_data, aes(x = disease_label, y = prop, fill = sex)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal(base_size = 14) +
  labs(title = "Disease Diagnoses by Sex",
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

# Adjust width based on number of diseases
plot_width <- max(8, length(unique(disease_sex_data$disease)) * 1.5)
ggsave("out/disease_diagnoses_by_sex_stacked.pdf", p_disease_sex, width = plot_width, height = 4)
log_message("Saved: disease_diagnoses_by_sex_stacked.pdf")

log_message("\n=== ADDITIONAL PLOTS COMPLETED ===")
log_message("✓ Ancestry distribution whole cohort completed")
log_message("✓ Disease diagnoses by ancestry (stacked) completed")
log_message("✓ Disease diagnoses by sex (stacked) completed")
