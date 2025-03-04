library(readxl) ## To load excel sheet
library(dplyr) # Data grammar and manipulation
library(rstatix) # Shapiro Wilk and effect size
library(psych) #descriptives
library(kableExtra) #tables
library(lme4) #linear mixed effects models (LMM)
library(lmerTest) #anova like output for LMM
library(ggplot2) #data visualization
library(ggpubr)#data visualization
library(ggprism)##makes plots look like graphad
library(table1) #for descriptives
library(tidyverse)
library(tidyr)
install.packages("purrr")  # Only needed if not installed
library(purrr)

install.packages("readx1")
install.packages("dplyr")
install.packages("kableExtra")
install.packages("lme4")
install.packages("lmerTest")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("ggprism")
install.packages("table1")
install.packages("readxl")
install.packages("psych")
install.packages("tidyverse")
install.packages("ggcorrplot")
install.packages("tidyr")

df <- read_excel("~/cor.xlsx", sheet = "Carotid_AG")
View (df)


# Define the variables for normality check
vars_to_check <- c("ESS", "Heart_rate", "RPE", "VO2", "Lactate", "DP")

# Check normality for each group (Young, Stroke) using Shapiro-Wilk test
normality_results <- df %>%
  group_by(Group) %>%
  summarise(across(all_of(vars_to_check), ~ shapiro_test(.x)$p.value, .names = "shapiro_{col}"))

# Print normality results
print(normality_results)

# Visualize normality using histograms
df %>%
  pivot_longer(cols = all_of(vars_to_check), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Value, fill = Group)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, position = "identity") +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  ggtitle("Distribution of Variables by Group")

df %>%
  pivot_longer(cols = all_of(vars_to_check), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(sample = Value, color = Group)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  ggtitle("Q-Q Plots for Normality Check")



compute_correlation <- function(df, vars_to_check) {
  results_list <- list()
  
  for (grp in unique(df$Group)) {
    subset_df <- df %>% filter(Group == grp)
    
    cor_results <- map_dfr(vars_to_check, function(var) {
      if (sum(!is.na(subset_df[[var]])) > 1) {  # Ensure enough data for correlation
        method <- ifelse(
          normality_results %>% filter(Group == grp) %>% pull(paste0("shapiro_", var)) > 0.05,
          "pearson", "spearman"
        )
        test <- cor.test(subset_df$ESS, subset_df[[var]], method = method)
        
        tibble(Group = grp, Variable = var, Correlation = test$estimate, P_Value = test$p.value)
      } else {
        tibble(Group = grp, Variable = var, Correlation = NA, P_Value = NA)
      }
    })
    
    results_list[[grp]] <- cor_results
  }
  
  return(bind_rows(results_list))  # Combine all results into a single dataframe
}

# Compute correlations for Young and Stroke groups separately
cor_results_grouped <- compute_correlation(df, vars_to_check)

# Print correlation results for each group
print("Correlation Results by Group:")
print(cor_results_grouped)

### **Step 3: Compute Correlations Across All Participants (Ignoring Groups)**
cor_results_all <- map_dfr(vars_to_check, function(var) {
  if (sum(!is.na(df[[var]])) > 1) {  # Ensure enough data
    method <- ifelse(shapiro_test(df[[var]])$p.value > 0.05, "pearson", "spearman")
    test <- cor.test(df$ESS, df[[var]], method = method)
    
    tibble(Variable = var, Correlation = test$estimate, P_Value = test$p.value)
  } else {
    tibble(Variable = var, Correlation = NA, P_Value = NA)
  }
})

# Print correlation results across all participants
print("Correlation Results Across All Participants:")
print(cor_results_all)

### **Step 4: Visualize Scatter Plots**
scatter_plot <- function(df, x_var, y_var) {
  ggplot(df, aes_string(x = x_var, y = y_var, color = "Group")) +
    geom_point(size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
    theme_minimal() +
    ggtitle(paste("ESS vs", y_var, "by Group"))
}

# Generate scatter plots for key variables
scatter_plot(df, "ESS", "Heart_rate")
scatter_plot(df, "ESS", "VO2")
scatter_plot(df, "ESS", "RPE")

### **Step 5: Correlation Heatmap**
cor_matrix <- df %>%
  select(all_of(vars_to_check), ESS) %>%
  cor(method = "spearman", use = "pairwise.complete.obs")

ggcorrplot(cor_matrix, method = "square", type = "lower",
           lab = TRUE, colors = c("blue", "white", "red"),
           title = "Correlation Matrix of ESS and Metabolic Variables")



cor_results_grouped <- compute_correlation(df, vars_to_check)

# Print correlation results for each group
print("Correlation Results by Group:")
print(cor_results_grouped)

### **Step 3: Compute Correlations Across All Participants (Ignoring Groups)**
cor_results_all <- map_dfr(vars_to_check, function(var) {
  if (sum(!is.na(df[[var]])) > 1) {  # Ensure enough data
    method <- ifelse(shapiro_test(df[[var]])$p.value > 0.05, "pearson", "spearman")
    test <- cor.test(df$ESS, df[[var]], method = method)
    
    tibble(Variable = var, Correlation = round(test$estimate, 3), P_Value = round(test$p.value, 4))
  } else {
    tibble(Variable = var, Correlation = NA, P_Value = NA)
  }
})

# Print correlation results across all participants
print("Correlation Results Across All Participants:")
print(cor_results_all)

### **Step 4: Visualize Scatter Plots**
scatter_plot <- function(df, x_var, y_var) {
  ggplot(df, aes_string(x = x_var, y = y_var, color = "Group")) +
    geom_point(size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
    theme_minimal() +
    ggtitle(paste("ESS vs", y_var, "by Group"))
}

# Generate scatter plots for key variables
scatter_plot(df, "ESS", "Heart_rate")
scatter_plot(df, "ESS", "VO2")
scatter_plot(df, "ESS", "RPE")

### **Step 5: Correlation Heatmap**
cor_matrix <- df %>%
  select(all_of(vars_to_check), ESS) %>%
  cor(method = "spearman", use = "pairwise.complete.obs")

ggcorrplot(cor_matrix, method = "square", type = "lower",
           lab = TRUE, colors = c("blue", "white", "red"),
           title = "Correlation Matrix of ESS and Metabolic Variables")




# Load required libraries
library(tidyverse)
library(rstatix)
library(ggcorrplot)
library(purrr)  # For map_dfr()

# Define variables for correlation
vars_to_check <- c("Heart_rate", "RPE", "VO2", "Lactate", "DP")

### **Step 1: Check Normality for Each Group and Intensity**
normality_results <- df %>%
  group_by(Group, Intensity) %>%
  summarise(across(all_of(vars_to_check), ~ shapiro_test(.x)$p.value, .names = "shapiro_{col}"), .groups = "drop")

# Print normality test results
print("Normality Test Results:")
print(normality_results)

df$Intenisty <- as.factor(df$Intensity)

df$Intensity <- ordered(df$Intensity,
                        levels = c("Baseline", "Low", "Moderate", "High"))
### **Step 2: Compute Correlations Within Each Group and Intensity**
compute_correlation <- function(df, vars_to_check) {
  results_list <- list()
  
  for (grp in unique(df$Group)) {
    for (int in unique(df$Intensity)) {
      subset_df <- df %>% filter(Group == grp, Intensity == int)
      
      cor_results <- map_dfr(vars_to_check, function(var) {
        if (sum(!is.na(subset_df[[var]])) > 1) {  # Ensure enough data for correlation
          method <- ifelse(
            normality_results %>% filter(Group == grp, Intensity == int) %>% pull(paste0("shapiro_", var)) > 0.05,
            "pearson", "spearman"
          )
          test <- cor.test(subset_df$ESS, subset_df[[var]], method = method)
          
          tibble(Group = grp, Intensity = int, Variable = var, Correlation = round(test$estimate, 3), P_Value = round(test$p.value, 4))
        } else {
          tibble(Group = grp, Intensity = int, Variable = var, Correlation = NA, P_Value = NA)
        }
      })
      
      results_list[[paste(grp, int, sep = "_")]] <- cor_results
    }
  }
  
  return(bind_rows(results_list))  # Combine all results into a single dataframe
}

# Compute correlations for Young and Stroke groups at each Intensity
cor_results_grouped_intensity <- compute_correlation(df, vars_to_check)

# Print correlation results for each group and intensity
print("Correlation Results by Group and Intensity:")
print(cor_results_grouped_intensity)

### **Step 3: Compute Correlations Across All Participants (Ignoring Group and Intensity)**
cor_results_all <- map_dfr(vars_to_check, function(var) {
  if (sum(!is.na(df[[var]])) > 1) {  # Ensure enough data
    method <- ifelse(shapiro_test(df[[var]])$p.value > 0.05, "pearson", "spearman")
    test <- cor.test(df$ESS, df[[var]], method = method)
    
    tibble(Variable = var, Correlation = round(test$estimate, 3), P_Value = round(test$p.value, 4))
  } else {
    tibble(Variable = var, Correlation = NA, P_Value = NA)
  }
})

# Print correlation results across all participants
print("Correlation Results Across All Participants:")
print(cor_results_all)

### **Step 4: Visualize Scatter Plots by Group and Intensity**
scatter_plot <- function(df, x_var, y_var) {
  ggplot(df, aes_string(x = x_var, y = y_var, color = "Intensity")) +
    geom_point(size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
    theme_minimal() +
    facet_wrap(~ Group) +  # Separate plots by Group
    ggtitle(paste("ESS vs", y_var, "by Group and Intensity"))
}

# Generate scatter plots for key variables
scatter_plot(df, "ESS", "Heart_rate")
scatter_plot(df, "ESS", "VO2")
scatter_plot(df, "ESS", "RPE")

### **Step 5: Correlation Heatmap Across All Intensities**
cor_matrix <- df %>%
  select(all_of(vars_to_check), ESS) %>%
  cor(method = "spearman", use = "pairwise.complete.obs")

ggcorrplot(cor_matrix, method = "square", type = "lower",
           lab = TRUE, colors = c("blue", "white", "red"),
           title = "Correlation Matrix of ESS and Metabolic Variables")