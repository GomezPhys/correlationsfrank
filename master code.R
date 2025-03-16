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
library(irr)

df <- read_excel("~/cor.xlsx", sheet = "Carotid_AG")
View (df)

shapiro.test(df$ESS)
shapiro.test(df$Heart_rate)
shapiro.test(df$RPE)
shapiro.test(df$Lactate)
shapiro.test(df$Workload)
shapiro.test(df$VO2)
shapiro.test(df$DP)

#####All together#####
  #####HR######
  cor_test <- cor.test(df$ESS, df$Heart_rate, method = "spearman")
  print(cor_test)
  
  ggplot(df, aes(x = Heart_rate, y = ESS)) +
    geom_point(alpha = 0.7) +  # Scatter points
    geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Loess smoothing
    labs(title = "Spearman Correlation: HR vs ESS",
         subtitle = paste("rho =", round(cor_test$estimate, 3), " | p =", signif(cor_test$p.value, 3)),
         x = "Heart Rate",
         y = "ESS") +
    theme_classic()
  
  ggplot(df, aes(x = Heart_rate, y = ESS)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Non-linear trend
    facet_grid(Group ~ Intensity) +  # Separate plots per Group & Intensity
    labs(title = "Spearman Correlation: HR vs ESS",
         x = "Heart Rate",
         y = "ESS") +
    theme_classic()
  
  icc_result <- icc(df[, c("ESS", "Heart_rate")], model = "twoway", type = "agreement", unit = "single")
  print(icc_result)
  
  # Prepare ICC results for plotting
  icc_plot_data <- icc_results %>%
    mutate(Intensity = factor(Intensity, levels = c("Baseline", "Low", "Moderate", "High")))  # Keep correct order
  
  # Plot ICC values with confidence intervals
  ggplot(icc_plot_data, aes(x = Intensity, y = ICC, fill = Group)) +
    geom_bar(stat = "identity", position = "dodge") +  # Bar plot for ICC
    geom_errorbar(aes(ymin = ICC_lower, ymax = ICC_upper), width = 0.2, position = position_dodge(0.9)) +  # Add CI
    labs(title = "Intraclass Correlation (ICC): ESS vs HR",
         x = "Intensity",
         y = "ICC Value") +
    scale_fill_manual(values = c("Young" = "blue", "Stroke" = "red")) +  # Color by Group
    theme_classic()
  
  
  df <- df %>%
    mutate(Mean = (ESS + Heart_rate) / 2,
           Diff = ESS - Heart_rate)
  
  mean_diff <- mean(df$Diff)
  sd_diff <- sd(df$Diff)
  
  ggplot(df, aes(x = Mean, y = Diff)) +
    geom_point() +
    geom_hline(yintercept = mean_diff, linetype = "solid", color = "blue") +
    geom_hline(yintercept = mean_diff + 1.96 * sd_diff, linetype = "dashed", color = "red") +
    geom_hline(yintercept = mean_diff - 1.96 * sd_diff, linetype = "dashed", color = "red") +
    labs(title = "Bland-Altman Plot", x = "Mean of ESS and HR", y = "Difference (ESS - HR)") +
    theme_classic()

  #####Workload####
  cor_test <- cor.test(df$ESS, df$Workload, method = "spearman")
  print(cor_test)
  
  ggplot(df, aes(x = Workload, y = ESS)) +
    geom_point(alpha = 0.7) +  # Scatter points
    geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Loess smoothing
    labs(title = "Spearman Correlation: Workload vs ESS",
         subtitle = paste("rho =", round(cor_test$estimate, 3), " | p =", signif(cor_test$p.value, 3)),
         x = "Workload",
         y = "ESS") +
    theme_classic()
  
  icc_result <- icc(df[, c("ESS", "Workload")], model = "twoway", type = "agreement", unit = "single")
  print(icc_result)
  
  df <- df %>%
    mutate(Mean = (ESS + Workload) / 2,
           Diff = ESS - Workload)
  
  mean_diff <- mean(df$Diff)
  sd_diff <- sd(df$Diff)
  
  ggplot(df, aes(x = Mean, y = Diff)) +
    geom_point() +
    geom_hline(yintercept = mean_diff, linetype = "solid", color = "blue") +
    geom_hline(yintercept = mean_diff + 1.96 * sd_diff, linetype = "dashed", color = "red") +
    geom_hline(yintercept = mean_diff - 1.96 * sd_diff, linetype = "dashed", color = "red") +
    labs(title = "Bland-Altman Plot", x = "Mean of ESS and Workload", y = "Difference (ESS - Workload)") +
    theme_classic()
  
  #####RPE#####
  cor_test <- cor.test(df$ESS, df$RPE, method = "spearman")
  print(cor_test)
  
  ggplot(df, aes(x = RPE, y = ESS)) +
    geom_point(alpha = 0.7) +  # Scatter points
    geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Loess smoothing
    labs(title = "Spearman Correlation: Workload vs ESS",
         subtitle = paste("rho =", round(cor_test$estimate, 3), " | p =", signif(cor_test$p.value, 3)),
         x = "RPE",
         y = "ESS") +
    theme_classic()
  
  icc_result <- icc(df[, c("ESS", "RPE")], model = "twoway", type = "agreement", unit = "single")
  print(icc_result)
  
  df <- df %>%
    mutate(Mean = (ESS + RPE) / 2,
           Diff = ESS - RPE)
  
  mean_diff <- mean(df$Diff)
  sd_diff <- sd(df$Diff)
  
  ggplot(df, aes(x = Mean, y = Diff)) +
    geom_point() +
    geom_hline(yintercept = mean_diff, linetype = "solid", color = "blue") +
    geom_hline(yintercept = mean_diff + 1.96 * sd_diff, linetype = "dashed", color = "red") +
    geom_hline(yintercept = mean_diff - 1.96 * sd_diff, linetype = "dashed", color = "red") +
    labs(title = "Bland-Altman Plot", x = "Mean of ESS and RPE", y = "Difference (ESS - RPE)") +
    theme_classic()
  
  #####Lactate####
  cor_test <- cor.test(df$ESS, df$Lactate, method = "spearman")
  print(cor_test)
  
  ggplot(df, aes(x = Lactate, y = ESS)) +
    geom_point(alpha = 0.7) +  # Scatter points
    geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Loess smoothing
    labs(title = "Spearman Correlation: Workload vs ESS",
         subtitle = paste("rho =", round(cor_test$estimate, 3), " | p =", signif(cor_test$p.value, 3)),
         x = "Lactate",
         y = "ESS") +
    theme_classic()
  
  icc_result <- icc(df[, c("ESS", "Lactate")], model = "twoway", type = "agreement", unit = "single")
  print(icc_result)
  
  df <- df %>%
    mutate(Mean = (ESS + Lactate) / 2,
           Diff = ESS - Lactate)
  
  mean_diff <- mean(df$Diff)
  sd_diff <- sd(df$Diff)
  
  ggplot(df, aes(x = Mean, y = Diff)) +
    geom_point() +
    geom_hline(yintercept = mean_diff, linetype = "solid", color = "blue") +
    geom_hline(yintercept = mean_diff + 1.96 * sd_diff, linetype = "dashed", color = "red") +
    geom_hline(yintercept = mean_diff - 1.96 * sd_diff, linetype = "dashed", color = "red") +
    labs(title = "Bland-Altman Plot", x = "Mean of ESS and Lactate", y = "Difference (ESS - Lactate)") +
    theme_classic()
  #####VO2
  cor_test <- cor.test(df$ESS, df$Lactate, method = "spearman")
  print(cor_test)
  
  ggplot(df, aes(x = Lactate, y = ESS)) +
    geom_point(alpha = 0.7) +  # Scatter points
    geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Loess smoothing
    labs(title = "Spearman Correlation: Workload vs ESS",
         subtitle = paste("rho =", round(cor_test$estimate, 3), " | p =", signif(cor_test$p.value, 3)),
         x = "Lactate",
         y = "ESS") +
    theme_classic()
  
  icc_result <- icc(df[, c("ESS", "Lactate")], model = "twoway", type = "agreement", unit = "single")
  print(icc_result)
  
  df <- df %>%
    mutate(Mean = (ESS + Lactate) / 2,
           Diff = ESS - Lactate)
  
  mean_diff <- mean(df$Diff)
  sd_diff <- sd(df$Diff)
  
  ggplot(df, aes(x = Mean, y = Diff)) +
    geom_point() +
    geom_hline(yintercept = mean_diff, linetype = "solid", color = "blue") +
    geom_hline(yintercept = mean_diff + 1.96 * sd_diff, linetype = "dashed", color = "red") +
    geom_hline(yintercept = mean_diff - 1.96 * sd_diff, linetype = "dashed", color = "red") +
    labs(title = "Bland-Altman Plot", x = "Mean of ESS and Lactate", y = "Difference (ESS - Lactate)") +
    theme_classic()
  #####VO2####
  cor_test <- cor.test(df$ESS, df$VO2, method = "spearman")
  print(cor_test)
  
  ggplot(df, aes(x = VO2, y = ESS)) +
    geom_point(alpha = 0.7) +  # Scatter points
    geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Loess smoothing
    labs(title = "Spearman Correlation: VO2 vs ESS",
         subtitle = paste("rho =", round(cor_test$estimate, 3), " | p =", signif(cor_test$p.value, 3)),
         x = "VO2",
         y = "ESS") +
    theme_classic()
  
  icc_result <- icc(df[, c("ESS", "VO2")], model = "twoway", type = "agreement", unit = "single")
  print(icc_result)
  
  df <- df %>%
    mutate(Mean = (ESS + VO2) / 2,
           Diff = ESS - VO2)
  
  mean_diff <- mean(df$Diff)
  sd_diff <- sd(df$Diff)
  
  ggplot(df, aes(x = Mean, y = Diff)) +
    geom_point() +
    geom_hline(yintercept = mean_diff, linetype = "solid", color = "blue") +
    geom_hline(yintercept = mean_diff + 1.96 * sd_diff, linetype = "dashed", color = "red") +
    geom_hline(yintercept = mean_diff - 1.96 * sd_diff, linetype = "dashed", color = "red") +
    labs(title = "Bland-Altman Plot", x = "Mean of ESS and VO2", y = "Difference (ESS - VO2)") +
    theme_classic()

  
  #####DP####
  cor_test <- cor.test(df$ESS, df$DP, method = "spearman")
  print(cor_test)
  
  ggplot(df, aes(x = DP, y = ESS)) +
    geom_point(alpha = 0.7) +  # Scatter points
    geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Loess smoothing
    labs(title = "Spearman Correlation: DP vs ESS",
         subtitle = paste("rho =", round(cor_test$estimate, 3), " | p =", signif(cor_test$p.value, 3)),
         x = "DP",
         y = "ESS") +
    theme_classic()
  
  icc_result <- icc(df[, c("ESS", "DP")], model = "twoway", type = "agreement", unit = "single")
  print(icc_result)
  
 
  
  
#####ONLY STROKE####
  #####HR#####
  # Compute correlation separately for each Group and Intensity
  cor_results <- df %>%
    group_by(Group, Intensity) %>%
    summarise(
      cor_test = list(cor.test(Heart_rate, ESS, method = "spearman")),
      .groups = "drop"
    ) %>%
    mutate(
      rho = sapply(cor_test, function(x) x$estimate),
      p_value = sapply(cor_test, function(x) x$p.value)
    ) %>%
    select(Group, Intensity, rho, p_value)
  
  print(cor_results)
  
  ggplot(df, aes(x = Heart_rate, y = ESS)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Non-linear trend
    facet_grid(Group ~ Intensity) +  # Separate plots per Group & Intensity
    labs(title = "Spearman Correlation: HR vs ESS",
         x = "Heart Rate",
         y = "ESS") +
    theme_classic()
  
  
  icc_results <- df %>%
    group_by(Group, Intensity) %>%
    summarise(
      icc_test = list(icc(data.frame(ESS, Heart_rate), model = "twoway", type = "agreement", unit = "single")),
      .groups = "drop"
    ) %>%
    mutate(
      ICC = sapply(icc_test, function(x) x$value),
      ICC_lower = sapply(icc_test, function(x) x$lbound),
      ICC_upper = sapply(icc_test, function(x) x$ubound),
      ICC_p_value = sapply(icc_test, function(x) x$p.value)
    ) %>%
    select(Group, Intensity, ICC, ICC_lower, ICC_upper, ICC_p_value)
  
  print(icc_results)
  
  
  # Prepare ICC results for plotting
  icc_plot_data <- icc_results %>%
    mutate(Intensity = factor(Intensity, levels = c("Baseline", "Low", "Moderate", "High")))  # Keep correct order
  
  # Plot ICC values with confidence intervals
  ggplot(icc_plot_data, aes(x = Intensity, y = ICC, fill = Group)) +
    geom_bar(stat = "identity", position = "dodge") +  # Bar plot for ICC
    geom_errorbar(aes(ymin = ICC_lower, ymax = ICC_upper), width = 0.2, position = position_dodge(0.9)) +  # Add CI
    labs(title = "Intraclass Correlation (ICC): ESS vs HR",
         x = "Intensity",
         y = "ICC Value") +
    scale_fill_manual(values = c("Young" = "blue", "Stroke" = "red")) +  # Color by Group
    theme_classic()
  
  
  # Create Bland-Altman plots separately for each group and intensity
  df %>%
    group_by(Group, Intensity) %>%
    mutate(Mean = (ESS + Heart_rate) / 2,
           Diff = ESS - Heart_rate) %>%
    ggplot(aes(x = Mean, y = Diff)) +
    geom_point(alpha = 0.7) +
    geom_hline(aes(yintercept = mean(Diff)), color = "blue", linetype = "solid") +
    geom_hline(aes(yintercept = mean(Diff) + 1.96 * sd(Diff)), color = "red", linetype = "dashed") +
    geom_hline(aes(yintercept = mean(Diff) - 1.96 * sd(Diff)), color = "red", linetype = "dashed") +
    facet_grid(Group ~ Intensity) +  # Creates separate plots per Group & Intensity
    labs(title = "Bland-Altman Plot: ESS vs Heart_rate",
         x = "Mean of ESS and Heart_rate",
         y = "Difference (ESS - Heart_rate)") +
    theme_classic()
  
  
  #####Workload####
  # Compute correlation separately for each Group and Intensity
  cor_results <- df %>%
    group_by(Group, Intensity) %>%
    summarise(
      cor_test = list(cor.test(Workload, ESS, method = "spearman")),
      .groups = "drop"
    ) %>%
    mutate(
      rho = sapply(cor_test, function(x) x$estimate),
      p_value = sapply(cor_test, function(x) x$p.value)
    ) %>%
    select(Group, Intensity, rho, p_value)
  
  print(cor_results)
  
  ggplot(df, aes(x = Workload, y = ESS)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Non-linear trend
    facet_grid(Group ~ Intensity) +  # Separate plots per Group & Intensity
    labs(title = "Spearman Correlation: HR vs ESS",
         x = "Workload",
         y = "ESS") +
    theme_classic()
  
  
  
  icc_results <- df %>%
    group_by(Group, Intensity) %>%
    summarise(
      icc_test = list(icc(data.frame(ESS, Workload), model = "twoway", type = "agreement", unit = "single")),
      .groups = "drop"
    ) %>%
    mutate(
      ICC = sapply(icc_test, function(x) x$value),
      ICC_lower = sapply(icc_test, function(x) x$lbound),
      ICC_upper = sapply(icc_test, function(x) x$ubound),
      ICC_p_value = sapply(icc_test, function(x) x$p.value)
    ) %>%
    select(Group, Intensity, ICC, ICC_lower, ICC_upper, ICC_p_value)
  
  print(icc_results)
  

  
  # Prepare ICC results for plotting
  icc_plot_data <- icc_results %>%
    mutate(Intensity = factor(Intensity, levels = c("Baseline", "Low", "Moderate", "High")))  # Keep correct order
  
  # Plot ICC values with confidence intervals
  ggplot(icc_plot_data, aes(x = Intensity, y = ICC, fill = Group)) +
    geom_bar(stat = "identity", position = "dodge") +  # Bar plot for ICC
    geom_errorbar(aes(ymin = ICC_lower, ymax = ICC_upper), width = 0.2, position = position_dodge(0.9)) +  # Add CI
    labs(title = "Intraclass Correlation (ICC): ESS vs Workload",
         x = "Intensity",
         y = "ICC Value") +
    scale_fill_manual(values = c("Young" = "blue", "Stroke" = "red")) +  # Color by Group
    theme_classic()
  
  
  
  # Create Bland-Altman plots separately for each group and intensity
  df %>%
    group_by(Group, Intensity) %>%
    mutate(Mean = (ESS + Workload) / 2,
           Diff = ESS - Workload) %>%
    ggplot(aes(x = Mean, y = Diff)) +
    geom_point(alpha = 0.7) +
    geom_hline(aes(yintercept = mean(Diff)), color = "blue", linetype = "solid") +
    geom_hline(aes(yintercept = mean(Diff) + 1.96 * sd(Diff)), color = "red", linetype = "dashed") +
    geom_hline(aes(yintercept = mean(Diff) - 1.96 * sd(Diff)), color = "red", linetype = "dashed") +
    facet_grid(Group ~ Intensity) +  # Creates separate plots per Group & Intensity
    labs(title = "Bland-Altman Plot: ESS vs Workload",
         x = "Mean of ESS and Workload",
         y = "Difference (ESS - Workload)") +
    theme_classic()
  #####RPE####
  # Compute correlation separately for each Group and Intensity
  cor_results <- df %>%
    group_by(Group, Intensity) %>%
    summarise(
      cor_test = list(cor.test(RPE, ESS, method = "spearman")),
      .groups = "drop"
    ) %>%
    mutate(
      rho = sapply(cor_test, function(x) x$estimate),
      p_value = sapply(cor_test, function(x) x$p.value)
    ) %>%
    select(Group, Intensity, rho, p_value)
  
  print(cor_results)
  
  ggplot(df, aes(x = RPE, y = ESS)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Non-linear trend
    facet_grid(Group ~ Intensity) +  # Separate plots per Group & Intensity
    labs(title = "Spearman Correlation: RPE vs ESS",
         x = "RPE",
         y = "ESS") +
    theme_classic()
  
  
  icc_results <- df %>%
    group_by(Group, Intensity) %>%
    summarise(
      icc_test = list(icc(data.frame(ESS, RPE), model = "twoway", type = "agreement", unit = "single")),
      .groups = "drop"
    ) %>%
    mutate(
      ICC = sapply(icc_test, function(x) x$value),
      ICC_lower = sapply(icc_test, function(x) x$lbound),
      ICC_upper = sapply(icc_test, function(x) x$ubound),
      ICC_p_value = sapply(icc_test, function(x) x$p.value)
    ) %>%
    select(Group, Intensity, ICC, ICC_lower, ICC_upper, ICC_p_value)
  
  print(icc_results)
  
  # Prepare ICC results for plotting
  icc_plot_data <- icc_results %>%
    mutate(Intensity = factor(Intensity, levels = c("Baseline", "Low", "Moderate", "High")))  # Keep correct order
  
  # Plot ICC values with confidence intervals
  ggplot(icc_plot_data, aes(x = Intensity, y = ICC, fill = Group)) +
    geom_bar(stat = "identity", position = "dodge") +  # Bar plot for ICC
    geom_errorbar(aes(ymin = ICC_lower, ymax = ICC_upper), width = 0.2, position = position_dodge(0.9)) +  # Add CI
    labs(title = "Intraclass Correlation (ICC): ESS vs RPE",
         x = "Intensity",
         y = "ICC Value") +
    scale_fill_manual(values = c("Young" = "blue", "Stroke" = "red")) +  # Color by Group
    theme_classic()
  
  
  
  # Create Bland-Altman plots separately for each group and intensity
  df %>%
    group_by(Group, Intensity) %>%
    mutate(Mean = (ESS + RPE) / 2,
           Diff = ESS - RPE) %>%
    ggplot(aes(x = Mean, y = Diff)) +
    geom_point(alpha = 0.7) +
    geom_hline(aes(yintercept = mean(Diff)), color = "blue", linetype = "solid") +
    geom_hline(aes(yintercept = mean(Diff) + 1.96 * sd(Diff)), color = "red", linetype = "dashed") +
    geom_hline(aes(yintercept = mean(Diff) - 1.96 * sd(Diff)), color = "red", linetype = "dashed") +
    facet_grid(Group ~ Intensity) +  # Creates separate plots per Group & Intensity
    labs(title = "Bland-Altman Plot: ESS vs RPE",
         x = "Mean of ESS and RPE",
         y = "Difference (ESS - HR)") +
    theme_classic()
  #####Lactate####
  # Compute correlation separately for each Group and Intensity
  cor_results <- df %>%
    group_by(Group, Intensity) %>%
    summarise(
      cor_test = list(cor.test(Lactate, ESS, method = "spearman")),
      .groups = "drop"
    ) %>%
    mutate(
      rho = sapply(cor_test, function(x) x$estimate),
      p_value = sapply(cor_test, function(x) x$p.value)
    ) %>%
    select(Group, Intensity, rho, p_value)
  
  print(cor_results)
  
  ggplot(df, aes(x = Lactate, y = ESS)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Non-linear trend
    facet_grid(Group ~ Intensity) +  # Separate plots per Group & Intensity
    labs(title = "Spearman Correlation: Lactate vs ESS",
         x = "Lactate",
         y = "ESS") +
    theme_classic()
  
  icc_results <- df %>%
    group_by(Group, Intensity) %>%
    summarise(
      icc_test = list(icc(data.frame(ESS, Lactate), model = "twoway", type = "agreement", unit = "single")),
      .groups = "drop"
    ) %>%
    mutate(
      ICC = sapply(icc_test, function(x) x$value),
      ICC_lower = sapply(icc_test, function(x) x$lbound),
      ICC_upper = sapply(icc_test, function(x) x$ubound),
      ICC_p_value = sapply(icc_test, function(x) x$p.value)
    ) %>%
    select(Group, Intensity, ICC, ICC_lower, ICC_upper, ICC_p_value)
  
  print(icc_results)
  
  # Prepare ICC results for plotting
  icc_plot_data <- icc_results %>%
    mutate(Intensity = factor(Intensity, levels = c("Baseline", "Low", "Moderate", "High")))  # Keep correct order
  
  # Plot ICC values with confidence intervals
  ggplot(icc_plot_data, aes(x = Intensity, y = ICC, fill = Group)) +
    geom_bar(stat = "identity", position = "dodge") +  # Bar plot for ICC
    geom_errorbar(aes(ymin = ICC_lower, ymax = ICC_upper), width = 0.2, position = position_dodge(0.9)) +  # Add CI
    labs(title = "Intraclass Correlation (ICC): ESS vs Lactate",
         x = "Intensity",
         y = "ICC Value") +
    scale_fill_manual(values = c("Young" = "blue", "Stroke" = "red")) +  # Color by Group
    theme_classic()
  
  # Create Bland-Altman plots separately for each group and intensity
  df %>%
    group_by(Group, Intensity) %>%
    mutate(Mean = (ESS + Lactate) / 2,
           Diff = ESS - Lactate) %>%
    ggplot(aes(x = Mean, y = Diff)) +
    geom_point(alpha = 0.7) +
    geom_hline(aes(yintercept = mean(Diff)), color = "blue", linetype = "solid") +
    geom_hline(aes(yintercept = mean(Diff) + 1.96 * sd(Diff)), color = "red", linetype = "dashed") +
    geom_hline(aes(yintercept = mean(Diff) - 1.96 * sd(Diff)), color = "red", linetype = "dashed") +
    facet_grid(Group ~ Intensity) +  # Creates separate plots per Group & Intensity
    labs(title = "Bland-Altman Plot: ESS vs Lactate",
         x = "Mean of ESS and Lactate",
         y = "Difference (ESS - Lactate)") +
    theme_classic()
  #####VO2####
  # Compute correlation separately for each Group and Intensity
  cor_results <- df %>%
    group_by(Group, Intensity) %>%
    summarise(
      cor_test = list(cor.test(VO2, ESS, method = "spearman")),
      .groups = "drop"
    ) %>%
    mutate(
      rho = sapply(cor_test, function(x) x$estimate),
      p_value = sapply(cor_test, function(x) x$p.value)
    ) %>%
    select(Group, Intensity, rho, p_value)
  
  print(cor_results)
  
  ggplot(df, aes(x = VO2, y = ESS)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Non-linear trend
    facet_grid(Group ~ Intensity) +  # Separate plots per Group & Intensity
    labs(title = "Spearman Correlation: VO2 vs ESS",
         x = "VO2",
         y = "ESS") +
    theme_classic()
  
  
  icc_results <- df %>%
    group_by(Group, Intensity) %>%
    summarise(
      icc_test = list(icc(data.frame(ESS, VO2), model = "twoway", type = "agreement", unit = "single")),
      .groups = "drop"
    ) %>%
    mutate(
      ICC = sapply(icc_test, function(x) x$value),
      ICC_lower = sapply(icc_test, function(x) x$lbound),
      ICC_upper = sapply(icc_test, function(x) x$ubound),
      ICC_p_value = sapply(icc_test, function(x) x$p.value)
    ) %>%
    select(Group, Intensity, ICC, ICC_lower, ICC_upper, ICC_p_value)
  
  print(icc_results)
  
  
  # Prepare ICC results for plotting
  icc_plot_data <- icc_results %>%
    mutate(Intensity = factor(Intensity, levels = c("Baseline", "Low", "Moderate", "High")))  # Keep correct order
  
  # Plot ICC values with confidence intervals
  ggplot(icc_plot_data, aes(x = Intensity, y = ICC, fill = Group)) +
    geom_bar(stat = "identity", position = "dodge") +  # Bar plot for ICC
    geom_errorbar(aes(ymin = ICC_lower, ymax = ICC_upper), width = 0.2, position = position_dodge(0.9)) +  # Add CI
    labs(title = "Intraclass Correlation (ICC): ESS vs VO2",
         x = "Intensity",
         y = "ICC Value") +
    scale_fill_manual(values = c("Young" = "blue", "Stroke" = "red")) +  # Color by Group
    theme_classic()
  
  # Create Bland-Altman plots separately for each group and intensity
  df %>%
    group_by(Group, Intensity) %>%
    mutate(Mean = (ESS + VO2) / 2,
           Diff = ESS - VO2) %>%
    ggplot(aes(x = Mean, y = Diff)) +
    geom_point(alpha = 0.7) +
    geom_hline(aes(yintercept = mean(Diff)), color = "blue", linetype = "solid") +
    geom_hline(aes(yintercept = mean(Diff) + 1.96 * sd(Diff)), color = "red", linetype = "dashed") +
    geom_hline(aes(yintercept = mean(Diff) - 1.96 * sd(Diff)), color = "red", linetype = "dashed") +
    facet_grid(Group ~ Intensity) +  # Creates separate plots per Group & Intensity
    labs(title = "Bland-Altman Plot: ESS vs VO2",
         x = "Mean of ESS and VO2",
         y = "Difference (ESS - VO2)") +
    theme_classic()
  
  #####DP####
  # Compute correlation separately for each Group and Intensity
  cor_results <- df %>%
    group_by(Group, Intensity) %>%
    summarise(
      cor_test = list(cor.test(DP, ESS, method = "spearman")),
      .groups = "drop"
    ) %>%
    mutate(
      rho = sapply(cor_test, function(x) x$estimate),
      p_value = sapply(cor_test, function(x) x$p.value)
    ) %>%
    select(Group, Intensity, rho, p_value)
  
  print(cor_results)
  
  
  ggplot(df, aes(x = DP, y = ESS)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Non-linear trend
    facet_grid(Group ~ Intensity) +  # Separate plots per Group & Intensity
    labs(title = "Spearman Correlation: DP vs ESS",
         x = "DP",
         y = "ESS") +
    theme_classic()
  
  icc_results <- df %>%
    group_by(Group, Intensity) %>%
    summarise(
      icc_test = list(icc(data.frame(ESS, DP), model = "twoway", type = "agreement", unit = "single")),
      .groups = "drop"
    ) %>%
    mutate(
      ICC = sapply(icc_test, function(x) x$value),
      ICC_lower = sapply(icc_test, function(x) x$lbound),
      ICC_upper = sapply(icc_test, function(x) x$ubound),
      ICC_p_value = sapply(icc_test, function(x) x$p.value)
    ) %>%
    select(Group, Intensity, ICC, ICC_lower, ICC_upper, ICC_p_value)
  
  print(icc_results)
  
  # Prepare ICC results for plotting
  icc_plot_data <- icc_results %>%
    mutate(Intensity = factor(Intensity, levels = c("Baseline", "Low", "Moderate", "High")))  # Keep correct order
  
  # Plot ICC values with confidence intervals
  ggplot(icc_plot_data, aes(x = Intensity, y = ICC, fill = Group)) +
    geom_bar(stat = "identity", position = "dodge") +  # Bar plot for ICC
    geom_errorbar(aes(ymin = ICC_lower, ymax = ICC_upper), width = 0.2, position = position_dodge(0.9)) +  # Add CI
    labs(title = "Intraclass Correlation (ICC): ESS vs DP",
         x = "Intensity",
         y = "ICC Value") +
    scale_fill_manual(values = c("Young" = "blue", "Stroke" = "red")) +  # Color by Group
    theme_classic()
  
  # Create Bland-Altman plots separately for each group and intensity
  df %>%
    group_by(Group, Intensity) %>%
    mutate(Mean = (ESS + DP) / 2,
           Diff = ESS - DP) %>%
    ggplot(aes(x = Mean, y = Diff)) +
    geom_point(alpha = 0.7) +
    geom_hline(aes(yintercept = mean(Diff)), color = "blue", linetype = "solid") +
    geom_hline(aes(yintercept = mean(Diff) + 1.96 * sd(Diff)), color = "red", linetype = "dashed") +
    geom_hline(aes(yintercept = mean(Diff) - 1.96 * sd(Diff)), color = "red", linetype = "dashed") +
    facet_grid(Group ~ Intensity) +  # Creates separate plots per Group & Intensity
    labs(title = "Bland-Altman Plot: ESS vs DP",
         x = "Mean of ESS and DP",
         y = "Difference (ESS - DP)") +
    theme_classic()