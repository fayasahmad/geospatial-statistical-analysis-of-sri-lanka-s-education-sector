required_pkgs <- c("ggplot2","corrplot","car","lmtest")
to_install <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages({
  library(ggplot2); library(corrplot); library(car); library(lmtest)
})


try({
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    this_path <- rstudioapi::getSourceEditorContext()$path
    if (nzchar(this_path)) setwd(dirname(this_path))
  }
}, silent = TRUE)

csv_path <- "UQR.csv"
if (!file.exists(csv_path)) {
  message("UQR.csv not found in ", getwd(), ". Please choose the file...")
  csv_path <- file.choose()  # opens a file picker
}
uqr <- read.csv(csv_path, stringsAsFactors = FALSE)

names(uqr) <- make.names(names(uqr))

if ("Institution.Type" %in% names(uqr)) uqr$Institution.Type <- factor(uqr$Institution.Type)

num_cols <- intersect(c(
  "Student.Enrollment",
  "Faculty.Salary..Avg.",
  "Research.Funding..Million.USD.",
  "Graduation.Rate....",
  "Student.Faculty.Ratio",
  "Tuition.Fees..USD.",
  "Employment.Rate....",
  "University.Ranking.Score"
), names(uqr))

out_dir <- "appendix_A_outputs"
if (!dir.exists(out_dir)) dir.create(out_dir)

sink_to <- function(path, expr) {
  con <- file(path, open = "wt", encoding = "UTF-8")
  sink(con); on.exit({sink(); close(con)}, add = TRUE)
  eval.parent(substitute(expr))
}

sink_to(file.path(out_dir, "01_descriptives.txt"), {
  cat("=== Descriptive Statistics (numeric columns) ===\n")
  if (length(num_cols) > 0) print(summary(uqr[, num_cols]))
  cat("\n=== Missing Values Count (all columns) ===\n")
  print(colSums(is.na(uqr)))
})

sink_to(file.path(out_dir, "02_normality_shapiro.txt"), {
  cat("=== Shapiro-Wilk Normality Tests ===\n")
  for (v in num_cols) {
    x <- uqr[[v]]
    if (sum(!is.na(x)) >= 3 && length(unique(na.omit(x))) > 1) {
      sw <- shapiro.test(x)
      cat(sprintf("%s: W=%.4f, p=%.6f\n", v, unname(sw$statistic), sw$p.value))
    } else {
      cat(sprintf("%s: Skipped (insufficient variation or N)\n", v))
    }
  }
})

if (length(num_cols) > 1) {
  cor_mat <- suppressWarnings(cor(uqr[, num_cols], use = "pairwise.complete.obs"))
  write.csv(round(cor_mat, 3), file.path(out_dir, "03_correlation_matrix.csv"), row.names = TRUE)
  
  corrplot(cor_mat, method = "color", type = "upper", addCoef.col = "black", tl.col = "black", number.cex = .7)
  dev.copy(png, filename = file.path(out_dir, "03_correlation_heatmap.png"), width = 1200, height = 1000, res = 150)
  dev.off()
}

pairs_to_plot <- intersect(c(
  "Graduation.Rate....",
  "Employment.Rate....",
  "Research.Funding..Million.USD.",
  "Faculty.Salary..Avg.",        # <-- Faculty Salary INCLUDED
  "Student.Faculty.Ratio",
  "Tuition.Fees..USD."
), names(uqr))

if ("University.Ranking.Score" %in% names(uqr)) {
  for (v in pairs_to_plot) {
    p <- ggplot(uqr, aes_string(x = v, y = "University.Ranking.Score")) +
      geom_point(alpha = .7) +
      geom_smooth(method = "lm", se = TRUE) +
      labs(title = paste("Scatterplot:", v, "vs University Ranking Score"),
           x = v, y = "University Ranking Score")
    print(p)  # <-- shows in IDE Plots pane
    ggsave(filename = file.path(out_dir, sprintf("04_scatter_%s_vs_URS.png", v)),
           plot = p, width = 7, height = 5, dpi = 150)
  }
}

sink_to(file.path(out_dir, "05_simple_regressions.txt"), {
  cat("=== Simple Linear Regressions (URS ~ predictor) ===\n")
  for (v in setdiff(num_cols, "University.Ranking.Score")) {
    f <- as.formula(paste("University.Ranking.Score ~", v))
    m <- lm(f, data = uqr)
    cat("\nModel:", deparse(f), "\n")
    print(summary(m))
  }
})

if (all(c("University.Ranking.Score","Faculty.Salary..Avg.") %in% names(uqr))) {
  m_fac <- lm(University.Ranking.Score ~ Faculty.Salary..Avg., data = uqr)
  # Print summary to Console AND save separately
  cat("\n=== Simple LM: URS ~ Faculty.Salary..Avg. ===\n"); print(summary(m_fac))
  sink_to(file.path(out_dir, "05b_simple_regression_Faculty_Salary.txt"), {
    cat("=== Simple Linear Regression: URS ~ Faculty.Salary..Avg. ===\n"); print(summary(m_fac))
  })
  p_fac <- ggplot(uqr, aes(Faculty.Salary..Avg., University.Ranking.Score)) +
    geom_point(alpha = .8) +
    geom_smooth(method = "lm", se = TRUE) +
    labs(title = "Faculty Salary vs University Ranking Score",
         x = "Faculty Salary (Avg.)", y = "University Ranking Score")
  print(p_fac)  # <-- IDE
  ggsave(file.path(out_dir, "04_scatter_Faculty.Salary..Avg..._vs_URS.png"),
         plot = p_fac, width = 7, height = 5, dpi = 150)
}

full_predictors <- intersect(c(
  "Graduation.Rate....","Employment.Rate....","Research.Funding..Million.USD.",
  "Faculty.Salary..Avg.", "Student.Faculty.Ratio","Student.Enrollment","Tuition.Fees..USD."
), names(uqr))

if ("University.Ranking.Score" %in% names(uqr) && length(full_predictors) > 0) {
  full_formula <- as.formula(paste("University.Ranking.Score ~", paste(full_predictors, collapse = " + ")))
  mlr <- lm(full_formula, data = uqr)
  
  print(summary(mlr))
  
  sink_to(file.path(out_dir, "06_multiple_regression_summary.txt"), {
    cat("=== Multiple Linear Regression (Full) ===\n"); print(summary(mlr))
    cat("\n=== ANOVA ===\n"); print(anova(mlr))
    cat("\n=== 95% Confidence Intervals ===\n"); print(confint(mlr))
  })
  
  plot(mlr, which = 1)  # Residuals vs Fitted
  dev.copy(png, file.path(out_dir, "07_diagnostics_1_residuals_vs_fitted.png"), width = 800, height = 600, res = 120); dev.off()
  plot(mlr, which = 2)  # Q-Q
  dev.copy(png, file.path(out_dir, "07_diagnostics_2_qq.png"), width = 800, height = 600, res = 120); dev.off()
  plot(mlr, which = 3)  # Scale-Location
  dev.copy(png, file.path(out_dir, "07_diagnostics_3_scale_location.png"), width = 800, height = 600, res = 120); dev.off()
  plot(mlr, which = 4)  # Cook's distance
  dev.copy(png, file.path(out_dir, "07_diagnostics_4_cooks.png"), width = 800, height = 600, res = 120); dev.off()
  
  write.csv(as.data.frame(vif(mlr)), file.path(out_dir, "08_vif.csv"))
  bp <- bptest(mlr)
  sink_to(file.path(out_dir, "09_heteroscedasticity_bptest.txt"), { print(bp) })
}

if (all(c("Institution.Type","University.Ranking.Score") %in% names(uqr))) {
  p_box <- ggplot(uqr, aes(x = Institution.Type, y = University.Ranking.Score, fill = Institution.Type)) +
    geom_boxplot(alpha = .6, show.legend = FALSE) +
    labs(title = "University Ranking Score by Institution Type", x = "Institution Type", y = "University Ranking Score")
  print(p_box)
  ggsave(file.path(out_dir, "10_boxplot_URS_by_InstitutionType.png"), plot = p_box, width = 9, height = 6, dpi = 130)
  
  sink_to(file.path(out_dir, "11_ttest_URS_by_InstitutionType.txt"), {
    cat("=== Welch Two Sample t-test: URS by Institution Type ===\n")
    print(t.test(University.Ranking.Score ~ Institution.Type, data = uqr))
  })
}

need_cols <- c("University.Ranking.Score","Graduation.Rate....","Employment.Rate....",
               "Research.Funding..Million.USD.","Faculty.Salary..Avg.","Student.Faculty.Ratio",
               "Student.Enrollment","Tuition.Fees..USD.")
if (all(need_cols %in% names(uqr))) {
  uqr_std <- within(uqr, {
    z_URS <- scale(University.Ranking.Score)
    z_Grad <- scale(Graduation.Rate....)
    z_Emp  <- scale(Employment.Rate....)
    z_Res  <- scale(Research.Funding..Million.USD.)
    z_Sal  <- scale(Faculty.Salary..Avg.)          # <-- Faculty Salary included
    z_SFR  <- scale(Student.Faculty.Ratio)
    z_Enroll <- scale(Student.Enrollment)
    z_Tuit   <- scale(Tuition.Fees..USD.)
  })
  std_mlr <- lm(z_URS ~ z_Grad + z_Emp + z_Res + z_Sal + z_SFR + z_Enroll + z_Tuit, data = uqr_std)
  print(summary(std_mlr))  # quick look in console
  sink_to(file.path(out_dir, "12_standardized_coefficients.txt"), {
    cat("=== Standardized Coefficients (Beta weights) ===\n")
    print(summary(std_mlr))
  })
}

sink_to(file.path(out_dir, "99_sessionInfo.txt"), { print(sessionInfo()) })

message("All outputs saved to: ", normalizePath(out_dir))