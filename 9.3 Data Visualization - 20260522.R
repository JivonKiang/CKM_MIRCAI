# ======================================
# 0. 彻底清理R环境
# ======================================
rm(list=ls())
gc()
cat("✅ 已彻底清理R环境，释放内存\n")

# ======================================
# 第一步：加载所有必需包
# ======================================
required_packages <- c("ggplot2", "patchwork", "dplyr", "writexl", "readr", "stringr")
missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}
invisible(lapply(required_packages, library, character.only = TRUE))
cat("✅ 已加载所有必需包\n")

# ======================================
# 第二步：读取你的校准结果数据
# ======================================
OUT_DIR <- "9_Nomogram_Analysis"
if(!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)
calibration_rds <- file.path(OUT_DIR, "full_calibration_data.rds")
cal_list <- readRDS(calibration_rds)
cat("✅ 已加载校准结果数据\n")

# ======================================
# 第三步：预加载所有数据（格式问题已解决）
# ======================================
# 3.1 预加载有序Logistic所有分组数据
cat("✅ 正在预加载有序Logistic校准数据...\n")
ord_data_list <- list()
for (group_name in names(cal_list$ordinal)) {
  cal_obj <- cal_list$ordinal[[group_name]]
  # 格式问题解决：先unclass去掉特殊类，再转data.frame
  cal_df <- data.frame(unclass(cal_obj))
  ord_data_list[[group_name]] <- cal_df
  cat(paste0("  已预加载：", group_name, " 数据，列名：", paste(colnames(cal_df), collapse = " | "), "\n"))
}

# 3.2 预加载Cox所有分期+时间点数据
cat("✅ 正在预加载Cox分层校准数据...\n")
cox_data_list <- list()
time_label_map <- list(y1 = "1-Year Survival", y3 = "3-Year Survival", y5 = "5-Year Survival")
for (stage_name in names(cal_list$cox_strat)) {
  cox_data_list[[stage_name]] <- list()
  for (time_key in names(time_label_map)) {
    cal_obj <- cal_list$cox_strat[[stage_name]][[time_key]]
    # 格式问题解决：先unclass去掉特殊类，再转data.frame
    cal_df <- data.frame(unclass(cal_obj))
    cox_data_list[[stage_name]][[time_key]] <- cal_df
    cat(paste0("  已预加载：", stage_name, " - ", time_label_map[[time_key]], " 数据，列名：", paste(colnames(cal_df), collapse = " | "), "\n"))
  }
}
cat("✅ 所有数据预加载完成，列名已校验\n")

# ======================================
# 第四步：全局绘图配置（SCI期刊标准配色）
# ======================================
theme_set(theme_bw(base_family = "sans"))
# 颜色定义：黑色对角线，整体校准线蓝色，训练集橙色，验证集红色
COLOR_DIAG <- "#000000"      # 理想校准对角线（黑色虚线）
COLOR_OVERALL <- "#2E86AB"   # 整体校准线（蓝色）
COLOR_TRAIN <- "#F18F01"    # 训练集（橙色）
COLOR_VALID <- "#C70039"     # 验证集（红色）

# 图片尺寸调整
cal_ord_w <- 3200; cal_ord_h <- 2400; cal_ord_res <- 300
cal_ord_resid_w <- 3600; cal_ord_resid_h <- 2400; cal_ord_resid_res <- 300
cal_cox_resid_w <- 4200; cal_cox_resid_h <- 3800; cal_cox_resid_res <- 300

# ======================================
# 模块1：有序Logistic整体校准曲线（单独一张图）
# ======================================
ORDINAL_X <- "predy"
ORDINAL_Y_OVERALL <- "calibrated.orig"

# 逐个生成plot
p1 <- ggplot(ord_data_list[["1vs2"]], aes(x = predy, y = calibrated.orig)) +
  geom_line(color = COLOR_OVERALL, linewidth = 1.5) +
  geom_abline(slope = 1, intercept = 0, color = COLOR_DIAG, linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "Calibration: CKM 1 vs 2",
    x = "Predicted Probability",
    y = "Observed Probability"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  xlim(0, 1) + ylim(0, 1)

p2 <- ggplot(ord_data_list[["2vs3"]], aes(x = predy, y = calibrated.orig)) +
  geom_line(color = COLOR_OVERALL, linewidth = 1.5) +
  geom_abline(slope = 1, intercept = 0, color = COLOR_DIAG, linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "Calibration: CKM 2 vs 3",
    x = "Predicted Probability",
    y = "Observed Probability"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  xlim(0, 1) + ylim(0, 1)

p3 <- ggplot(ord_data_list[["3vs4a"]], aes(x = predy, y = calibrated.orig)) +
  geom_line(color = COLOR_OVERALL, linewidth = 1.5) +
  geom_abline(slope = 1, intercept = 0, color = COLOR_DIAG, linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "Calibration: CKM 3 vs 4a",
    x = "Predicted Probability",
    y = "Observed Probability"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  xlim(0, 1) + ylim(0, 1)

p4 <- ggplot(ord_data_list[["4avs4b"]], aes(x = predy, y = calibrated.orig)) +
  geom_line(color = COLOR_OVERALL, linewidth = 1.5) +
  geom_abline(slope = 1, intercept = 0, color = COLOR_DIAG, linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "Calibration: CKM 4a vs 4b",
    x = "Predicted Probability",
    y = "Observed Probability"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  xlim(0, 1) + ylim(0, 1)

# 组合2×2图
ord_overall_plot <- (p1 + p2) / (p3 + p4)

# 保存图片
png(file.path(OUT_DIR, "1_Ordinal_Logistic_Overall_Calibration.png"), 
    width = cal_ord_w, height = cal_ord_h, res = cal_ord_res)
print(ord_overall_plot)
dev.off()
cat("✅ 有序Logistic整体校准曲线已保存至：", file.path(OUT_DIR, "1_Ordinal_Logistic_Overall_Calibration.png"), "\n")

# ======================================
# 模块2：有序Logistic校准残差图（修复版：图例强制在底部生效）
# ======================================
ORDINAL_X <- "predy"
ORDINAL_Y_TRAIN <- "training"
ORDINAL_Y_VALID <- "test"

# 逐个生成plot：仅第一个子图保留图例，其他子图隐藏图例（核心修复1）
p_r1 <- ggplot(ord_data_list[["1vs2"]], aes(x = predy)) +
  geom_line(aes(y = training, color = "Training"), linewidth = 1.5, alpha = 0.9) +
  geom_line(aes(y = test, color = "Validation"), linewidth = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, color = COLOR_DIAG, linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "Calibration Residual: CKM 1 vs 2",
    x = "Predicted Probability",
    y = "Residual",
    color = "Dataset"
  ) +
  scale_color_manual(values = c("Training" = COLOR_TRAIN, "Validation" = COLOR_VALID)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.box = "horizontal"
  ) +
  xlim(0, 1) + ylim(-0.1, 0.1)

# 其他子图隐藏图例
p_r2 <- ggplot(ord_data_list[["2vs3"]], aes(x = predy)) +
  geom_line(aes(y = training, color = "Training"), linewidth = 1.5, alpha = 0.9) +
  geom_line(aes(y = test, color = "Validation"), linewidth = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, color = COLOR_DIAG, linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "Calibration Residual: CKM 2 vs 3",
    x = "Predicted Probability",
    y = "Residual"
  ) +
  scale_color_manual(values = c("Training" = COLOR_TRAIN, "Validation" = COLOR_VALID)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "none" # 隐藏图例
  ) +
  xlim(0, 1) + ylim(-0.1, 0.1)

p_r3 <- ggplot(ord_data_list[["3vs4a"]], aes(x = predy)) +
  geom_line(aes(y = training, color = "Training"), linewidth = 1.5, alpha = 0.9) +
  geom_line(aes(y = test, color = "Validation"), linewidth = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, color = COLOR_DIAG, linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "Calibration Residual: CKM 3 vs 4a",
    x = "Predicted Probability",
    y = "Residual"
  ) +
  scale_color_manual(values = c("Training" = COLOR_TRAIN, "Validation" = COLOR_VALID)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "none" # 隐藏图例
  ) +
  xlim(0, 1) + ylim(-0.1, 0.1)

p_r4 <- ggplot(ord_data_list[["4avs4b"]], aes(x = predy)) +
  geom_line(aes(y = training, color = "Training"), linewidth = 1.5, alpha = 0.9) +
  geom_line(aes(y = test, color = "Validation"), linewidth = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, color = COLOR_DIAG, linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "Calibration Residual: CKM 4a vs 4b",
    x = "Predicted Probability",
    y = "Residual"
  ) +
  scale_color_manual(values = c("Training" = COLOR_TRAIN, "Validation" = COLOR_VALID)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "none" # 隐藏图例
  ) +
  xlim(0, 1) + ylim(-0.1, 0.1)

# 组合2×2图，合并图例，在整体图上强制图例在底部（核心修复2）
ord_resid_plot <- (p_r1 + p_r2) / (p_r3 + p_r4) + 
  plot_layout(guides = "collect") + # 合并图例
  theme(legend.position = "bottom") + # 整体图强制图例在底部
  labs(caption = "Note: Residual = Observed - Predicted. Residual close to 0 indicates good calibration performance") +
  theme(plot.caption = element_text(hjust = 0.5, size = 10, face = "italic"))

# 保存图片
png(file.path(OUT_DIR, "2_Ordinal_Logistic_Calibration_Residual.png"), 
    width = cal_ord_resid_w, height = cal_ord_resid_h, res = cal_ord_resid_res)
print(ord_resid_plot)
dev.off()
cat("✅ 有序Logistic校准残差图已保存至：", file.path(OUT_DIR, "2_Ordinal_Logistic_Calibration_Residual.png"), "\n")

# ======================================
# 模块3：Cox校准残差图（修复版：图例强制在底部生效）
# ======================================
COX_X <- "calibrated"
COX_Y_TRAIN <- "training"
COX_Y_VALID <- "test"

# 逐个生成所有Cox plot：仅第一个子图保留图例，其他子图隐藏图例
# CKM_1 分期
p_c1_y1 <- ggplot(cox_data_list[["CKM_1"]][["y1"]], aes(x = calibrated)) +
  geom_line(aes(y = training, color = "Training"), linewidth = 1.5, alpha = 0.9) +
  geom_line(aes(y = test, color = "Validation"), linewidth = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, color = COLOR_DIAG, linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "CKM_1 - 1-Year Survival Calibration",
    x = "Predicted Survival Probability",
    y = "Residual",
    color = "Dataset"
  ) +
  scale_color_manual(values = c("Training" = COLOR_TRAIN, "Validation" = COLOR_VALID)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.box = "horizontal"
  )

# 其他子图隐藏图例
p_c1_y3 <- ggplot(cox_data_list[["CKM_1"]][["y3"]], aes(x = calibrated)) +
  geom_line(aes(y = training, color = "Training"), linewidth = 1.5, alpha = 0.9) +
  geom_line(aes(y = test, color = "Validation"), linewidth = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, color = COLOR_DIAG, linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "CKM_1 - 3-Year Survival Calibration",
    x = "Predicted Survival Probability",
    y = "Residual"
  ) +
  scale_color_manual(values = c("Training" = COLOR_TRAIN, "Validation" = COLOR_VALID)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    legend.position = "none" # 隐藏图例
  )

p_c1_y5 <- ggplot(cox_data_list[["CKM_1"]][["y5"]], aes(x = calibrated)) +
  geom_line(aes(y = training, color = "Training"), linewidth = 1.5, alpha = 0.9) +
  geom_line(aes(y = test, color = "Validation"), linewidth = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, color = COLOR_DIAG, linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "CKM_1 - 5-Year Survival Calibration",
    x = "Predicted Survival Probability",
    y = "Residual"
  ) +
  scale_color_manual(values = c("Training" = COLOR_TRAIN, "Validation" = COLOR_VALID)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    legend.position = "none" # 隐藏图例
  )

# CKM_2 分期
p_c2_y1 <- ggplot(cox_data_list[["CKM_2"]][["y1"]], aes(x = calibrated)) +
  geom_line(aes(y = training, color = "Training"), linewidth = 1.5, alpha = 0.9) +
  geom_line(aes(y = test, color = "Validation"), linewidth = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, color = COLOR_DIAG, linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "CKM_2 - 1-Year Survival Calibration",
    x = "Predicted Survival Probability",
    y = "Residual"
  ) +
  scale_color_manual(values = c("Training" = COLOR_TRAIN, "Validation" = COLOR_VALID)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    legend.position = "none" # 隐藏图例
  )

p_c2_y3 <- ggplot(cox_data_list[["CKM_2"]][["y3"]], aes(x = calibrated)) +
  geom_line(aes(y = training, color = "Training"), linewidth = 1.5, alpha = 0.9) +
  geom_line(aes(y = test, color = "Validation"), linewidth = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, color = COLOR_DIAG, linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "CKM_2 - 3-Year Survival Calibration",
    x = "Predicted Survival Probability",
    y = "Residual"
  ) +
  scale_color_manual(values = c("Training" = COLOR_TRAIN, "Validation" = COLOR_VALID)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    legend.position = "none" # 隐藏图例
  )

p_c2_y5 <- ggplot(cox_data_list[["CKM_2"]][["y5"]], aes(x = calibrated)) +
  geom_line(aes(y = training, color = "Training"), linewidth = 1.5, alpha = 0.9) +
  geom_line(aes(y = test, color = "Validation"), linewidth = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, color = COLOR_DIAG, linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "CKM_2 - 5-Year Survival Calibration",
    x = "Predicted Survival Probability",
    y = "Residual"
  ) +
  scale_color_manual(values = c("Training" = COLOR_TRAIN, "Validation" = COLOR_VALID)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    legend.position = "none" # 隐藏图例
  )

# CKM_3 分期
p_c3_y1 <- ggplot(cox_data_list[["CKM_3"]][["y1"]], aes(x = calibrated)) +
  geom_line(aes(y = training, color = "Training"), linewidth = 1.5, alpha = 0.9) +
  geom_line(aes(y = test, color = "Validation"), linewidth = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, color = COLOR_DIAG, linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "CKM_3 - 1-Year Survival Calibration",
    x = "Predicted Survival Probability",
    y = "Residual"
  ) +
  scale_color_manual(values = c("Training" = COLOR_TRAIN, "Validation" = COLOR_VALID)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    legend.position = "none" # 隐藏图例
  )

p_c3_y3 <- ggplot(cox_data_list[["CKM_3"]][["y3"]], aes(x = calibrated)) +
  geom_line(aes(y = training, color = "Training"), linewidth = 1.5, alpha = 0.9) +
  geom_line(aes(y = test, color = "Validation"), linewidth = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, color = COLOR_DIAG, linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "CKM_3 - 3-Year Survival Calibration",
    x = "Predicted Survival Probability",
    y = "Residual"
  ) +
  scale_color_manual(values = c("Training" = COLOR_TRAIN, "Validation" = COLOR_VALID)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    legend.position = "none" # 隐藏图例
  )

p_c3_y5 <- ggplot(cox_data_list[["CKM_3"]][["y5"]], aes(x = calibrated)) +
  geom_line(aes(y = training, color = "Training"), linewidth = 1.5, alpha = 0.9) +
  geom_line(aes(y = test, color = "Validation"), linewidth = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, color = COLOR_DIAG, linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "CKM_3 - 5-Year Survival Calibration",
    x = "Predicted Survival Probability",
    y = "Residual"
  ) +
  scale_color_manual(values = c("Training" = COLOR_TRAIN, "Validation" = COLOR_VALID)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    legend.position = "none" # 隐藏图例
  )

# CKM_4a 分期
p_c4a_y1 <- ggplot(cox_data_list[["CKM_4a"]][["y1"]], aes(x = calibrated)) +
  geom_line(aes(y = training, color = "Training"), linewidth = 1.5, alpha = 0.9) +
  geom_line(aes(y = test, color = "Validation"), linewidth = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, color = COLOR_DIAG, linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "CKM_4a - 1-Year Survival Calibration",
    x = "Predicted Survival Probability",
    y = "Residual"
  ) +
  scale_color_manual(values = c("Training" = COLOR_TRAIN, "Validation" = COLOR_VALID)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    legend.position = "none" # 隐藏图例
  )

p_c4a_y3 <- ggplot(cox_data_list[["CKM_4a"]][["y3"]], aes(x = calibrated)) +
  geom_line(aes(y = training, color = "Training"), linewidth = 1.5, alpha = 0.9) +
  geom_line(aes(y = test, color = "Validation"), linewidth = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, color = COLOR_DIAG, linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "CKM_4a - 3-Year Survival Calibration",
    x = "Predicted Survival Probability",
    y = "Residual"
  ) +
  scale_color_manual(values = c("Training" = COLOR_TRAIN, "Validation" = COLOR_VALID)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    legend.position = "none" # 隐藏图例
  )

p_c4a_y5 <- ggplot(cox_data_list[["CKM_4a"]][["y5"]], aes(x = calibrated)) +
  geom_line(aes(y = training, color = "Training"), linewidth = 1.5, alpha = 0.9) +
  geom_line(aes(y = test, color = "Validation"), linewidth = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, color = COLOR_DIAG, linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "CKM_4a - 5-Year Survival Calibration",
    x = "Predicted Survival Probability",
    y = "Residual"
  ) +
  scale_color_manual(values = c("Training" = COLOR_TRAIN, "Validation" = COLOR_VALID)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    legend.position = "none" # 隐藏图例
  )

# CKM_4b 分期
p_c4b_y1 <- ggplot(cox_data_list[["CKM_4b"]][["y1"]], aes(x = calibrated)) +
  geom_line(aes(y = training, color = "Training"), linewidth = 1.5, alpha = 0.9) +
  geom_line(aes(y = test, color = "Validation"), linewidth = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, color = COLOR_DIAG, linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "CKM_4b - 1-Year Survival Calibration",
    x = "Predicted Survival Probability",
    y = "Residual"
  ) +
  scale_color_manual(values = c("Training" = COLOR_TRAIN, "Validation" = COLOR_VALID)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    legend.position = "none" # 隐藏图例
  )

p_c4b_y3 <- ggplot(cox_data_list[["CKM_4b"]][["y3"]], aes(x = calibrated)) +
  geom_line(aes(y = training, color = "Training"), linewidth = 1.5, alpha = 0.9) +
  geom_line(aes(y = test, color = "Validation"), linewidth = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, color = COLOR_DIAG, linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "CKM_4b - 3-Year Survival Calibration",
    x = "Predicted Survival Probability",
    y = "Residual"
  ) +
  scale_color_manual(values = c("Training" = COLOR_TRAIN, "Validation" = COLOR_VALID)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    legend.position = "none" # 隐藏图例
  )

p_c4b_y5 <- ggplot(cox_data_list[["CKM_4b"]][["y5"]], aes(x = calibrated)) +
  geom_line(aes(y = training, color = "Training"), linewidth = 1.5, alpha = 0.9) +
  geom_line(aes(y = test, color = "Validation"), linewidth = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, color = COLOR_DIAG, linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "CKM_4b - 5-Year Survival Calibration",
    x = "Predicted Survival Probability",
    y = "Residual"
  ) +
  scale_color_manual(values = c("Training" = COLOR_TRAIN, "Validation" = COLOR_VALID)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    legend.position = "none" # 隐藏图例
  )

# 把所有plot存入列表
cox_plots <- list(
  p_c1_y1, p_c1_y3, p_c1_y5,
  p_c2_y1, p_c2_y3, p_c2_y5,
  p_c3_y1, p_c3_y3, p_c3_y5,
  p_c4a_y1, p_c4a_y3, p_c4a_y5,
  p_c4b_y1, p_c4b_y3, p_c4b_y5
)

# 用patchwork组合5×3图，合并所有子图的图例，在整体图上强制图例在底部（核心修复3）
cox_final_plot <- (p_c1_y1 + p_c1_y3 + p_c1_y5) / 
  (p_c2_y1 + p_c2_y3 + p_c2_y5) / 
  (p_c3_y1 + p_c3_y3 + p_c3_y5) / 
  (p_c4a_y1 + p_c4a_y3 + p_c4a_y5) / 
  (p_c4b_y1 + p_c4b_y3 + p_c4b_y5) +
  plot_layout(guides = "collect") + # 合并所有图例
  theme(legend.position = "bottom") + # 整体图强制图例在底部
  labs(caption = "Note: Residual = Observed - Predicted. Residual close to 0 indicates good calibration performance") +
  theme(plot.caption = element_text(hjust = 0.5, size = 10, face = "italic"))

# 保存图片
png(file.path(OUT_DIR, "3_Cox_Calibration_Residual.png"), 
    width = cal_cox_resid_w, height = cal_cox_resid_h, res = cal_cox_resid_res)
print(cox_final_plot)
dev.off()
cat("✅ Cox校准残差图已保存至：", file.path(OUT_DIR, "3_Cox_Calibration_Residual.png"), "\n")

# ======================================
# 最终完成提示
# ======================================
cat("\n🎉 所有任务完成！生成的文件均保存在", OUT_DIR, "目录下：\n")
cat("1. 有序Logistic整体校准曲线：1_Ordinal_Logistic_Overall_Calibration.png\n")
cat("2. 有序Logistic校准残差图：2_Ordinal_Logistic_Calibration_Residual.png\n")
cat("3. Cox校准残差图：3_Cox_Calibration_Residual.png\n")