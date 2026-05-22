# ==============================================================================
# 脚本9.2【临床科研终极定稿·全CKM统一版】
# 1. 有序Logistic：CKM分期全对比校准(1vs2/2vs3/3vs4a/4avs4b)
# 2. Cox模型：按CKM分期分层+1/3/5年校准
# 3. 校准数据缓存RDS | 图片尺寸独立修改 | 极速运行
# 4. 全局严格统一：全部CKM 无任何CKD字样
# ==============================================================================
rm(list=ls())
gc()
options(scipen=999)
set.seed(123)

# 加载包
library(tidyverse)
library(survival)
library(rms)
library(gridExtra)
library(dcurves)
library(patchwork)

# ===================== 基础配置 =====================
OUT_DIR <- "9_Nomogram_Analysis"
if(!dir.exists(OUT_DIR)) dir.create(OUT_DIR,recursive = T)
load(file.path(OUT_DIR, "9_Precompute_Results.RDS"))
train_data <- as.data.frame(train_data)

# 强制定义CKM分期为有序分类（核心：有序Logistic前提）
stage_levels <- c("1","2","3","4a","4b")
train_data$CKM_stage_ab <- factor(train_data$CKM_stage_ab, 
                                  levels = stage_levels, 
                                  ordered = TRUE)
dd <- datadist(train_data)
options(datadist = "dd")

# 固定参数
INPUT_KEEP <- 4
BOOT_TIMES <- 200  # 校准加速
RDS_PATH <- file.path(OUT_DIR, "full_calibration_data.rds")
# CKM分期定义
CKM_STAGES <- c("1","2","3","4a","4b")
# 有序Logistic对比组
ORD_PAIRS <- list(c("1","2"), c("2","3"), c("3","4a"), c("4a","4b"))

# ==============================================================================
# 1. 完整趋势图（去除1~7变量）| 尺寸独立修改
# ==============================================================================
# 图片尺寸
trend_w <- 3200; trend_h <- 1600; trend_res <- 300
png(file.path(OUT_DIR, "Variable_Selection_Trend.png"), trend_w, trend_h, res=trend_res)

plot_data <- result_df %>% mutate(remove_n = factor(remove_n, levels = sort(unique(remove_n))))
summary_data <- result_df %>% group_by(remove_n) %>% summarise(
  med_log = median(Log_AUC), med_cox = median(Cox_C), .groups = "drop"
)

p1 <- ggplot(plot_data, aes(x=remove_n, y=Log_AUC)) + geom_boxplot(fill="#0072B2") +
  labs(title="AUC Distribution", x="Removed Variables", y="AUC") + theme_bw() + theme(plot.title=element_text(hjust=0.5,face="bold"))
p2 <- ggplot(plot_data, aes(x=remove_n, y=Cox_C)) + geom_boxplot(fill="#D55E00") +
  labs(title="C-index Distribution", x="Removed Variables", y="C-index") + theme_bw() + theme(plot.title=element_text(hjust=0.5,face="bold"))
p3 <- ggplot(summary_data, aes(x=remove_n, y=med_log, group=1)) + geom_line(linewidth=2, color="#0072B2") + geom_point(size=4) +
  labs(title="Median AUC Trend", x="Removed Variables", y="Median AUC") + theme_bw() + theme(plot.title=element_text(hjust=0.5,face="bold"))
p4 <- ggplot(summary_data, aes(x=remove_n, y=med_cox, group=1)) + geom_line(linewidth=2, color="#D55E00") + geom_point(size=4) +
  labs(title="Median C-index Trend", x="Removed Variables", y="Median C-index") + theme_bw() + theme(plot.title=element_text(hjust=0.5,face="bold"))

grid.arrange(p1, p2, p3, p4, ncol=2)
dev.off()

# ==============================================================================
# 2. 最优7变量 + 热图（4位小数+变量图注）| 尺寸独立修改
# ==============================================================================
result_df_7 <- result_df %>% filter(keep_n == INPUT_KEEP)
best_var <- result_df_7 %>% filter(!is.na(total_score)) %>% arrange(desc(total_score)) %>% slice(1)
FINAL_VARS <- strsplit(best_var$vars, ",")[[1]]
best_caption <- paste("Best Variables:", paste(FINAL_VARS, collapse = " + "))

# 图片尺寸
heat_w <- 3500; heat_h <- 5500; heat_res <- 300
png(file.path(OUT_DIR, "Model_Performance_Heatmap.png"), heat_w, heat_h, res=heat_res)

heat_data <- result_df_7 %>% filter(!is.na(total_score)) %>% arrange(desc(total_score)) %>% mutate(group_label = factor(paste0("Model_", row_number())))
heat_plot <- heat_data %>% select(group_label, Log_AUC, Cox_C, total_score) %>% pivot_longer(cols=-group_label, names_to="Metric", values_to="Score")

ggplot(heat_plot, aes(x=Metric, y=group_label, fill=Score)) +
  geom_tile(color="white") + geom_text(aes(label=sprintf("%.4f", Score)), fontface="bold") +
  scale_fill_gradient(low="#F8F9FA", high="#DC3545") +
  labs(title="4-Variable Model Performance", y="Model Rank", caption=best_caption) +
  theme_bw() + theme(plot.title=element_text(hjust=0.5,face="bold"), plot.caption=element_text(hjust=0.5))
dev.off()

# ==============================================================================
# 3. 模型构建（有序Logistic + Cox）
# ==============================================================================
# 有序Logistic回归（CKM分期预测）
final_ord <- lrm(as.formula(paste("CKM_stage_ab ~", paste(FINAL_VARS, collapse="+"))), 
                 data=train_data, x=T, y=T)
# Cox回归（生存预测）
cox_form <- as.formula(paste("Surv(Time, Status) ~", paste(FINAL_VARS, collapse="+")))
final_cox <- cph(cox_form, data=train_data, x=T, y=T, surv=T)

# ==============================================================================
# 4. 列线图（官方标准写法）| 尺寸独立修改
# ==============================================================================
# 有序Logistic列线图
nom_ord <- nomogram(final_ord, maxscale=100)
nom_w <- 4500; nom_h <- 2500; nom_res <- 300
png(file.path(OUT_DIR, "Ordinal_Logistic_Nomogram.png"), nom_w, nom_h, res=nom_res)
plot(nom_ord)
title(main="CKM Stage Prediction Nomogram", cex.main=2.5)
dev.off()

# Cox列线图
surv_func <- Survival(final_cox) 
nom_cox <- nomogram(final_cox, fun=list(function(x)surv_func(365,x), function(x)surv_func(1095,x), function(x)surv_func(1825,x)), 
                    lp=T, funlabel=c('1-Year','3-Year','5-Year Survival'), maxscale=100)
png(file.path(OUT_DIR, "Cox_Nomogram.png"), nom_w, nom_h, res=nom_res)
plot(nom_cox, lplabel="Linear Predictor", xfrac=0.2, tcl=-0.2, lmgp=0.1)
title(main="Survival Prediction Nomogram", cex.main=2.5)
dev.off()

# ==============================================================================
# 5. ✅ 校准数据预计算 + 保存RDS（仅计算1次，永久免算）
# ==============================================================================
if (!file.exists(RDS_PATH)) {
  cat("正在计算全套校准数据（仅首次运行）...\n")
  cal_list <- list()
  
  # -------- 5.1 有序Logistic：全CKM分期对校准(1vs2/2vs3/3vs4a/4avs4b) --------
  cal_list$ordinal <- lapply(ORD_PAIRS, function(pair){
    sub_dat <- train_data[train_data$CKM_stage_ab %in% pair, ]
    sub_dat$CKM_stage_ab <- droplevels(sub_dat$CKM_stage_ab)
    calibrate(lrm(as.formula(paste("CKM_stage_ab ~", paste(FINAL_VARS, collapse="+"))), 
                  data=sub_dat, x=T, y=T), B=BOOT_TIMES)
  })
  names(cal_list$ordinal) <- paste(sapply(ORD_PAIRS, `[`, 1), 
                                   sapply(ORD_PAIRS, `[`, 2), sep="vs")
  
  # -------- 5.2 Cox：按CKM分期分层 + 1/3/5年校准 --------
  cal_list$cox_strat <- lapply(CKM_STAGES, function(stage){
    sub_dat <- train_data[train_data$CKM_stage_ab == stage, ]
    cat(paste0("CKM ",stage," stage sample size: ",nrow(sub_dat),"\n"))
    list(
      y1 = calibrate(final_cox, B=BOOT_TIMES, u=365, data=sub_dat),
      y3 = calibrate(final_cox, B=BOOT_TIMES, u=1095, data=sub_dat),
      y5 = calibrate(final_cox, B=BOOT_TIMES, u=1825, data=sub_dat)
    )
  })
  names(cal_list$cox_strat) <- paste0("CKM_", CKM_STAGES)
  
  # 保存所有校准数据
  saveRDS(cal_list, RDS_PATH)
  cat("校准数据保存完成：", RDS_PATH, "\n")
}else{
  cal_list <- readRDS(RDS_PATH)
  cat("直接加载缓存校准数据，跳过自举计算\n")
}

# 输出最终纳入变量
cat("\n=============================================\n")
cat("Final Included 7 Core Variables:\n")
print(FINAL_VARS)
cat("=============================================\n")


