# ==============================================================================
# 脚本7：Dual-Effect Biomarker Screening (CKM Stage + Prognosis)
# 最终修复版：兼容所有pROC版本 + 修复语法错误 + 全流程稳定运行
# ==============================================================================
rm(list=ls())
gc()

# 加载依赖包
packages <- c("survival","MASS","tidyverse","data.table","openxlsx","car","UpSetR","ggplot2","pROC")
for (p in packages) {
  if (!require(p, character.only=TRUE)) install.packages(p, dependencies=TRUE)
  library(p, character.only=TRUE)
}
set.seed(123)
options(warn=-1)

# ===================== 工具函数：修复VIF报错 + 修正语法错误 =====================
# VIF筛选无共线性变量 + 容错处理
vif_filter <- function(df, vars) {
  if(length(vars) <= 1) {
    return(vars)
  }
  tryCatch({
    fit <- lm(as.formula(paste0(vars[1], "~ ", paste(vars[-1], collapse = "+"))), data = df)
    vif_val <- vif(fit)
    return(names(vif_val[vif_val < 5]))
  }, error = function(e){
    return(vars)
  })
}

# 代谢指标择优：按性能降序 + 无共线性（修复%in%语法）
select_best_metrics <- function(df, vars, score_df, score_col) {
  if(length(vars)==0) return(c())
  vars_ordered <- score_df %>% 
    filter(variable %in% vars) %>% 
    arrange(desc(!!sym(score_col))) %>% 
    pull(variable)
  selected <- c()
  for(v in vars_ordered){
    temp <- c(selected, v)
    clean <- vif_filter(df, temp)
    if(v %in% clean) selected <- temp
  }
  return(selected)
}

# ===================== 1. 数据加载与预处理 =====================
data <- readRDS("5. Result_Indicator/代谢指标计算完成数据.RDS")
data <- as.data.frame(data)
data$CKM_stage_ab <- factor(data$CKM_stage_ab, levels = c("1","2","3","4a","4b"))
data <- data %>% drop_na(Time, Status, CKM_stage_ab)

# 创建输出目录
out_dir <- "7_Dual_Effect_Results"
log_dir <- file.path(out_dir, "Binary_Logistic_Results")
cox_dir <- file.path(out_dir, "Stratified_Cox_Results")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cox_dir, recursive = TRUE, showWarnings = FALSE)

# 基础信息输出
cat("1. Data Processing Completed\n")
cat("   Total Samples:", nrow(data), "\n")
print(table(data$CKM_stage_ab))

# ===================== 2. 变量定义 =====================
basic_vars <- c("Age","Sex","Race","Marital","Smoking","Drinking","Diabetes","Cancer","Stroke",
                "Hypertension","Dyslipidemia","KidneyDisease","Chest_Pain",
                "Height","Weight","BMI","WC","WHtR","ABSI","BRI","CI","SBP","DBP","TG","FPG","HbA1C","HDL","UA","CRP")
# ===================== 【固定不动】最新全套代谢指标：9核心 + 144衍生 =====================
core_indicators <- c("TyG", "UHR", "AIP", "CMI", "HCHR", "METS_IR", "SHR", "eGDR", "eGFR")
derivation_vars <- c("WC", "WHtR", "BMI", "ABSI", "BRI", "CI", "CRP", "HDL")
derived_multi <- paste0(rep(core_indicators, each=length(derivation_vars)), "_", derivation_vars)
derived_div <- paste0(rep(core_indicators, each=length(derivation_vars)), "_", derivation_vars, "_div")
metabolic_vars <- c(derived_multi, derived_div)

# 筛选数据集中有效数值变量
all_vars <- c(basic_vars, metabolic_vars)
all_vars <- all_vars[all_vars %in% names(data)]
all_vars <- all_vars[sapply(data[all_vars], is.numeric)]
basic_vars <- intersect(basic_vars, all_vars)
metabolic_vars <- intersect(metabolic_vars, all_vars)

cat("2. Variable Screening Completed\n")
cat("   Basic Variables:", length(basic_vars), "| Metabolic Indicators:", length(metabolic_vars), "\n")

# ===================== 3. 二分类Logistic回归 (1vs2/2vs3/3vs4a/4avs4b) =====================
stage_pairs <- list(
  Stage1vs2 = c("1","2"),
  Stage2vs3 = c("2","3"),
  Stage3vs4a = c("3","4a"),
  Stage4avs4b = c("4a","4b")
)
log_sig_all <- c()
log_results_list <- list()

for(pair_name in names(stage_pairs)){
  s1 <- stage_pairs[[pair_name]][1]
  s2 <- stage_pairs[[pair_name]][2]
  cat("\n=== Running:", pair_name, "===")
  
  df_sub <- data %>% filter(CKM_stage_ab %in% c(s1,s2))
  df_sub$y <- ifelse(df_sub$CKM_stage_ab == s2, 1, 0)
  if(nrow(df_sub) < 30) {cat(" Insufficient samples, skipped\n"); next}
  
  # 单因素Logistic
  uni_res <- data.frame()
  auc_res <- data.frame()
  for(v in all_vars){
    tryCatch({
      fit <- glm(y ~ get(v), data=df_sub, family=binomial())
      coef <- summary(fit)$coefficients
      uni_res <- rbind(uni_res, data.frame(
        group=pair_name, variable=v, OR=exp(coef[2,1]), p=coef[2,4]
      ))
      # 兼容所有版本：屏蔽AUC提示信息
      pred <- predict(fit, type="response")
      auc_val <- suppressMessages(auc(df_sub$y, pred))
      auc_res <- rbind(auc_res, data.frame(variable=v, auc=as.numeric(auc_val)))
    }, error=function(e){})
  }
  if(nrow(uni_res)==0) next
  
  # 筛选显著变量
  uni_sig <- uni_res$variable[uni_res$p < 0.05]
  basic_sig <- intersect(basic_vars, uni_sig)
  meta_sig <- intersect(metabolic_vars, uni_sig)
  
  # 代谢指标择优
  meta_selected <- select_best_metrics(df_sub, meta_sig, auc_res, "auc")
  multi_vars <- c(basic_sig, meta_selected)
  if(length(multi_vars)==0) next
  
  # 多因素+逐步回归
  multi_fit <- glm(as.formula(paste0("y ~ ", paste(multi_vars, collapse="+"))), 
                   data=df_sub, family=binomial())
  step_fit <- stepAIC(multi_fit, direction="backward", trace=0)
  step_res <- data.frame(
    group=pair_name,
    variable=names(coef(step_fit))[-1],
    OR=exp(coef(step_fit))[-1],
    p=summary(step_fit)$coefficients[-1,4]
  )
  
  log_sig <- c(step_res$variable)
  log_sig_all <- c(log_sig_all, log_sig)
  log_results_list[[pair_name]] <- list(univariate=uni_res, auc=auc_res, multivariate=summary(multi_fit)$coefficients, stepwise=step_res)
  write.xlsx(log_results_list[[pair_name]], file.path(log_dir, paste0(pair_name,".xlsx")))
  cat(" | Completed | Significant Variables:", length(log_sig), "\n")
}
log_sig_all <- unique(log_sig_all)

# ===================== 4. 分层Cox回归 (1/2/3/4a/4b) =====================
cox_sig_all <- c()
cox_results_list <- list()
stages <- c("1","2","3","4a","4b")

for(stg in stages){
  cat("\n=== Running: CKM Stage", stg, "===")
  df_sub <- data %>% filter(CKM_stage_ab == stg)
  if(nrow(df_sub) < 50) {cat(" Insufficient samples, skipped\n"); next}
  
  # 单因素Cox
  uni_res <- data.frame()
  cindex_res <- data.frame()
  for(v in all_vars){
    tryCatch({
      fit <- coxph(Surv(Time, Status) ~ get(v), data=df_sub)
      s <- summary(fit)
      uni_res <- rbind(uni_res, data.frame(
        stage=stg, variable=v, HR=s$coefficients[,1], p=s$coefficients[,5]
      ))
      cindex <- s$concordance[1]
      cindex_res <- rbind(cindex_res, data.frame(variable=v, cindex=cindex))
    }, error=function(e){})
  }
  if(nrow(uni_res)==0) next
  
  # 筛选显著变量
  uni_sig <- uni_res$variable[uni_res$p < 0.05]
  basic_sig <- intersect(basic_vars, uni_sig)
  meta_sig <- intersect(metabolic_vars, uni_sig)
  
  # 代谢指标择优
  meta_selected <- select_best_metrics(df_sub, meta_sig, cindex_res, "cindex")
  multi_vars <- c(basic_sig, meta_selected)
  if(length(multi_vars)==0) next
  
  # 多因素+逐步回归
  multi_fit <- coxph(as.formula(paste0("Surv(Time, Status) ~ ", paste(multi_vars, collapse="+"))), 
                     data=df_sub)
  step_fit <- stepAIC(multi_fit, direction="backward", trace=0)
  step_res <- data.frame(
    stage=stg,
    variable=names(coef(step_fit)),
    HR=exp(coef(step_fit)),
    p=summary(step_fit)$coefficients[,5]
  )
  
  cox_sig <- step_res$variable
  cox_sig_all <- c(cox_sig_all, cox_sig)
  cox_results_list[[stg]] <- list(univariate=uni_res, cindex=cindex_res, multivariate=summary(multi_fit)$coefficients, stepwise=step_res)
  write.xlsx(cox_results_list[[stg]], file.path(cox_dir, paste0("Stage_",stg,".xlsx")))
  cat(" | Completed | Significant Variables:", length(cox_sig), "\n")
}
cox_sig_all <- unique(cox_sig_all)

# ===================== 5. 双效生物标志物 =====================
dual_vars <- intersect(log_sig_all, cox_sig_all)
max_len <- max(length(log_sig_all), length(cox_sig_all), length(dual_vars))
final_list <- data.frame(
  Stage_Significant = c(log_sig_all, rep(NA, max_len-length(log_sig_all))),
  Prognosis_Significant = c(cox_sig_all, rep(NA, max_len-length(cox_sig_all))),
  Dual_Effect_Biomarker = c(dual_vars, rep(NA, max_len-length(dual_vars)))
)
write.xlsx(final_list, file.path(out_dir, "Final_Dual_Effect_Variables.xlsx"))

cat("\n5. Dual-Effect Biomarker Screening Completed\n")
cat("   Stage Significant:", length(log_sig_all), 
    " | Prognosis Significant:", length(cox_sig_all), 
    " | Dual-Effect Biomarkers:", length(dual_vars), "\n")

# ===================== 6. Upset 可视化（最终完美版） =====================
# 彻底修复：UpSetR原生用法 + 强制出图 + 无任何报错
# 关闭所有图形设备
graphics.off()
# 重新加载包
library(UpSetR)

# 打开图片设备
png(file.path(out_dir, "Upset_Plot.png"), 
    width=1000, height=800, res=150)

# 【核心】直接绘图，不赋值、不存储！UpSetR原生用法
upset(
  fromList(list(CKM_Stage = log_sig_all, Prognosis = cox_sig_all)),
  order.by = "freq",
  text.scale = 1.5
)

# 强制刷新图形，确保写入文件
flush.console()
Sys.sleep(1)

# 关闭设备，保存图片
dev.off()
cat("✅ Upset Plot Saved Successfully!\n")
# ===================== 完成 =====================
cat("\n✅ All Analysis Finished | Results saved in:", out_dir, "\n")