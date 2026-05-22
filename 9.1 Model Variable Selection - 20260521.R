# ==============================================================================
# 脚本9.1【终极修复版】：过滤C-index NA组合 + 无共线性
# 核心：加入Cox模型容错，只保留能成功计算C-index的组合
# ==============================================================================
rm(list=ls())
gc()
options(scipen=999)
set.seed(123)

# 加载包
packages <- c("tidyverse","survival","MASS","openxlsx","car","rms","caret","gridExtra")
for (p in packages) {
  if (!require(p, character.only = T)) install.packages(p, dependencies = T)
  library(p, character.only = T)
}
select <- dplyr::select
group_by <- dplyr::group_by
summarise <- dplyr::summarise
arrange <- dplyr::arrange

# ===================== 核心配置 =====================
TRAIN_RATIO <- 0.7
OUT_DIR <- "9_Nomogram_Analysis"
dir.create(OUT_DIR, showWarnings = F, recursive = T)

# 你的8个无共线性变量
# ===================== 核心：定义结果文件路径（与原脚本完全一致） =====================
result_path <- "8_Integrated_Screening_Results/8_Integrated_Screening_Results.xlsx"

# ===================== 读取两个核心数据表 =====================
# 1. 读取【最优筛选变量列表】
ALL_VARS <- read.xlsx(result_path, sheet = "Optimal_Vars")
ALL_VARS <- ALL_VARS$Variables
TOTAL_VARS <- length(ALL_VARS)
MAX_REMOVE <- TOTAL_VARS - 1

# ===================== 1. 数据加载 + 划分 =====================
cat("========== 数据加载 ==========\n")
data <- readRDS("5. Result_Indicator/代谢指标计算完成数据.RDS")

# 数据预处理
data$CKM_stage_ab <- factor(data$CKM_stage_ab, levels = c("1","2","3","4a","4b"), ordered = T)
data$Status <- as.integer(data$Status)
data$Time <- as.numeric(data$Time)

model_data <- data %>% 
  select(all_of(ALL_VARS), CKM_stage_ab, Time, Status) %>% 
  drop_na()

# 分层抽样
train_idx <- createDataPartition(model_data$CKM_stage_ab, p=TRAIN_RATIO, list=F)
train_data <- model_data[train_idx, ]
valid_data <- model_data[-train_idx, ]

# 转换为数据框
train_data <- as.data.frame(train_data)
valid_data <- as.data.frame(valid_data)

# rms初始化
dd <- datadist(train_data)
options(datadist = dd)
cat("✅ 训练集：",nrow(train_data),"｜验证集：",nrow(valid_data),"\n")
cat("✅ 当前总变量数：", TOTAL_VARS, "\n")

# ===================== 2. 性能计算函数【关键修复：容错处理】 =====================
get_both_perf <- function(vars, data){
  # Logistic模型（无风险）
  f_log <- as.formula(paste("CKM_stage_ab ~", paste(vars, collapse = "+")))
  fit_log <- lrm(f_log, data = data, x = T, y = T)
  log_c <- as.numeric(fit_log$stats["C"])
  
  # Cox模型【容错处理】：拟合失败则返回NA
  f_cox <- as.formula(paste("Surv(Time, Status) ~", paste(vars, collapse = "+")))
  fit_cox <- try(cph(f_cox, data = data, x = T, y = T, surv = T), silent = TRUE)
  
  if(inherits(fit_cox, "try-error")){
    return(c(log_c, NA)) # Cox模型失败，返回NA
  }
  
  # 检查Dxy是否为NA
  if(is.na(fit_cox$stats["Dxy"])){
    return(c(log_c, NA))
  }
  
  cox_c <- as.numeric(0.5 + fit_cox$stats["Dxy"] / 2)
  return(c(log_c, cox_c))
}

# ===================== 3. 遍历组合 =====================
cat("\n========== 开始计算 ==========\n")
result_df <- data.frame()
skip_count <- 0 

# 全变量（8个）
perf0 <- get_both_perf(ALL_VARS, train_data)
if(!is.na(perf0[2])){ # 仅当C-index非NA时保留
  result_df <- rbind(result_df, data.frame(
    remove_n = 0, keep_n = TOTAL_VARS, vars = paste(ALL_VARS, collapse = ","),
    Log_AUC = perf0[1], Cox_C = perf0[2]
  ))
} else { skip_count <- skip_count + 1 }

# 遍历删除1~7个变量
for(remove_n in 1:MAX_REMOVE){
  keep_n <- TOTAL_VARS - remove_n
  comb <- combn(ALL_VARS, keep_n, simplify = F)
  cat("删除",remove_n,"个变量 | 总组合：",length(comb),"｜计算中...\n")
  
  for(i in 1:length(comb)){
    vars_now <- comb[[i]]
    perf <- get_both_perf(vars_now, train_data)
    
    # 关键：仅保留C-index非NA的组合
    if(is.na(perf[2])){
      skip_count <- skip_count + 1
      next
    }
    
    result_df <- rbind(result_df, data.frame(
      remove_n = remove_n, keep_n = keep_n, vars = paste(vars_now, collapse = ","),
      Log_AUC = perf[1], Cox_C = perf[2]
    ))
  }
}

# ===================== 4. 过滤 + 保存 =====================
cat("\n===========================================\n")
cat("✅ 有效组合数：", nrow(result_df), "\n")
cat("❌ 剔除C-index NA组合数：", skip_count, "\n")
cat("===========================================\n")

# 计算总分（此时无NA）
result_df <- result_df %>% mutate(total_score = round((Log_AUC + Cox_C)/2, 6))

save(
  list = c("result_df", "train_data", "valid_data", "ALL_VARS", "dd", "OUT_DIR"),
  file = file.path(OUT_DIR, "9_Precompute_Results.RDS")
)

cat("\n🎉🎉🎉 脚本9.1 运行完成！已剔除所有C-index NA组合！\n")