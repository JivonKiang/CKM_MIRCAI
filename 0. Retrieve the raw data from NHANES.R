# ==============================================================================
# NHANES 脚本一 | 分步提取+分步合并+分步保存 | 终极稳健版
# 🔥 新增：KIQ文件夹 + KIQ20 → Kidney disease（肾病）
# 🔥 确认：Diabetes=3 → 统一为 No
# 编码规则100%统一 | 完美衔接后续清洗脚本
# ==============================================================================
rm(list=ls())
gc()
library(foreign)
library(dplyr)
library(purrr)
library(tidyr)

# ===================== 基础配置 =====================
PATH_RESULT <- "Result/"
PATH_TEMP <- paste0(PATH_RESULT, "step_temp.rds")
if(!dir.exists(PATH_RESULT)) dir.create(PATH_RESULT)

# ===================== 【通用】标准化读取函数 =====================
read_nhanes_module <- function(folder, var_patterns) {
  cat("\n🔍 读取模块：", folder, "\n")
  files <- list.files(folder, full.names = T, pattern = "\\.xpt$", ignore.case = TRUE)
  cat("📂 总文件数：", length(files), "\n")
  
  df_list <- lapply(files, function(x){
    tryCatch({
      df <- foreign::read.xport(x)
      cat("✅ 读取：", basename(x), "\n")
      return(df)
    }, error = function(e) { NULL })
  })
  df_list <- compact(df_list)
  if(length(df_list)==0) return(tibble(SEQN = integer()))
  
  merged <- df_list %>% reduce(full_join, by="SEQN") %>% distinct(SEQN, .keep_all=T)
  
  result <- tibble(SEQN = merged$SEQN)
  for(pat in var_patterns){
    cols <- grep(pat, colnames(merged), ignore.case = TRUE, value = TRUE)
    result[[pat]] <- if(length(cols)>0) coalesce(!!!merged[cols]) else NA
  }
  
  cat("✅ 模块提取完成 | 样本数：", nrow(result), "\n")
  return(result)
}

# ===================== 脂质模块专用函数 =====================
read_cholesterol <- function(folder){
  cat("\n🔍 读取脂质模块\n")
  files <- list.files(folder, full.names = T, pattern = "\\.xpt$", ignore.case = TRUE)
  df_list <- lapply(files, function(x){
    tryCatch({foreign::read.xport(x)}, error=function(e){NULL})
  }) %>% compact()
  
  chol_data <- df_list %>% reduce(full_join, by="SEQN") %>% distinct(SEQN, .keep_all=T)
  chol_final <- chol_data %>%
    transmute(
      SEQN = SEQN,
      LBXTC = coalesce(!!!select(., contains("LBXTC"), contains("LB2TC"))),
      LBDHDL = coalesce(!!!select(., contains("LBDHDD"), contains("LBXHDD"), contains("LB2HDL"))),
      LBDLDL = coalesce(!!!select(., contains("LBDLDL"), contains("LB2LDL"))),
      LBXTR = coalesce(!!!select(., contains("LBXTR"), contains("LB2TR")))
    ) %>% filter(!is.na(SEQN))
  return(chol_final)
}

# ===================== 🔥 肾病模块专用函数（1:1复刻脂质逻辑） =====================
read_kidney <- function(folder){
  cat("\n🔍 读取肾病模块(KIQ)\n")
  files <- list.files(folder, full.names = T, pattern = "\\.xpt$", ignore.case = TRUE)
  df_list <- lapply(files, function(x){
    tryCatch({foreign::read.xport(x)}, error=function(e){NULL})
  }) %>% compact()
  
  kiq_data <- df_list %>% reduce(full_join, by="SEQN") %>% distinct(SEQN, .keep_all=T)
  # ✅ 核心：合并所有周期肾病变量（KIQ020/KIQ022/大小写变体），和脂质逻辑完全一样
  kiq_final <- kiq_data %>%
    transmute(
      SEQN = SEQN,
      # 自动匹配所有肾病相关变量，coalesce取有效值
      Kidney_Disease_Raw = coalesce(!!!select(., contains("KIQ020"), contains("KIQ022"), contains("KIQ20")))
    ) %>% filter(!is.na(SEQN))
  return(kiq_final)
}

# ===================== 模块定义【✅ 新增KIQ模块】 =====================
modules <- list(
  demo = list(path = "original data/DEMO", vars = c("RIDAGEYR","RIAGENDR","DMDMARTL","DMDEDUC2","RIDRETH3")),
  smq  = list(path = "original data/SMQ",  vars = c("SMQ020")),
  alq  = list(path = "original data/ALQ",  vars = c("ALQ101")),
  paq  = list(path = "original data/PAQ",  vars = c("PAQ605","PAQ620","PAQ635")),
  dpq  = list(path = "original data/DPQ",  vars = c("DPQ030")),
  diq  = list(path = "original data/DIQ",  vars = c("DIQ010")),
  mcq  = list(path = "original data/MCQ",  vars = c("MCQ220","MCQ160F","MCQ160C","MCQ160B","MCQ160E","MCQ160D")),
  bpq  = list(path = "original data/BPQ",  vars = c("BPQ020","BPQ080")),
  bmx  = list(path = "original data/BMX",  vars = c("BMXHT","BMXWT","BMXBMI","BMXWAIST")),
  bpx  = list(path = "original data/BPX",  vars = c("BPXSY3","BPXDI3")),
  gh   = list(path = "original data/Glycohemoglobin", vars = c("LBXGH")),
  chol = list(path = "original data/Cholesterol"),
  crp  = list(path = "original data/CRP",  vars = c("LBXCRP")),
  glu  = list(path = "original data/Glucose", vars = c("LBXGLU")),
  ua   = list(path = "original data/UA",   vars = c("LBXSUA","LBXSBU","LBXSCR")),
  cbc  = list(path = "original data/CBC",  vars = c("LBXPLTSI")),
  cdq  = list(path = "original data/CDQ",  vars = c("CDQ001","CDQ008","CDQ010")),
  # ✅ 修复：用肾病专用函数，和chol完全一致
  kiq  = list(path = "original data/KIQ")
)

# ===================== 初始化合并统计列表 =====================
merge_log <- list()
log_index <- 1

# ===================== 核心：分步提取 + 合并 + 保存 =====================
cat("\n=====================================\n")
cat("🚀 开始分步合并所有模块\n")
cat("=====================================\n")

# 1. 初始化基础数据
final_data <- readRDS("original data/mortality/final_mortality.rds")
saveRDS(final_data, PATH_TEMP)
cat("\n✅ 初始基础数据已保存：", nrow(final_data), "行\n")

# 2. 循环合并所有模块
for(name in names(modules)){
  cat("\n=====================================")
  cat("\n📌 当前处理模块：", name)
  cat("\n📥 读取上一步临时文件...")
  
  temp_data <- readRDS(PATH_TEMP)
  
  if(name == "chol"){
    module_df <- read_cholesterol(modules[[name]]$path)
  }else if(name == "kiq"){
    module_df <- read_kidney(modules[[name]]$path)
  }else{
    module_df <- read_nhanes_module(modules[[name]]$path, modules[[name]]$vars)
  }
  
  # 合并统计
  pre_nrow <- nrow(temp_data)
  pre_ncol <- ncol(temp_data)
  pre_seqn <- n_distinct(temp_data$SEQN)
  
  mod_nrow <- nrow(module_df)
  mod_ncol <- ncol(module_df)
  mod_seqn <- n_distinct(module_df$SEQN)
  
  merged_data <- left_join(temp_data, module_df, by = "SEQN")
  cat("\n🔗 合并完成 | 总行数：", nrow(merged_data))
  
  post_nrow <- nrow(merged_data)
  post_ncol <- ncol(merged_data)
  post_seqn <- n_distinct(merged_data$SEQN)
  add_cols <- post_ncol - pre_ncol
  
  merge_log[[log_index]] <- tibble(
    模块名称 = name,
    合并前_行数 = pre_nrow,
    合并前_列数 = pre_ncol,
    合并前_唯一SEQN = pre_seqn,
    提取模块_行数 = mod_nrow,
    提取模块_列数 = mod_ncol,
    提取模块_唯一SEQN = mod_seqn,
    合并后_行数 = post_nrow,
    合并后_列数 = post_ncol,
    合并后_唯一SEQN = post_seqn,
    本次新增列数 = add_cols
  )
  log_index <- log_index + 1
  
  saveRDS(merged_data, PATH_TEMP)
  cat("\n💾 本步骤已保存，继续下一个模块...\n")
}

# 输出合并统计总表
cat("\n\n=====================================\n")
cat("📊 【全步骤合并统计总表】\n")
cat("=====================================\n")
merge_stats <- bind_rows(merge_log)
print(merge_stats, n = Inf)
write.csv(merge_stats, paste0(PATH_RESULT, "合并过程统计报表.csv"), row.names = F)

# ===================== 最终变量重命名【✅ 修复肾病变量合并】 =====================
cat("\n=====================================\n")
cat("🎉 所有模块合并完成，开始最终整理\n")
cat("=====================================\n")

final_data <- readRDS(PATH_TEMP)

final_data <- final_data %>% rename(
  Time = any_of("Time_day"),
  Age = any_of("RIDAGEYR"),
  Sex = any_of("RIAGENDR"),
  `Marital status` = any_of("DMDMARTL"),
  `Education level` = any_of("DMDEDUC2"),
  Race = any_of("RIDRETH3"),
  Smoking = any_of("SMQ020"),
  Drinking = any_of("ALQ101"),
  `Vigorous work activity` = any_of("PAQ605"),
  `Moderate work activity` = any_of("PAQ620"),
  `Mild work activity` = any_of("PAQ635"),
  `Sleep health` = any_of("DPQ030"),
  Diabetes = any_of("DIQ010"),
  Cancer = any_of("MCQ220"),
  Stroke = any_of("MCQ160F"),
  Hypertension = any_of("BPQ020"),
  Hyperlipidemia = any_of("BPQ080"),
  `Coronary heart disease` = any_of("MCQ160C"),
  `Congestive heart failure` = any_of("MCQ160B"),
  `Myocardial infarction` = any_of("MCQ160E"),
  `Angina pectoris` = any_of("MCQ160D"),
  `Chest pain` = any_of("CDQ001"),
  `Severe chest pain` = any_of("CDQ008"),
  `Shortness of breath` = any_of("CDQ010"),
  # ✅ 直接用合并好的肾病变量
  `Kidney disease` = any_of("Kidney_Disease_Raw"),
  Height = any_of("BMXHT"),
  Weight = any_of("BMXWT"),
  BMI = any_of("BMXBMI"),
  WC = any_of("BMXWAIST"),
  SBP = any_of("BPXSY3"),
  DBP = any_of("BPXDI3"),
  HbA1C = any_of("LBXGH"),
  TC = any_of("LBXTC"),
  HDL = any_of("LBDHDL"),
  LDL = any_of("LBDLDL"),
  TG = any_of("LBXTR"),
  CRP = any_of("LBXCRP"),
  FPG = any_of("LBXGLU"),
  UA = any_of("LBXSUA"),
  BUN = any_of("LBXSBU"),
  Scr = any_of("LBXSCR"),
  PLT = any_of("LBXPLTSI")
)
# ❌ 删掉之前所有报错的 mutate( coalesce() ) 和 select(-KIQxx)

# ==============================================================================
# 🔥 分类变量编码【✅ 肾病变量统一编码 + 糖尿病3=No】
# ==============================================================================
cat("\n🔧 开始分类变量编码与异常值清洗...\n")

# 通用编码映射（所有二分类变量：1=是，2=否，7=拒绝，9=不知道）
wheezy_map <- c("1" = "1_Yes", "2" = "2_No", "7" = "7_Refused", "9" = "9_Don't know")

# 性别编码
final_data$Sex <- case_when(
  final_data$Sex == 1 ~ "1_Male",
  final_data$Sex == 2 ~ "2_Female",
  TRUE ~ NA_character_
)

# 种族编码
final_data$Race <- case_when(
  final_data$Race == 1 ~ "1_Mexican American",
  final_data$Race == 2 ~ "2_Other Hispanic",
  final_data$Race == 3 ~ "3_Non-Hispanic White",
  final_data$Race == 4 ~ "4_Non-Hispanic Black",
  final_data$Race == 6 ~ "6_Non-Hispanic Asian",
  final_data$Race == 7 ~ "7_Other Race",
  TRUE ~ NA_character_
)

# ✅【糖尿病规则】1=Yes，2/3=No，完全符合你的要求
diabetes_map <- c("1" = "1_Yes", "2" = "2_No","3" = "2_No", "7" = "7_Refused", "9" = "9_Don't know")
final_data$Diabetes <- diabetes_map[as.character(final_data$Diabetes)]

# 人口学变量
final_data$`Marital status` <- wheezy_map[as.character(final_data$`Marital status`)]
final_data$`Education level` <- wheezy_map[as.character(final_data$`Education level`)]

# 生活方式
final_data$Smoking <- wheezy_map[as.character(final_data$Smoking)]
final_data$Drinking <- wheezy_map[as.character(final_data$Drinking)]
final_data$`Vigorous work activity` <- wheezy_map[as.character(final_data$`Vigorous work activity`)]
final_data$`Moderate work activity` <- wheezy_map[as.character(final_data$`Moderate work activity`)]
final_data$`Mild work activity` <- wheezy_map[as.character(final_data$`Mild work activity`)]
final_data$`Sleep health` <- wheezy_map[as.character(final_data$`Sleep health`)]

# 疾病史（✅ 新增肾病变量，编码完全统一）
final_data$Cancer <- wheezy_map[as.character(final_data$Cancer)]
final_data$Stroke <- wheezy_map[as.character(final_data$Stroke)]
final_data$Hypertension <- wheezy_map[as.character(final_data$Hypertension)]
final_data$Hyperlipidemia <- wheezy_map[as.character(final_data$Hyperlipidemia)]
final_data$`Coronary heart disease` <- wheezy_map[as.character(final_data$`Coronary heart disease`)]
final_data$`Congestive heart failure` <- wheezy_map[as.character(final_data$`Congestive heart failure`)]
final_data$`Myocardial infarction` <- wheezy_map[as.character(final_data$`Myocardial infarction`)]
final_data$`Angina pectoris` <- wheezy_map[as.character(final_data$`Angina pectoris`)]
final_data$`Kidney disease` <- wheezy_map[as.character(final_data$`Kidney disease`)] # ✅ 肾病

# 胸痛症状
final_data$`Chest pain` <- wheezy_map[as.character(final_data$`Chest pain`)]
final_data$`Severe chest pain` <- wheezy_map[as.character(final_data$`Severe chest pain`)]
final_data$`Shortness of breath` <- wheezy_map[as.character(final_data$`Shortness of breath`)]

# 合并心血管疾病总变量
final_data$Heart_disease <- ifelse(
  final_data$`Coronary heart disease`=="1_Yes" | final_data$`Congestive heart failure`=="1_Yes" |
    final_data$`Myocardial infarction`=="1_Yes" | final_data$`Angina pectoris`=="1_Yes", 
  "1_Yes", "2_No"
)

# 数值型异常值清洗
final_data <- final_data %>%
  mutate(
    Age = ifelse(Age < 18 | Age > 110, NA, Age),
    Height = ifelse(Height < 120 | Height > 210, NA, Height),
    Weight = ifelse(Weight < 25 | Weight > 200, NA, Weight),
    WC = ifelse(WC < 50 | WC > 150, NA, WC),
    SBP = ifelse(SBP < 60 | SBP > 260, NA, SBP),
    DBP = ifelse(DBP < 40 | DBP > 150, NA, DBP),
    BMI = ifelse(BMI > 100 | BMI < 10, NA, BMI)
  )

# 无效值清洗
final_data <- final_data %>%
  mutate(
    Diabetes = ifelse(Diabetes %in% c("7_Refused","9_Don't know"), NA, Diabetes),
    Hypertension = ifelse(Hypertension %in% c("7_Refused","9_Don't know"), NA, Hypertension),
    `Kidney disease` = ifelse(`Kidney disease` %in% c("7_Refused","9_Don't know"), NA, `Kidney disease`) # ✅ 肾病无效值清洗
  )

# 数据回填
final_data <- final_data %>%
  mutate(diabetes = Diabetes, hyperten = Hypertension)

# 删除无用字段
final_data <- final_data %>% select(-ucod_leading)

# 保存最终数据
saveRDS(final_data, paste0(PATH_RESULT, "清洗但未插补的数据.RDS"))
cat("\n✅ 最终数据已保存：Result/清洗但未插补的数据.RDS\n")

# 缺失值统计
na_detail <- final_data %>%
  summarise(across(everything(), ~sum(is.na(.x)))) %>%
  pivot_longer(cols = everything(), names_to = "变量名称", values_to = "缺失数量") %>%
  mutate(总样本数 = nrow(final_data), `缺失率(%)` = round(缺失数量/总样本数*100,2)) %>%
  arrange(desc(`缺失率(%)`))

cat("\n=====================================\n")
cat("📋 最终缺失值统计\n")
print(na_detail, n=Inf)
write.csv(na_detail, paste0(PATH_RESULT, "缺失值统计报表.csv"), row.names = F)

# 删除临时文件
unlink(PATH_TEMP)
cat("\n🎉 全部完成！✅ 新增肾病变量 | ✅ 糖尿病规则正确\n")