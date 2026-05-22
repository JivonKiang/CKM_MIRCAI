rm(list = ls())
library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)
library(haven)
library(lubridate)
library(data.table)

# ===================== 路径配置 =====================
path_main <- "data/Harmonized CHARLS/H_CHARLS_D_Data.dta"
path_health <- list(
  wave1 = "data/wave1/Health_Status_and_Functioning.dta",
  wave2 = "data/wave2/Health_Status_and_Functioning.dta",
  wave3 = "data/wave3/Health_Status_and_Functioning.dta",
  wave4 = "data/wave4/Health_Status_and_Functioning.dta"
)
path_blood <- list(
  wave1_2011 = "data/wave1/Blood.dta",
  wave3_2015 = "data/wave3/Blood.dta"
)
dir.create("Result", showWarnings = FALSE)

# ===================== 全自动数据质检函数 =====================
data_qa <- function(df, df_name = "数据集") {
  cat(paste0("\n", "=", strrep("-", 50), "\n"))
  cat(paste0("📊 【自动质检报告】", df_name, " (", nrow(df), "行 × ", ncol(df), "列)\n"))
  cat(paste0("=", strrep("-", 50), "\n"))
  
  for (col in colnames(df)) {
    col_type <- class(df[[col]])[1]
    na_rate <- round(sum(is.na(df[[col]]) | is.nan(df[[col]])) / nrow(df) * 100, 2)
    
    if (is.numeric(df[[col]])) {
      vals <- df[[col]][!is.na(df[[col]]) & !is.nan(df[[col]])]
      rng <- if(length(vals)>0) paste0("[", round(min(vals),2), ", ", round(max(vals),2), "]") else "全缺失"
      cat(sprintf("🔹 %-15s | 类型:%-8s | 缺失率:%5.1f%% | 范围:%s\n", col, col_type, na_rate, rng))
    } else {
      unique_n <- length(unique(na.omit(df[[col]])))
      cat(sprintf("🔹 %-15s | 类型:%-8s | 缺失率:%5.1f%% | 唯一值:%d\n", col, col_type, na_rate, unique_n))
    }
  }
  cat(paste0("=", strrep("-", 50), "\n"))
}

# ===================== 工具函数 =====================
get_mode <- function(x) {
  x <- na.omit(x)
  if(length(x) == 0) return(NA)
  freq <- table(x)
  mode_val <- names(freq)[which.max(freq)]
  return(mode_val)
}

# 临床校验函数（匹配官方原始单位）
data_check <- function(df) {
  cat("\n📊 临床数据校验结果（官方原始单位）：\n")
  all_ok <- TRUE
  
  core_vars_check <- list(
    分类变量 = c("Sex", "Chest.pain", "Stroke", "Heart.disease", "Kidney.Disease"),
    数值变量 = c("Age", "Height", "Weight", "Waist", "Scr", "FPG", "HbA1C", "SBP", "DBP", "TG", "HDL")
  )
  # 官方检测限/临床正常范围
  num_standard <- list(
    Age = c(18, 110),    Height = c(1.2, 2.1),  Weight = c(25, 200), Waist = c(50, 150),
    Scr   = c(0.1, 25),    # mg/dL
    FPG   = c(2, 450),     # mg/dL
    HbA1C = c(0, 40),      # %
    SBP   = c(60, 260),  DBP = c(40, 150),
    TG    = c(4, 1000),    # mg/dL
    HDL   = c(3, 120)      # mg/dL
  )
  
  for (v in core_vars_check$数值变量) {
    if (!v %in% colnames(df)) { cat(paste0("❌ 缺失核心变量 -> ", v, "\n")); all_ok <- FALSE; next }
    non_na_vals <- df[[v]][!is.na(df[[v]])]
    if (length(non_na_vals) == 0) { cat(paste0("⚠️  ", v, " 无有效数值\n")); all_ok <- FALSE; next }
    val_range <- range(non_na_vals)
    cat(paste0(v, " 范围：[", round(val_range[1],2), ", ", round(val_range[2],2), "]\n"))
    if (val_range[1] < num_standard[[v]][1] || val_range[2] > num_standard[[v]][2]) {
      cat(paste0("⚠️  ", v, " 超出官方检测限\n")); all_ok <- FALSE
    }
  }
  for (v in core_vars_check$分类变量) {
    if (!v %in% colnames(df)) { cat(paste0("❌ 缺失核心变量 -> ", v, "\n")); all_ok <- FALSE }
  }
  return(all_ok)
}

# ===================== 分波基础数据合并 =====================
data1 <- read_dta(path_main)
vars_w1 <- c("r1iwy","r1iwm","r1iwstat","r1agey","ragender","r1mstat","s1educ_c","r1hukou",
             "r1smokev","r1drinkev","r1vgact_c","r1mdact_c","r1ltact_c","r1sleeprl",
             "r1diabe","r1cancre","r1stroke","r1hibpe","r1hearte","r1dyslipe","r1kidneye",
             "r1mheight","r1mweight","r1mbmi","r1mwaist","r1systo","r1diasto")
common_vars <- c("radyear", "radmonth", "ID_w1")
vars_w1 <- c(vars_w1, common_vars)

vars_w2 <- gsub("r1", "r2", vars_w1)
vars_w3 <- gsub("r1", "r3", vars_w1)
vars_w4 <- gsub("r1", "r4", vars_w1)

check_vars <- function(vars, data) {
  missing <- vars[!vars %in% colnames(data)]
  if(length(missing)>0) warning("缺失变量：", paste(missing, collapse=", "))
  return(vars[vars %in% colnames(data)])
}

extracted <- list()
extracted$w1 <- data1 %>% select(all_of(check_vars(vars_w1, data1))) %>% mutate(wave = 1)
extracted$w2 <- data1 %>% select(all_of(check_vars(vars_w2, data1))) %>% mutate(wave = 2)
extracted$w3 <- data1 %>% select(all_of(check_vars(vars_w3, data1))) %>% mutate(wave = 3)
extracted$w4 <- data1 %>% select(all_of(check_vars(vars_w4, data1))) %>% mutate(wave = 4)

for(i in 2:4) names(extracted[[i]]) <- gsub(paste0("r",i), "r1", names(extracted[[i]]))
combined_data <- bind_rows(extracted)
names(combined_data)[names(combined_data)=="ID_w1"] <- "ID"

# 重命名+基础清洗
combined_data <- combined_data %>% rename(
  Age = r1agey, Sex = ragender, Marital_status = r1mstat, Education_level = s1educ_c,
  Hukou_status = r1hukou, Smoking = r1smokev, Drinking = r1drinkev,
  Intensive_PA = r1vgact_c, Moderate_PA = r1mdact_c, Light_PA = r1ltact_c,
  Sleep_health = r1sleeprl, Diabetes = r1diabe, Cancer = r1cancre, Stroke = r1stroke,
  Hypertension = r1hibpe, Heart_disease = r1hearte, Dyslipidemia = r1dyslipe,
  Kidney_Disease = r1kidneye, Height = r1mheight, Weight = r1mweight, BMI = r1mbmi,
  Waist = r1mwaist, SBP = r1systo, DBP = r1diasto
) %>%
  mutate(
    Age = ifelse(Age < 18 | Age > 110, NA, Age),
    Height = ifelse(Height < 1.2 | Height > 2.1, NA, Height),
    Weight = ifelse(Weight < 25 | Weight > 200, NA, Weight),
    Waist = ifelse(Waist < 50 | Waist > 150, NA, Waist),
    SBP = ifelse(SBP < 60 | SBP > 260, NA, SBP),
    DBP = ifelse(DBP < 40 | DBP > 150, NA, DBP),
    BMI = ifelse(BMI > 100 | BMI < 10, NA, BMI)
  )

data_qa(combined_data, "基础多波原始数据")

# 基础数据聚合
continuous_vars <- c("Age","Height","Weight","BMI","Waist","SBP","DBP")
factor_vars <- c("Sex","Marital_status","Education_level","Hukou_status",
                 "Smoking","Drinking","Intensive_PA","Moderate_PA","Light_PA","Sleep_health",
                 "Diabetes","Cancer","Stroke","Hypertension","Heart_disease","Dyslipidemia","Kidney_Disease")

combined_data_agg <- combined_data %>%
  group_by(ID) %>%
  summarise(
    across(all_of(continuous_vars[continuous_vars %in% colnames(.)]), ~mean(.x, na.rm = TRUE)),
    across(all_of(factor_vars[factor_vars %in% colnames(.)]), ~get_mode(.x)),
    .groups = "drop"
  )

# ===================== 血检数据清洗（保持官方原始单位） =====================
blood_2011 <- read_dta(path_blood$wave1_2011)
blood_w1 <- blood_2011 %>% select(ID, newhba1c, newcho, newhdl, newldl, newcrp, newtg, newglu, newua, newbun, newcrea, qc1_vb009)
colnames(blood_w1) <- c("ID","HbA1C","TC","HDL","LDL","CRP","TG","FPG","UA","BUN","Scr","PLT")

blood_2015 <- read_dta(path_blood$wave3_2015)
blood_w3 <- blood_2015 %>% select(ID, bl_hbalc, bl_cho, bl_hdl, bl_ldl, bl_crp, bl_tg, bl_glu, bl_ua, bl_bun, bl_crea, bl_plt)
colnames(blood_w3) <- c("ID","HbA1C","TC","HDL","LDL","CRP","TG","FPG","UA","BUN","Scr","PLT")

# 合并+清洗无效值
blood_clean <- bind_rows(blood_w1, blood_w3) %>%
  mutate(ID = as.character(ID)) %>%
  mutate(across(-c(ID, PLT), ~ifelse(.x %in% c(88, 99), NA, .x))) %>%
  mutate(across(-ID, as.numeric)) %>%
  # 官方检测限过滤
  mutate(
    HbA1C = ifelse(HbA1C < 0 | HbA1C > 40, NA, HbA1C),
    TC = ifelse(TC < 3 | TC > 800, NA, TC),
    HDL = ifelse(HDL < 3 | HDL > 120, NA, HDL),
    LDL = ifelse(LDL < 3 | LDL > 400, NA, LDL),
    CRP = ifelse(CRP < 0.1 | CRP > 20, NA, CRP),
    TG = ifelse(TG < 4 | TG > 1000, NA, TG),
    FPG = ifelse(FPG < 2 | FPG > 450, NA, FPG),
    UA = ifelse(UA < 0 | UA > 20, NA, UA),
    BUN = ifelse(BUN < 5 | BUN > 100, NA, BUN),
    Scr = ifelse(Scr < 0.1 | Scr > 25, NA, Scr)
  ) %>%
  mutate(across(where(is.numeric), ~ifelse(is.nan(.) | is.infinite(.), NA, .)))

data_qa(blood_clean, "血检数据（官方原始单位）")

# 按ID聚合
blood_cols <- c("HbA1C","TC","HDL","LDL","CRP","TG","FPG","UA","BUN","Scr","PLT")
blood_agg <- blood_clean %>%
  group_by(ID) %>%
  summarise(across(all_of(blood_cols), ~mean(.x, na.rm=TRUE)), .groups="drop")

# ===================== 胸痛数据 =====================
Health1 <- read_dta(path_health$wave1)
Health2 <- read_dta(path_health$wave2)
chest_data <- bind_rows(Health1 %>% select(ID, da003, da004), Health2 %>% select(ID, da003, da004)) %>%
  mutate(da003=ifelse(da003%in%c(8,9),NA,da003), da004=ifelse(da004%in%c(8,9),NA,da004))

chest_agg <- chest_data %>% group_by(ID) %>%
  summarise(Chest_pain=get_mode(da003), Chest_Pains_When_Climbing=get_mode(da004), .groups="drop")

# ===================== 数据合并 =====================
final_merge <- combined_data_agg %>% 
  left_join(chest_agg, by="ID") %>% 
  left_join(blood_agg, by="ID")

# ===================== 生存结局 =====================
df <- combined_data %>% select(ID, wave, r1iwy, r1iwm, r1iwstat, radyear, radmonth)
colnames(df) <- c("ID","wave","follow_year","follow_month","follow_status","death_year","death_month")

cycle_dt <- data.table(wave=1:4,
                       start=as.Date(c("2011-01-01","2013-01-01","2015-01-01","2017-01-01")),
                       end=as.Date(c("2012-12-31","2014-12-31","2016-12-31","2018-12-31")))

dt <- as.data.table(df) %>% merge(cycle_dt, by="wave")
dt[, Status := fifelse(any(follow_status %in% c(5,6)), 1L, 0L), by=ID]
dt[, Time := as.integer(ifelse(!is.na(make_date(floor(death_year), death_month,15)), 
                               make_date(floor(death_year), death_month,15)-start, end-start)), by=ID]

surv_data <- dt %>% select(ID, Status, Time) %>% distinct(ID, .keep_all=TRUE)

# ===================== 🔥 核心：整合所有分类变量释义/标签 =====================
result_data <- left_join(surv_data, final_merge, by="ID") %>%
  mutate(across(where(is.numeric), ~ifelse(is.nan(.) | is.infinite(.), NA, .))) %>%
  # 统一变量名格式（点分隔，适配后续代码）
  rename_with(~gsub("_", ".", .x), -ID) %>%
  # -------------------------- 分类变量编码+释义 --------------------------
# 1. 性别
mutate(Sex = case_when(
  Sex == 1 ~ "1_Male",
  Sex == 2 ~ "2_Female",
  TRUE ~ as.character(Sex)
)) %>%
  # 2. 婚姻状况
  mutate(Marital.status = case_when(
    Marital.status == 1 ~ "1_married",
    Marital.status == 3 ~ "3_partnered",
    Marital.status == 4 ~ "4_separated",
    Marital.status == 5 ~ "5_divorced",
    Marital.status == 7 ~ "7_widowed",
    Marital.status == 8 ~ "8_never married",
    TRUE ~ as.character(Marital.status)
  )) %>%
  # 3. 教育水平 + 低频合并
  mutate(Education.level = case_when(
    Education.level == 1 ~ "1_No formal education illiterate",
    Education.level == 2 ~ "2_Did not finish primary school but capa",
    Education.level == 3 ~ "3_Sishu",
    Education.level == 4 ~ "4_Elementary school",
    Education.level == 5 ~ "5_Middle school",
    Education.level == 6 ~ "6_High school",
    Education.level == 7 ~ "7_Vocational school",
    Education.level == 8 ~ "8_Two/Three Year College/Associate degree",
    Education.level == 9 ~ "9_Four Year College/Bachelor's degree",
    Education.level == 10 ~ "10_Post-graduated(Master/PhD)",
    TRUE ~ as.character(Education.level)
  )) %>%
  # 4. 户口
  mutate(Hukou.status = case_when(
    Hukou.status == 1 ~ "1_Agricultural hukou",
    Hukou.status == 2 ~ "2_Non-agricultural hukou",
    Hukou.status == 3 ~ "3_Unified residence hukou",
    Hukou.status == 4 ~ "4_Do not have hukou",
    TRUE ~ as.character(Hukou.status)
  )) %>%
  # 5. 吸烟/饮酒
  mutate(Smoking = case_when(Smoking == 0 ~ "2_No", Smoking == 1 ~ "1_Yes", TRUE ~ as.character(Smoking)),
         Drinking = case_when(Drinking == 0 ~ "2_No", Drinking == 1 ~ "1_Yes", TRUE ~ as.character(Drinking))) %>%
  # 6. 睡眠健康
  mutate(Sleep.health = case_when(
    Sleep.health == 1 ~ "1_Rarely or none of the time < 1 day",
    Sleep.health == 2 ~ "2_Some or a little of the time 1-2 days",
    Sleep.health == 3 ~ "3_Occasionally or a moderate amount of 3-4 days",
    Sleep.health == 4 ~ "4_Most or all of the time 5-7 days",
    TRUE ~ as.character(Sleep.health)
  )) %>%
  # 7. 慢性病（0=No,1=Yes）
  mutate(across(c(Diabetes, Cancer, Stroke, Hypertension, Dyslipidemia, Heart.disease, Kidney.Disease),
                ~case_when(.x == 0 ~ "2_No", .x == 1 ~ "1_Yes", TRUE ~ as.character(.x)))) %>%
  # 8. 体力活动
  mutate(across(c(Intensive.PA, Moderate.PA, Light.PA),
                ~case_when(.x == 0 ~ "2_No", .x == 1 ~ "1_Yes", TRUE ~ as.character(.x)))) %>%
  # 9. 胸痛症状
  mutate(across(c(Chest.pain, Chest.Pains.When.Climbing),
                ~case_when(.x == 1 ~ "1_Yes", .x == 2 ~ "2_No", TRUE ~ as.character(.x))))

# 去除教育水平低频类别
freq <- table(result_data$Education.level)
low_freq_levels <- names(freq[freq < 10])
result_data <- result_data[!(result_data$Education.level %in% low_freq_levels), ]

# ===================== 最终质检+保存 =====================
data_qa(result_data, "最终整合数据集（含分类释义）")
data_valid <- data_check(result_data)

# 保存最终文件（直接用于插补，无需后续分类处理）
saveRDS(result_data, "Result/清洗但未插补的数据.RDS")
write.csv(result_data, "Result/待插补数据_原始单位_含分类释义.csv", row.names = FALSE)

cat("\n============= 脚本完成 =============\n")
cat("✅ 血检指标保持官方原始单位\n✅ 所有分类变量已添加完整释义\n✅ 变量名统一格式\n✅ 直接用于后续插补流程\n")