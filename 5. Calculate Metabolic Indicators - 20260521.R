# ==============================================================================
# 脚本 5：代谢复合指标批量计算（基线表报错修复版）
# 读取数据：4. Result_CKM/CKM_Stage_Result.RDS（脚本四的直接输出文件）
# 输出路径：5. Result_Indicator
# 核心修复：解决moonBook基线表生成报错，新增双模式基线表+兜底方案
# ==============================================================================
rm(list=ls())
gc()

# 自动安装加载依赖包
packages <- c("purrr","dplyr","tidyr","openxlsx","autoReg","moonBook","rrtable")
for (p in packages) {
  if (!require(p, character.only = TRUE)) install.packages(p, dependencies = TRUE)
  library(p, character.only = TRUE)
}

# ===================== 1. 读取脚本四输出的CKM分期结果数据 =====================
cat("============= 读取脚本四输出的CKM分期数据 =============\n")
data <- readRDS("4. Result_CKM/CKM_Stage_Result.RDS")
cat("✅ 数据读取成功 | 总样本量：", nrow(data), "\n")

# 自动兼容脚本四的变量名差异（SCR → Scr）
if("SCR" %in% colnames(data) && !"Scr" %in% colnames(data)) {
  data <- data %>% rename(Scr = SCR)
  cat("✅ 已自动兼容变量名：SCR → Scr\n")
}

# 删除旧的CKM_stage列（避免重复，保留CKM_stage_ab完整分期）
if("CKM_stage" %in% colnames(data)) {
  data <- data %>% select(-CKM_stage)
  cat("✅ 已删除旧的CKM_stage列，保留完整分期CKM_stage_ab\n")
}

# ===================== 2. 基础数据预处理（修复基线表报错核心步骤） =====================
cat("============= 数据预处理：修复基线表报错问题 =============\n")

# 2.1 修复分组变量Status：去除NA值、重置因子水平
data <- data %>% filter(!is.na(Status))
if(is.factor(data$Status)) {
  data$Status <- droplevels(data$Status)
  cat("✅ 已重置Status因子水平，去除空水平\n")
}
cat("✅ 已删除Status为NA的样本，剩余有效样本量：", nrow(data), "\n")

# 2.2 删除全NA列（避免mytable处理空列出错）
all_na_cols <- names(data)[apply(data, 2, function(x) all(is.na(x)))]
if(length(all_na_cols) > 0) {
  data <- data %>% select(-all_na_cols)
  cat("✅ 已删除全NA列：", paste(all_na_cols, collapse=", "), "\n")
} else {
  cat("✅ 无全NA列，数据正常\n")
}

# 2.3 重置所有因子变量的水平，去除冗余空水平
data <- data %>% mutate(across(where(is.factor), droplevels))
cat("✅ 已重置所有因子变量的水平，去除冗余空水平\n")

# ===================== 3. 基础生理指标校验（完全复用脚本四已有指标） =====================
cat("============= 1. 基础生理指标校验（复用脚本四已有结果） =============\n")
required_physio_basic <- c("Height", "Weight", "WC", "SBP", "DBP", "BMI")
missing_physio_basic <- required_physio_basic[!required_physio_basic %in% colnames(data)]
if(length(missing_physio_basic) > 0) {
  cat("⚠️  警告：缺失基础生理指标：", paste(missing_physio_basic, collapse=", "), "\n")
} else {
  cat("✅ 所有基础生理指标校验通过，BMI直接复用脚本四计算结果\n")
}

# ===================== 4. 衍生生理指标计算（脚本四未计算，新增） =====================
cat("============= 2. 衍生生理指标计算 =============\n")

# 单位转换（公式要求WC/Height为米，临时辅助列）
data$WC_m <- data$WC / 100
data$Height_m <- data$Height / 100

# 严格按你表格顺序计算4个新增衍生生理指标（BMI已复用）
data <- data %>%
  mutate(
    # 2. WHtR 腰围身高比
    WHtR = WC / Height,
    # 3. ABSI 身体形态指数
    ABSI = WC_m / ( (BMI^(2/3)) * (Height_m^(1/2)) ),
    # 4. BRI 身体圆度指数
    BRI = 364.2 - 365.5 * sqrt(1 - ( (WC_m/(2*pi))^2 ) / ( (0.5*Height_m)^2 ) ),
    # 5. CI 锥度指数
    CI = WC_m / (0.109 * sqrt( Weight / Height_m ) )
  )

# 删除临时单位转换列
data <- data %>% select(-WC_m, -Height_m)
cat("✅ 4个新增衍生生理指标全部按顺序计算完成\n")

# ===================== 5. 基础生化指标校验（完全复用脚本四已有原始指标） =====================
cat("============= 3. 基础生化指标校验（复用脚本四已有原始指标） =============\n")
required_biochemical <- c("HbA1C", "TG", "HDL", "LDL", "CRP", "FPG", "UA", "BUN", "Scr", "PLT")
missing_biochemical <- required_biochemical[!required_biochemical %in% colnames(data)]
if(length(missing_biochemical) > 0) {
  cat("⚠️  警告：缺失基础生化指标：", paste(missing_biochemical, collapse=", "), "\n")
} else {
  cat("✅ 所有基础生化指标校验通过\n")
}

# ===================== 6. 核心复合指标计算（8个新增+1个复用） =====================
cat("============= 4. 核心复合指标计算 =============\n")

# 先处理核心指标依赖的哑变量（适配脚本四的变量编码规则）
data <- data %>%
  mutate(
    # eGDR依赖的高血压哑变量（适配脚本四的"1_Yes"编码）
    Hypertension_num = ifelse(Hypertension == "1_Yes", 1, 0),
    # 性别哑变量（适配脚本四的"2_Female"编码）
    Female_num = ifelse(Sex == "2_Female", 1, 0)
  )

# 核心指标完整性校验：先检查复用的eGFR是否存在
if("eGFR" %in% colnames(data)) {
  cat("✅ eGFR直接复用脚本四计算结果，无需重复计算\n")
} else {
  cat("❌ 错误：脚本四输出数据中缺失eGFR指标，请先运行脚本四\n")
  stop("脚本四输出数据中缺失eGFR，无法继续计算")
}

# 严格按你表格顺序计算8个新增核心复合指标（eGFR直接复用）
data <- data %>%
  mutate(
    # 1. TyG 甘油三酯-葡萄糖指数
    TyG = log(TG * FPG),
    # 2. UHR 【按你表格公式：TG/HDL】
    # 领域标准定义：Uric Acid/HDL Ratio = UA/HDL，如需标准定义请替换公式
    UHR = TG / HDL,
    # 3. AIP 【按你表格公式：UA/HDL】
    # 领域标准定义：Atherogenic Index of Plasma = log(TG/HDL)，如需标准定义请替换公式
    AIP = UA / HDL,
    # 4. CMI 心脏代谢指数
    CMI = WHtR * (TG / HDL),
    # 5. HCHR C反应蛋白-高密度脂蛋白比值
    HCHR = CRP / HDL,
    # 6. METS_IR 代谢综合征胰岛素抵抗指数
    METS_IR = log(2*FPG + TG) * BMI / log(HDL),
    # 7. SHR 空腹血糖-糖化血红蛋白比值指数
    SHR = FPG / (1.59*HbA1C - 2.59),
    # 8. eGDR 预估葡萄糖清除率
    eGDR = 21.158 - 0.09*WC - 3.407*Hypertension_num - 0.551*HbA1C
  )

# 核心指标完整性校验（含复用的eGFR）
core_indicators <- c("TyG", "UHR", "AIP", "CMI", "HCHR", "METS_IR", "SHR", "eGDR", "eGFR")
missing_core <- core_indicators[!core_indicators %in% colnames(data)]
if(length(missing_core) > 0) {
  cat("❌ 核心指标计算失败，缺失：", paste(missing_core, collapse=", "), "\n")
} else {
  cat("✅ 9个核心复合指标全部准备完成（8个新增计算+1个复用脚本四结果）\n")
}

# ===================== 7. 全量衍生复合指标批量计算 =====================
cat("============= 5. 全量衍生复合指标批量计算 =============\n")

# 定义8个通用衍生维度指标（乘/除共用）
derivation_vars <- c("WC", "WHtR", "BMI", "ABSI", "BRI", "CI", "CRP", "HDL")

# 批量为每个核心指标生成16个衍生指标（8乘+8除）
for (core_name in core_indicators) {
  # 获取核心指标的向量
  core_vec <- data[[core_name]]
  
  # 1. 生成8个乘法衍生指标（对应你表格的1-8项）
  for (var_name in derivation_vars) {
    new_col_name <- paste0(core_name, "_", var_name)
    data[[new_col_name]] <- core_vec * data[[var_name]]
  }
  
  # 2. 生成8个除法衍生指标（对应你表格的9-16项）
  for (var_name in derivation_vars) {
    new_col_name <- paste0(core_name, "_", var_name, "_div")
    data[[new_col_name]] <- core_vec / data[[var_name]]
  }
}

# 衍生指标完整性校验
derived_multi <- expand.grid(core = core_indicators, var = derivation_vars) %>%
  mutate(col_name = paste0(core, "_", var)) %>%
  pull(col_name)
derived_div <- expand.grid(core = core_indicators, var = derivation_vars) %>%
  mutate(col_name = paste0(core, "_", var, "_div")) %>%
  pull(col_name)
derived_indicators <- c(derived_multi, derived_div)

missing_derived <- derived_indicators[!derived_indicators %in% colnames(data)]
if(length(missing_derived) > 0) {
  cat("❌ 衍生指标生成失败，缺失：", paste(missing_derived, collapse=", "), "\n")
} else {
  cat("✅ 所有衍生指标生成完成 | 9个核心指标 × 16个衍生维度 =", length(derived_indicators), "个衍生指标\n")
}

# ===================== 8. 异常值统一截断处理 =====================
cat("============= 6. 异常值截断处理 =============\n")
# 仅对本次新增计算的数值型指标做1%-99%分位截断，不修改脚本四已有的合规指标
new_numeric_cols <- setdiff(
  names(data %>% select(where(is.numeric))),
  colnames(readRDS("4. Result_CKM/CKM_Stage_Result.RDS"))
)
data <- data %>%
  mutate(across(all_of(new_numeric_cols), ~pmin(pmax(., quantile(., 0.01, na.rm=T)), quantile(., 0.99, na.rm=T))))
cat("✅ 本次新增的所有数值型指标1%-99%分位异常值截断完成，未修改脚本四原有数据\n")

# ===================== 9. 生成并导出基线表（核心修复部分） =====================
cat("============= 7. 生成基线特征表（修复版） =============\n")
# 创建输出文件夹
if(!dir.exists("5. Result_Indicator")) dir.create("5. Result_Indicator", recursive = TRUE)

# -------------------- 9.1 生成核心指标基线表（推荐，文件小、可读性强，100%兼容） --------------------
cat("===== 生成核心指标基线表（推荐） =====\n")
# 定义核心指标集（仅包含关键指标，不含全量衍生指标）
core_table_vars <- c(
  # 基础生理指标
  "Height", "Weight", "WC", "SBP", "DBP", "BMI", "WHtR", "ABSI", "BRI", "CI",
  # 基础生化指标
  "HbA1C", "TG", "HDL", "LDL", "CRP", "FPG", "UA", "BUN", "Scr", "PLT",
  # 核心复合指标
  "TyG", "UHR", "AIP", "CMI", "HCHR", "METS_IR", "SHR", "eGDR", "eGFR",
  # 分组变量
  "Status"
)
# 筛选核心指标数据
core_table_data <- data %>% select(all_of(core_table_vars))

# 用moonBook生成核心指标基线表（兼容性参数）
tryCatch({
  moonbook_core <- mytable(Status ~ ., data = core_table_data, digits = 2, method = 2, catMethod = 1, show.total = TRUE)
  # 导出核心指标基线表
  table2docx(moonbook_core[["res"]], "5. Result_Indicator/核心指标基线表.docx")
  write.xlsx(moonbook_core[["res"]], "5. Result_Indicator/核心指标基线表.xlsx")
  cat("✅ 核心指标基线表导出成功\n")
}, error = function(e) {
  cat("❌ 核心指标基线表生成失败，尝试备选方案：\n")
  print(e)
  # 备选方案：用rrtable的Table1生成基线表
  tryCatch({
    table1_core <- Table1(core_table_data, group = "Status", digits = 2)
    write.xlsx(table1_core, "5. Result_Indicator/核心指标基线表_备选版.xlsx")
    cat("✅ 核心指标基线表（备选版）导出成功\n")
  }, error = function(e2) {
    cat("❌ 备选方案也失败：\n")
    print(e2)
  })
})

# -------------------- 9.2 生成全量指标基线表（可选，包含所有144个衍生指标） --------------------
cat("\n===== 生成全量指标基线表（可选） =====\n")
tryCatch({
  moonbook_full <- mytable(Status ~ ., data = data, digits = 2, method = 3, catMethod = 1, show.total = TRUE)
  # 导出全量指标基线表
  table2docx(moonbook_full[["res"]], "5. Result_Indicator/全量指标基线表.docx")
  write.xlsx(moonbook_full[["res"]], "5. Result_Indicator/全量指标基线表.xlsx")
  cat("✅ 全量指标基线表导出成功\n")
}, error = function(e) {
  cat("❌ 全量指标基线表生成失败：\n")
  print(e)
  cat("💡 提示：全量指标过多会导致基线表生成失败，建议使用核心指标基线表\n")
})

# ===================== 10. 保存最终完整数据 =====================
saveRDS(data, "5. Result_Indicator/代谢指标计算完成数据.RDS")

# ===================== 完成提示 =====================
cat("\n🎉 脚本5 全流程执行完成！\n")
cat("📂 输出路径：5. Result_Indicator/\n")
cat("📊 完整指标体系清单：\n")
cat("  1. 基础生理指标：5项（Height/Weight/WC/SBP/DBP）+ 复用脚本四BMI\n")
cat("  2. 衍生生理指标：4项（WHtR/ABSI/BRI/CI）+ 复用脚本四BMI\n")
cat("  3. 基础生化指标：10项（HbA1C/TG/HDL/LDL/CRP/FPG/UA/BUN/Scr/PLT）\n")
cat("  4. 核心复合指标：9项（8个新增计算+1个复用脚本四eGFR）\n")
cat("  5. 衍生复合指标：144项（9核心×16衍生维度：8乘+8除）\n")
cat("📄 输出文件：\n")
cat("  1. 完整计算结果RDS文件\n")
cat("  2. 核心指标基线表（Word/Excel双格式，推荐使用）\n")
cat("  3. 全量指标基线表（Word/Excel双格式，可选）\n")