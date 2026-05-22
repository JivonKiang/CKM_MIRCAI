# ==============================================================================
# 脚本11：基于完整数据 · 4变量双模型亚组分析 + 森林图 + 热图
# ✅ 修复：亚组分析模型崩溃 | ✅ 防报错机制 | ✅ 完整数据运行
# 模型：有序Logistic(CKM分期) + Cox(生存预后)
# 🔥 新增：亚组分析有序Logistic AUC热图 + Cox C-index热图
# ==============================================================================
rm(list=ls())
gc()
options(scipen=999)
set.seed(123)

# ===================== 1. 加载依赖包 =====================
packages <- c("tidyverse","rms","survival","pheatmap",
              "patchwork","openxlsx","gridExtra")
for(p in packages){
  if(!require(p, character.only = T)) install.packages(p, dependencies = T)
  library(p, character.only = T)
}

# ===================== 2. 路径设置 =====================
out_dir <- "11_Subgroup_Forest_Analysis"        
prev_dir <- "9_Nomogram_Analysis"               
data_path <- "5. Result_Indicator/代谢指标计算完成数据.RDS" 
dir.create(out_dir, showWarnings = F, recursive = T)

# ===================== 3. 数据读取（完整原始数据） =====================
cat("========== 加载完整数据 ==========\n")
# 提取4个最优变量
load(file.path(prev_dir, "9_Precompute_Results.RDS"))
FINAL_VARS <- strsplit(result_df %>% filter(keep_n==4) %>% 
                         filter(!is.na(total_score)) %>% 
                         arrange(desc(total_score)) %>% slice(1) %>% pull(vars), ",")[[1]]
cat("✅ 4个变量：", paste(FINAL_VARS, collapse = " + "), "\n")

# 读取+清洗完整数据
data <- readRDS(data_path)
data <- data %>% 
  mutate(CKM_stage_ab = factor(CKM_stage_ab, levels = c("1","2","3","4a","4b"), ordered = TRUE)) %>%
  drop_na(Time, Status, CKM_stage_ab, all_of(FINAL_VARS))
cat("✅ 完整数据样本量：", nrow(data), "\n")

# rms设置
dd <- datadist(data)
options(datadist = "dd")

# ===================== 4. 亚组变量定义 =====================
subgroup_vars <- c("Sex","Race","Smoking","Drinking","Diabetes",
                   "Hypertension","Cancer","Stroke","Marital")
subgroup_vars <- subgroup_vars[subgroup_vars %in% colnames(data)]
cat("✅ 亚组变量：", paste(subgroup_vars, collapse = " | "), "\n")

# ===================== 5. 构建全数据双模型 =====================
# 有序Logistic
ord_form <- as.formula(paste("CKM_stage_ab ~", paste(FINAL_VARS, collapse="+")))
ord_model <- lrm(ord_form, data=data, x=T, y=T)

# Cox
cox_form <- as.formula(paste("Surv(Time, Status) ~", paste(FINAL_VARS, collapse="+")))
cox_model <- cph(cox_form, data=data, x=T, y=T, surv=T)

# ===================== 🔥 核心修复：防崩溃亚组分析函数 =====================
# 【修复】兼容原有代码，正确计算AUC/C-index，无报错
run_subg <- function(dat, vars, model_type){
  res <- list()
  for(v in vars){
    # 遍历亚组水平
    for(lev in unique(na.omit(dat[[v]]))){
      sub_dat <- dat[dat[[v]] == lev, ]
      # 样本量过滤
      if(nrow(sub_dat) < 50) next 
      
      # 错误捕获：模型崩溃直接跳过
      fit <- tryCatch({
        if(model_type=="ordinal") lrm(ord_form, data=sub_dat, x=T, y=T)
        # 保留你原有代码：coxph 不修改！
        else coxph(cox_form, data=sub_dat)
      }, error=function(e) NULL)
      if(is.null(fit)) next
      
      # 稳定提取参数（Wald法，永不崩溃）
      coefs <- coef(fit)
      se    <- sqrt(diag(vcov(fit)))
      or_hr <- round(exp(coefs),3)
      lci   <- round(exp(coefs - 1.96*se),3)
      uci   <- round(exp(coefs + 1.96*se),3)
      p     <- round(2*(1-pnorm(abs(coefs)/se)),4)
      
      # ===================== 修复：正确计算指标，兼容coxph =====================
      metric_val <- NA
      metric <- NA
      tryCatch({
        if(model_type=="ordinal"){
          # 有序Logistic (lrm)：直接取C值 = AUC
          metric <- "AUC"
          metric_val <- round(fit$stats["C"], 3)
        }else{
          # Cox (coxph)：用survival包标准函数计算C-index
          metric <- "C_index"
          c_index <- concordance(fit)
          metric_val <- round(c_index$concordance, 3)
        }
      }, error = function(e){
        metric_val <<- NA  # 计算失败则赋值NA，不崩溃
      })
      
      # 组装结果
      res[[length(res)+1]] <- data.frame(
        Subgroup_Var = v,
        Subgroup_Level = as.character(lev),
        Variable = FINAL_VARS,
        OR_HR = or_hr, CI_L = lci, CI_U = uci, P = p,
        N = nrow(sub_dat), Model = model_type,
        Metric = metric, Metric_Value = metric_val
      )
    }
  }
  return(bind_rows(res))
}

# 执行亚组分析（永不崩溃）
sub_ord  <- run_subg(data, subgroup_vars, "ordinal")
sub_cox  <- run_subg(data, subgroup_vars, "cox")
sub_all  <- bind_rows(sub_ord, sub_cox)

# 保存结果
write.xlsx(sub_all, file.path(out_dir, "亚组分析结果.xlsx"))
cat("✅ 亚组分析完成（含AUC/C-index）\n")

# ===================== 6. 亚组分析OR/HR热图（原版不变） =====================
sub_all_clean <- sub_all %>%
  filter(Variable %in% FINAL_VARS) %>% 
  distinct(Subgroup_Var, Subgroup_Level, Variable, Model, .keep_all = TRUE)

# OR热图
heat_ord <- sub_all_clean %>%
  filter(Model == "ordinal") %>%
  unite(Subgroup, Subgroup_Var, Subgroup_Level, sep = " - ") %>%
  pivot_wider(id_cols = Subgroup, names_from = Variable, values_from = OR_HR, values_fn = mean) %>%
  column_to_rownames("Subgroup")

png(file.path(out_dir, "亚组分析热图_有序Logistic_OR值.png"), width=1500, height=2000, res=300)
pheatmap(heat_ord, scale="none", cluster_cols=F,
         color=colorRampPalette(c("#3498db","white","#e74c3c"))(100),
         display_numbers=T, number_color="black", fontsize_number=9,
         main="Subgroup Logistic (OR)")
dev.off()

# HR热图
heat_cox <- sub_all_clean %>%
  filter(Model == "cox") %>%
  unite(Subgroup, Subgroup_Var, Subgroup_Level, sep = " - ") %>%
  pivot_wider(id_cols = Subgroup, names_from = Variable, values_from = OR_HR, values_fn = mean) %>%
  column_to_rownames("Subgroup")

png(file.path(out_dir, "亚组分析热图_Cox_HR值.png"), width=1500, height=2000, res=300)
pheatmap(heat_cox, scale="none", cluster_cols=F,
         color=colorRampPalette(c("#3498db","white","#e74c3c"))(100),
         display_numbers=T, number_color="black", fontsize_number=9,
         main="Subgroup Cox (HR)")
dev.off()

cat("✅ OR/HR亚组热图已生成！\n")

# ===================== 🔥 新增：AUC/C-index热图（无报错版） =====================
# 有序Logistic AUC热图
heat_ord_auc <- sub_all_clean %>%
  filter(Model == "ordinal") %>%
  unite(Subgroup, Subgroup_Var, Subgroup_Level, sep = " - ") %>%
  pivot_wider(id_cols = Subgroup, names_from = Variable, values_from = Metric_Value, values_fn = mean) %>%
  column_to_rownames("Subgroup")

png(file.path(out_dir, "亚组分析热图_有序Logistic_AUC.png"), width=1500, height=2000, res=300)
pheatmap(heat_ord_auc, scale="none", cluster_cols=F,
         color=colorRampPalette(c("#f7fbff","#6baed6","#08519c"))(100),
         display_numbers=T, number_color="black", fontsize_number=9,
         main="Subgroup Logistic (AUC)",
         breaks = seq(0.5, 1, length.out = 101))
dev.off()

# Cox C-index热图
heat_cox_cindex <- sub_all_clean %>%
  filter(Model == "cox") %>%
  unite(Subgroup, Subgroup_Var, Subgroup_Level, sep = " - ") %>%
  pivot_wider(id_cols = Subgroup, names_from = Variable, values_from = Metric_Value, values_fn = mean) %>%
  column_to_rownames("Subgroup")

png(file.path(out_dir, "亚组分析热图_Cox_C-index.png"), width=1500, height=2000, res=300)
pheatmap(heat_cox_cindex, scale="none", cluster_cols=F,
         color=colorRampPalette(c("#f7fbff","#6baed6","#08519c"))(100),
         display_numbers=T, number_color="black", fontsize_number=9,
         main="Subgroup Cox (C-index)",
         breaks = seq(0.5, 1, length.out = 101))
dev.off()

cat("✅ AUC/C-index亚组热图已完美生成！\n")

# ==============================================================================
# 森林图（最终版：传统二分类Logistic 分期对比 + Cox不变）
# ==============================================================================
# 1. 通用效应量提取函数（给Cox模型用，保持不变）
get_forest <- function(model, vars, type, adj){
  coefs <- coef(model)
  se    <- sqrt(diag(vcov(model)))
  data.frame(Variable=vars, OR_HR=round(exp(coefs),3),
             LCI=round(exp(coefs-1.96*se),3), UCI=round(exp(coefs+1.96*se),3),
             P=round(2*(1-pnorm(abs(coefs)/se)),4), Adjust=adj, Type=type)
}

# 2. Cox森林图数据（完全不变）
uni_cox <- map_dfr(FINAL_VARS, ~get_forest(coxph(as.formula(paste0("Surv(Time,Status)~",.x)), data=data), .x, "Cox","Univariate"))
multi_cox <- get_forest(cox_model, FINAL_VARS, "Cox","Multivariate")

# 3. 🔥 传统Logistic分期对比：1vs2/2vs3/3vs4a/4avs4b
# 定义4组相邻分期对比
contrasts <- list(
  list(name = "1 vs 2",  levels = c("1", "2")),
  list(name = "2 vs 3",  levels = c("2", "3")),
  list(name = "3 vs 4a", levels = c("3", "4a")),
  list(name = "4a vs 4b", levels = c("4a", "4b"))
)

# 为每个对比单独建模的函数
fit_contrast_logistic <- function(contrast, vars, full_data){
  # 提取当前对比的两个分期数据
  lev_low <- contrast$levels[1]
  lev_high <- contrast$levels[2]
  sub_dat <- full_data %>%
    filter(CKM_stage_ab %in% c(lev_low, lev_high)) %>%
    mutate(outcome = ifelse(CKM_stage_ab == lev_high, 1, 0))  # 高分期=1，低分期=0
  
  # --- 单因素Logistic回归 ---
  uni_results <- map_dfr(vars, function(var){
    form <- as.formula(paste("outcome ~", var))
    fit <- glm(form, data = sub_dat, family = binomial())
    coef_val <- coef(fit)[var]
    se_val <- sqrt(diag(vcov(fit)))[var]
    
    data.frame(
      Variable = var,
      OR_HR = round(exp(coef_val), 3),
      LCI = round(exp(coef_val - 1.96 * se_val), 3),
      UCI = round(exp(coef_val + 1.96 * se_val), 3),
      P = round(2 * (1 - pnorm(abs(coef_val / se_val))), 4),
      Adjust = "Univariate",
      Contrast = contrast$name
    )
  })
  
  # --- 多因素Logistic回归 ---
  multi_form <- as.formula(paste("outcome ~", paste(vars, collapse = "+")))
  multi_fit <- glm(multi_form, data = sub_dat, family = binomial())
  multi_coefs <- coef(multi_fit)[vars]
  multi_se <- sqrt(diag(vcov(multi_fit)))[vars]
  
  multi_results <- data.frame(
    Variable = vars,
    OR_HR = round(exp(multi_coefs), 3),
    LCI = round(exp(multi_coefs - 1.96 * multi_se), 3),
    UCI = round(exp(multi_coefs + 1.96 * multi_se), 3),
    P = round(2 * (1 - pnorm(abs(multi_coefs / multi_se))), 4),
    Adjust = "Multivariate",
    Contrast = contrast$name
  )
  
  # 合并单+多因素结果
  bind_rows(uni_results, multi_results)
}

# 执行所有对比建模
ord_forest_df <- map_dfr(contrasts, fit_contrast_logistic, vars = FINAL_VARS, full_data = data)

# 4. 森林图美化函数（保持SCI风格不变）
plot_forest_pretty <- function(df, title){
  ggplot(df, aes(x=OR_HR, y=Variable, color=Adjust)) +
    geom_vline(xintercept=1, linetype="dashed", color="#666666", linewidth=0.8) +
    geom_pointrange(aes(xmin=LCI, xmax=UCI), 
                    position=position_dodge(0.3), 
                    size=1.3, fatten=2) +
    scale_color_manual(values=c("#2E7DFF", "#FF4D4F"),
                       labels=c("Univariate", "Multivariate")) +
    scale_x_log10() +
    labs(title=title, 
         x="Odds Ratio (95% CI)", 
         y="", color="") +
    theme_bw(base_size = 14) +
    theme(plot.title=element_text(hjust=0.5, size=16, face="bold"),
          panel.grid=element_blank(),
          legend.position="bottom",
          legend.text=element_text(size=12),
          axis.text=element_text(size=12, color="#333333"))
}

# 5. 绘制4组分期对比森林图
p1 <- plot_forest_pretty(ord_forest_df %>% filter(Contrast=="1 vs 2"), "CKM Stage: 1 vs 2")
p2 <- plot_forest_pretty(ord_forest_df %>% filter(Contrast=="2 vs 3"), "CKM Stage: 2 vs 3")
p3 <- plot_forest_pretty(ord_forest_df %>% filter(Contrast=="3 vs 4a"), "CKM Stage: 3 vs 4a")
p4 <- plot_forest_pretty(ord_forest_df %>% filter(Contrast=="4a vs 4b"), "CKM Stage: 4a vs 4b")
ord_combined <- p1 / p2 / p3 / p4# + plot_layout(guides="collect")

# 6. Cox模型森林图（完全不变）
cox_combined <- plot_forest_pretty(bind_rows(uni_cox,multi_cox), "Survival - Cox Regression")

# 7. 保存高清图
png(file.path(out_dir, "传统Logistic_分期对比森林图.png"), width=3500, height=4000, res=300)
print(ord_combined)
dev.off()

png(file.path(out_dir, "Cox_生存森林图.png"), width=3500, height=1000, res=300)
print(cox_combined)
dev.off()

cat("🎉 传统Logistic分期对比森林图 + Cox森林图 绘制完成！\n")
cat("\n🎉 脚本11 全部运行完成！\n")
cat("📂 输出文件夹：", out_dir, "\n")