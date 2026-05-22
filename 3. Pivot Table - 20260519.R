# ====================== 脚本 3：基线特征分析 ======================
# 适配：合并清洗后的最终数据（承接前面的清洗插补脚本）
rm(list=ls())

# ====================== 自动安装/加载依赖包（已补全缺失包） ======================
packages <- c(
  "data.table","dplyr","ggplot2","patchwork",
  "moonBook","writexl","pheatmap","RColorBrewer","tidyr","tibble"
)
for (p in packages) {
  if (!require(p, character.only = TRUE)) install.packages(p, dependencies = TRUE)
  library(p, character.only = TRUE)
}

# ====================== 标准化文件夹结构 ======================
main_dir <- "3. baseline analysis"
sub_dir <- file.path(main_dir, "single_plots")
if (!dir.exists(main_dir)) dir.create(main_dir, recursive = TRUE)
if (!dir.exists(sub_dir)) dir.create(sub_dir, recursive = TRUE)

# ====================== 读取合并清洗后的最终数据 ======================
df_all <- readRDS("Result_Clean_Impute/最终合规数据.RDS")

# ====================== 数据检查 ======================
cat("============== 检查合并后的最终数据 ==============\n")
cat("数据维度：", nrow(df_all), "行 ×", ncol(df_all), "列\n")
cat("数据集分组：", unique(df_all$Dataset), "\n")
cat("生存状态：", unique(df_all$Status), "\n")
cat("种族变量：", unique(df_all$Race), "\n\n")

# ====================== 变量定义 ======================
cont_vars <- c("Age","Height","Weight","BMI","WC","SBP","DBP","HbA1C","FPG",
               "TC","HDL","LDL","TG","CRP","UA","BUN","SCR","PLT")
cat_vars <- c("Dataset", "Status", "CVD", "Race", "Chest_Pain", "Dyslipidemia", "Kidney_Disease")

df_all <- df_all %>% select(all_of(c(cont_vars, cat_vars)))

# ====================== 基线表分析 ======================
cat("正在生成基线表...\n")
tb_all <- mytable(
  Dataset ~ Age + Height + Weight + BMI + WC + SBP + DBP + HbA1C + FPG +
    TC + HDL + LDL + TG + CRP + UA + BUN + SCR + PLT + 
    Status + CVD + Race + Chest_Pain + Dyslipidemia + Kidney_Disease,
  data = df_all, method = 3, digits = 2
)

tb_status <- mytable(
  Dataset + Status ~ ., 
  data = df_all %>% select(-Race),
  method = 3, digits = 2
)

tb_cvd <- mytable(
  Dataset + CVD ~ ., 
  data = df_all %>% select(-Race), 
  method = 3, digits = 2
)

baseline_tables <- list(
  "整体基线对比"       = tb_all$res,                
  "1数据集_生存状态"    = tb_status[[1]]$res,
  "2数据集_生存状态"    = tb_status[[2]]$res,  
  "1数据集_心血管疾病"   = tb_cvd[[1]]$res,  
  "2数据集_心血管疾病"   = tb_cvd[[2]]$res            
)
write_xlsx(baseline_tables, path = file.path(main_dir, "01_全套基线表.xlsx"))

# ====================== 绘图主题 ======================
my_theme <- theme_bw() +
  theme(
    plot.title = element_text(hjust=0.5, size=13, face="bold"),
    axis.text = element_text(size=10),
    legend.position = "bottom",
    panel.grid = element_line(linetype = "dashed")
  )
color_db <- c("NHANES"="#4361EE", "CHARLS"="#F72585")

# ====================== 分类变量堆叠图 ======================
p1 <- df_all %>% count(Dataset, Status) %>% group_by(Dataset) %>% mutate(pct=n/sum(n)*100) %>%
  ggplot(aes(x=Dataset, y=pct, fill=factor(Status))) +
  geom_col(position="stack", color="white", linewidth=0.5) +
  scale_fill_manual(values=c("0"="#E3F2FD","1"="#FFEBEE"), name="生存状态\n(0=存活,1=死亡)") +
  labs(title="生存状态分布", y="百分比(%)") + my_theme

p2 <- df_all %>% count(Dataset, CVD) %>% group_by(Dataset) %>% mutate(pct=n/sum(n)*100) %>%
  ggplot(aes(x=Dataset, y=pct, fill=factor(CVD))) +
  geom_col(position="stack", color="white", linewidth=0.5) +
  scale_fill_manual(values=c("No"="#E3F2FD","Yes"="#FFEBEE"), name="CVD") +
  labs(title="心血管疾病分布", y="百分比(%)") + my_theme

p_stack <- p1 + p2 + plot_layout(ncol=2)
ggsave(filename = file.path(sub_dir, "01_生存状态.png"), plot = p1, width=7, height=5, dpi=300)
ggsave(filename = file.path(sub_dir, "02_CVD分布.png"), plot = p2, width=7, height=5, dpi=300)
ggsave(filename = file.path(main_dir, "02_分类变量堆叠图合集.png"), plot = p_stack, width=14, height=6, dpi=300)

# ====================== 云雨图 ======================
cat("正在绘制连续变量云雨图...\n")
for(v in cont_vars){
  p <- ggplot(df_all, aes(x = Dataset, y = .data[[v]], fill = Dataset)) +
    geom_violin(alpha = 0.7, width = 0.8) +
    geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
    geom_jitter(shape = 21, size = 0.8, alpha = 0.4, width = 0.1) +
    scale_fill_manual(values = color_db) +
    labs(title = paste0("变量：", v), y = "测量值") +
    my_theme
  
  ggsave(filename = file.path(sub_dir, paste0("03_云雨图_", v, ".png")), 
         plot = p, width=7, height=5, dpi=300)
}

core <- c("Age","BMI","SBP","FPG","TC","HDL")
p_rain <- df_all %>% 
  select(Dataset, all_of(core)) %>% 
  pivot_longer(-Dataset) %>%
  ggplot(aes(x = Dataset, y = value, fill = Dataset)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  facet_wrap(~name, scales = "free_y") +
  scale_fill_manual(values = color_db) +
  labs(title = "核心连续变量基线对比（云雨图）") +
  my_theme

ggsave(filename = file.path(main_dir, "03_核心指标云雨图合集.png"), 
       plot = p_rain, width=16, height=10, dpi=300)

# ====================== ✅修复版：指标对比热图 ======================
mean_mat <- df_all %>% 
  group_by(Dataset) %>% 
  summarise(across(all_of(cont_vars), mean, na.rm=T)) 

# 修复：base R 原生写法，永不报错
rownames(mean_mat) <- mean_mat$Dataset
mean_mat <- mean_mat %>% select(-Dataset)

png(filename = file.path(main_dir, "04_指标对比热图.png"), 
    width=3000, height=1800, res=300)
pheatmap(
  t(scale(t(mean_mat))), 
  color = colorRampPalette(rev(brewer.pal(9,"RdBu")))(100),
  main = "NHANES vs CHARLS 标准化指标对比",
  cluster_rows = F, border_color = "white", show_numbers = T
)
dev.off()

# ====================== 完成提示 ======================
cat("\n✅ 脚本3 全部执行完成！适配合并后的最终数据！\n")
cat("📂 结果路径：", main_dir, "\n")
cat("📊 已生成：1个多工作表Excel基线表 + 全套可视化图表\n")