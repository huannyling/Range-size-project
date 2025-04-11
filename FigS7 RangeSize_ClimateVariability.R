library(CEMT) #my functions 
library(dplyr)
library(relaimpo)
library(glmnet)
library(caret)
library(lattice)
#extract clmate variables
clm0 <- read.csv('clm_var_variation_28.csv');head(clm0)
clmVar <- names(clm0)[1:28];
clmVar <- tolower(clmVar); clmVar
yVar <-c("Lat_range")
yMax <-c("extent.ymax")
#extract clm variables for specific locations (lat/lon)
#df<-read.csv("1-total_1970s_coldRng.csv")
df<-read.csv("2-family_average_North.csv")

data<-unique(df[,c(clmVar,yVar)])
#data<-unique(df[,c(clmVar,yMax)])
x <- as.matrix(scale(data[, clmVar])) # predictors
y <- scale(data$Lat_range) 
#y <- scale(data$extent.ymax) 

# Assuming 'x' is your matrix of predictors and 'y' is your dependent variable

# Initialize an empty data frame to store results
results <- data.frame(
  Variable = character(0),
  Coefficient = numeric(0),
  P_Value = numeric(0),
  stringsAsFactors = FALSE
)

# Perform linear regression for each predictor
for (i in seq_along(colnames(x))) {
  # Fit the model
  model <- lm(y ~ x[, i])
  
  # Extract the coefficient and p-value for the predictor
  coef_i <- coef(summary(model))[2, "Estimate"]
  p_value_i <- coef(summary(model))[2, "Pr(>|t|)"]
  
  # Append the results
  results <- rbind(results, data.frame(
    Variable = colnames(x)[i],
    Coefficient = coef_i,
    P_Value = p_value_i
  ))
}

# Print the results
print(results)

write.csv(results, "Lat_28clmVar_coef_pValue.csv")





#### range size varaibles selection ####
# 读取数据
df <- read.csv("2-family_average_North.csv")

# 设置变量
clmVar <- names(df)[12:95]  # 气候变量
yVar <- "Lat_range"         # 响应变量
data <- unique(df[, c(clmVar, yVar)])

# 标准化数据
x_scaled <- scale(data[, clmVar])
y_scaled <- scale(data[, yVar])
data_scaled <- as.data.frame(cbind(x_scaled, y_scaled))
names(data_scaled)[ncol(data_scaled)] <- yVar

# 全模型公式
full_formula <- as.formula(paste(yVar, "~", paste(clmVar, collapse = "+")))

# 空模型和全模型
null_model <- lm(as.formula(paste(yVar, "~ 1")), data = data_scaled)
full_model <- lm(full_formula, data = data_scaled)

# 执行逐步回归（双向逐步选择）
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "both", trace = TRUE)

# 查看最终模型的选择变量
summary(step_model)

# 提取选择的变量名称
selected_vars <- names(coef(step_model))[-1]  # 去掉截距
print("Selected variables for SEM:")
print(selected_vars)

model <- '
  # Range size 直接受到热量、水分和生长季因素影响
  Lat_range ~ dd5_at + dd_0_sm + tmax_tmin_at + ppt_at + cmd_wt + nffd_at + emt

  # 无霜期影响热量因子
  dd5_at ~ nffd_at + nffd_wt + nffd_sm
  dd_0_sm ~ nffd_sm

  # 降水影响水分赤字和蒸发
  cmd_wt ~ ppt_wt + pas_wt
  eref_at ~ ppt_at
  eref_wt ~ ppt_wt

  # tmax_tmin 表示温度变化范围，可能受到干燥度（ahm）影响
  tmax_tmin_at ~ ahm
  tmax_tmin_wt ~ ahm
'
library(lavaan)
fit <- lavaan::sem(model, data = data_scaled)
summary(fit, standardized = TRUE, fit.measures = TRUE)


model_improved <- '
  # Lat_range 的直接气候驱动
  Lat_range ~ dd5_at + tmax_tmin_at + ppt_at + cmd_wt + emt

  # 热量积累受无霜日影响
  dd5_at ~ nffd_at + nffd_wt

  # 夏季积温受夏季无霜日影响
  dd_0_sm ~ nffd_sm

  # 干旱指数受最小降水影响
  cmd_wt ~ pas_wt

  # 极端温差受干燥度指数影响
  tmax_tmin_at ~ ahm
'
fit_improved <- lavaan::sem(model_improved, data = data_scaled)
summary(fit_improved, fit.measures = TRUE, standardized = TRUE)


library(semTools)

# 获取修改指数（modification indices）
modification_suggestions <- modindices(fit_improved)

# 查看所有 MI 大于 10 的路径（通常更值得考虑）
modification_suggestions_filtered <- subset(modification_suggestions, mi > 10)

# 按 MI 从大到小排序
modification_suggestions_sorted <- modification_suggestions_filtered[order(-modification_suggestions_filtered$mi), ]

# 查看前10个建议路径
head(modification_suggestions_sorted, 10)



model_updated <- '
  Lat_range ~ dd5_at + dd_0_sm + tmax_tmin_at + ppt_at + cmd_wt + nffd_at + emt
  dd5_at ~ nffd_at + nffd_wt + nffd_sm
  dd_0_sm ~ nffd_sm
  cmd_wt ~ ppt_wt + pas_wt + nffd_wt + dd5_at + emt + nffd_at
  eref_at ~ ppt_at
  eref_wt ~ ppt_wt
  tmax_tmin_at ~ ahm + ppt_at
  tmax_tmin_wt ~ ahm
  pas_wt ~ cmd_wt
  ahm ~ tmax_tmin_at
'
fit_updated <- lavaan::sem(model_updated, data = data_scaled)
summary(fit_updated, fit.measures = TRUE, standardized = TRUE)
# 获取修改指数（modification indices）
modification_suggestions <- modindices(fit_updated )

# 查看所有 MI 大于 10 的路径（通常更值得考虑）
modification_suggestions_filtered <- subset(modification_suggestions, mi > 10)

# 按 MI 从大到小排序
modification_suggestions_sorted <- modification_suggestions_filtered[order(-modification_suggestions_filtered$mi), ]

# 查看前10个建议路径
head(modification_suggestions_sorted, 10)


model_refined <- '
  Lat_range ~ dd5_at + dd_0_sm + tmax_tmin_at + ppt_at + cmd_wt + nffd_at + emt
  dd5_at ~ nffd_at + nffd_wt + nffd_sm
  dd_0_sm ~ nffd_sm
  cmd_wt ~ ppt_wt + pas_wt + nffd_wt + dd5_at + emt + nffd_at
  eref_at ~ ppt_at + ppt_wt + eref_wt
  eref_wt ~ ppt_wt + ppt_at
  tmax_tmin_at ~ ppt_at + tmax_tmin_wt + dd5_at
  tmax_tmin_wt ~ ahm
  pas_wt ~ cmd_wt
'
fit_refined <- lavaan::sem(model_refined, data = data_scaled)
summary(fit_refined, fit.measures = TRUE, standardized = TRUE)
modificationIndices(fit_refined) |> subset(mi > 10)


model_final <- '
  # 响应变量
  Lat_range ~ dd5_at + dd_0_sm + tmax_tmin_at + ppt_at + cmd_wt + nffd_at + emt

  # dd5 和 dd_0 构建机制
  dd5_at ~ nffd_at + nffd_wt + nffd_sm
  dd_0_sm ~ nffd_sm

  # 干旱度
  cmd_wt ~ ppt_wt + pas_wt + nffd_wt + dd5_at + emt + nffd_at

  # tmax_tmin 表示日温差
  tmax_tmin_at ~ ppt_at + tmax_tmin_wt + dd5_at
  tmax_tmin_wt ~ ahm + cmd_wt + eref_at + eref_wt + pas_wt

  # 潜在蒸散 (EREF)
  eref_at ~ ppt_at + ppt_wt + eref_wt
  eref_wt ~ ppt_wt + ppt_at + dd5_at + cmd_wt + emt + nffd_at + nffd_wt

  # 辅助路径
  pas_wt ~ cmd_wt
'
fit_final <- lavaan::sem(model_final, data = data_scaled)

# 查看整体模型拟合
summary(fit_final, fit.measures = TRUE, standardized = TRUE)

# 可选：继续查看 MI 是否还有优化空间
modificationIndices(fit_final) |> subset(mi > 10)

model_final_v2 <- '
  # 1. 响应变量
  Lat_range ~ dd5_at + dd_0_sm + tmax_tmin_at + ppt_at + cmd_wt + nffd_at + emt + tmax_tmin_wt

  # 2. dd5 和 dd_0 的来源
  dd5_at ~ nffd_at + nffd_wt + nffd_sm
  dd_0_sm ~ nffd_sm

  # 3. 干旱度（cmd）受控制
  cmd_wt ~ ppt_wt + pas_wt + nffd_wt + dd5_at + emt + nffd_at

  # 4. 日温差
  tmax_tmin_at ~ ppt_at + tmax_tmin_wt + dd5_at + cmd_wt
  tmax_tmin_wt ~ ahm + cmd_wt + eref_at + eref_wt + pas_wt + emt

  # 5. 潜在蒸散
  eref_at ~ ppt_at + ppt_wt + eref_wt + dd5_at + cmd_wt + emt
  eref_wt ~ ppt_wt + ppt_at + dd5_at + cmd_wt + emt + nffd_at + nffd_wt + Lat_range

  # 6. 辅助
  pas_wt ~ cmd_wt
'
fit_final_v2 <- lavaan::sem(model_final_v2, data = data_scaled)

summary(fit_final_v2, fit.measures = TRUE, standardized = TRUE)

# 查看残差和 MI
modificationIndices(fit_final_v2) |> subset(mi > 10)


model_final_v3 <- '
  # 1. 响应变量
  Lat_range ~ dd5_at + dd_0_sm + tmax_tmin_at + ppt_at + cmd_wt + nffd_at + emt + tmax_tmin_wt

  # 2. dd5 和 dd_0 来源
  dd5_at ~ nffd_at + nffd_wt + nffd_sm
  dd_0_sm ~ nffd_sm

  # 3. 干旱度
  cmd_wt ~ ppt_wt + pas_wt + nffd_wt + dd5_at + emt + nffd_at + tmax_tmin_at + eref_at

  # 4. 日温差
  tmax_tmin_at ~ ppt_at + tmax_tmin_wt + dd5_at + cmd_wt + eref_at
  tmax_tmin_wt ~ ahm + cmd_wt + eref_at + eref_wt + pas_wt + emt + dd5_at

  # 5. 潜在蒸散
  eref_at ~ ppt_at + ppt_wt + eref_wt + dd5_at + cmd_wt + emt
  eref_wt ~ ppt_wt + ppt_at + dd5_at + cmd_wt + emt + nffd_at + nffd_wt + Lat_range

  # 6. 辅助路径
  pas_wt ~ cmd_wt

  # 7. 额外协变关系（可选）
  Lat_range ~~ eref_wt
'
fit_final_v3 <- lavaan::sem(model_final_v3, data = data_scaled)

summary(fit_final_v3, fit.measures = TRUE, standardized = TRUE)

modificationIndices(fit_final_v3) |> subset(mi > 10)
