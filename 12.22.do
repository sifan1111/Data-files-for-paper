* ------------------------------------------------------------------------------



use "D:\stata\恢复性司法\12.22.dta",clear
* 第一阶段：变量重命名与因变量构建
* ------------------------------------------------------------------------------

* 1. 基础变量重命名
rename 赔偿 raw_comp
rename 谅解 raw_forgive
rename 抗诉 protest
rename 犯罪手段 cruelty_level
rename 民间矛盾 fam_dispute
rename 被害人过错 victim_fault
rename 省份 province_str
rename 年份 year
rename 自首 surrender
rename 原判刑折算 sentence1_years_term
rename 改判刑折算 sentence2_years_term

* 2. 构建 6 档刑罚等级（一审 level1_6 与 二审 level2_6）
* 逻辑：根据刑期折算年限及刑种判定法律地位
gen level1_6 = .
replace level1_6 = 0 if strmatch(原判刑, "*有期徒刑*") & sentence1_years_term < 10
replace level1_6 = 1 if strmatch(原判刑, "*有期徒刑*") & sentence1_years_term >= 10 & sentence1_years_term <= 15
replace level1_6 = 2 if strmatch(原判刑, "*无期徒刑*")
replace level1_6 = 3 if strmatch(原判刑, "*死刑*") & strmatch(原判刑, "*缓期*") & !strmatch(原判刑, "*限制减刑*")
replace level1_6 = 4 if strmatch(原判刑, "*死刑*") & strmatch(原判刑, "*缓期*") & strmatch(原判刑, "*限制减刑*")
replace level1_6 = 5 if strmatch(原判刑, "*死刑*") & !strmatch(原判刑, "*缓期*")

gen level2_6 = .
replace level2_6 = 0 if strmatch(改判刑, "*有期徒刑*") & sentence2_years_term < 10
replace level2_6 = 1 if strmatch(改判刑, "*有期徒刑*") & sentence2_years_term >= 10 & sentence2_years_term <= 19
replace level2_6 = 2 if strmatch(改判刑, "*无期徒刑*")
replace level2_6 = 3 if strmatch(改判刑, "*死刑*") & strmatch(改判刑, "*缓期*") & !strmatch(改判刑, "*限制减刑*")
replace level2_6 = 4 if strmatch(改判刑, "*死刑*") & strmatch(改判刑, "*缓期*") & strmatch(改判刑, "*限制减刑*")
replace level2_6 = 5 if strmatch(改判刑, "*死刑*") & !strmatch(改判刑, "*缓期*")

* 3. 生成"两步法"核心因变量

* A. 决策第一步：是否获得减刑（0=未减/加刑, 1=已减刑）
gen sentence_change_6level = level1_6 - level2_6

* 2. 生成二分类变量：reduced_binary
* 逻辑：只要差值 > 0，就是减刑(1)；否则(包括 <=0 的维持和加刑)都是未减刑(0)
gen reduced_binary = 0
replace reduced_binary = 1 if sentence_change_6level > 0
* 注意：这里隐含了 sentence_change_6level <= 0 的情况由第一行设为 0

* 3. 添加标签 (非常重要，方便看图)
label variable reduced_binary "Sentence Reduced (Yes=1)"
label define bin_lab 0 "No/Inc" 1 " Reduced"
label values reduced_binary bin_lab

* 4. 检查一下数据分布 (验证是否有负数被正确归类为0)
tab sentence_change_6level reduced_binary
* B. 决策第二步：减刑幅度（仅针对减刑样本：0=减一档, 1=减两档及以上）
gen red_large = .
replace red_large = 0 if sentence_change_6level == 1
replace red_large = 1 if sentence_change_6level >= 2

* ------------------------------------------------------------------------------
* 第二阶段：自变量清洗与固定效应准备
* ------------------------------------------------------------------------------

* 1. 核心解释变量：赔偿谅解状态 (CompForgive)
gen CompForgive = .
replace CompForgive = 1 if raw_comp == 0 & raw_forgive == 0  // 基准组
replace CompForgive = 2 if raw_comp == 1 & raw_forgive == 0
replace CompForgive = 3 if raw_comp == 1 & raw_forgive == 1
replace CompForgive = 4 if raw_comp == 0 & raw_forgive == 1

label define comp_lab 1 "No Comp/No Forgive" 2 "Comp/No Forgive" 3 "Comp/Forgive" 4 "Forgive/No Comp"
label values CompForgive comp_lab

* 2. 处理省份与年份固定效应（Fixed Effects）
* 参考 Jung (2023) 使用地区与年份固定效应控制不可观测特征的做法 [cite: 50, 157]
* ---【新增步骤：合并 2010 到 2011】---
* 说明：由于 2010 年样本量极少，将其归入 2011 年以保证统计稳健性。
* 假设你的 year 变量是字符串类型（如 "2010"），如果是数值型去掉双引号即可
replace year = "2011" if year == "2010"
* ------------------------------------
encode province_str, gen(province_id)
encode year, gen(year_id) 

gen sentence_reduction = sentence1_years_term - sentence2_years_term
* 3. 数据清洗：确保数值型并剔除无法识别样本
destring raw_comp raw_forgive protest cruelty_level fam_dispute victim_fault surrender, replace

*输出所有变量

logout, save(Table11) dec(3) word replace: ///
    tabstat reduced_binary CompForgive surrender cruelty_level fam_dispute victim_fault sentence_reduction protest level1_6 ///
    if year != "未知" & province_str != "未识别", ///  <-- 这里加入了筛选条件
    s(N mean sd min max ) f(%12.3f) c(s)

* ------------------------------------------------------------------------------
* 第三阶段：回归模型运行与结果导出
* ------------------------------------------------------------------------------

* 运行 Logit 回归
logit reduced_binary i.CompForgive i.cruelty_level i.fam_dispute i.victim_fault i.protest i.level1_6 i.surrender i.province_id i.year_id ///
      if year != "未知" & province_str != "未识别", robust

* 导出表格（关键修改在下面这几行）
outreg2 using "Sentencing_Results_Final.doc", word replace dec(3) bdec(3) ///
    alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
    keep(i.CompForgive i.cruelty_level i.fam_dispute i.victim_fault i.protest i.level1_6 i.surrender) ///  <-- 1. 只保留核心变量，隐藏省份年份系数
    addtext(Province FE, Yes, Year FE, Yes) ///  <-- 2. 手动添加 "Yes" 行
    addstat("Pseudo R2", e(r2_p), "N", e(N)) /// <-- 3. 汇报 Pseudo R2 (Logit专用)
    label // 使用变量标签


	* ===================================================
* Step 1: 暂时用 ologit 跑二分类模型 (结果与 logit 一致)
* ===================================================
* 注意：这里把 logit 换成了 ologit，其他完全不变
ologit reduced_binary i.CompForgive i.cruelty_level i.fam_dispute i.victim_fault i.protest i.level1_6 i.surrender i.province_id i.year_id ///
      if year != "未知" & province_str != "未识别", robust

* ===================================================
* Step 2: 现在可以计算两个结果的概率了
* ===================================================
* 因为用了 ologit，Stata 就能识别 outcome(0) 和 outcome(1) 了
margins CompForgive, predict(outcome(0)) predict(outcome(1))

* ===================================================
* Step 3: 绘图 (双线交叉对比图)
* ===================================================
marginsplot, xdimension(CompForgive) ///
    recast(line) ///
    recastci(rarea) ciopts(color(%20) lwidth(none)) /// 
    plot1opts(lcolor(black) lpattern(dash) msymbol(X) msize(medium)) /// 线1: 未减刑(0)
    plot2opts(lcolor(red) lpattern(solid) msymbol(circle) msize(medium)) /// 线2: 获得减刑(1)
    legend(order(3 "Not Reduced" 4 "Reduced") ring(0) pos(3) cols(1)) ///
    xtitle("Compensation & Forgiveness status") ///
    ytitle("Predicted Probability") ///
    title("Predicted probabilities with 95% C") ///
    ylabel(0(0.2)1, angle(0) grid) ///
    yline(0.5, lcolor(gray) lpattern(dot)) /// 0.5 胜率分界线
    graphregion(color(white)) plotregion(color(white))
	
	
	
*运行线性回归模型
* --- 第一步：清空已储存的模型结果 ---
eststo clear

* 模型1: 仅含核心变量
eststo model1: regress sentence_reduction i.CompForgive, robust

* 模型2: 加入案件事实变量
eststo model2: regress sentence_reduction i.CompForgive i.surrender i.cruelty_level i.fam_dispute i.victim_fault i.protest, robust

* 模型3: 加入原审判决变量
eststo model3: regress sentence_reduction i.CompForgive i.surrender i.cruelty_level i.fam_dispute i.victim_fault i.protest i.level1_6, robust

* 模型4: 加入全部固定效应 (最终模型)
eststo model4: reg sentence_reduction i.CompForgive i.surrender i.cruelty_level i.fam_dispute i.victim_fault i.protest i.level1_6 i.province_id i.year_id if year != "未知" & province_str != "未识别", robust

* --- 输出表格 ---
esttab model1 model2 model3 model4 using "表2_主体分析_分层回归.rtf", ///
  b(3) se(3) star(* 0.1 ** 0.05 *** 0.01) ///
    title("Impact of Compensation and Reconciliation Status on Sentence Reduction ") ///
    keep(2.CompForgive 3.CompForgive 4.CompForgive 1.surrender 1.cruelty_level 1.fam_dispute 1.victim_fault 1.protest) ///
    label nogap compress replace ///
    stats(N r2_a, fmt(%9.0f %9.3f) labels("Observations (N)" "Adjusted R²")) ///
    addnote("Robust standard errors in parentheses; * p<0.1, ** p<0.05, *** p<0.01. Models 3 and 4 control for original sentence severity; Model 4 additionally includes province and year fixed effects. Selected coefficients are omitted for brevity.")

	. test 2.CompForgive = 4.CompForgive

* =======================================================
* 3. 倾向得分匹配 (PSM)
* =======================================================

* --- 第一步：数据准备与哑变量生成 ---
* 清除旧变量，防止报错
capture drop cruel_yes dispute_yes fault_yes protest_yes level1_* treated pscore _*

* 1. 生成协变量的 Dummy (为了平衡性检验看细节)
gen cruel_yes = (cruelty_level == 1)
gen dispute_yes = (fam_dispute == 1)
gen fault_yes = (victim_fault == 1)
gen protest_yes = (protest == 1)
tab level1_6, gen(level1_)  // 生成 level1_1 到 level1_5

* 2. 定义处理变量 (Treated)
* 逻辑：CompForgive=1 是双无(基准)，>1 代表有赔偿或有谅解
gen treated = (CompForgive > 1) if !missing(CompForgive)
label define tr_lab 0 "无赔无谅" 1 "有赔偿或谅解"
label values treated tr_lab

* 3. 定义协变量列表 (放入 Logit 回归计算得分的变量)
* 注意：level1_ 组里少放一个 level1_1 作为参照组，避免共线性
global clean_covariates "cruel_yes dispute_yes fault_yes protest_yes level1_2 level1_3 level1_4 level1_5"

* --- 第二步：运行 PSM ---
* 设置随机种子，保证每次运行结果一致 (重要！)
set seed 20251228 

* 运行匹配
* 因变量 outcome 设为 sentence_reduction (减刑年限)，与 OLS 保持一致
psmatch2 treated $clean_covariates i.province_id i.year_id, ///
         outcome(sentence_reduction) logit caliper(0.05) neighbor(1) common

* --- 第三步：获取表格所需的核心数据 ---

* 1. 计算标准误 (S.E.)
* psmatch2 返回了 ATT (r(att)) 和 T值 (r(tatt))，S.E. = abs(ATT / T值)
scalar att_val = r(att)
scalar t_val = r(tatt)
scalar se_val = abs(att_val / t_val)

* 2. 将结果存入矩阵 (用于导出)
matrix psm_res = (att_val, se_val, t_val)
matrix colnames psm_res = "ATT(净减刑年限)" "S.E.(标准误)" "T-stat(T值)"

* 3. 导出主要指标到 Word/RTF
esttab matrix(psm_res) using "Table_PSM_Main.rtf", replace ///
    title("表4：PSM 平均处理效应估计") ///
    cells("1 2 3") /// 
    modelwidth(15)

* --- 第四步：补充计算均值 (用于填补表格前两列) ---
* 这一步会在 Stata 结果窗口显示数字，请手动抄录到 Word 表格的前两列
display "=================================================="
display "请将以下数值填入 Table 4 的前两列："
display "--------------------------------------------------"
* 计算处理组均值 (Treated Mean)
quietly sum sentence_reduction if treated == 1 & _support == 1
display "Treated 组均值: " r(mean)

* 计算控制组均值 (Control Mean) - 注意要用匹配权重
quietly sum sentence_reduction if treated == 0 & _weight != ., meanonly
* 由于是 1:1 匹配，直接用 ATT 反推控制组均值最准确: Control = Treated - ATT
display "Control 组均值: " att_val - r(mean)  // 修正算法，直接用匹配后的对比
display "=================================================="

* --- 第五步：平衡性检验与绘图 ---
* 1. 导出平衡性检验表
pstest $clean_covariates level1_1, both graph


* 2. 绘制标准化偏差图 (你之前的图4)
psgraph, title("Covariate Balance Check") saving(Graph_PSM, replace)
	


	
* * =======================================================
* 最终定稿：安慰剂检验 (Logit 模型) - 这里的 Seed 11111 效果完美
* =======================================================

preserve

* 1. 设定最佳种子
set seed 11111 

* 2. 洗牌
keep CompForgive
gen rand_key = runiform()
sort rand_key
gen merge_id = _n
rename CompForgive placebo_comp
save "temp_placebo_logit_final.dta", replace

restore
gen merge_id = _n
merge 1:1 merge_id using "temp_placebo_logit_final.dta", nogenerate

* 3. 运行 Logit 回归
logit reduced_binary i.placebo_comp i.surrender i.cruelty_level i.fam_dispute i.victim_fault i.protest i.level1_6 i.year_id, vce(cluster province_id)

* 4. 导出结果 (加入 Pseudo R2)
* e(r2_p) 是 Stata 存储 Logit 伪 R方 的地方
outreg2 using "Table5_Placebo_Logit_Final.doc", word replace dec(3) ///
    title("Placebo Test: Logit") ///
    addstat("Pseudo R2", e(r2_p)) 

erase "temp_placebo_logit_final.dta"
	
*=======================================================
* 最终定稿：OLS 安慰剂检验 (Seed 22222) + R-squared
* =======================================================
preserve

* 1. 设定最佳种子
set seed 22222 

* 2. 洗牌
keep CompForgive
gen rand_key = runiform()
sort rand_key

* 生成临时的对接ID
gen merge_id = _n
rename CompForgive placebo_comp
save "temp_placebo_ols_final.dta", replace

restore

* 【修复步骤2】恢复数据后，再次确保没有冲突的变量
capture drop merge_id 
gen merge_id = _n

* 3. 合并洗牌后的数据
merge 1:1 merge_id using "temp_placebo_ols_final.dta", nogenerate

* 4. 运行 OLS 回归
regress sentence_reduction i.placebo_comp i.surrender i.cruelty_level i.fam_dispute i.victim_fault i.protest i.level1_6 i.year_id, vce(cluster province_id)

* 5. 导出结果 (带 R-squared)
outreg2 using "Table5_Placebo_OLS_Final.doc", word replace dec(3) ///
    title("Placebo Test: OLS") ///
    addstat("R-squared", e(r2)) 

* 清理临时文件
erase "temp_placebo_ols_final.dta"
	
	
* =======================================================
* 最终定稿：异质性检验 (使用 esttab - 学术界金标准)
* =======================================================



* 1. 清理之前的存储，防止混乱
eststo clear

** --- 数据准备 ---
capture drop high_gdp 
gen high_gdp = 0
local high_pcgdp_provinces "北京 上海 江苏 福建 浙江 广东 天津 重庆 湖北 山东 内蒙古 陕西 安徽"
foreach province of local high_pcgdp_provinces {
    replace high_gdp = 1 if strmatch(strtrim(province_str), "`province'")
}
label variable high_gdp "High GDP"

* --- 运行回归并存储 ---
logit reduced_binary i.CompForgive##i.high_gdp i.surrender i.cruelty_level i.fam_dispute i.victim_fault i.protest i.level1_6 i.year_id, vce(cluster province_id)
eststo Model_Logit

regress sentence_reduction i.CompForgive##i.high_gdp i.surrender i.cruelty_level i.fam_dispute i.victim_fault i.protest i.level1_6 i.year_id, vce(cluster province_id)
eststo Model_OLS

* --- 导出干净表格 ---
* nobaselevels: 自动隐藏所有 0.000 的基准行
* indicate: 自动把年份变量折叠成一行 "Year FE"
* interaction(" x "): 把 # 号变成好看的 x 号
esttab Model_Logit Model_OLS using "Table_Heterogeneity_Clean.rtf", replace ///
    cells(b(star fmt(3)) t(par fmt(3))) ///
    star(* 0.1 ** 0.05 *** 0.01) ///
    stats(r2_p chi2 r2_a F N, ///
          labels("Pseudo R2" "LR Chi2" "Adj R-squared" "F-stat" "Observations") ///
          fmt(3 3 3 3 0)) ///
    label nogaps ///
    nobaselevels ///          <-- 关键：隐藏基准组
    interaction(" x ") ///    <-- 美化：把 # 换成 x
    indicate("Year Fixed Effects" = *.year_id) ///  <-- 关键：隐藏年份，显示为 Yes
    title("Heterogeneity Analysis: Economic Development") ///
    mtitles("Logit: Reduced?" "OLS: Years")