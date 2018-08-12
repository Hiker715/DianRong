library(readxl)
library(ggplot2)
consume_loan <- read_excel('/Users/hbsz/Downloads/Consume_loan.xlsx', sheet = 1, na = 'NA')
str(consume_loan)
consume_loan <- consume_loan[, -c(3, 4, 8)]
consume_loan <- consume_loan[, -c(3, 5)]
null_posi <- which(is.na(consume_loan), arr.ind = TRUE)
consume_loan <- na.omit(consume_loan)
total_row_count <- nrow(consume_loan)
upper_limit <- quantile(consume_loan$申请人数, probs = 0.95)
consume_loan <- consume_loan[consume_loan$申请人数<upper_limit, ]
apply_0_row <- c(which(consume_loan$申请人数 == 0))
apply_0_row_sample <- sample(apply_0_row, floor(total_row_count*0.05), replace = FALSE)
consume_loan <- consume_loan[-apply_0_row_sample, ]
# 可视化描述
library(ggplot2)
reorder_size <- function(x){
  factor(x, levels = names(sort(table(x), decreasing = TRUE)))
}
p_city <- ggplot(consume_loan, aes(x = reorder_size(城市)))
city_pic <- p_city + geom_bar(fill = 'steelblue') + geom_text(aes(label = ..count.. ), stat = 'count', vjust = -0.5) + theme_grey(base_family = 'STKaiti') + theme(axis.title = element_text(size=13), axis.text = element_text(size=12, face = 'bold'))
city_pic
repay_freq <- data.frame(table(consume_loan$还款方式))
repay_freq <- repay_freq[order(repay_freq$Freq, decreasing = TRUE), ]
p_repay <- ggplot(repay_freq, aes(x = '', y = Freq, fill = Var1))
my_label <- as.vector(repay_freq$Var1)
my_label <- paste(my_label, '(', round(repay_freq$Freq/sum(repay_freq$Freq)*100, 2), '%)', sep = '')
repay_pic <- p_repay + geom_bar(stat = 'identity', width = 0.3) + coord_polar(theta = 'y') + theme_grey(base_family = 'STKaiti') + labs(x = '', y = '', title = '') + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), panel.grid=element_blank(), panel.border=element_blank()) + scale_fill_discrete(breaks = repay_freq$Var1, labels = my_label)  + theme(legend.text = element_text(size=16)) + guides(fill = guide_legend(title = NULL))
repay_pic
guran_freq <- data.frame(table(consume_loan$担保方式))
guran_freq <- guran_freq[order(guran_freq$Freq, decreasing = TRUE), ]
p_guran <- ggplot(guran_freq, aes(x = '', y = Freq, fill = Var1))
my_label <- as.vector(guran_freq$Var1)
my_label <- paste(my_label, '(', round(guran_freq$Freq/sum(guran_freq$Freq)*100, 2), '%)', sep = '')
guran_pic <- p_guran + geom_bar(stat = 'identity', width = 0.3) + coord_polar(theta = 'y') + theme_grey(base_family = 'STKaiti') + labs(x = '', y = '', title = '') + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), panel.grid=element_blank(), panel.border=element_blank()) + scale_fill_discrete(breaks = guran_freq$Var1, labels = my_label)  + theme(legend.text = element_text(size=16)) + guides(fill = guide_legend(title = NULL))
guran_pic
bank_row <- c(grep('银行',consume_loan$公司名称))
not_bank_row <- c(1:nrow(consume_loan))[-bank_row]
consume_loan[bank_row, '公司性质'] = '银行类'
consume_loan[not_bank_row, '公司性质'] = '非银行类'
comp_freq <- data.frame(table(consume_loan$公司性质))
comp_freq <- comp_freq[order(comp_freq$Freq, decreasing = TRUE), ]
p_comp <- ggplot(comp_freq, aes(x = '', y = Freq, fill = Var1))
my_label <- as.vector(comp_freq$Var1)
my_label <- paste(my_label, '(', round(comp_freq$Freq/sum(comp_freq$Freq)*100, 2), '%)', sep = '')
comp_pic <- p_comp + geom_bar(stat = 'identity', width = 0.3) + coord_polar(theta = 'y') + theme_grey(base_family = 'STKaiti') + labs(x = '', y = '', title = '') + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), panel.grid=element_blank(), panel.border=element_blank()) + scale_fill_discrete(breaks = comp_freq$Var1, labels = my_label)  + theme(legend.text = element_text(size=16)) + guides(fill = guide_legend(title = NULL))
comp_pic
# layout <- matrix(c(1,2,3,4), nrow = 2, byrow = TRUE)
# multiplot(plotlist = list(city_pic, repay_pic, guran_pic, comp_pic), layout = layout)
# library(gridExtra)
# grid.arrange(city_pic, repay_pic, guran_pic, comp_pic, ncol = 2, nrow = 2)
p_cost <- ggplot(consume_loan, aes(x = 还款总费用))
cost_pict <- p_cost + geom_histogram(aes(y = ..density..), binwidth = 0.2, fill = 'steelblue', color = 'black') + theme_grey(base_family = 'STKaiti') + geom_density() + labs(x = '还款总费用（万元）', y = '密度', title = '') + scale_x_continuous(breaks = c(1:6), labels = c(1:6))
cost_pict
p_uptime <- ggplot(consume_loan, aes(x = as.factor(期限最高范围)))
uptime_pic <- p_uptime + geom_bar(fill = 'steelblue') + geom_text(aes(label = ..count.. ), stat = 'count', vjust = -0.5) + theme_grey(base_family = 'STKaiti') + theme(axis.title = element_text(size=13), axis.text = element_text(size=12, face = 'bold')) + labs(x = '期限最高范围（月）', y = '计数', title = '')
uptime_pic

p_paytime <- ggplot(consume_loan, aes(x = factor(放款日期, levels = sort(as.numeric(unique(consume_loan$放款日期))))))
paytime_pic <- p_paytime + geom_bar(fill = 'steelblue') + geom_text(aes(label = ..count.. ), stat = 'count', vjust = -0.5) + theme_grey(base_family = 'STKaiti') + theme(axis.title = element_text(size=13), axis.text = element_text(size=12, face = 'bold')) + labs(x = '放款日期（天）', y = '计数', title = '')
paytime_pic
p_examtime <- ggplot(consume_loan, aes(x = factor(审批时间, levels = sort(as.numeric(unique(consume_loan$放款日期))))))
examtime_pic <- p_examtime + geom_bar(fill = 'steelblue') + geom_text(aes(label = ..count.. ), stat = 'count', vjust = -0.5) + theme_grey(base_family = 'STKaiti') + theme(axis.title = element_text(size=13), axis.text = element_text(size=12, face = 'bold')) + labs(x = '审批时间（天）', y = '计数', title = '')
examtime_pic
p_apply <- ggplot(consume_loan_model, aes(x = log(1 + 申请人数)))
apply_pict <- p_apply + geom_histogram(aes(y = ..density..), binwidth = 0.25, fill = 'steelblue', color = 'black') + theme_grey(base_family = 'STKaiti') + geom_density() + labs(x = 'log(申请人数)', y = '密度', title = '') + scale_x_continuous(breaks = c(2:9), labels = c(2:9))
apply_pict
consume_loan <- consume_loan_model
# 变量处理
consume_loan[consume_loan$还款方式 %in% c('到期还款', '随借随还'), '还款方式'] = '非分期还款'
consume_loan[consume_loan$担保方式 %in% c('自由选', '担保贷'), '担保方式'] = '自由选或担保贷'
consume_loan[consume_loan$期限最高范围 %in% c(60, 120, 360), '期限最高范围'] = 60
consume_loan[, '期限最高范围'] = consume_loan[, '期限最高范围']/12
consume_loan[as.numeric(consume_loan$放款日期) >= 6, '放款日期'] = '7'
consume_loan[consume_loan$审批时间 >= 4, '审批时间'] = '5'
consume_loan[, '申请人数'] = round(log(consume_loan[, '申请人数'] + 1), 3)
write.csv(consume_loan, file = '/Users/hbsz/Downloads/consume_loan_after.csv')
# python 部分，用与词频分析及添加申请条件引出的虚拟变量
import pandas as pd
import numpy as np
import jieba

consume_loan = pd.read_csv('/Users/hbsz/Downloads/consume_loan_after.csv')
apply_requst = ''.join(str(consume_loan.loc[: ,'申请条件']))
apply_requst =  ' '.join(jieba.cut(apply_requst))
count_dict = {}
apply_requst = apply_requst.split(' ')
for item in apply_requst:
  if item not in count_dict:
  count_dict[item] = 1
else:
  count_dict[item] += 1
count_dict = sorted(count_dict.items(), key = lambda item: item[1], reverse = True)

def contain_any(strs, lists):
  for item in lists:
  if item in strs:
  return True
return False
house_list = ['房产','按揭','房','商品房']
car_list = ['车辆','牌照','车']
job_list = ['工作','流水','记录','收入','工资','劳动合同','社保','公积金']
age_list = ['周岁','年龄']
reside_list = ['本地','居住','住址','常住','地区','固定']
control_list = ['以上','单位','满','半年','营业执照','不高','公务员','做生意','上班族','白领','月薪','事业','国有','公立','上海', '北京', '南京', '天津', '广州', '成都', '杭州', '济南', '深圳', '西安', '重庆', '青岛','以内','大于','至少']
for i in range(len(consume_loan)):
  if contain_any(consume_loan.loc[i, '申请条件'], house_list):
  consume_loan.loc[i, '有房'] = 1
if contain_any(consume_loan.loc[i, '申请条件'], car_list):
  consume_loan.loc[i, '有车'] = 1
if contain_any(consume_loan.loc[i, '申请条件'], job_list):
  consume_loan.loc[i, '有工作'] = 1
if contain_any(consume_loan.loc[i, '申请条件'], age_list):
  consume_loan.loc[i, '年龄限制'] = 1
if contain_any(consume_loan.loc[i, '申请条件'], reside_list):
  consume_loan.loc[i, '居住限制'] = 1
if contain_any(consume_loan.loc[i, '申请条件'], control_list):
  consume_loan.loc[i, '具体性'] = 1
consume_loan.to_csv('/Users/hbsz/Downloads/consume_loan_after2.csv')

# 读取数据以及建模
consume_loan <- read.csv('/Users/hbsz/Downloads/consume_loan_after1.csv', na = 'NA')
consume_loan[is.na(consume_loan)] <- 0
consume_loan <- consume_loan[, c(4:11, 13:20)]
consume_loan[consume_loan$城市 %in% c('北京','上海','广州','深圳'), '一线城市'] = '1'
consume_loan[is.na(consume_loan)] <- 0
# 用consume_loan_model_temp 测试模型
consume_loan_model_temp <- consume_loan


consume_loan_model_temp$期限最高范围 <- as.factor(consume_loan_model_temp$期限最高范围)
consume_loan_model_temp$还款方式 <- as.factor(consume_loan_model_temp$还款方式)
consume_loan_model_temp$担保方式 <- as.factor(consume_loan_model_temp$担保方式)
consume_loan_model_temp$公司性质 <- as.factor(consume_loan_model_temp$公司性质)
consume_loan_model_temp$放款日期 <- as.numeric(consume_loan_model_temp$放款日期)
consume_loan_model_temp$审批时间 <- as.numeric(consume_loan_model_temp$审批时间)
consume_loan_model_temp$一线城市 <- as.numeric(consume_loan_model_temp$一线城市)

str(consume_loan_model_temp)

fit <- lm(申请人数 ~ 还款总费用 + 期限最高范围 + 还款方式 + 放款日期 + 审批时间+担保方式 + 公司性质 + 有车 + 有房 + 年龄限制 + 具体性 + 有工作 + 居住限制 + 一线城市, data = consume_loan_model_temp)
summary(fit)
step(fit)
fit <- lm(申请人数 ~ 放款日期 + 审批时间+担保方式 + 公司性质 +  有房 + 年龄限制 + 具体性 + 有工作 + 居住限制 + 一线城市, data = consume_loan_model_temp)
summary(fit)
# 自由选或担保贷不显著，将其和抵押贷合并
levels(consume_loan_model_temp$担保方式)[levels(consume_loan_model_temp$担保方式) %in% c("抵押贷", "自由选或担保贷")] <- "非信用贷"
consume_loan_model_temp$还款方式 <- as.factor(consume_loan_model_temp$还款方式)
consume_loan_model_temp$担保方式 <- as.factor(consume_loan_model_temp$担保方式)
consume_loan_model_temp$公司性质 <- as.factor(consume_loan_model_temp$公司性质)
consume_loan_model_temp$放款日期 <- as.numeric(consume_loan_model_temp$放款日期)
consume_loan_model_temp$审批时间 <- as.numeric(consume_loan_model_temp$审批时间)
consume_loan_model_temp$一线城市 <- as.numeric(consume_loan_model_temp$一线城市)
fit <- lm(申请人数 ~ 还款总费用 + 期限最高范围 + 还款方式 + 放款日期 + 审批时间+担保方式 + 公司性质 + 有车 + 有房 + 年龄限制 + 具体性 + 有工作 + 居住限制 + 一线城市 + 抽象性, data = consume_loan_model_temp)
summary(fit)
step(fit)
fit <- lm(申请人数 ~ 还款方式 + 放款日期 + 审批时间+ 公司性质 +  年龄限制 + 具体性 + 有工作 +  一线城市 + 抽象性, data = consume_loan_model_temp)
summary(fit)
library(car)
vif(fit)
# 现在考虑将审批时间和放款日期加和处理
consume_loan_model_temp['流程工作日'] <- consume_loan_model_temp['放款日期'] + consume_loan_model_temp['审批时间']
fit <- lm(申请人数 ~ 还款总费用 + 期限最高范围 + 还款方式 + 流程工作日  +担保方式 + 公司性质 + 有车 + 有房 + 年龄限制 + 具体性 + 有工作 + 居住限制 + 一线城市 + 抽象性, data = consume_loan_model_temp)
summary(fit)
step(fit)
fit <- lm(申请人数 ~ 还款总费用 + 还款方式 +  公司性质 +  年龄限制 + 具体性 + 有工作 +  一线城市 + 抽象性, data = consume_loan_model_temp)
summary(fit)
# 有些变量不显著了
fit <- lm(申请人数 ~ 还款方式 +  公司性质 +  年龄限制 + 具体性 + 有工作 +  一线城市 + 抽象性, data = consume_loan_model_temp)
summary(fit)
# 逐步回归并不能去除多重共线性，要预先通过检验（自相关）发现有共线性，找到比较相关的变量，剔除其中一个
cor(consume_loan[, c(2,3,5,6,10,11,12,13,14,15,16,17)])

fit <- lm(申请人数 ~ 还款总费用 + 期限最高范围 + 还款方式 +  流程工作日+担保方式 + 公司性质 + 有车 + 有房 + 年龄限制 + 具体性 + 有工作 + 居住限制 + 一线城市 + 抽象性, data = consume_loan_model_temp)
summary(fit)
step(fit)
fit2 <- lm(申请人数 ~ 还款总费用 + 还款方式 + 公司性质 +  年龄限制 + 具体性 + 有工作 +  一线城市 + 抽象性, data = consume_loan_model_temp)
summary(fit2)
library(broom)
tidy(fit2)