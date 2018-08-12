library(readxl)
library(ggplot2)
# library(showtext)
# library(sysfonts)
# library(showtextdb)
# opar <- par(no.readonly = TRUE)
# par(family='STKaiti')
# par(opar)
consume_loan <- read_excel('/Users/hbsz/Downloads/Consume_loan.xlsx', sheet = 1, na = 'NA')
str(consume_loan)
# qplot(城市, 申请人数, data = consume_loan, ylab = '申请人数')
# qplot(城市, 申请人数, data = consume_loan, ylab = '申请人数', geom = 'boxplot')
# qplot(城市, data = consume_loan, geom = 'bar', theme_text(family=’STKaiti’))
p <- ggplot(consume_loan, aes(x = 还款总费用))
p + geom_histogram() + facet_wrap(~担保方式) + theme_grey(base_family = 'STKaiti')
qplot(还款总费用, ..density.., data = consume_loan, geom = 'histogram', facets = 担保方式~., binwidth = 0.1)+ theme_grey(base_family = 'STKaiti')
library(xtable)
as.data.frame(table(consume_loan$城市))
as.data.frame(table(consume_loan$还款方式))
as.data.frame(table(consume_loan$担保方式))
as.data.frame(table(consume_loan$期限最高范围))
as.data.frame(table(consume_loan$审批时间))
as.data.frame(table(consume_loan$放款日期))

qplot(还款总费用, data = consume_loan, geom = c('histogram', 'line'), binwidth = 0.1) + theme_grey(base_family = 'STKaiti') 
qplot(期限最高范围, data = consume_loan, geom = 'histogram', binwidth = 5, xlim = c(0, 130)) + theme_grey(base_family = 'STKaiti')

p + geom_bar() + xlab('City') + ylab('Count') +theme_grey(base_family = 'STKaiti')
large_than_0_row <- subset(consume_loan, subset = 申请人数>0)
str(large_than_0_row)
summary(large_than_0_row$申请人数)
p1 <- ggplot(large_than_0_row, aes(申请人数))
p1 + geom_histogram(binwidth = 10) + xlab('People') + ylab('Count') +theme_grey(base_family = 'STKaiti')
p2 <- ggplot(large_than_0_row, aes(城市,申请人数))
p2 + geom_boxplot()
table(large_than_0_row$申请人数)
quantile(large_than_0_row$申请人数, probs = 0.8)
p3 <- ggplot(consume_loan, aes(申请人数))
p3 + geom_histogram() + facet_wrap(consume_loan$担保方式)
table(consume_loan$担保方式)

equal_0_row <- subset(consume_loan, subset = 申请人数==0)
qplot(还款总费用, data = large_than_0_row, geom = 'histogram', binwidth = 0.1)
qplot(还款总费用, data = equal_0_row, geom = 'histogram', binwidth = 0.1)
table(consume_loan$放款日期)
table(consume_loan$审批时间)
tempt <- subset(consume_loan, subset = 还款方式=='随借随还')

free_data <- subset(consume_loan, subset = 担保方式=='自由选')


# 模型，多元回归
consume_loan_model <- consume_loan[, c(2,6,9,10,11,12,13,14)]
str(consume_loan_model)
consume_loan_model$放款日期 <- as.numeric(consume_loan_model$放款日期)
consume_loan_model[is.na(consume_loan_model$放款日期), 6] <- 3 
consume_loan_model[is.na(consume_loan_model$放款日期), 5] <- 0 
consume_loan_model$城市 <- as.factor(consume_loan_model$城市)
consume_loan_model$还款方式 <- as.factor(consume_loan_model$还款方式)
consume_loan_model$担保方式 <- as.factor(consume_loan_model$担保方式)
consume_loan_model$期限最高范围 <- consume_loan_model$期限最高范围/12
table(consume_loan_model$期限最高范围)
table(consume_loan_model$担保方式)
# consume_loan_model[is.na(consume_loan_model), ]
# which(is.na(consume_loan_model))
fit <- lm(申请人数 ~ 还款总费用 + 期限最高范围 + 还款方式 + 放款日期 + 审批时间 + 担保方式, data = consume_loan_model)
summary(fit)
# 发现很多变量不显著，R2值很低，认为存在共线性
# 对还款方式和担保方式做卡方检验（列联表检验），检查独立性
df <- table(consume_loan_model$还款方式, consume_loan_model$担保方式)
chisq.test(df)
# 数据不均衡，所以卡方检验可能不准确
# 现在通过可视化检查担保方式对申请人数分布的影响
# 通过密度曲线发现抵押贷和信用贷的申请人数分布相似
qplot(申请人数, ..density.., data = consume_loan_model, geom = 'histogram', facets = 担保方式~., binwidth = 11, xlim = c(0,1000), ylim = c(0, 0.01))+ theme_grey(base_family = 'STKaiti')
qplot(申请人数,  data = consume_loan_model, geom = 'histogram', facets = 担保方式~., binwidth = 11, xlim = c(0,1000), ylim = c(0, 80))+ theme_grey(base_family = 'STKaiti')
# 画qq图比较两者的分布
colla <- subset(consume_loan_model$申请人数, subset = consume_loan_model$担保方式=='抵押贷')
credit <- subset(consume_loan_model$申请人数, subset = consume_loan_model$担保方式=='信用贷')
quantile(colla, probs = 0.95)
colla <- colla[colla<=794.75]
quantile(credit, probs = 0.95)
credit <- credit[credit <= 5518.7]
qqplot(colla, credit)
# ks检验结果两者分布不一样
ks.test(colla, credit)
# 陷入僵局
fit <- lm(申请人数 ~ 放款日期 + 审批时间, data = consume_loan_model)
summary(fit)
# 描述性统计量
p2 <- ggplot(consume_loan_model, mapping = aes(x = 还款总费用))
p2 + geom_histogram(aes(y = ..density..), binwidth = 0.1, fill = 'steelblue') + theme_grey(base_family = 'STKaiti') + geom_density()
p2 + geom_density() + theme_grey(base_family = 'STKaiti')
p3 <- ggplot(consume_loan_model, aes(x = 期限最高范围))
p3 + geom_histogram() + theme_grey(base_family = 'STKaiti') 
p5 <- ggplot(consume_loan_model, aes(x = 放款日期))
p5 + geom_histogram(binwidth = 1, fill = 'steelblue', colour = 'black') + theme_grey(base_family = 'STKaiti') + scale_x_continuous(breaks = c(0:31))
p6 <- ggplot(consume_loan_model, aes(x = 审批时间))
p6 + geom_histogram(binwidth = 1, fill = 'steelblue', colour = 'black') + theme_grey(base_family = 'STKaiti') + scale_x_continuous(breaks = c(0:21))
p8 <- ggplot(consume_loan_model, mapping = aes(x = 申请人数))
p8 + geom_density() + theme_grey(base_family = 'STKaiti')
# 研究不同变量与申请人数的关系
# 现在将目标变量变成分类变量
sum(consume_loan_model$申请人数==0)
table(large_than_0_row$申请人数)
length(unique(large_than_0_row$申请人数))
apply_count <- large_than_0_row$申请人数
quantile(apply_count, probs = 0.25)
tapply(apply_count, factor(unique(apply_count)), quantile)
factor(apply_count)
data1 <- subset(consume_loan_model, subset = 还款方式=='到期还款')
data2 <- subset(consume_loan_model, subset = 还款方式=='随借随还')
log_apply =log(apply_count)
qplot(log_apply, geom = 'histogram')
bank_row <- c(grep('银行', consume_loan$公司名称))
bank_data <- consume_loan[bank_row, ]
sum(bank_data$申请人数==0)
qplot(log(放款日期), data = consume_loan_model, geom = 'density')
qplot(log(审批时间), data = consume_loan_model, geom = 'density')
qplot(放款日期, data = consume_loan_model, geom = 'density')
qplot(审批时间, data = consume_loan_model, geom = 'density')
quantile(large_than_0_row$申请人数, probs = 0.70)

# 现在进行数据预处理
consume_loan_model <- consume_loan[, c(2,6,9,10,11,12,13,14)]
consume_loan_model$放款日期 <- as.numeric(consume_loan_model$放款日期)
consume_loan_model[is.na(consume_loan_model$放款日期), 6] <- 3 
consume_loan_model[is.na(consume_loan_model$放款日期), 5] <- 0 
# consume_loan_model$城市 <- as.factor(consume_loan_model$城市)
# consume_loan_model$还款方式 <- as.factor(consume_loan_model$还款方式)
# consume_loan_model$担保方式 <- as.factor(consume_loan_model$担保方式)
# 城市分成一线城市和非一线城市，标记为1，0
consume_loan_model[consume_loan_model$城市 %in% c('北京','上海','广州','深圳'), 1] = '1'
consume_loan_model[consume_loan_model$城市 != '1', 1] = '0'
# 期限最高范围除以12
consume_loan_model$期限最高范围 <- consume_loan_model$期限最高范围/12
# 申请人数取log
consume_loan_model[consume_loan_model$申请人数>0, 8] = log(consume_loan_model[consume_loan_model$申请人数>0, 8])
qplot(申请人数, data = consume_loan_model, geom = 'histogram')
# 对不对审批日期和放款实践取log处理
qplot(log(审批时间)+1, data = consume_loan_model, geom = 'density')
# 680行审批时间有一个缺失值，用众数填补
consume_loan_model[which(is.na(consume_loan_model$审批时间)), 6] = 1
consume_loan_model[consume_loan_model$审批时间>0, 6] = log(consume_loan_model[consume_loan_model$审批时间>0, 6])
consume_loan_model[consume_loan_model$放款日期>0, 5] = log(consume_loan_model[consume_loan_model$放款日期>0, 5])
# 添加银行非银行限制
bank_row <- c(grep('银行',consume_loan$公司名称))
not_bank_row <- c(1:1046)[-bank_row]
consume_loan_model[bank_row, '公司性质'] = 1
consume_loan_model[not_bank_row, '公司性质'] = 0
# 去除数据前后5%，其中某些缺失值应该先删掉

# 先不对还款方式和担保方式进行重抽样
# 简单拟合一下
consume_loan_model_fit <- consume_loan
consume_loan_model_fit$还款方式 <- as.factor(consume_loan_model_fit$还款方式)
consume_loan_model_fit$担保方式 <- as.factor(consume_loan_model_fit$担保方式)
fit <- lm(申请人数 ~ 还款总费用 + 期限最高范围 + 还款方式 + 放款日期 + 审批时间 + 担保方式 + 城市, data = consume_loan_model)
summary(fit)
tstep <- step(fit)
summary(tstep)
summary(drop1(tstep))
fit <- lm(申请人数 ~ 还款方式  + 放款日期 + 审批时间  + 城市, data = consume_loan_model)
summary(fit)
# 效果很差

consume_loan_model

consume_loan_model_temp <- consume_loan
consume_loan_model_temp$城市 <- as.factor(consume_loan_model_temp$城市)
consume_loan_model_temp$期限最高范围 <- as.factor(consume_loan_model_temp$期限最高范围)
consume_loan_model_temp$还款方式 <- as.factor(consume_loan_model_temp$还款方式)
consume_loan_model_temp$担保方式 <- as.factor(consume_loan_model_temp$担保方式)
consume_loan_model_temp$公司性质 <- as.factor(consume_loan_model_temp$公司性质)
consume_loan_model_temp$放款日期 <- as.numeric(consume_loan_model_temp$放款日期)
consume_loan_model_temp$审批时间 <- as.numeric(consume_loan_model_temp$审批时间)
consume_loan_model_temp$一线城市 <- as.numeric(consume_loan_model_temp$一线城市)

consume_loan_model_temp[consume_loan_model_temp$城市 %in% c('北京','上海','广州','深圳'), '一线城市'] = '1'
consume_loan_model_temp[consume_loan_model_temp$城市 != '1', '一线城市'] = '0'

str(consume_loan_model_temp)
fit <- lm(申请人数 ~ 还款总费用 + 还款方式+放款日期+审批时间+担保方式 + 公司性质 +有车+有房+年龄限制+具体性+有工作+一线城市, data = consume_loan_model_temp)
summary(fit)
step(fit)
sum(consume_loan$申请人数 == 0)
consume_loan[consume_loan$申请人数==0, '申请人数类别'] = '1'
consume_loan[consume_loan$申请人数>0 & consume_loan$申请人数<=5.493, '申请人数类别'] = '2'
consume_loan[consume_loan$申请人数>5.493, '申请人数类别'] = '3'
model1 <- polr(as.factor(申请人数类别) ~还款总费用 + 还款方式+放款日期+审批时间+担保方式 + 公司性质 +有车+有房+年龄限制+具体性+有工作+一线城市, method = 'logistic', Hess = T, data = consume_loan_model_temp)
summary(model1)
str(consume_loan_model_temp)
