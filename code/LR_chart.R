zhixin <- read.table('/Users/heisenberg/Downloads/研究生课件/Intern/Task_three/zhixin_test.csv', 
                     header = T, sep = ',',fileEncoding = 'gbk')
names(zhixin)[6] <- 'score'

score_iv_ks <- iv_ks_for_score_2(zhixin, 'score', 'is_bad', 310, 790, 15)
write.csv(score_iv_ks, '/Users/heisenberg/Downloads/研究生课件/Intern/Task_three/score_result.csv')

score_iv_ks_ <- iv_ks_details(zhixin, 'score', 'is_bad')
write.csv(score_iv_ks_, '/Users/heisenberg/Downloads/研究生课件/Intern/Task_three/score_result_.csv')
sum(zhixin$is_bad==1)

# ROC plot
plot(
  score_iv_ks$acc_pct_0,
  score_iv_ks$acc_pct_1,
  'l',
  main = 'ROC',
  xlab = 'FPR',
  ylab = 'TPR',
  xlim = c(0, 1),
  ylim = c(0, 1),
  col = 'skyblue2',
  lwd = 3
)
lines(range(0, 1), range(0, 1), 'l', col = 'pink2', lwd = 3)
legend(0, 1, legend = c('score', 'baseline'), col = c('skyblue2', 'pink2'), pch = 15, cex = 0.9)
axis(1,at=seq(0,1,0.1))
grid()


# Gain Chart
plot(
  score_iv_ks$op,
  score_iv_ks$acc_pct_1,
  'l',
  main = 'Gain Chart',
  xlab = 'OP',
  ylab = 'Recall',
  xlim = c(0, 1),
  ylim = c(0, 1),
  col = 'skyblue2',
  lwd = 3
)
lines(range(0, 1), range(0, 1), 'l', col = 'pink2', lwd = 3)
legend(0, 1, legend = c('score', 'baseline'), col = c('skyblue2', 'pink2'), pch = 15, cex = 0.9)
axis(1,at=seq(0,1,0.1))
grid()



# Lift Chart
plot(
  score_iv_ks$op,
  score_iv_ks$lift_pre,
  'l',
  main = 'Lift Chart',
  xlab = 'OP',
  ylab = 'Lift',
  xlim = c(0, 1),
  ylim = c(0.5, 3),
  col = 'skyblue2',
  lwd = 3
)
lines(range(0, 1), range(1, 1), 'l', col = 'pink2', lwd = 3)
legend(0.75, 3, legend = c('score', 'baseline'), col = c('skyblue2', 'pink2'), pch = 15, cex = 0.9)
axis(1,at=seq(0.1,1,0.1))
grid()



# P/R
plot(
  score_iv_ks$acc_pct_1,
  score_iv_ks$acc_pre,
  'l',
  main = 'P-R Curve',
  xlab = 'Recall',
  ylab = 'Precision',
  xlim = c(0, 1),
  ylim = c(0, 1),
  col = 'skyblue2',
  lwd = 3
)
len = nrow(score_iv_ks)
pre = as.double(score_iv_ks$acc_pre[len - 2])
lines(range(0, 1), range(pre, pre), 'l', col = 'pink2', lwd = 3)
legend(0.75, 1, legend = c('score', 'baseline'), col = c('skyblue2', 'pink2'), pch = 15, cex = 0.9)
axis(1,at=seq(0,1,0.1))
grid()