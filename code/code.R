library(woe)
library(ggplot2)

# Calculate the iv, ks value summary for the dataset
#
# Args:
#   dataset: data frame with the input variables
#   varnames: variable name list for which iv, ks will be calculated
#   y: target column name
#   is_numeric: flag for continuous/categorical variable
#
# Returns:
#   data frame with iv, ks summary for all the input variables
iv_ks_summary <- function(dataset, varnames, y, is_numeric = TRUE) {
  iv_info <- iv.mult(dataset, y, TRUE, vars = varnames)
  var_num <- length(iv_info$Variable)
  
  iv <- vector(mode = "numeric", length = var_num)
  iv <- t(t(iv))
  ks <- vector(mode = "numeric", length = var_num)
  ks <- t(t(ks))
  i <- 1
  while (i <= var_num) {
    var_name <- iv_info$Variable[i]
    if (is_numeric) {
      var_details <- iv_ks_details(dataset, var_name, y)
    } else {
      var_details <- iv_ks_details(dataset, var_name, y, FALSE)
    }
    iv[i] <- tail(var_details$iv, 1)
    ks[i] <- max(var_details$ks)
    i <- i + 1
  }
  
  summary <-
    data.frame(iv_info$Variable, ks, iv, stringsAsFactors = FALSE)
  names(summary) <- c("variable_name", "ks", "iv")
  return (summary)
}

# Calculate the iv, ks value details for a specific variable
#
# Args:
#   dataset: data frame with the input variables
#   x: variable name for which iv, ks will be calculated
#   y: target column name
#   is_numeric: flag for continuous/categorical variable
#
# Returns:
#   data frame with iv, ks details
iv_ks_details <- function(dataset, x, y, is_numeric = TRUE) {
  if (is_numeric) {
    iv_info <- iv.num(dataset, x, y)
  } else {
    dataset[, x] <- as.character(dataset[, x])
    iv_info <- iv.str(dataset, x, y)
  }
  
  bins <-
    data.frame(
      as.character(iv_info$class),
      iv_info$outcome_0 + iv_info$outcome_1,
      iv_info$outcome_0,
      iv_info$outcome_1,
      iv_info$pct_0,
      iv_info$pct_1,
      iv_info$odds,
      iv_info$woe,
      stringsAsFactors = FALSE
    )
  names(bins) <-
    c("bin",
      "bin_total",
      "cnt_0",
      "cnt_1",
      "pct_0",
      "pct_1",
      "odds",
      "woe")
  return (cal_iv_ks_details(bins))
}


# Calculate the iv, ks value details for a specific score (high -> bad, low -> good)
#
# Args:
#   dataset: data frame with the input variables
#   x: score for which iv, ks will be calculated
#   y: target column name
#   min_score: minimum score value
#   max_score: maximum score value
#   interval: score band interval
#
# Returns:
#   data frame with iv, ks details
iv_ks_for_score <- function(dataset, x, y, min_score, max_score, interval) {
  bin_num <- as.integer((max_score - min_score) / interval)
  total_0 <- nrow(subset(dataset, dataset[y] == 0))
  total_1 <- nrow(subset(dataset, dataset[y] == 1))
  
  class <- vector(mode = "character", length = bin_num)
  outcome_0 <- vector(mode = "numeric", length = bin_num)
  outcome_1 <- vector(mode = "numeric", length = bin_num)
  pct_0 <- vector(mode = "numeric", length = bin_num)
  pct_1 <- vector(mode = "numeric", length = bin_num)
  odds <- vector(mode = "numeric", length = bin_num)
  woe <- vector(mode = "numeric", length = bin_num)
  
  for (i in 1:bin_num) {
    high <- max_score - (i - 1) * interval
    low <- high - interval
    if (i == bin_num) {
      class[i] <- paste("[", low, ";",  high, "]")
      outcome_0[i] <-
        nrow(subset(dataset, dataset[x] <= high &
                      dataset[x] >= low & dataset[y] == 0))
      outcome_1[i] <-
        nrow(subset(dataset, dataset[x] <= high &
                      dataset[x] >= low & dataset[y] == 1))
    } else {
      class[i] <- paste("(", low, ";",  high, "]")
      outcome_0[i] <-
        nrow(subset(dataset, dataset[x] <= high &
                      dataset[x] > low & dataset[y] == 0))
      outcome_1[i] <-
        nrow(subset(dataset, dataset[x] <= high &
                      dataset[x] > low & dataset[y] == 1))
    }
    pct_0[i] <- outcome_0[i] / total_0
    pct_1[i] <- outcome_1[i] / total_1
    odds[i] <- pct_0[i] / (pct_1[i] + 0.0001)
    if (pct_0[i] == 0 || pct_1[i] == 0) {
      woe[i] <- 0
    } else {
      woe[i] <- log(odds[i])
    }
  }
  
  bins <-
    data.frame(
      class,
      outcome_0 + outcome_1,
      outcome_0,
      outcome_1,
      pct_0,
      pct_1,
      odds,
      woe,
      stringsAsFactors = FALSE
    )
  names(bins) <-
    c("bin",
      "bin_total",
      "cnt_0",
      "cnt_1",
      "pct_0",
      "pct_1",
      "odds",
      "woe")
  return (cal_iv_ks_details(bins))
}


# Calculate the iv, ks value details for a specific score (high -> good, low -> bad)
#
# Args:
#   dataset: data frame with the input variables
#   x: score for which iv, ks will be calculated
#   y: target column name
#   min_score: minimum score value
#   max_score: maximum score value
#   interval: score band interval
#
# Returns:
#   data frame with iv, ks details
iv_ks_for_score_2 <- function(dataset, x, y, min_score, max_score, interval) {
  bin_num <- as.integer((max_score - min_score) / interval)
  total_0 <- nrow(subset(dataset, dataset[y] == 0))
  total_1 <- nrow(subset(dataset, dataset[y] == 1))
  
  class <- vector(mode = "character", length = bin_num)
  outcome_0 <- vector(mode = "numeric", length = bin_num)
  outcome_1 <- vector(mode = "numeric", length = bin_num)
  pct_0 <- vector(mode = "numeric", length = bin_num)
  pct_1 <- vector(mode = "numeric", length = bin_num)
  odds <- vector(mode = "numeric", length = bin_num)
  woe <- vector(mode = "numeric", length = bin_num)
  
  for (i in 1:bin_num) {
    low <- min_score + (i - 1) * interval
    high <- low + interval
    if (i == 1) {
      class[i] <- paste("[", low, ";",  high, "]")
      outcome_0[i] <-
        nrow(subset(dataset, dataset[x] <= high &
                      dataset[x] >= low & dataset[y] == 0))
      outcome_1[i] <-
        nrow(subset(dataset, dataset[x] <= high &
                      dataset[x] >= low & dataset[y] == 1))
    } else {
      class[i] <- paste("(", low, ";",  high, "]")
      outcome_0[i] <-
        nrow(subset(dataset, dataset[x] <= high &
                      dataset[x] > low & dataset[y] == 0))
      outcome_1[i] <-
        nrow(subset(dataset, dataset[x] <= high &
                      dataset[x] > low & dataset[y] == 1))
    }
    pct_0[i] <- outcome_0[i] / total_0
    pct_1[i] <- outcome_1[i] / total_1
    odds[i] <- pct_0[i] / (pct_1[i] + 0.0001)
    if (pct_0[i] == 0 || pct_1[i] == 0) {
      woe[i] <- 0
    } else {
      woe[i] <- log(odds[i])
    }
  }
  
  bins <-
    data.frame(
      class,
      outcome_0 + outcome_1,
      outcome_0,
      outcome_1,
      pct_0,
      pct_1,
      odds,
      woe,
      stringsAsFactors = FALSE
    )
  names(bins) <-
    c("bin",
      "bin_total",
      "cnt_0",
      "cnt_1",
      "pct_0",
      "pct_1",
      "odds",
      "woe")
  return (cal_iv_ks_details(bins))
}


# Calculate the iv, ks value details given bin counts
#
# Args:
#   bin_data: data frame with bin counts for each binning category
#
# Returns:
#   data frame with iv, ks details for each binning category
cal_iv_ks_details <- function(bin_data) {
  bin_num <- nrow(bin_data)
  acc_pct_0 <- vector(mode = "numeric", length = bin_num)
  acc_pct_0 <- t(t(acc_pct_0))
  acc_pct_1 <- vector(mode = "numeric", length = bin_num)
  acc_pct_1 <- t(t(acc_pct_1))
  pct_diff <- vector(mode = "numeric", length = bin_num)
  pct_diff <- t(t(pct_diff))
  iv <- vector(mode = "numeric", length = bin_num)
  iv <- t(t(iv))
  ks <- vector(mode = "numeric", length = bin_num)
  ks <- t(t(ks))
  op <- vector(mode = "numeric", length = bin_num)
  op <- t(t(op))
  acc_pre <- vector(mode = "numeric", length = bin_num)
  acc_pre <- t(t(acc_pre))
  lift_pre <- vector(mode = "numeric", length = bin_num)
  lift_pre <- t(t(lift_pre))
  
  i <- 1
  cur_acc_pct_0 <- 0
  cur_acc_pct_1 <- 0
  cur_acc_total <- 0
  cur_acc_1 <- 0
  total <- sum(bin_data$bin_total)
  total_1 <- sum(bin_data$cnt_1)
  pre <- total_1 / total
  digits_num <- 4
  while (i <= bin_num) {
    options(digits = digits_num)
    
    cur_acc_pct_0 <- cur_acc_pct_0 + bin_data$pct_0[i]
    acc_pct_0[i] <- cur_acc_pct_0
    cur_acc_pct_1 <- cur_acc_pct_1 + bin_data$pct_1[i]
    acc_pct_1[i] <- cur_acc_pct_1
    pct_diff[i] <- bin_data$pct_0[i] - bin_data$pct_1[i]
    iv[i] <- pct_diff[i] * bin_data$woe[i]
    ks[i] <- abs((acc_pct_1[i] - acc_pct_0[i]))
    cur_acc_total <- cur_acc_total + bin_data$bin_total[i]
    op[i] <- cur_acc_total / total
    cur_acc_1 <- cur_acc_1 + bin_data$cnt_1[i]
    if (cur_acc_total > 0) {
      acc_pre[i] <- cur_acc_1 / cur_acc_total
      lift_pre[i] <- acc_pre[i] / pre
    } else {
      acc_pre[i] <- 0
      lift_pre[i] <- 0
    }
    i <- i + 1
  }
  
  
  options(digits = digits_num)
  result <-
    data.frame(
      cbind(
        bin_data[1:4],
        round(bin_data$woe, digits_num),
        round(ks, digits_num),
        round(iv, digits_num),
        round(bin_data$odds, digits_num),
        round(bin_data$pct_0, digits_num),
        round(bin_data$pct_1, digits_num),
        round(acc_pct_0, digits_num),
        round(acc_pct_1, digits_num),
        round(op, digits_num),
        round(acc_pre, digits_num),
        round(lift_pre, digits_num)
      )
      ,
      stringsAsFactors = FALSE
    )
  names(result) <-
    c(
      "bin",
      "bin_total",
      "cnt_0",
      "cnt_1",
      "woe",
      "ks",
      "iv",
      "odds",
      "pct_0",
      "pct_1",
      "acc_pct_0",
      "acc_pct_1",
      "op",
      "acc_pre",
      "lift_pre"
    )
  
  options(digits = digits_num)
  stat_row <-
    c(
      "Total",
      sum(bin_data$bin_total),
      sum(bin_data$cnt_0),
      sum(bin_data$cnt_1),
      "",
      round(max(ks), digits_num),
      round(sum(iv), digits_num),
      "",
      sum(bin_data$pct_0),
      sum(bin_data$pct_1),
      "",
      "",
      "",
      "",
      ""
    )
  result <- rbind(result, stat_row)
  return (result)
}

# categorical variables
data <-
  read.csv(file = 'd:/umeng_s_code108109.csv', header = TRUE, sep = ',')
iv_ks <- iv_ks_details(data, "s_code108", "is_bad", FALSE)
write.csv(iv_ks, "d:/iv_ks_detail.csv")
summary <-
  iv_ks_summary(
    data,
    c(
      'contact_blacklist_cnt_1d',
      'contact_df_cnt_1d',
      'contact_m3_cnt_1d',
      'contact_blacklist_ratio_1d',
      'contact_df_ratio_1d',
      'contact_blacklist_cnt_2d',
      'contact_df_cnt_2d',
      'contact_m3_cnt_2d',
      'callin_blacklist_cnt',
      'callin_df_cnt'
    ),
    "is_bad",
    FALSE
  )
write.csv(summary, "d:/iv_ks_summary.csv")


# continuous variables
riskfactor <-
  read.csv(file = 'd:/umeng_details.csv', header = TRUE, sep = ',')
iv_ks_riskfactor <-
  iv_ks_details(riskfactor, "device_price", "is_bad")
write.csv(iv_ks_riskfactor, "d:/iv_ks_detail.csv")
continuous_summary <-
  iv_ks_summary(
    riskfactor,
    c(
      'city_freq',
      'sns_90d',
      'tools_90d',
      'woman_90d',
      'country_freq',
      'app_stability_7d',
      'loan_7d',
      'finance_90d',
      'car_7d',
      'province_rec',
      'property_180d',
      'device_rank',
      'travel_180d',
      'reading_180d',
      'woman_180d',
      'entertainment_180d',
      'app_stability_180d',
      'sns_7d',
      'ip_90d',
      'reading_7d',
      'top_90d',
      'shopping_90d',
      'car_90d',
      'finance_7d',
      'travel_7d',
      'shopping_180d',
      'car_180d',
      'sns_180d',
      'service_7d',
      'health_7d',
      'education_180d',
      'launch_day',
      'game_90d',
      'tail_180d',
      'health_90d',
      'education_90d',
      'education_7d',
      'entertainment_90d',
      'province_freq',
      'woman_7d',
      'loan_180d',
      'tail_90d',
      'reading_90d',
      'tools_180d',
      'loan_90d',
      'property_90d',
      'shopping_7d',
      'finance_180d',
      'game_7d',
      'entertainment_7d',
      'health_180d',
      'city_rec',
      'tools_7d',
      'service_180d',
      'device_price',
      'top_180d',
      'tail_7d',
      'game_180d',
      'service_90d',
      'app_stability_90d',
      'country_rec',
      'top_7d',
      'travel_90d'
    ),
    "is_bad",
    TRUE
  )
write.csv(continuous_summary, "d:/iv_ks_summary2.csv")


# model scores
data <-
  read.csv(file = 'd:/tencent_score.csv', header = TRUE, sep = '\t')
iv_ks <- iv_ks_for_score(data, 'score', 'curr_label', 0, 1, 0.01)
iv_ks_1 <- iv_ks_for_score_2(data, 's_creditscore', 'is_bad', 0, 845, 1)
iv_ks_2 <- iv_ks_for_score_2(data, 'modelscore', 'is_bad', 0, 1, 0.01)
write.csv(iv_ks, "d:/iv_ks_detail.csv")
write.csv(iv_ks_1, "d:/iv_ks_detail_1.csv")
write.csv(iv_ks_2, "d:/iv_ks_detail_2.csv")




# ROC plot
plot(
  iv_ks$acc_pct_0,
  iv_ks$acc_pct_1,
  'l',
  main = 'ROC',
  xlab = 'FPR',
  ylab = 'TPR',
  xlim = c(0, 1),
  ylim = c(0, 1),
  col = 'skyblue2',
  lwd = 3
)
lines(iv_ks_1$acc_pct_0, iv_ks_1$acc_pct_1, 'l', col = 'green4', lwd = 3)
lines(iv_ks_2$acc_pct_0, iv_ks_2$acc_pct_1, 'l', col = 'orange', lwd = 3)
lines(range(0, 1), range(0, 1), 'l', col = 'pink2', lwd = 3)
legend(0, 1, legend = c('l_creditscore', 's_creditscore', 'scorecard', 'baseline'), col = c('skyblue2', 'green4', 'orange', 'pink2'), pch = 15, cex = 0.9)
axis(1,at=seq(0,1,0.1))
grid()


# Gain Chart
plot(
  iv_ks$op,
  iv_ks$acc_pct_1,
  'l',
  main = 'Gain Chart',
  xlab = 'OP',
  ylab = 'Recall',
  xlim = c(0, 1),
  ylim = c(0, 1),
  col = 'skyblue2',
  lwd = 3
)
lines(iv_ks_1$op, iv_ks_1$acc_pct_1, 'l', col = 'green4', lwd = 3)
lines(iv_ks_2$op, iv_ks_2$acc_pct_1, 'l', col = 'orange', lwd = 3)
lines(range(0, 1), range(0, 1), 'l', col = 'pink2', lwd = 3)
legend(0, 1, legend = c('l_creditscore', 's_creditscore', 'scorecard', 'baseline'), col = c('skyblue2', 'green4', 'orange', 'pink2'), pch = 15, cex = 0.9)
axis(1,at=seq(0,1,0.1))
grid()



# Score based Gain Chart
plot(
  seq(100, 0, -1),
  iv_ks$acc_pct_1,
  'l',
  main = 'Gain Chart',
  xlab = 'Score',
  ylab = 'Recall',
  xlim = c(100, 0),
  ylim = c(0, 1),
  col = 'skyblue2',
  lwd = 3
)
lines(seq(100, 0, -1), iv_ks_1$acc_pct_1, 'l', col = 'green4', lwd = 3)
lines(seq(100, 0, -1), seq(0, 1, 0.01), 'l', col = 'pink2', lwd = 3)
legend(100, 1, legend = c('riskscore', 'scorecard', 'baseline'), col = c('skyblue2', 'green4', 'pink2'), pch = 15, cex = 0.9)
grid()


# Lift Chart
plot(
  iv_ks$op,
  iv_ks$lift_pre,
  'l',
  main = 'Lift Chart',
  xlab = 'OP',
  ylab = 'Lift',
  xlim = c(0, 1),
  ylim = c(0.5, 3),
  col = 'skyblue2',
  lwd = 3
)
lines(iv_ks_1$op, iv_ks_1$lift_pre, 'l', col = 'green4', lwd = 3)
lines(iv_ks_2$op, iv_ks_2$lift_pre, 'l', col = 'orange', lwd = 3)
lines(range(0, 1), range(1, 1), 'l', col = 'pink2', lwd = 3)
legend(0.75, 3, legend = c('l_creditscore', 's_creditscore', 'scorecard', 'baseline'), col = c('skyblue2', 'green4', 'orange', 'pink2'), pch = 15, cex = 0.9)
axis(1,at=seq(0.1,1,0.1))
grid()


# Score based Lift Chart
plot(
  seq(100, 0, -1),
  iv_ks$lift_pre,
  'l',
  main = 'Lift Chart',
  xlab = 'Score',
  ylab = 'Recall',
  xlim = c(100, 0),
  ylim = c(0, 3.5),
  col = 'skyblue2',
  lwd = 3
)
lines(seq(100, 0, -1), iv_ks_1$lift_pre, 'l', col = 'green4', lwd = 3)
lines(range(0, 100), range(1, 1), 'l', col = 'pink2', lwd = 3)
legend(100, 1, legend = c('riskscore', 'scorecard', 'baseline'), col = c('skyblue2', 'green4', 'pink2'), pch = 15, cex = 0.9)
grid()


# P/R
plot(
  iv_ks$acc_pct_1,
  iv_ks$acc_pre,
  'l',
  main = 'P-R Curve',
  xlab = 'Recall',
  ylab = 'Precision',
  xlim = c(0, 1),
  ylim = c(0, 1),
  col = 'skyblue2',
  lwd = 3
)
lines(iv_ks_1$acc_pct_1, iv_ks_1$acc_pre, 'l', col = 'green4', lwd = 3)
lines(iv_ks_2$acc_pct_1, iv_ks_2$acc_pre, 'l', col = 'orange', lwd = 3)
len = nrow(iv_ks)
pre = as.double(iv_ks$acc_pre[len - 2])
lines(range(0, 1), range(pre, pre), 'l', col = 'pink2', lwd = 3)
legend(0.75, 1, legend = c('l_creditscore', 's_creditscore', 'scorecard', 'baseline'), col = c('skyblue2', 'green4', 'orange', 'pink2'), pch = 15, cex = 0.9)
axis(1,at=seq(0,1,0.1))
grid()


# correlation
plot(
  data$l_creditscore,
  data$s_creditscore,
  'p',
  main = 'Correlation',
  xlab = 'l_creditscore',
  ylab = 's_creditscore',
  xlim = c(0, 1000),
  ylim = c(0, 1000),
  col = 'purple2',
  lwd = 2
)
grid()
cor(data$l_creditscore, data$s_creditscore)