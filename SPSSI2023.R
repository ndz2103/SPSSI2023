
library(dplyr)
library(readr)
library(tidyr)
library(gridExtra)
library(ggplot2)


# functions ---------------------------------------------------------------

correlation_matrix <- function(df, 
                               type = "pearson",
                               digits = 3, 
                               decimal.mark = ".",
                               use = "all", 
                               show_significance = TRUE, 
                               replace_diagonal = FALSE, 
                               replacement = ""){
  
  # check arguments
  stopifnot({
    is.numeric(digits)
    digits >= 0
    use %in% c("all", "upper", "lower")
    is.logical(replace_diagonal)
    is.logical(show_significance)
    is.character(replacement)
  })
  # we need the Hmisc package for this
  require(Hmisc)
  
  # retain only numeric and boolean columns
  isNumericOrBoolean = vapply(df, function(x) is.numeric(x) | is.logical(x), logical(1))
  if (sum(!isNumericOrBoolean) > 0) {
    cat('Dropping non-numeric/-boolean column(s):', paste(names(isNumericOrBoolean)[!isNumericOrBoolean], collapse = ', '), '\n\n')
  }
  df = df[isNumericOrBoolean]
  
  # transform input data frame to matrix
  x <- as.matrix(df)
  
  # run correlation analysis using Hmisc package
  correlation_matrix <- Hmisc::rcorr(x, type = )
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  # transform correlations to specific character format
  Rformatted = formatC(R, format = 'f', digits = digits, decimal.mark = decimal.mark)
  
  # if there are any negative numbers, we want to put a space before the positives to align all
  if (sum(R < 0) > 0) {
    Rformatted = ifelse(R > 0, paste0(' ', Rformatted), Rformatted)
  }
  
  # add significance levels if desired
  if (show_significance) {
    # define notions for significance levels; spacing is important.
    stars <- ifelse(is.na(p), "   ", ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   "))))
    Rformatted = paste0(Rformatted, stars)
  }
  # build a new matrix that includes the formatted correlations and their significance stars
  Rnew <- matrix(Rformatted, ncol = ncol(x))
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep =" ")
  
  # replace undesired values
  if (use == 'upper') {
    Rnew[lower.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (use == 'lower') {
    Rnew[upper.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (replace_diagonal) {
    diag(Rnew) <- replacement
  }
  
  return(Rnew)
}

# data processing ---------------------------------------------------------------

spssi2023_raw <- read_csv("~/Desktop/Research/SPSSI2023/anes_gss2020.csv")

spssi2023_raw <- select(spssi2023_raw, c(AGE_1A, SEX_1A, RACE_1A, SEXORNT_1A, ATTEND_1A, MARHOMO1_1A,
                                         POLVIEWS_1A, V202166, V202172, V202472a, V202472b, V202472c,
                                         V202472d, V202472e, V202474a, V202474b, V202474c, V202474d,
                                         V202474e))

spssi2023_raw <- dplyr::rename(spssi2023_raw, age = AGE_1A, sex = SEX_1A, race = RACE_1A, sexori = SEXORNT_1A, relig_attend = ATTEND_1A, 
                                              equal_mar = MARHOMO1_1A, poli = POLVIEWS_1A, feel_LGB = V202166, feel_T = V202172,
                                              lgb_fam = V202472a, lgb_relate = V202472b, lgb_neigh = V202472c, lgb_cowork = V202472d,
                                              lgb_friend = V202472e, trans_fam = V202474a, trans_relate = V202474b,
                                              trans_neigh = V202474c, trans_cowork = V202474d, trans_friend = V202474e)

# create lgbt_close variable ----------------------------------------------

# fam = 5; relate = 4; friend = 3; cowork = 2; neigh = 1

spssi2023_raw$lgb_fam <- ifelse(spssi2023_raw$lgb_fam == 1, 5, 0)
spssi2023_raw$lgb_relate <- ifelse(spssi2023_raw$lgb_relate == 1, 4, 0)
spssi2023_raw$lgb_friend <- ifelse(spssi2023_raw$lgb_friend == 1, 3, 0)
spssi2023_raw$lgb_neigh <- ifelse(spssi2023_raw$lgb_neigh == 1, 2, 0)
spssi2023_raw$lgb_cowork <- ifelse(spssi2023_raw$lgb_cowork == 1, 1, 0)

spssi2023_raw$lgb_close <- (spssi2023_raw$lgb_fam + spssi2023_raw$lgb_relate + 
                            spssi2023_raw$lgb_friend + spssi2023_raw$lgb_neigh + 
                            spssi2023_raw$lgb_cowork)

spssi2023_raw$trans_fam <- ifelse(spssi2023_raw$trans_fam == 1, 5, 0)
spssi2023_raw$trans_relate <- ifelse(spssi2023_raw$trans_relate == 1, 4, 0)
spssi2023_raw$trans_friend <- ifelse(spssi2023_raw$trans_friend == 1, 3, 0)
spssi2023_raw$trans_neigh <- ifelse(spssi2023_raw$trans_neigh == 1, 2, 0)
spssi2023_raw$trans_cowork <- ifelse(spssi2023_raw$trans_cowork == 1, 1, 0)

spssi2023_raw$trans_close <- (spssi2023_raw$trans_fam + spssi2023_raw$trans_relate + 
                              spssi2023_raw$trans_friend + spssi2023_raw$trans_neigh + 
                              spssi2023_raw$trans_cowork)

# average score of lgb_close and trans_close
spssi2023_raw$lgbt_close <- rowMeans(spssi2023_raw[,20:21])

# analysis ----------------------------------------------------------------

# class = numeric
spssi2023_raw[,1:22] <- lapply(spssi2023_raw[,1:22], as.numeric)

# reverse score equal_mar
spssi2023_raw$equal_mar <- (6-spssi2023_raw$equal_mar)

# only 18+ year old
spssi2023_raw$age[spssi2023_raw$age < 18] <- NA

# wrangle feel_LGB and feel_T, create aggregate score feel_LGBT
spssi2023_raw$feel_LGB[spssi2023_raw$feel_LGB < 0] <- NA
spssi2023_raw$feel_T[spssi2023_raw$feel_T < 0] <- NA
spssi2023_raw$feel_LGBT <- rowMeans(spssi2023_raw[,8:9])

spssi2023_raw <- dplyr::rename(spssi2023_raw, lgbt_feel = feel_LGBT)

# remove unnecessary or redundant columns; reorder columns
spssi2023_raw <- spssi2023_raw[,c(1:7, 22:23)]
spssi2023_clean <- select(spssi2023_raw, c(poli, relig_attend, lgbt_feel, equal_mar, lgbt_close, everything()))

# write csv of cleaned data

write_csv(spssi2023_clean, "~/Desktop/Research/SPSSI2023/spssi2023_clean.csv")

# correlation matrix ------------------------------------------------------

# create df for correlation matrix
cor <- rename(spssi2023_clean, "Political Ideology" = poli, "Religiosity" = relig_attend,
                               "Pro-Marriage Equality" = equal_mar, "LGBT Favorability" = lgbt_feel,
                               "LGBT Closeness" = lgbt_close)
cor <- cor[,1:5]

# create publication-ready correlation matrix

cor.mat <- as.data.frame(correlation_matrix(cor), type="pearson", digits=3)
pdf("~/Desktop/Research/SPSSI2023/cor_mat.pdf", height = 4, width = 12)
grid.table(cor.mat)
dev.off()

# individual correlation tests
attach(spssi2023_clean)

# religiosity x political ideology
cor.test(relig_attend, poli) # t = 7.570, df = 523, p < .001, r = .314

# relig + poli x lgbt_feel
cor.test(relig_attend, lgbt_feel) # t = -3.579, df = 513, p < .001, r = -.156
cor.test(poli, lgbt_feel) # t = -9.083, df = 510, p < .001, r = -.373

# relig + poli x equal_mar
cor.test(relig_attend, equal_mar) # t = -9.743, df = 346, p < .001, r = -.464
cor.test(poli, equal_mar) # t = -8.848, df = 343, p < .001, r = -.431

# regression --------------------------------------------------------------

# use only complete cases
spssi2023_regression <- drop_na(spssi2023_clean)

# marriage equality
me1 <- lm(equal_mar ~ relig_attend + poli + age + sex + race + sexori, spssi2023_regression)
me2 <- lm(equal_mar ~ relig_attend*lgbt_close + poli*lgbt_close + age + sex + race + sexori, spssi2023_regression)
anova(me1, me2)

me_list <- list(me1, me2)
lapply(me_list, summary)

# lgbt favorability

fvr1 <- lm(lgbt_feel ~ relig_attend + poli + age + sex + race + sexori, spssi2023_regression)
fvr2 <- lm(lgbt_feel ~ relig_attend*lgbt_close + poli*lgbt_close + age + sex + race + sexori, spssi2023_regression)
anova(fvr1, fvr2)

fvr_list <- list(fvr1, fvr2)
lapply(fvr_list, summary)

# create list of regression output for equal_mar

df_me1 <- data.frame()
df_me2 <- data.frame()

for(x in 1:length(me_list)){
  temp <- data.frame(term = names(coef(me_list[[x]])),
                     coef = coef(me_list[[x]]),
                     se = summary(me_list[[x]])$coefficients[, "Std. Error"],
                     t_value = summary(me_list[[x]])$coefficients[, "t value"],
                     p_value = summary(me_list[[x]])$coefficients[, "Pr(>|t|)"],
                     type = paste0("Model", x))
  if(x == 1){
    df_me1 <- temp
  }
  if(x == 2){
    df_me2 <- temp
  }
  if(x == 3){
    df_me3 <- temp
  }
  if(x == 4){
    df_me4 <- temp
  }
}

me_list <- list(df_me1, df_me2)
ls()
rm(df_me1,df_me2, me1, me2)

# create list of regression output for lgbt_feel

df_fvr1 <- data.frame()
df_fvr2 <- data.frame()

for(x in 1:length(fvr_list)){
  temp <- data.frame(term = names(coef(fvr_list[[x]])),
                     coef = coef(fvr_list[[x]]),
                     se = summary(fvr_list[[x]])$coefficients[, "Std. Error"],
                     t_value = summary(fvr_list[[x]])$coefficients[, "t value"],
                     p_value = summary(fvr_list[[x]])$coefficients[, "Pr(>|t|)"],
                     type = paste0("Model", x))
  
  if(x == 1){
    df_fvr1 <- temp
  }
  if(x == 2){
    df_fvr2 <- temp
  }
  if(x == 3){
    df_fvr3 <- temp
  }
  if(x == 4){
    df_fvr4 <- temp
  }
}

fvr_list <- list(df_fvr1, df_fvr2)
ls()
rm(df_fvr1, df_fvr2, fvr1, fvr2)

# save regressions as workbooks -------------------------------------------

## library(openxlsx)
## 
## # equal_mar workbook
## temp <- list(me1 = data.frame(me_list[[1]]),
##              me2 = data.frame(me_list[[2]]))
## 
## wb <- createWorkbook()
## 
## for (sheet_name in names(temp)) {
##   addWorksheet(wb, sheetName = sheet_name)
##   writeData(wb, sheet_name, temp[[sheet_name]])
## }
## 
## saveWorkbook(wb, file = "~/Desktop/me_list.xlsx", overwrite=T)
## 
## # lgbt_feel workbook
## temp <- list(fvr1 = data.frame(fvr_list[[1]]),
##              fvr2 = data.frame(fvr_list[[2]]))
## 
## wb <- createWorkbook()
## 
## for (sheet_name in names(temp)) {
##   addWorksheet(wb, sheetName = sheet_name)
##   writeData(wb, sheet_name, temp[[sheet_name]])
## }
## 
## saveWorkbook(wb, file = "~/Desktop/fvr_list.xlsx", overwrite=T)
## 
## # clean workspace
## rm(temp, sheet_name, wb)

# data visualization ------------------------------------------------------

# predict values

me1 <- function(a, b, c, d, f, g) {
  6.266 - .164*a - .259*b - .018*c + .299*d - .141*f - .061*g
}

me2 <- function(a, b, c, d, f, g, h, i, j) {
  6.412 - .187*a - .161*b - .322*c - .018*d + .277*f - .133*g - .004*h + .022*i + .041*j
}

fvr1 <- function(a, b, c, d, f, g) {
  116.376 + .114*a - 6.699*b - .204*c + 10.758*d - 7.366*f - 7.196*g
}

fvr2 <- function(a, b, c, d, f, g, h, i, j) {
  107.318 + 0.706*a - 1.074*b - 8.125*c - .296*d + 9.993*f - 6.35*g - 4.38*h - .028*i + 1.14*j
}

attach(spssi2023_regression)

spssi2023_regression$me1 <- me1(relig_attend, poli, age, sex, race, sexori)
spssi2023_regression$me2 <- me2(relig_attend, lgbt_close, poli, age, sex, race, sexori, relig_attend*lgbt_close, poli*lgbt_close)

spssi2023_regression$fvr1 <- fvr1(relig_attend, poli, age, sex, race, sexori)
spssi2023_regression$fvr2 <- fvr2(relig_attend, lgbt_close, poli, age, sex, race, sexori, relig_attend*lgbt_close, poli*lgbt_close)

# visualize

me <- ggplot(spssi2023_regression, aes(poli*lgbt_close, equal_mar))+
  geom_jitter(width=2, height=.5, alpha=.6, pch=20, color="gray20") +
  stat_smooth(geom="line", aes(y=me1, x=poli*lgbt_close), method="lm", color = "firebrick1", se=F, alpha=.5) +
  geom_smooth(aes(y=me2, x=poli*lgbt_close), method="lm", color="firebrick4", se=F) +
  theme_minimal() +
  labs(title=NULL,
        x = expression(paste("Political Ideology * LGBT Closeness")), 
        y = expression(atop("Support for Marriage Equality", atop(italic("1 = Disapprove to 5 = Approve"))))) +
  theme(plot.title = element_text(face="bold")) +
  annotate("text", x=30, y=1.5, label=expression(paste(italic("R")^{2}, italic(" ="), italic(" .39"))), size=3) +   
  annotate("text", x=30, y=1.2, label=expression(paste(italic("y = "), italic("6.412 "), "+ ", italic(".041 ")*"x"[1], italic(" + k"))), size=3) +
  scale_y_continuous(breaks=seq(0, 5, 1))

fvr <- ggplot(spssi2023_regression, aes(poli*lgbt_close, lgbt_feel)) +
  geom_jitter(width=2, height=.5, alpha=.6, pch=20, color="gray20")+
  stat_smooth(geom="line",aes(y=fvr1, x=poli*lgbt_close), method="lm", color = "dodgerblue", se=F, alpha=.5) +
  geom_smooth(aes(y=fvr2, x=poli*lgbt_close), method="lm", color="dodgerblue3", se=F) +
  theme_minimal() +
  labs(title=NULL, 
        x = expression(paste("Political Ideology * LGBT Closeness"), size=2), 
        y = expression(atop("LGBT Favorability", atop(italic("0 = Disfavor to 100 = Favor"))))) +
  theme(plot.title = element_text(face="bold")) +
  annotate("text", x=30, y=20, label=expression(paste(italic("R")^{2}, italic(" ="), italic(" .319"))), size=3) +   
  annotate("text", x=30, y=12, label=expression(paste(italic("y = "), italic("107.318 "), "+ ", italic("1.41 ")*"x"[1], italic(" + k"))), size=3) +
  scale_y_continuous(breaks=seq(0, 100, 20))
 
