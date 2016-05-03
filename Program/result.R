library(ggplot2)

#1 number worker testse
test1 <- function(){
  result.df1 <<- onevar.df("test1.csv")
  xplot(result.df1)
}

#2 worker probability test (mv)(normal)
test2 <- function(){
  result.df2 <<- onevar.df("test2.csv")
  xplot(result.df2)
}

tests <- function(){
  result.dfs <<- onevar.df("tests.csv")
  result.dfs2 <<- onevar.df("tests2.csv")
  result.dfs3 <<- onevar.df("tests3.csv")
  
  plot <- nlineplot(list(result.dfs, result.dfs2, result.dfs3),list("MV","MV Soft","MV Soft + Cut"),"Data.Fusion")
  plot + ggtitle("Performance between Majority Voting")
}

testa <- function(){
  result.dfa <<- onevar.df("testa.csv")
  result.dfa2 <<- onevar.df("testa2.csv")
  result.dfa3 <<- onevar.df("testa3.csv")
  
  plot <- nlineplot(list(result.dfa, result.dfa3),list("MV","MV Soft + Cut"),"Data.Fusion")
  plot + ggtitle("Performance between Majority Voting")
}

test2.1 <- function(){
  result.df2.1 <<- onevar.df("test2.1.csv")
  xplot(result.df2.1)
}

#3 worker probability test (em)(normal)
test3 <- function(){
  result.df3 <<- onevar.df("test3.csv")
  result.df3 <<- enlarge(result.df3)
  
  xplot(result.df3)
}

enlarge <- function(df){
  row <- nrow(df)
  for(i in 1:(row - 1)){
    start <- df[i,1]
    end <- df[i+1,1] 
    
    size <- (end-start)
    value <- seq(df[i,2], df[i+1,2], length.out = size + 1)
    
    for(ii in 2:size){
      df <- rbind(df, c(start + ii - 1,value[ii]))
    }
  }
  df <- df[with(df, order(df[,1])), ]
  df
}

#4 worker probability test (mv)(random)
test4 <- function(){
  result.df4 <<- onevar.df("test4.csv")
  xplot(result.df4)
}

#5 worker probability test (mv)(soft)(normal)
test5 <- function(){
  #result.df5 <<- twovar.df("test5.csv")
  result.df5 <<- twovar2.df("test5.csv","test5plus.csv")
  temp <- result.df5
  result.df5[temp$cut.workers.per.round == 0,"cut.workers.per.round"] <<- "0%"
  result.df5[temp$cut.workers.per.round > 0 
                                   & temp$cut.workers.per.round <= 25,"cut.workers.per.round"] <<- "1-25%"
  result.df5[temp$cut.workers.per.round > 25 
             & temp$cut.workers.per.round <= 50,"cut.workers.per.round"] <<- "25-50%"
  
  result.df5 <<- aggregate(correctness ~ .,data = result.df5, mean)
  result.df5$cut.workers.per.round <- factor(result.df5$cut.workers.per.round)
  
  plot <- x2plot(result.df5)
  plot + ggtitle("Performance of Majority Voting with Soft Penalty")
}

test5soft <- function(){
  result.df5soft <<- onevar.df("test5soft.csv")
  
  result.df5soft <<- aggregate(correctness ~ .,data = result.df5soft, mean)
  
  plot <- xplot(result.df5soft)
  plot + ggtitle("Performance of Majority Voting with Soft Penalty")
}

test5hard <- function(){
  result.df5hard <<- onevar.df("test5hard.csv")

  result.df5hard <<- aggregate(correctness ~ .,data = result.df5hard, mean)
  
  plot <- xplot(result.df5hard)
  plot + ggtitle("Performance of Majority Voting with Hard Penalty")
}

test6soft <- function(){
  result.df6soft <<- onevar.df("test6soft.csv")
  
  result.df6soft <<- aggregate(correctness ~ .,data = result.df6soft, mean)
  
  plot <- xplot(result.df6soft)
  plot + ggtitle("Performance of Majority Voting with Soft Penalty")
}

test6hard <- function(){
  result.df6hard <<- onevar.df("test6hard.csv")
  
  result.df6hard <<- aggregate(correctness ~ .,data = result.df6hard, mean)
  
  plot <- xplot(result.df6hard)
  plot + ggtitle("Performance of Majority Voting with Soft Penalty")
}



#6 worker probability test (em)(soft)(normal)
test6 <- function(){
  result.df6 <<- twovar.df("test6.csv")
  #result.df6 <<- twovar2.df("test6.csv","test6plus.csv")
  temp <- result.df6

  result.df6[temp$cut.workers.per.round == 0,"cut.workers.per.round"] <<- "0%"
  result.df6[temp$cut.workers.per.round > 0 
             & temp$cut.workers.per.round <= 25,"cut.workers.per.round"] <<- "1-25%"
  result.df6[temp$cut.workers.per.round > 25 
             & temp$cut.workers.per.round <= 50,"cut.workers.per.round"] <<- "25-50%"
  
  result.df6 <<- aggregate(correctness ~ .,data = result.df6, mean)
  result.df6$cut.workers.per.round <- factor(result.df6$cut.workers.per.round)
  
  plot <- x2plot(result.df6)
  plot + ggtitle("Performance of EM Algorithm with Soft Penalty")
}

#7 mv adversaries type 3
test7 <- function(){
  result.df7 <<- twovar.df("test7.csv")
  result.df7$workers.probability <- factor(result.df7$workers.probability)
  
  x2plot(result.df7) + ggtitle("Performance of Majority Voting with Adversaries Type 3")
}

#8 mv adversaries type 2
test8 <- function(){
  result.df8 <<- twovar.df("test8.csv")
  result.df8$workers.probability <- factor(result.df8$workers.probability)
  
  x2plot(result.df8) + ggtitle("Performance of Majority Voting with Adversaries Type 2")
}

#9 mv adversaries type 1 normal
test9 <- function(){
  result.df9 <<- twovar.df("test9.csv")
  result.df9$workers.probability <- factor(result.df9$workers.probability)

  x2plot(result.df9) + ggtitle("Performance of Majority Voting with Adversaries Type 1")
}

#9 mv adversaries type 1 random
test9.1 <- function(){
  result.df9.1 <<- twovar.df("test9.1.csv")
  result.df9.1$workers.probability <- factor(result.df9.1$workers.probability)
  
  x2plot(result.df9.1) + ggtitle("Performance of Majority Voting with Adversaries Type 1")
}

#10 em adversaries type 3
test10 <- function(){
  result.df10 <<- twovar.df("test10.csv")
  result.df10$workers.probability <- factor(result.df10$workers.probability)
  
  x2plot(result.df10)
}

#11 em adversaries type 2
test11 <- function(){
  result.df11 <<- twovar.df("test11.csv")
  result.df11$workers.probability <- factor(result.df11$workers.probability)
  
  x2plot(result.df11)
}

#12 em adversaries type 1
test12 <- function(){
  result.df12 <<- twovar.df("test12.csv")
  result.df12$workers.probability <- factor(result.df12$workers.probability)
  
  x2plot(result.df12)
}

#13 mv adversaries type 3 soft
test13 <- function(){
  result.df13 <<- threevar.df("test13.csv")
  
  temp <- result.df13
  result.df13[temp$cut.workers.per.round == 0 & temp$workers.probability == 50
              ,"cut.workers.per.round"] <<- "0%, worker-prob 50%"
  result.df13[temp$cut.workers.per.round == 0 & temp$workers.probability == 100
              ,"cut.workers.per.round"] <<- "0%, worker-prob 100%"
  result.df13[temp$cut.workers.per.round == 25 & temp$workers.probability == 50
              ,"cut.workers.per.round"] <<- "25%, worker-prob 50%"
  result.df13[temp$cut.workers.per.round == 25 & temp$workers.probability == 100
              ,"cut.workers.per.round"] <<- "25%, worker-prob 100%"
  result.df13[temp$cut.workers.per.round == 50 & temp$workers.probability == 50
              ,"cut.workers.per.round"] <<- "50%, worker-prob 50%"
  result.df13[temp$cut.workers.per.round == 50 & temp$workers.probability == 100
              ,"cut.workers.per.round"] <<- "50%, worker-prob 100%"
  
  result.df13 <<- result.df13[,c(3,2,4)]
  result.df13 <<- aggregate(correctness ~ .,data = result.df13, mean)
  
  result.df13$cut.workers.per.round <- factor(result.df13$cut.workers.per.round)
  
  plot <- x2plot(result.df13) + ggtitle("Performance of Majority Voting with Soft Penalty and Adversaries Type 3")
  
  plot + scale_colour_manual(values = c("#f9007c", "#ffa2d0", "#0ca3ff", "#86d1ff", "#00d00e", "#82ff8a")) 
}

#14 mv adversaries type 2 soft
test14 <- function(){
  result.df14 <<- threevar.df("test14.csv")
  
  temp <- result.df14
  result.df14[temp$cut.workers.per.round == 0 & temp$workers.probability == 50
              ,"cut.workers.per.round"] <<- "0%, worker-prob 50"
  result.df14[temp$cut.workers.per.round == 0 & temp$workers.probability == 100
              ,"cut.workers.per.round"] <<- "0%, worker-prob 100"
  result.df14[temp$cut.workers.per.round == 25 & temp$workers.probability == 50
              ,"cut.workers.per.round"] <<- "25%, worker-prob 50"
  result.df14[temp$cut.workers.per.round == 25 & temp$workers.probability == 100
              ,"cut.workers.per.round"] <<- "25%, worker-prob 100"
  result.df14[temp$cut.workers.per.round == 50 & temp$workers.probability == 50
              ,"cut.workers.per.round"] <<- "50%, worker-prob 50"
  result.df14[temp$cut.workers.per.round == 50 & temp$workers.probability == 100
              ,"cut.workers.per.round"] <<- "50%, worker-prob 100"
  
  result.df14 <<- result.df14[,c(3,2,4)]
  result.df14 <<- aggregate(correctness ~ .,data = result.df14, mean)
  
  result.df14$cut.workers.per.round <- factor(result.df14$cut.workers.per.round)
  
  plot <- x2plot(result.df14) + ggtitle("Performance of Majority Voting with Soft Penalty and Adversaries Type 2")
  
  
  plot + scale_colour_manual(values = c("#f9007c", "#ffa2d0", "#0ca3ff", "#86d1ff", "#00d00e", "#82ff8a")) 
}

#15 mv adversaries type 2 soft
test15 <- function(){
  result.df15 <<- threevar.df("test15.csv")
  
  temp <- result.df15
  result.df15[temp$cut.workers.per.round == 0 & temp$workers.probability == 50
              ,"cut.workers.per.round"] <<- "0%, worker-prob 50"
  result.df15[temp$cut.workers.per.round == 0 & temp$workers.probability == 100
              ,"cut.workers.per.round"] <<- "0%, worker-prob 100"
  result.df15[temp$cut.workers.per.round == 25 & temp$workers.probability == 50
              ,"cut.workers.per.round"] <<- "25%, worker-prob 50"
  result.df15[temp$cut.workers.per.round == 25 & temp$workers.probability == 100
              ,"cut.workers.per.round"] <<- "25%, worker-prob 100"
  result.df15[temp$cut.workers.per.round == 50 & temp$workers.probability == 50
              ,"cut.workers.per.round"] <<- "50%, worker-prob 50"
  result.df15[temp$cut.workers.per.round == 50 & temp$workers.probability == 100
              ,"cut.workers.per.round"] <<- "50%, worker-prob 100"
  
  result.df15 <<- result.df15[,c(3,2,4)]
  result.df15 <<- aggregate(correctness ~ .,data = result.df15, mean)
  
  result.df15$cut.workers.per.round <- factor(result.df15$cut.workers.per.round)
  
  plot <- x2plot(result.df15) + ggtitle("Performance of Majority Voting with Soft Penalty and Adversaries Type 1")
  
  plot + scale_colour_manual(values = c("#f9007c", "#ffa2d0", "#0ca3ff", "#86d1ff", "#00d00e", "#82ff8a")) 
}

#16 em adversaries type 3 soft
test16 <- function(){
  result.df16 <<- threevar.df("test16.csv")
  
  temp <- result.df16
  result.df16[temp$cut.workers.per.round == 0 & temp$workers.probability == 50
              ,"cut.workers.per.round"] <<- "0%, worker-prob 50"
  result.df16[temp$cut.workers.per.round == 0 & temp$workers.probability == 100
              ,"cut.workers.per.round"] <<- "0%, worker-prob 100"
  result.df16[temp$cut.workers.per.round == 25 & temp$workers.probability == 50
              ,"cut.workers.per.round"] <<- "25%, worker-prob 50"
  result.df16[temp$cut.workers.per.round == 25 & temp$workers.probability == 100
              ,"cut.workers.per.round"] <<- "25%, worker-prob 100"
  result.df16[temp$cut.workers.per.round == 50 & temp$workers.probability == 50
              ,"cut.workers.per.round"] <<- "50%, worker-prob 50"
  result.df16[temp$cut.workers.per.round == 50 & temp$workers.probability == 100
              ,"cut.workers.per.round"] <<- "50%, worker-prob 100"
  
  result.df16 <<- result.df16[,c(3,2,4)]
  result.df16 <<- aggregate(correctness ~ .,data = result.df16, mean)
  
  result.df16$cut.workers.per.round <- factor(result.df16$cut.workers.per.round)
  
  plot <- x2plot(result.df16) + ggtitle("Performance of EM Algorithm with Soft Penalty and Adversaries Type 3")
  
  plot + scale_colour_manual(values = c("#f9007c", "#ffa2d0", "#0ca3ff", "#86d1ff", "#00d00e", "#82ff8a")) 
}

#17 em adversaries type 3 soft
test17 <- function(){
  result.df17 <<- threevar.df("test17.csv")
  
  temp <- result.df17
  result.df17[temp$cut.workers.per.round == 0 & temp$workers.probability == 50
              ,"cut.workers.per.round"] <<- "0%, worker-prob 50"
  result.df17[temp$cut.workers.per.round == 0 & temp$workers.probability == 100
              ,"cut.workers.per.round"] <<- "0%, worker-prob 100"
  result.df17[temp$cut.workers.per.round == 25 & temp$workers.probability == 50
              ,"cut.workers.per.round"] <<- "25%, worker-prob 50"
  result.df17[temp$cut.workers.per.round == 25 & temp$workers.probability == 100
              ,"cut.workers.per.round"] <<- "25%, worker-prob 100"
  result.df17[temp$cut.workers.per.round == 50 & temp$workers.probability == 50
              ,"cut.workers.per.round"] <<- "50%, worker-prob 50"
  result.df17[temp$cut.workers.per.round == 50 & temp$workers.probability == 100
              ,"cut.workers.per.round"] <<- "50%, worker-prob 100"
  
  result.df17 <<- result.df17[,c(3,2,4)]
  result.df17 <<- aggregate(correctness ~ .,data = result.df17, mean)
  
  result.df17$cut.workers.per.round <- factor(result.df17$cut.workers.per.round)
  
  plot <- x2plot(result.df17)+ ggtitle("Performance of EM Algorithm with Soft Penalty and Adversaries Type 2")
  
  plot + scale_colour_manual(values = c("#f9007c", "#ffa2d0", "#0ca3ff", "#86d1ff", "#00d00e", "#82ff8a")) 
}

#18 em adversaries type 3 soft
test18 <- function(){
  result.df18 <<- threevar.df("test18.csv")
  
  temp <- result.df18
  result.df18[temp$cut.workers.per.round == 0 & temp$workers.probability == 50
              ,"cut.workers.per.round"] <<- "0%, worker-prob 50"
  result.df18[temp$cut.workers.per.round == 0 & temp$workers.probability == 100
              ,"cut.workers.per.round"] <<- "0%, worker-prob 100"
  result.df18[temp$cut.workers.per.round == 25 & temp$workers.probability == 50
              ,"cut.workers.per.round"] <<- "25%, worker-prob 50"
  result.df18[temp$cut.workers.per.round == 25 & temp$workers.probability == 100
              ,"cut.workers.per.round"] <<- "25%, worker-prob 100"
  result.df18[temp$cut.workers.per.round == 50 & temp$workers.probability == 50
              ,"cut.workers.per.round"] <<- "50%, worker-prob 50"
  result.df18[temp$cut.workers.per.round == 50 & temp$workers.probability == 100
              ,"cut.workers.per.round"] <<- "50%, worker-prob 100"
  
  result.df18 <<- result.df18[,c(3,2,4)]
  result.df18 <<- aggregate(correctness ~ .,data = result.df18, mean)
  
  result.df18$cut.workers.per.round <- factor(result.df18$cut.workers.per.round)
  
  plot <- x2plot(result.df18)+ ggtitle("Performance of EM Algorithm with Soft Penalty and Adversaries Type 1
                                       ")
  
  
  plot + scale_colour_manual(values = c("#f9007c", "#ffa2d0", "#0ca3ff", "#86d1ff", "#00d00e", "#82ff8a")) 
}

#s1 MV vs EM normal (no penalty)
s1 <- function(){
  plot <- nlineplot(list(result.df2, result.df3),list("MV","EM"),"Data.Fusion")
  plot + ggtitle("Performance between Majority Voting and EM Algorithm with Normal Workers")
}

s1diff <- function(){
  result <- rep(0,100)
  result[30:70] <- result.df3[,2] - result.df2[30:70,2]
  qplot(x = 1:100, y = result, geom = "blank") + geom_point(colour = "grey") + 
    geom_smooth(se = FALSE, span = 0.2, size = 1.2, colour = "#061954") + 
   theme_bw() + xlab("Workers Probability") + ylab("Different Correctness") +
    ggtitle("Different Correctness of EM Algorithm and Majority Voting")
}

#s2 MV vs EM type 3 (no penalty)
s2 <- function(){
  plot <- nlineplot(list(result.df7[result.df7$workers.probability == 50,],
                 result.df7[result.df7$workers.probability == 100,],
                 result.df10[result.df10$workers.probability == 50,],
                 result.df10[result.df10$workers.probability == 100,]),
                 list("MV worker-prob 50%","MV worker-prob 100%","EM worker-prob 50%","EM worker-prob 100%"),"Data.Fusion")
  plot + scale_colour_manual(values = c("#f9007c", "#ffa2d0", "#0ca3ff", "#86d1ff")) +
     ggtitle("Performance between Majority Voting and EM Algorithm with Adversaries Type 3")
}

s2diff <- function(){
  result <- rep(0,100)
  result <- result.df7[102:202,3] - result.df10[12:22,3]
  qplot(x = 1:100, y = result, geom = "blank") + geom_point(colour = "grey") + 
    geom_smooth(se = FALSE, span = 0.2, size = 1.2, colour = "#061954") + 
    theme_bw() + xlab("Workers Probability") + ylab("Different Correctness") +
    ggtitle("Different Correctness of EM Algorithm and Majority Voting")
}

#s3 MV vs EM type 2 (no penalty)
s3 <- function(){
  plot <- nlineplot(list(result.df8[result.df8$workers.probability == 50,],
                 result.df8[result.df8$workers.probability == 100,],
                 result.df11[result.df11$workers.probability == 50,],
                 result.df11[result.df11$workers.probability == 100,]),
            list("MV worker-prob 50","MV worker-prob 100","EM worker-prob 50","EM worker-prob 100"),"Data.Fusion")
  plot + scale_colour_manual(values = c("#f9007c", "#ffa2d0", "#0ca3ff", "#86d1ff")) +
    ggtitle("Performance between Majority Voting and EM Algorithm with Adversaries Type 2") 
}

#s4 MV vs EM type 1 (no penalty)
s4 <- function(){
  plot <- nlineplot(list(result.df9.1[result.df9.1$workers.probability == 50,],
                 result.df9.1[result.df9.1$workers.probability == 100,],
                 result.df12[result.df12$workers.probability == 50,],
                 result.df12[result.df12$workers.probability == 100,]),
                 list("MV worker-prob 50","MV worker-prob 100","EM worker-prob 50","EM worker-prob 100"),"Data.Fusion")
  
  
  plot + scale_colour_manual(values = c("#f9007c", "#ffa2d0", "#0ca3ff", "#86d1ff"))  +
    ggtitle("Performance between Majority Voting and EM Algorithm with Adversaries Type 1")  
}

#ss1 MV vs vs MV Soft vs MV Hard normal (no penalty)
ss1 <- function(){
  plot <- nlineplot(list(result.df2, result.df5hard, result.df5soft),list("MV","MV Hard","MV Soft"),"Data.Fusion")
  plot + ggtitle("Performance between Majority Voting and Hard Soft with Normal Workers")
}

ss1diff <- function(){
  df <- data.frame()
  
  result <- rep(0,100)
  result <- result.df2[,2] - result.df5soft[,2]
  df <- rbind(df,setNames(data.frame(0:100, result, rep("MV Soft",101)),names(df)) )
  
  result <- rep(0,100)
  result <- result.df2[,2] - result.df5hard[,2]
  df <- rbind(df,setNames(data.frame(0:100, result, rep("MV Hard",101)),names(df)) )
  
  result <- rep(0,100)
  result <- result.df2[,2] - result.df6soft[,2]
  df <- rbind(df,setNames(data.frame(0:100, result, rep("EM Soft",101)),names(df)) )
  
  result <- rep(0,100)
  result <- result.df2[,2] - result.df6hard[,2]
  df <- rbind(df,setNames(data.frame(0:100, result, rep("EM Hard",101)),names(df)) )
  
  colnames(df) <- c("worker.prob","correctness","set")
  
  
  ggplot(data = df, aes(x = df[,1], y = df[,2], colour = set)) + geom_point(size = 0.3) + 
    stat_smooth(se = FALSE, span = 0.3, size = 1.2, aes(fill = set)) + 
    theme_bw() + xlab("Workers Probability") + ylab("Different Correctness") +
    ggtitle("Different Correctness of Majority Voting with Reputation Algorithm")
}

onevar.df <- function(file){
  df <- read.csv(paste("result",file,sep = "/"), skip = 6)
  df <- df[, c(2,length(df))]
  colnames(df)[2] <- "correctness" 
  
  df <- aggregate(correctness ~ .,data = df, mean)
  df 
}

twovar.df <- function(file){
  df <- read.csv(paste("result",file,sep = "/"), skip = 6)
  df <- df[, c(2:3,length(df))]
  colnames(df)[3] <- "correctness" 
  df <- aggregate(correctness ~ .,data = df, mean)
  
  df 
}

twovar2.df <- function(file,file2){
  df <- read.csv(paste("test",file,sep = "/"), skip = 6)
  df <- rbind(df,  df <- read.csv(paste("test",file2,sep = "/"), skip = 6))
  df <- df[, c(2:3,length(df))]
  colnames(df)[3] <- "correctness" 
  df <- aggregate(correctness ~ .,data = df, mean)
  
  df 
}

threevar.df <- function(file){
  df <- read.csv(paste("result",file,sep = "/"), skip = 6)
  df <- df[, c(2:4,length(df))]
  colnames(df)[4] <- "correctness" 
  df <- aggregate(correctness ~ .,data = df, mean)
  
  df 
}

xplot <- function(df){
  ggplot(data = df, aes_string(x = colnames(df)[1], y = "correctness")) +
    geom_line(size=1.2) + ggtitle("Average Correctness Plot") + ylim(0,100)
}

x2plot <- function(df){
  ggplot(data = df, aes_string(x = colnames(df)[1], y = "correctness",
                                group = colnames(df)[2], colour= colnames(df)[2])) + 
    geom_line(size=1.2) + ylim(0,100)
}

nlineplot <- function(list.df,list.name,legend.name = "set", title = "Plot", xmin = 0, xmax = 100, ymin = 0, ymax = 100){
  df <- do.call(rbind, list.df)

  temp <- c()
  cut <- length(list.name) /  length(list.df)
  for(i in 1:length(list.name)){
    temp <- c(temp, rep(list.name[[i]] , nrow(list.df[[ceiling(i / cut)]]) / cut))
  }
  
  df[,legend.name] <- temp

  ggplot(data = df, aes_string(x = colnames(df)[1], y = "correctness",group = legend.name, color = legend.name)) + 
    geom_line(size=1.2, linetype = 1) +
    ggtitle(title) + ylim(ymin,ymax) + xlim(xmin,xmax)
}

diffchange <- function(df){
  diff <- aggregate(correctness ~ cut.workers.per.round ,data = df, mean)
  diff$change <- round(diff$correctness - diff$correctness[1], 5)
  
  View(diff)
}

diffchange1 <- function(df){
  diff <- aggregate(correctness ~ cut.workers.per.round ,data = df, mean)
  diff <- diff[c(1,3,5),]
  diff$change <- round(diff$correctness - diff$correctness[1], 5)
  
  View(diff)
}

diffchange2 <- function(df){
  diff <- aggregate(correctness ~ cut.workers.per.round ,data = df, mean)
  diff <- diff[c(2,4,6),]
  diff$change <- round(diff$correctness - diff$correctness[1], 5)
  
  View(diff)
}

cutcheck <- function(){
  cut <- onevar.df("newcut0.csv")
  ggplot(cut, aes(x = cut[,1], y = cut[,2])) + geom_smooth(se = FALSE, colour="black") + 
    theme_bw() + xlab("Percent of cut workers") + ylab("Correctness") +
    geom_abline(intercept = cut[1,2], slope = 0, colour = "green", size = 1.2) +
    scale_x_discrete(breaks = 1:20) + ggtitle("Correctness by Percent of Cut Workers")
}

cut1check <- function(){
  cut <- onevar.df("newcut1.csv")
  ggplot(cut, aes(x = cut[,1], y = cut[,2])) + geom_smooth(se = FALSE, colour="black") + 
    theme_bw() + xlab("Percent of cut workers") + ylab("Correctness") +
    geom_abline(intercept = cut[1,2], slope = 0, colour = "green", size = 1.2) +
    scale_x_discrete(breaks = 1:20) + ggtitle("Correctness by Percent of Cut Workers")
}

#17/3/59
#result1 : cut workers every 30,90 ticks [soft penalty]
result1 <- function(){
  result <- twovar.df("result1.csv")

  result <<- aggregate(correctness ~ .,data = result, mean)
  result$round.cut <- factor(result$round.cut)
  
  plot <- x2plot(result)
  plot + ggtitle("Performance of Majority Voting with Soft Penalty")
}

#result2 : test area 100 soft - no
result2 <- function(){
  result <- twovar.df("result2.csv")
  
  result <<- aggregate(correctness ~ .,data = result, mean)
  
  plot <- x2plot(result)
  plot + ggtitle("Performance of Majority Voting with Soft Penalty")
}

#result3 : test area 100 soft - no
result3 <- function(){
  result <- onevar.df("result3.csv")
  
  plot <- xplot(result)
  plot + ggtitle("Performance of Majority Voting with Soft Penalty")
}

#result4 : test set soft-no 100
result4 <- function(){
  result <- twovar.df("result4.csv")
  
  result <<- aggregate(correctness ~ .,data = result, mean)
  
  plot <- x2plot(result)
  plot + ggtitle("Performance of Majority Voting with Soft Penalty")
}

#result5 : test set soft-no 1
result5 <- function(){
  result <- twovar.df("result5.csv")
  
  result <<- aggregate(correctness ~ .,data = result, mean)
  
  plot <- x2plot(result)
  plot + ggtitle("Performance of Majority Voting with Soft Penalty")
}

a <- function(var){
  result <<- onevar.df(paste0("a",var,".csv"))
  xplot(result)
}

A <- function(vec){
  param <- paste0("a",vec,".csv")
  result <- lapply(param, onevar.df)
  
  result[[1]] <- result[[1]]
  
  plot <- nlineplot(result,list("MV","MV Soft"),"Data.Fusion", xmin = 40, xmax = 60)
  plot + ggtitle("Performance between Majority Voting")
}

b <- function(var){
  result <<- onevar.df(paste0("b",var,".csv"))
  xplot(result)
}

all <- function(vec){
  param <- paste0(vec,".csv")
  result <- lapply(param, onevar.df)

  plot <- nlineplot(result,list("MV","MV Soft"),"Data.Fusion")
  plot + ggtitle("Performance between Majority Voting")
}

mv <- function(){
  vec <- c("dummy100","mv11", "mv12","mv2","mv3")
  param <- paste0(vec,".csv")
  result <<- lapply(param, onevar.df)
  
  plot <- nlineplot(result,list("No Adversaries","Type 1 Flood","Type 1 Normal","Type 2","Type 3"),"Data.Fusion")
  plot + ggtitle("Performance between Majority Voting") + theme_bw()
}

mvfix <- function(){
  vec <- c("dummy100","a3", "a3.2","a2","a1")
  param <- paste0(vec,".csv")
  result <<- lapply(param, onevar.df)
  
  plot <- nlineplot(result,list("No Adversaries","Type 1 Flood","Type 1 Normal","Type 2","Type 3"),"Data.Fusion")
  plot + ggtitle("Performance between Majority Voting")
}

mvsoft <- function(){
  vec <- c("dummy100","mvhard11", "mvhard12","mvhard2","mvhard3")
  param <- paste0(vec,".csv")
  result <<- lapply(param, onevar.df)
  
  plot <- nlineplot(result,list("No Adversaries","Type 1 Flood","Type 1 Normal","Type 2","Type 3"),"Data.Fusion")
  plot + ggtitle("Performance between Majority Voting and Soft Penalty")
}

mvhard <- function(){
  vec <- c("dummy100","mvhard11", "mvhard12","mvhard2","mvhard3")
  param <- paste0(vec,".csv")
  result <<- lapply(param, onevar.df)
  
  plot <- nlineplot(result,list("No Adversaries","Type 1 Flood","Type 1 Normal","Type 2","Type 3"),"Data.Fusion")
  plot + ggtitle("Performance between Majority Voting and Hard Penalty")
}

em <- function(type = 0){
  vec <- c("dummy100","em11", "em12","em2","em3")
  param <- paste0(vec,".csv")
  result <<- lapply(param, onevar.df)
  
  plot <- nlineplot(result,list("No Adversaries","Type 1 Flood","Type 1 Normal","Type 2","Type 3"),"Data.Fusion")
  plot + ggtitle("Performance between EM Algorithm")
  
}

emfix <- function(){
  vec <- c("dummy100","emfix11", "emfix12","emfix2","emfix3")
  param <- paste0(vec,".csv")
  result <<- lapply(param, onevar.df)
  
  plot <- nlineplot(result,list("No Adversaries","Type 1 Flood","Type 1 Normal","Type 2","Type 3"),"Data.Fusion")
  plot + ggtitle("Performance between EM Algorithm ")
}

emsoft <- function(type = 0){
  vec <- c("dummy100","emsoft11", "emsoft12","emsoft2","emsoft3")
  param <- paste0(vec,".csv")
  result <<- lapply(param, onevar.df)
  
  plot <- nlineplot(result,list("No Adversaries","Type 1 Flood","Type 1 Normal","Type 2","Type 3"),"Data.Fusion")
  plot + ggtitle("Performance between EM Algorithm") + theme_bw()
  
}

emhard <- function(type = 0){
  vec <- c("dummy100","emsoft11", "emsoft12","emsoft2","emsoft3")
  param <- paste0(vec,".csv")
  result <<- lapply(param, onevar.df)
  
  plot <- nlineplot(result,list("No Adversaries","Type 1 Flood","Type 1 Normal","Type 2","Type 3"),"Data.Fusion")
  plot + ggtitle("Performance between EM Algorithm and Hard Penalty")
  
}

mvsoftcut <- function(){
  vec <- c("dummy100","mvsoft11cut","mvsoft12cut","mvsoft2cut","mvsoft3cut")
  param <- paste0(vec,".csv")
  result <<- lapply(param, onevar.df)
  
  plot <- nlineplot(result,list("No Adversaries","Type 1 Flood","Type 1 Normal","Type 2","Type 3"),"Data.Fusion")
  plot + ggtitle("Performance between Majority Voting and Hard Penalty")
}

mvsoftratio <- function(){
  vec <- c("mvsoft2ratio")
  param <- paste0(vec,".csv")
  result <<- lapply(param, onevar.df)
  
  plot <- nlineplot(result,list("Type 3 Flood"),"Data.Fusion", ymax = 1)
  plot + ggtitle("Performance between Majority Voting and Soft Penalty")
}

mvsoftrp <- function(){
  vec <- c("mvsoft3precision","mvsoft3recall")
  param <- paste0(vec,".csv")
  result <<- lapply(param, onevar.df)
  
  plot <- nlineplot(result,list("Precision","Recall"),"Data.Fusion",ymax = 1)
  plot + ggtitle("Performance between Majority Voting and Soft Penalty")  + 
    geom_vline(xintercept = 15, colour="black", linetype = "longdash") +
    geom_label(aes(x=15, label="number of cut workers", y= 1), colour="black", vjust= -0.3) +
    theme_bw() + scale_x_continuous(breaks = seq(5,45,5), limits= c(xmin = 5,xmax = 45)) +
    labs(y = "precision & recall")
  
}

mv3dday1 <- function(){
  vec <- c("mv3dday1","mvsoft3dday1","mvhard3dday1","emsoft3dday1","emhard3dday1")
  param <- paste0(vec,".csv")
  result <<- lapply(param, onevar.df)
  
  diff.result <<- lapply(result[-1], function(r){
    data.frame(max.count.send = r[,1], correctness = r[,2] - result[[1]][,2])
  })
  
  plot <- nlineplot(diff.result,list("MV Soft","MV Hard","EM Soft","EM Hard"),"Data.Fusion.with.Reputation.Management",ymin = -5, ymax = 5)
  plot + ggtitle("Performance between Majority Voting and Soft Penalty") +
    theme_bw() + scale_x_continuous(breaks = seq(2,16,2), limits= c(xmin = 2,xmax = 16)) +
    geom_hline(aes(yintercept = 0), colour="black", linetype = "longdash") +
    geom_label(aes(y=0, label="data fusion only",x = 4), colour="black", vjust= -0.3)

}

mv3dday2 <- function(){
  vec <- c("mvsoft3dday2","mvhard3dday2")
  param <- paste0(vec,".csv")
  result <<- lapply(param, onevar.df)
  
  
  plot1 <<- nlineplot(result,list("Soft Penalty","Hard Penalty"),"Reputation.Management",ymin = 0.5,ymax = 1)  + 
    ggtitle("Precision between Majority Voting and Soft Penalty \nwith Adversial Workers Type 3")  + 
    theme_bw() + scale_x_continuous(breaks = seq(15,40,5), limits= c(xmin = 15,xmax = 40)) +
    labs(y = "precision")
  
  
  vec <- c("mvsoft2dday2","mvhard2dday2")
  param <- paste0(vec,".csv")
  result <<- lapply(param, onevar.df)
  
  plot2 <<- nlineplot(result,list("Soft Penalty","Hard Penalty"),"Reputation.Management",ymax = 1)  + 
    ggtitle("Precision between Majority Voting and Soft Penalty \nwith Adversial Workers Type 2")  + 
    theme_bw() + scale_x_continuous(breaks = seq(15,40,5), limits= c(xmin = 15,xmax = 40)) +
    labs(y = "precision")
  
  vec <- c("mvsoft11dday","mvhard11dday2")
  param <- paste0(vec,".csv")
  result <<- lapply(param, onevar.df)
  
  plot3 <<- nlineplot(result,list("Soft Penalty","Hard Penalty"),"Reputation.Management",ymax = 1)  + 
    ggtitle("Precision between Majority Voting and Soft Penalty \nwith Adversial Workers Type 1")  + 
    theme_bw() + scale_x_continuous(breaks = seq(15,40,5), limits= c(xmin = 15,xmax = 40)) +
    labs(y = "precision")
  
  multiplot(plot1,plot2,plot3, cols = 1)
}

mv3dday3 <- function(){
  vec <- c("mv3dday3","mvsoft3dday3")
  param <- paste0(vec,".csv")
  result <<- lapply(param, onevar.df)
  
  
  plot <- nlineplot(result,list("No","Soft","Hard"),"Data.Fusion",xmin = 1,xmax = 10)
  plot + ggtitle("Performance between Majority Voting and Soft Penalty") +
    theme_bw() + scale_x_continuous(breaks = seq(2,10,2), limits= c(xmin = 2,xmax = 12))

}


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = ceiling(numPlots/cols), nrow = cols)
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

getMain <- function(name){
  vec <- c(mv = "Majority Voting",
           em = "EM Algoritm",
           mvsoft = "Majority Voting and Soft Penalty",
           mvhard = "Majority Voting and Hard Penalty",
           emsoft = "EM Algoritm and Soft Penalty",
           emhard = "EM Algoritm and Hard Penalty")
  vec[name]
}

precision <- function(name){
  vec <- paste0(name,c("3precision","2precision","11precision","12precision"))
  param <- paste0(vec,".csv")
  result <<- lapply(param, onevar.df)
  
  
  plot <- nlineplot(result,list("Type 3","Type 2", "Type 1 : flood", "Type 1 : Normal"),"Data.Fusion",ymax = 1)
  plot + ggtitle(paste("Performance between",getMain(name)))  + 
    geom_vline(xintercept = 15, colour="black", linetype = "longdash") +
    geom_label(aes(x=15, label="number of cut workers", y= 1), colour="black", vjust= -0.3) +
    theme_bw() + scale_x_continuous(breaks = seq(5,50,5), limits= c(xmin = 5,xmax = 40)) +
    labs(y = "precision")
  
}

recall <- function(name){
  vec <- paste0(name,c("3recall","2recall","11recall","12recall"))
  param <- paste0(vec,".csv")
  result <<- lapply(param, onevar.df)
  
  
  plot <- nlineplot(result,list("Type 3","Type 2", "Type 1 : flood", "Type 1 : Normal"),"Data.Fusion",ymax = 1)
  plot + ggtitle(paste("Performance between",getMain(name))) + 
    geom_vline(xintercept = 15, colour="black", linetype = "longdash") +
    geom_label(aes(x=15, label="number of cut workers", y= 1), colour="black", vjust= -0.3) +
    theme_bw() + scale_x_continuous(breaks = seq(5,100,5), limits= c(xmin = 5,xmax = 40)) +
    labs(y = "recall")
  
}

prestartcut <- function(name){
  vec <- paste0(name,c("3prestartcut","2prestartcut","11prestartcut","12prestartcut"))
  param <- paste0(vec,".csv")
  result <<- lapply(param, onevar.df)
  
  
  plot <- nlineplot(result,list("Type 3","Type 2", "Type 1 : flood", "Type 1 : Normal"),"Data.Fusion",ymax = 1)
  plot + ggtitle(paste("Performance between",getMain(name))) + 
    theme_bw() + scale_x_continuous(breaks = 1:33, limits= c(xmin = 1,xmax = 33))+
    labs(y = "precision")
  
}

restartcut <- function(name){
  vec <- paste0(name,c("3restartcut","2restartcut","11restartcut","12restartcut"))
  param <- paste0(vec,".csv")
  result <<- lapply(param, onevar.df)
  
  
  plot <- nlineplot(result,list("Type 3","Type 2", "Type 1 : flood", "Type 1 : Normal"),"Data.Fusion",ymax = 1)
   plot + ggtitle(paste("Performance between",getMain(name))) + 
  theme_bw() + scale_x_continuous(breaks = 1:33, limits= c(xmin = 1,xmax = 33))+
  labs(y = "recall")
  
}

mvsoftrestartcut <- function(){
  vec <- c("mvsoft3restartcut","mvsoft2restartcut","mvsoft11restartcut","mvsoft12restartcut")
  param <- paste0(vec,".csv")
  result <<- lapply(param, onevar.df)
  
  
  plot <- nlineplot(result,list("Type 3","Type 2", "Type 1 : flood", "Type 1 : Normal"),"Data.Fusion",ymax = 1)
  plot + ggtitle("Performance between Majority Voting and Soft Penalty")  + 
  theme_bw() + scale_x_continuous(breaks = 1:33, limits= c(xmin = 1,xmax = 33))
}

mvsoftprestartcut <- function(){
  vec <- c("mvsoft3prestartcut","mvsoft2prestartcut","mvsoft11prestartcut","mvsoft12prestartcut")
  param <- paste0(vec,".csv")
  result <<- lapply(param, onevar.df)
  
  
  plot <- nlineplot(result,list("Type 3","Type 2", "Type 1 : flood", "Type 1 : Normal"),"Data.Fusion",ymax = 1)
  plot + ggtitle("Performance between Majority Voting and Soft Penalty")  + 
    theme_bw() + scale_x_continuous(breaks = 1:33, limits= c(xmin = 1,xmax = 33))
  
}

mvhardrestartcut <- function(){
  vec <- c("mvhard3restartcut","mvhard2restartcut","mvhard11restartcut","mvhard12restartcut")
  param <- paste0(vec,".csv")
  result <<- lapply(param, onevar.df)
  
  
  plot <- nlineplot(result,list("Type 3","Type 2", "Type 1 : flood", "Type 1 : Normal"),"Data.Fusion",ymax = 1)
  plot + ggtitle("Performance between Majority Voting and Soft Penalty")  + 
    theme_bw() + scale_x_continuous(breaks = 1:33, limits= c(xmin = 1,xmax = 33))
  
}

mvhardprestartcut <- function(){
  vec <- c("mvhard3prestartcut","mvhard2prestartcut","mvhard11prestartcut","mvhard12prestartcut")
  param <- paste0(vec,".csv")
  result <<- lapply(param, onevar.df)
  
  
  plot <- nlineplot(result,list("Type 3","Type 2", "Type 1 : flood", "Type 1 : Normal"),"Data.Fusion",ymax = 1)
  plot + ggtitle("Performance between Majority Voting and Soft Penalty")  + 
    theme_bw() + scale_x_continuous(breaks = 1:33, limits= c(xmin = 1,xmax = 33))
  
}

morecut <- function(name){
  vec <- paste0(name,c("3morecut","2morecut","11morecut","12morecut"))
  param <- paste0(vec,".csv")
  result <<- lapply(param, onevar.df)
  
  
  plot <- nlineplot(result,list("Type 3","Type 2", "Type 1 : flood", "Type 1 : Normal"),"Data.Fusion",ymax = 100)
  plot + ggtitle(paste("Performance between",getMain(name)))  + 
    theme_bw() + scale_x_continuous(breaks = 1:9, limits= c(xmin = 1,xmax = 9)) +
    labs(y = "precision")
}

add <- function(name){
  vec <- paste0(name,c("3add","2add","11add","12add"))
  param <- paste0(vec,".csv")
  result <<- lapply(param, oneshot.df)
  
  plot <- nlineplot(result,list("Type 3","Type 2", "Type 1 : flood", "Type 1 : Normal"),"Data.Fusion",ymax = 100)
  plot + ggtitle(paste("Performance between",getMain(name)))  + 
    theme_bw() + scale_x_continuous(breaks = seq(0,48,2), limits= c(xmin = 1,xmax = 48))
}


add2 <- function(name){
  vec <- paste0(name,c("3add2","2add2","11add2","12add2"))
  param <- paste0(vec,".csv")
  result <<- lapply(param, oneshot.df)
  
  plot <- nlineplot(result,list("Type 3","Type 2", "Type 1 : flood", "Type 1 : Normal"),"Data.Fusion",ymax = 100)
  plot + ggtitle(paste("Performance between",getMain(name)))  + 
    theme_bw() + scale_x_continuous(breaks = seq(0,48,2), limits= c(xmin = 1,xmax = 48))
}

add.diff <- function(name){
  vec <- paste0(c("mv","mvsoft","mvhard"),name,"add")
  param <- paste0(vec,".csv")
  result <<- lapply(param, oneshot.df)
  
  plot <- nlineplot(result,list("MV","MV Soft","MV Hard"),"Data.Fusion",ymax = 100)
  plot + ggtitle(paste("Performance between",name))  + 
    theme_bw() + scale_x_continuous(breaks = seq(0,48,2), limits= c(xmin = 1,xmax = 48))
}


oneshot.df <- function(file){
  df <- read.csv(paste("result",file,sep = "/"), skip = 16)
  df <- df[1:2]
  df[,1] <- sapply(df[,1], function(x) x + 1)
  colnames(df)[2] <- "correctness" 
  
  df <- aggregate(correctness ~ .,data = df, mean)
  df 
}

area <- function(name,rate = 0){
  vec <- paste0(name,c("3area","2area","11area","12area"))
  param <- paste0(vec,".csv")
  result <<- lapply(param, twovar.df)
  
  if(rate > 0){
    result <<- lapply(result, function(x){ x[x[,2] ==  rate, ] })
  }

  
  plot <- nlineplot(result,expand.grid.list(c("Type 3","Type 2", "Type 1 : flood", "Type 1 : Normal"), c("25","50","75"),rate),"Data.Fusion",ymax = 100)
  plot + ggtitle(paste("Performance between",getMain(name)))  + 
    theme_bw() + scale_x_continuous(breaks = 1:5, limits= c(xmin = 1,xmax = 5))
}

arearate1 <- function(name){
  vec <- c(paste0("mv",name),
           paste0(c("mvsoft","mvsoft","emhard","emhard","emhard"),name,"cut"))
  param <- paste0(vec,".csv")
  result <<- lapply(param, onevar.df)
  
  plot <- nlineplot(result,list("MV","MV Soft","MV Hard","EM","EM Soft","EM Hard"),"Data.Fusion",ymin = 50,ymax = 100)
  plot + ggtitle(paste("Performance between Data Fusion and Reputation Managament in 25 area \n with Adversaries Type",name))  + 
    theme_bw() + scale_x_continuous(breaks = seq(0,100,10), limits= c(xmin = 0,xmax = 50))
}

arearate <- function(name){
  vec <- paste0(c("mv","mvsoft","mvhard","em","emsoft","emhard"),name,"arearate")
  param <- paste0(vec,".csv")
  result <<- lapply(param, onevar.df)
  
  plot <- nlineplot(result,list("MV","MV Soft","MV Hard","EM","EM Soft","EM Hard"),"Data.Fusion",ymin = 50,ymax = 100)
  plot + ggtitle(paste("Performance between Data Fusion and Reputation Managament in 25 area \n with Adversaries Type",name))  + 
    theme_bw() + scale_x_continuous(breaks = seq(0,100,10), limits= c(xmin = 0,xmax = 50))
}

expand.grid.list <- function(a,b,rate){
  df <- expand.grid(a, b)
  df <- df[df[,2] == rate,]
  df <- df[order(df[,1]),]
  
  as.list(apply(df , 1, paste, collapse="."))
}

#result2 <- lapply(result, function(x){x[x[1] <= 50,]})
#sapply(result2,function(x){mean(x[,2] )})
