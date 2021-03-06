
rm(list = ls())

library(data.table)
library(ggplot2)
library(cowplot)
library(dplyr)

session_simulate <- function(n_trials, nsims, beta) {
  # create an empty list
  result_for_each_condition <- list()
  for (i in 1:3) {
    # condition on learning rate
    if(i==1){
      positive_lr <- 0.45
      negative_lr <- 0.15
    }else if(i==2){
      positive_lr <- 0.30
      negative_lr <- 0.30
    }else if(i==3){
      positive_lr <- 0.15
      negative_lr <- 0.45
    }
    # initials for the simulation and the trials
    preffered_for_each_condition <- list()
    q1_for_each_trial <- matrix(NA, nrow=nsims, ncol=n_trials)
    q2_for_each_trial <- matrix(NA, nrow=nsims, ncol=n_trials)
    c1_for_each_trial <- matrix(NA, nrow=nsims, ncol=n_trials)
    c2_for_each_trial <- matrix(NA, nrow=nsims, ncol=n_trials)
    
    for (s in 1:nsims) {
      # initials for the trials
      Q = c(0,0)
      outcome = matrix(NA, n_trials, 2)
      dv1 = rep(NA,n_trials)
      dv2 = rep(NA, n_trials)
      prob_choose_1 = rep(NA,n_trials)
      prob_choose_2 = rep(NA,n_trials)
      choose_1 = rep(NA,n_trials)
      choose_2 = rep(NA,n_trials)
      for (t in 1:n_trials) {
        p1 = 0.50
        p2 = 0.50
        dv1[t] <- (Q[2]-Q[1])
        dv2[t] <- (Q[1]-Q[2])
        prob_choose_1[t] = 1/(1+exp(beta*dv1[t]))
        prob_choose_2[t] = 1/(1+exp(beta*dv2[t]))
        choose_1 <- as.numeric(runif(1)<=prob_choose_1[t])
        choose_2 <- as.numeric(runif(1)<=prob_choose_2[t])
        c1_for_each_trial[s,t] <- choose_1
        c2_for_each_trial[s,t] <- choose_2
        outcome[t,1] = as.numeric(runif(1)<=p1)
        outcome[t,2] = as.numeric(runif(1)<=p2)
        PE1 <- outcome[t,1]-Q[1]
        PE2 <- outcome[t,2]-Q[2]
        if(PE1>0 & c1_for_each_trial[s,t]==1){
          Q[1] <- Q[1] + positive_lr*PE1
        }else if(PE1<0 & c1_for_each_trial[s,t]==0){
          Q[1] <- Q[1] + positive_lr*PE1
        }else if(PE1<0 & c1_for_each_trial[s,t] == 1){
          Q[1] <- Q[1] + negative_lr*PE1
        }else if(PE1>0 & c1_for_each_trial[s,t] == 0){
          Q[1] <- Q[1] + negative_lr*PE1
        }
        if (PE2>0 & c2_for_each_trial[s,t] == 1){
          Q[2] <- Q[2] + positive_lr*PE2
        }else if (PE2>0 & c2_for_each_trial[s,t] == 0){
          Q[2] <- Q[2] + negative_lr*PE2
        } else if(PE2<0 & c2_for_each_trial[s,t] == 1){
          Q[2] <- Q[2] + negative_lr*PE2
        }else if(PE2<0 & c2_for_each_trial[s,t] == 0){
          Q[2] <- Q[2] + positive_lr*PE2
        }
        q1_for_each_trial[s,t] <- Q[1]
        q2_for_each_trial[s,t] <- Q[2]
      } # end of trials loop
    } # end of simulation loop
    c1_mean <- apply(c1_for_each_trial, 2, mean)
    c2_mean <- apply(c2_for_each_trial, 2, mean)
    nc1 <- sum(c1_mean>0.50)
    nc2 <- sum(c2_mean>0.50)
    condition=i
    if(nc1 > nc2){
      q1=apply(q1_for_each_trial, 2, mean)
      c1=apply(c1_for_each_trial, 2, mean)
      df=data.frame(Q=q1,C=c1,condition)
    }else{
      q2=apply(q2_for_each_trial, 2, mean)
      c2=apply(c2_for_each_trial, 2, mean)
      df=data.frame(Q=q2,C=c2,condition)
    }
    result_for_each_condition[[i]] <- df
  } # end of condition loop
  names(result_for_each_condition) <- paste0('condition',1:3)
  return(result_for_each_condition)
}

n_trials = 24
nsims    = 1000
beta     = 3
middle_point <- round(n_trials/2)
df  <- session_simulate(n_trials, nsims, beta)
df
df <- do.call(rbind, df)
df$condition <- factor(df$condition, levels = 1:3, labels = c('greater','less','equal'))
long=data.frame(trial=rep(1:n_trials,3),
                Q=df$Q,
                Choose=df$C,
                Condition=df$condition)
long$Condition <- as.factor(long$Condition)
long$trial <- as.numeric(long$trial)
legend_labels <- c(expression(alpha[`+`]>alpha[`-`]),
                   expression(alpha[`+`]<alpha[`-`]),
                   expression(alpha[`+`]==alpha[`-`]))
ggplot(long, aes(x=trial, y=Q))+
  geom_line(aes(color = Condition), size=2)+
  geom_point(aes(color = Condition), size=2)+
  xlab('Trial')+ylab('Preferred Choice Rate')+
  theme_bw() +
  scale_y_continuous(n.breaks = 10,limits = c(0, 1))+
  scale_x_continuous(breaks = 1:n_trials)+
  scale_color_manual(name='State',values = c("red", "blue", 'green'), labels = parse(text = legend_labels))
# ggsave('plot_Preferred.jpg')


ggplot(long, aes(x=trial, y=Choose))+
  geom_line(aes(color = Condition), size=2)+
  geom_point(aes(color = Condition), size=2)+
  xlab('Trial')+ylab('preferred choice rate')+
  theme_bw() +
  scale_y_continuous(n.breaks = 10,limits = c(0, 1))+
  scale_x_continuous(breaks = 1:n_trials)+
  scale_color_manual(name='State',values = c("red", "blue", 'green'), labels = parse(text = legend_labels))
# ggsave('plot_Choosen.jpg')
