
rm(list = ls())

library(data.table)
library(ggplot2)
library(cowplot)
library(dplyr)

session_simulate <- function(n_trials, nsim_c, beta, positive_lr, negative_lr) {
  result_for_each_condition <- list()
  for (i in 1:3) {
    dv1 = rep(NA,n_trials)
    dv2 = rep(NA, n_trials)
    prob_choose_1 = rep(NA,n_trials)
    prob_choose_2 = rep(NA,n_trials)
    choose_1 = rep(NA,n_trials)
    choose_2 = rep(NA,n_trials)
    Choices_for_each_trial <- matrix(NA, nrow=n_sims, ncol=n_trials)
    q1_for_each_trial <- matrix(NA, nrow=n_sims, ncol=n_trials)
    q2_for_each_trial <- matrix(NA, nrow=n_sims, ncol=n_trials)
    c1_for_each_trial <- matrix(NA, nrow=n_sims, ncol=n_trials)
    c2_for_each_trial <- matrix(NA, nrow=n_sims, ncol=n_trials)
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
    for (t in 1:n_trials){
      middle_point <- round(n_trials/2)
      if(t <= middle_point){
        p1 = 0.75
        p2 = 0.25
      }else{
        p1 = 0.25
        p2 = 0.75
      }
      #---------------------------------------
      # start: simulate n times for each trial
      #---------------------------------------
      Qs = c(0, 0)
      outcomes = c()
      n_sims_for_choices <- nsim_c
      for (s in 1:n_sims_for_choices){
        dv1 <- (Qs[2]-Qs[1])
        dv2 <- (Qs[1]-Qs[2])
        prob_choose_1 = 1/(1+exp(beta*dv1))
        prob_choose_2 = 1/(1+exp(beta*dv2))
        choose_1 <- as.numeric(runif(1)<=prob_choose_1)
        choose_2 <- as.numeric(runif(1)<=prob_choose_2)
        outcomes[1] = as.numeric(runif(1)<=p1)
        outcomes[2] = as.numeric(runif(1)<=p2)
        PEs <- outcomes[1]-Qs[1]
        PEs2 <- outcomes[2]-Qs[2]
        if(PEs>0){
          Qs[1] <- Qs[1] + positive_lr*PEs
        }else if(PEs<0){
          Qs[1] <- Qs[1] + negative_lr*PEs
        }
        if(PEs2>0){
          Qs[2] <- Qs[2] + positive_lr*PEs2
        }else if(PEs2<0){
          Qs[2] <- Qs[2] + negative_lr*PEs2
        }
   
        q1_for_each_trial[s,t] <- Qs[1]
        q2_for_each_trial[s,t] <- Qs[2]
        c1_for_each_trial[s,t] <- choose_1
        c2_for_each_trial[s,t] <- choose_2
      }
    }
    q1=apply(q1_for_each_trial, 2, mean)
    q2=apply(q2_for_each_trial, 2, mean)
    c1=apply(c1_for_each_trial, 2, mean)
    c2=apply(c2_for_each_trial, 2, mean)
    condition=i
    df=data.frame(q1,q2,c1,c2,condition)
    result_for_each_condition <- rbind(result_for_each_condition,df)
    #---------------------------------------
    # end: simulate n times for each trial
  }

  return(result_for_each_condition)
}

n_sims = 1000
nsim_c = 1000
n_trials = 24
middle_point <- round(n_trials/2)
beta = 3

# new plot
Result  <- session_simulate(n_trials, nsim_c, beta, positive_lr, negative_lr)
df <- Result
df$condition <- factor(df$condition, levels = 1:3, labels = c('greater','less','equal'))
long=data.frame(trial=rep(1:n_trials,3),
                Q1=df$q1,
                Q2=df$q2,
                Choose1=df$c1,
                Choose2=df$c2,
                Condition=df$condition)
long$Condition <- as.factor(long$Condition)
long$trial <- as.numeric(long$trial)
legend_labels <- c(expression(alpha[`+`]>alpha[`-`]),
                   expression(alpha[`+`]<alpha[`-`]),
                   expression(alpha[`+`]==alpha[`-`]))
ggplot(long, aes(x=trial, y=Q1))+
  geom_line(aes(color = Condition), size=2)+
  geom_point(aes(color = Condition), size=2)+
  xlab('Trial')+ylab('Q1')+
  theme_cowplot() +
  scale_y_continuous(n.breaks = 10,limits = c(0, 1))+
  scale_x_continuous(breaks = 1:n_trials)+
  scale_color_manual(name='State',values = c("red", "blue", 'green'), labels = parse(text = legend_labels))+
  geom_vline(xintercept = middle_point, linetype=2, size=1, col='grey')



ggplot(long, aes(x=trial, y=Q2))+
  geom_line(aes(color = Condition), size=2)+
  geom_point(aes(color = Condition), size=2)+
  xlab('Trial')+ylab('Q2')+
  theme_cowplot() +
  scale_y_continuous(n.breaks = 10,limits = c(0, 1))+
  scale_x_continuous(breaks = 1:n_trials)+
  scale_color_manual(name='State',values = c("red", "blue", 'green'), labels = parse(text = legend_labels))+
  geom_vline(xintercept = middle_point, linetype=2, size=1, col='grey')

ggplot(long, aes(x=trial, y=Choose2))+
  geom_line(aes(color = Condition), size=2)+
  geom_point(aes(color = Condition), size=2)+
  xlab('Trial')+ylab('Choose2')+
  theme_cowplot() +
  scale_y_continuous(n.breaks = 10,limits = c(0, 1))+
  scale_x_continuous(breaks = 1:n_trials)+
  scale_color_manual(name='State',values = c("red", "blue", 'green'), labels = parse(text = legend_labels))+
  geom_vline(xintercept = middle_point, linetype=2, size=1, col='grey')


ggplot(long, aes(x=trial, y=Choose1))+
  geom_line(aes(color = Condition), size=2)+
  geom_point(aes(color = Condition), size=2)+
  xlab('Trial')+ylab('Choose1')+
  theme_cowplot() +
  scale_y_continuous(n.breaks = 10,limits = c(0, 1))+
  scale_x_continuous(breaks = 1:n_trials)+
  scale_color_manual(name='State',values = c("red", "blue", 'green'), labels = parse(text = legend_labels))+
  geom_vline(xintercept = middle_point, linetype=2, size=1, col='grey')


