#clear screen
rm(list = ls())

#load packages (install if have not already)
library(data.table)
library(ggplot2)
library(cowplot)
library(dplyr)


session_simulate <- function(n_trials, shift, nsim_pref, beta, positive_lr_local, negative_lr_local) {
  if(shift==FALSE){
    p1 = 0.75
    p2 = 0.25
    #inital Q of each bandit
    Q = c(0, 0)
    #initalise these - store trial by trial variables
    Q_inital = matrix(as.numeric(NA), n_trials, 2)
    Q_new = matrix(as.numeric(NA), n_trials, 2)
    outcome = matrix(as.numeric(NA), n_trials, 2)
    dv = rep(NA,n_trials)
    prob_choose_left = rep(NA,n_trials)
    choose_left = rep(NA,n_trials)
    correct_choice = rep(NA,n_trials)
    preffered_for_each_trial <- c()
    context = sample(c(TRUE,FALSE), size=n_trials, prob = c(0.5,0.5), replace = T)
    
    for (t in 1:n_trials){
      #---------------------------------------
      # start: simulate n times for each trial
      #---------------------------------------
      Qs = c(0, 0)
      Q_initals = c()
      Q_news = c()
      outcomes = c()
      plr_index <- 0
      nlr_index <- 0
      elr_index <- 0
      positive_lr_s = positive_lr_local
      negative_lr_s = negative_lr_local
      preffered_choice <- matrix(NA, nrow = n_sims, ncol = 3)
      n_sims_for_preferred <- nsim_pref
      for (s in 1:n_sims_for_preferred){
        
        contexts  = sample(c(TRUE,FALSE), size=1, prob = c(0.5,0.5), replace = T)
        Q_initals = Qs
        dvs = Q_initals[1] - Q_initals[2] # PE
        
        prob_choose_lefts = 1 - (1/(1+exp(beta*dvs)))
        choose_lefts = as.numeric(runif(1)<=prob_choose_lefts)
        
        outcomes[1] = as.numeric(runif(1)<=p1)
        outcomes[2] = as.numeric(runif(1)<=p2)
        
        if(contexts==FALSE){
          positive_lr_s <- 0
          negative_lr_s <- 0
        }
        if (contexts==FALSE){
          outcomes[1] = -1*outcomes[1]
          outcomes[2] = -1*outcomes[2]
        }
        # updates
        PEs <- outcomes[1]-outcomes[2]
        if(PEs>0){
          Qs[1] = (1-positive_lr_s)*Qs[1] + positive_lr_s*outcomes[1]
          Qs[2] = (1-positive_lr_s)*Qs[2] + positive_lr_s*outcomes[2]
          plr_index <- plr_index + 1
        }else if(PEs<0){
          Qs[1] = (1-negative_lr_s)*Qs[1] + negative_lr_s*outcomes[1]
          Qs[2] = (1-negative_lr_s)*Qs[2] + negative_lr_s*outcomes[2]
          nlr_index <- nlr_index + 1
        }else{
          Qs[1] = (1-negative_lr_s)*Qs[1] + negative_lr_s*outcomes[1]
          Qs[2] = (1-negative_lr_s)*Qs[2] + negative_lr_s*outcomes[2]
          elr_index <- elr_index + 1
        }
        
        preffered_choice[s,]=c(plr_index,nlr_index,elr_index)
      }
      preffered_choice <- cbind(preffered_choice,t)
      #---------------------------------------
      # end: simulate n times for each trial
      #---------------------------------------
      Q_inital[t, 1:2] = Q
      dv[t] = Q_inital[t, 1] - Q_inital[t, 2] # PE
      
      prob_choose_left[t] = 1 - (1/(1+exp(beta*dv[t])))
      choose_left[t] = as.numeric(runif(1)<=prob_choose_left[t])
      
      outcome[t, 1] = as.numeric(runif(1)<=p1)
      outcome[t, 2] = as.numeric(runif(1)<=p2)
      
      if (context[t]==FALSE){
        outcome[t, 1] = -1*outcome[t, 1]
        outcome[t, 2] = -1*outcome[t, 2]
      }
      
      if (choose_left[t]==1 & context[t]==FALSE){
        correct_choice[t] = 0
      } else if (choose_left[t]==0 & context[t]==TRUE){
        correct_choice[t] = 0
      } else {
        correct_choice[t] = 1
      }
      # updates
      PE <- outcome[t,1]-outcome[t, 2]
      # condition on learning rate
      if(context[t]==FALSE){
        positive_lr_local <- 0
        negative_lr_local <- 0
      }
      if(PE>0){
        # plr_index <- plr_index + 1
        Q[1] = (1-positive_lr_local)*Q[1] + positive_lr_local*outcome[t, 1]
        Q[2] = (1-positive_lr_local)*Q[2] + positive_lr_local*outcome[t, 2]
      }else{
        # nlr_index <- nlr_index + 1
        Q[1] = (1-negative_lr_local)*Q[1] + negative_lr_local*outcome[t, 1]
        Q[2] = (1-negative_lr_local)*Q[2] + negative_lr_local*outcome[t, 2]
      }
      # store new (updated) Q value
      Q_new[t, ] = Q
      preffered_for_each_trial=rbind(preffered_for_each_trial,preffered_choice)
    }
    divide_trials <- 3
    steps <- n_trials/divide_trials
    position <- rep(1:steps, each=divide_trials)
    if(length(position) < n_trials){
      position <- c(rep(1:steps, each=divide_trials), ceiling(steps))
      warning(paste('n_trials should be a multiplication of',divide_trials))
    }
    dat_comp = data.table(Q_inital)
    dat_comp$trials = seq(1, n_trials)
    dat_comp$dv = dv
    dat_comp$outcome_l = outcome[, 1]
    dat_comp$outcome_r = outcome[, 2]
    dat_comp$choose_l = choose_left
    dat_comp$correct_choice = correct_choice
    dat_comp$position <- position
    dat_comp$context <- context
  }else{
    Q = c(0, 0)
    Q_inital = matrix(as.numeric(NA), n_trials, 2)
    Q_new = matrix(as.numeric(NA), n_trials, 2)
    outcome = matrix(as.numeric(NA), n_trials, 2)
    dv = rep(NA,n_trials)
    prob_choose_left = rep(NA,n_trials)
    choose_left = rep(NA,n_trials)
    correct_choice = rep(NA,n_trials)
    preffered_for_each_trial <- c()
    context = sample(c(TRUE,FALSE), size=n_trials, prob = c(0.5,0.5), replace = T)
    
    for (t in 1:n_trials){
      middle_point <- round(n_trials/2)
      if(t <= 14){
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
      Q_initals = c()
      Q_news = c()
      outcomes = c()
      plr_index <- 0
      nlr_index <- 0
      elr_index <- 0
      positive_lr_s = positive_lr_local
      negative_lr_s = negative_lr_local
      preffered_choice <- matrix(NA, nrow = n_sims, ncol = 3)
      n_sims_for_preferred <- nsim_pref
      for (s in 1:n_sims_for_preferred){
        
        contexts  = sample(c(TRUE,FALSE), size=1, prob = c(0.5,0.5), replace = T)
        Q_initals = Qs
        dvs = Q_initals[1] - Q_initals[2] # PE
        
        prob_choose_lefts = 1 - (1/(1+exp(beta*dvs)))
        choose_lefts = as.numeric(runif(1)<=prob_choose_lefts)
        
        outcomes[1] = as.numeric(runif(1)<=p1)
        outcomes[2] = as.numeric(runif(1)<=p2)
        
        if(contexts==FALSE){
          positive_lr_s <- 0
          negative_lr_s <- 0
        }
        if (contexts==FALSE){
          outcomes[1] = -1*outcomes[1]
          outcomes[2] = -1*outcomes[2]
        }
        # updates
        PEs <- outcomes[1]-outcomes[2]
        if(PEs>0){
          Qs[1] = (1-positive_lr_s)*Qs[1] + positive_lr_s*outcomes[1]
          Qs[2] = (1-positive_lr_s)*Qs[2] + positive_lr_s*outcomes[2]
          plr_index <- plr_index + 1
        }else if(PEs<0){
          Qs[1] = (1-negative_lr_s)*Qs[1] + negative_lr_s*outcomes[1]
          Qs[2] = (1-negative_lr_s)*Qs[2] + negative_lr_s*outcomes[2]
          nlr_index <- nlr_index + 1
        }else{
          Qs[1] = (1-negative_lr_s)*Qs[1] + negative_lr_s*outcomes[1]
          Qs[2] = (1-negative_lr_s)*Qs[2] + negative_lr_s*outcomes[2]
          elr_index <- elr_index + 1
        }
        
        preffered_choice[s,]=c(plr_index,nlr_index,elr_index)
      }
      preffered_choice <- cbind(preffered_choice,t)
      #---------------------------------------
      # end: simulate n times for each trial
      #---------------------------------------
      Q_inital[t, 1:2] = Q
      dv[t] = Q_inital[t, 1] - Q_inital[t, 2] # PE
      
      prob_choose_left[t] = 1 - (1/(1+exp(beta*dv[t])))
      choose_left[t] = as.numeric(runif(1)<=prob_choose_left[t])
      
      outcome[t, 1] = as.numeric(runif(1)<=p1)
      outcome[t, 2] = as.numeric(runif(1)<=p2)
      
      if (context[t]==FALSE){
        outcome[t, 1] = -1*outcome[t, 1]
        outcome[t, 2] = -1*outcome[t, 2]
      }
      
      if (choose_left[t]==1 & context[t]==FALSE){
        correct_choice[t] = 0
      } else if (choose_left[t]==0 & context[t]==TRUE){
        correct_choice[t] = 0
      } else {
        correct_choice[t] = 1
      }
      # updates
      PE <- outcome[t,1]-outcome[t, 2]
      # condition on learning rate
      if(context[t]==FALSE){
        positive_lr_local <- 0
        negative_lr_local <- 0
      }
      if(PE>0){
        # plr_index <- plr_index + 1
        Q[1] = (1-positive_lr_local)*Q[1] + positive_lr_local*outcome[t, 1]
        Q[2] = (1-positive_lr_local)*Q[2] + positive_lr_local*outcome[t, 2]
      }else{
        # nlr_index <- nlr_index + 1
        Q[1] = (1-negative_lr_local)*Q[1] + negative_lr_local*outcome[t, 1]
        Q[2] = (1-negative_lr_local)*Q[2] + negative_lr_local*outcome[t, 2]
      }
      # store new (updated) Q value
      Q_new[t, ] = Q
      preffered_for_each_trial=rbind(preffered_for_each_trial,preffered_choice)
    }
    divide_trials <- 3
    steps <- n_trials/divide_trials
    position <- rep(1:steps, each=divide_trials)
    if(length(position) < n_trials){
      position <- c(rep(1:steps, each=divide_trials), ceiling(steps))
      warning(paste('n_trials should be a multiplication of',divide_trials))
    }
    dat_comp = data.table(Q_inital)
    dat_comp$trials = seq(1, n_trials)
    dat_comp$dv = dv
    dat_comp$outcome_l = outcome[, 1]
    dat_comp$outcome_r = outcome[, 2]
    dat_comp$choose_l = choose_left
    dat_comp$correct_choice = correct_choice
    dat_comp$position <- position
    dat_comp$context <- context
  }
  
  return(list(dataset=dat_comp, preferred_choice=preffered_for_each_trial))
}
    
n_sims = 20
nsim_pref = 20
n_trials = 27
beta = 3
positive_lr_local = 0.3
negative_lr_local = 0.7

#----------------------------------
# without shift (break point)
#----------------------------------
for (sim in 1:n_sims){
  dat_comp <- session_simulate(n_trials, shift=FALSE, nsim_pref, beta, positive_lr_local, negative_lr_local)
  dat_comp  <- dat_comp$dataset
  # rate accuracy in every three steps
  steps <- n_trials/3
  position <- seq(3,n_trials, by=3)
  rate_accuracy <- dat_comp %>% group_by(position) %>% summarise(rate_accuracy=round(mean(correct_choice)*100,2)) %>% 
    mutate(position_in_df=seq(3,n_trials, by=3))
  dat_comp$accuracy <- rep(NA,nrow(dat_comp))
  dat_comp$accuracy[rate_accuracy$position_in_df] <- rate_accuracy$rate_accuracy
  if (sim==1){
    dat_comp_all = dat_comp
  } else {
    dat_comp_all = rbind(dat_comp_all, dat_comp)
  }
}
# overview of rate accuracy in every three steps
accuracy_in_every_3_steps <- as.numeric(na.omit(dat_comp_all$accuracy))
accuracy_in_every_3_steps

# mean of accuracy in every 3 trials for each trial
df <- dat_comp_all %>% group_by(trials) %>% summarise(acc_mean=mean(accuracy, na.rm=T))
df <- na.omit(df)
ggplot(df, aes(x=trials, y=acc_mean)) +
  geom_point(size=4, color='blue')+
  geom_line(size=2, color='red')+
  xlab('Trials')+ylab('Mean Accuracy')+
  ggtitle('Mean Accuracy in Every 3 Trials')+
  theme_cowplot()+
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     n.breaks = 10,
                     limits = c(0, 100))+
  scale_x_continuous(breaks = df$trials)
# mean of accuracy in every 3 trials for each trial by context

df <- dat_comp_all %>% group_by(trials,context) %>% summarise(acc_mean=mean(accuracy, na.rm=T))
df <- na.omit(df)
ggplot(df, aes(x=trials, y=acc_mean, color=context)) +
  geom_point(size=4)+
  geom_line(size=2)+
  xlab('Trials')+ylab('Mean Accuracy')+
  ggtitle('Mean Accuracy in Every 3 Trials')+
  theme_cowplot()+
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     n.breaks = 10,
                     limits = c(0, 100))+
  scale_x_continuous(breaks = df$trials)

# new plot
dat_comp  <- session_simulate(n_trials, shift=FALSE, nsim_pref, beta, positive_lr_local, negative_lr_local)
preffered <- dat_comp$preferred_choice
# rownames(preffered) <- rep(1:n_trials, each=n_sims)
colnames(preffered) <- c('greater','less','equal', 'trial')
preffered <- as.data.frame(preffered)
head(preffered)
H <- preffered %>% group_by(trial) %>% summarise(greater=sum(greater)/sum(greater,less,equal), less=sum(less)/sum(greater,less,equal), equal=sum(equal)/sum(greater,less,equal)) 
# H <- preffered %>% group_by(trial) %>% summarise(greater=mean(greater), less=mean(less), equal=mean(equal)) 
H <- as.data.frame(H)
long <- reshape(H, ids = row.names(H),
                times = names(H)[-1], v.names = 'value',
                varying = list(names(H)[-1]), direction = "long")
colnames(long) <- c('trial','state','value', 'id')
rownames(long) <- NULL
long$state <- as.factor(long$state)
long$trial <- as.numeric(long$trial)
legend_labels <- c(expression(alpha[`+`]>alpha[`-`]),
                   expression(alpha[`+`]<alpha[`-`]),
                   expression(alpha[`+`]==alpha[`-`]))
ggplot(long, aes(x=trial, y=value))+
  geom_line(aes(color = state), size=2)+
  geom_point(aes(color = state), size=2)+
  xlab('Trial')+ylab('Preferred Choice Rate')+
  theme_cowplot() +
  scale_y_continuous(n.breaks = 10,
                     limits = c(0, 1))+
  scale_x_continuous(breaks = 1:n_trials)+
  scale_color_manual(name='State',values = c("red", "blue", 'green'), labels = parse(text = legend_labels))

### plot average Q values over trials
#average
bandit_plot = dat_comp_all[, .(high_prob = mean(V1), low_prob = mean(V2)), by=.(trials, context)]

#convert to long form
bandit_plot = melt(bandit_plot,  id.vars = c('trials', 'context'),
                   variable.name = 'bandit')

ggplot(bandit_plot, aes(x=trials, y=value, colour = bandit, shape=context, size=5))+ 
  geom_point() +
  theme_cowplot()+
  ylab("Qvalue")+
  ylim(-1,1)

### plot proportion of correct choices over time
correct_plot = dat_comp_all[, .(percent_correct = sum(correct_choice)/.N), by=.(trials, context)]
ggplot(correct_plot, aes(x=trials, y=percent_correct, colour = context)) +
  geom_line()+
  theme_cowplot()+
  ylim(0,1)

#----------------------------------
# with shift (break point) - new
#----------------------------------
n_sims = 20
nsim_pref = 20
n_trials = 27
beta = 3
positive_lr_local = 0.3
negative_lr_local = 0.7
for (sim in 1:n_sims){
  dat_comp <- session_simulate(n_trials, shift=TRUE, nsim_pref, beta, positive_lr_local, negative_lr_local)
  dat_comp  <- dat_comp$dataset
  # rate accuracy in every three steps
  steps <- n_trials/3
  position <- seq(3,n_trials, by=3)
  rate_accuracy <- dat_comp %>% group_by(position) %>% summarise(rate_accuracy=round(mean(correct_choice)*100,2)) %>% 
    mutate(position_in_df=seq(3,n_trials, by=3))
  dat_comp$accuracy <- rep(NA,nrow(dat_comp))
  dat_comp$accuracy[rate_accuracy$position_in_df] <- rate_accuracy$rate_accuracy
  if (sim==1){
    dat_comp_all = dat_comp
  } else {
    dat_comp_all = rbind(dat_comp_all, dat_comp)
  }
}
# overview of rate accuracy in every three steps
accuracy_in_every_3_steps <- as.numeric(na.omit(dat_comp_all$accuracy))
accuracy_in_every_3_steps

# mean of accuracy in every 3 trials for each trial
df <- dat_comp_all %>% group_by(trials) %>% summarise(acc_mean=mean(accuracy, na.rm=T))
df <- na.omit(df)
ggplot(df, aes(x=trials, y=acc_mean)) +
  geom_point(size=4, color='blue')+
  geom_line(size=2, color='red')+
  xlab('Trials')+ylab('Mean Accuracy')+
  ggtitle('Mean Accuracy in Every 3 Trials')+
  theme_cowplot()+
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     n.breaks = 10,
                     limits = c(0, 100))+
  scale_x_continuous(breaks = df$trials)+
  geom_vline(xintercept = round(n_trials/2), color='green', linetype=2, size=1)+
  geom_hline(yintercept = 50, color='grey', linetype=1, size=1)

# mean of accuracy in every 3 trials for each trial by context

df <- dat_comp_all %>% group_by(trials,context) %>% summarise(acc_mean=mean(accuracy, na.rm=T))
df <- na.omit(df)
ggplot(df, aes(x=trials, y=acc_mean, color=context)) +
  geom_point(size=4)+
  geom_line(size=2)+
  xlab('Trials')+ylab('Mean Accuracy')+
  ggtitle('Mean Accuracy in Every 3 Trials')+
  theme_cowplot()+
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     n.breaks = 10,
                     limits = c(0, 100))+
  scale_x_continuous(breaks = df$trials)+
  geom_vline(xintercept = round(n_trials/2), color='green', linetype=2,size=1)+
  geom_hline(yintercept = 50, color='grey', linetype=1, size=1)



# new plot
dat_comp  <- session_simulate(n_trials, shift=TRUE, nsim_pref, beta, positive_lr_local, negative_lr_local)
preffered <- dat_comp$preferred_choice
# rownames(preffered) <- rep(1:n_trials, each=n_sims)
colnames(preffered) <- c('greater','less','equal', 'trial')
preffered <- as.data.frame(preffered)
head(preffered)
H <- preffered %>% group_by(trial) %>% summarise(greater=sum(greater)/sum(greater,less,equal), less=sum(less)/sum(greater,less,equal), equal=sum(equal)/sum(greater,less,equal)) 
# H <- preffered %>% group_by(trial) %>% summarise(greater=mean(greater), less=mean(less), equal=mean(equal)) 
H <- as.data.frame(H)
long <- reshape(H, ids = row.names(H),
                times = names(H)[-1], v.names = 'value',
                varying = list(names(H)[-1]), direction = "long")
colnames(long) <- c('trial','state','value', 'id')
rownames(long) <- NULL
long$state <- as.factor(long$state)
long$trial <- as.numeric(long$trial)
legend_labels <- c(expression(alpha[`+`]>alpha[`-`]),
                   expression(alpha[`+`]<alpha[`-`]),
                   expression(alpha[`+`]==alpha[`-`]))
ggplot(long, aes(x=trial, y=value))+
  geom_line(aes(color = state), size=2)+
  geom_point(aes(color = state), size=2)+
  xlab('Trial')+ylab('Preferred Choice Rate')+
  theme_cowplot() +
  scale_y_continuous(n.breaks = 10,
                     limits = c(0, 1))+
  scale_x_continuous(breaks = 1:n_trials)+
  scale_color_manual(name='State',values = c("red", "blue", 'green'), labels = parse(text = legend_labels))+
  geom_vline(xintercept = round(n_trials/2), color='brown', linetype='dashed', size=1)+
  geom_hline(yintercept = 0.5, color='grey', linetype=1, size=1)


### plot average Q values over trials
#average
bandit_plot = dat_comp_all[, .(high_prob = mean(V1), low_prob = mean(V2)), by=.(trials, context)]

#convert to long form
bandit_plot = melt(bandit_plot,  id.vars = c('trials', 'context'),
                   variable.name = 'bandit')

ggplot(bandit_plot, aes(x=trials, y=value, colour = bandit, shape=context, size=5))+ 
  geom_point() +
  theme_cowplot()+
  ylab("Qvalue")+
  ylim(-1,1)+
  geom_vline(xintercept = round(n_trials/2), color='green', linetype='dashed', size=1)+
  geom_hline(yintercept = 0.5, color='grey', linetype=1, size=1)




### plot proportion of correct choices over time
correct_plot = dat_comp_all[, .(percent_correct = sum(correct_choice)/.N), by=.(trials, context)]
ggplot(correct_plot, aes(x=trials, y=percent_correct, colour = context)) +
  geom_line()+
  theme_cowplot()+
  ylim(0,1)+
  geom_vline(xintercept = round(n_trials/2), color='green', linetype='dashed', size=1)+
  geom_hline(yintercept = 0.5, color='grey', linetype=1, size=1)



