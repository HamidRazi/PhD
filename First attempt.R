#clear screen
rm(list = ls())

#load packages (install if have not already)
library(data.table)
library(ggplot2)
library(cowplot)
library(dplyr)

session_simulate <- function(n_trials, beta, positive_lr_local, negative_lr_local) {
  
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
  
  # steps <- seq(1,n_trials,3) #Here I've defined the True and False feedbacks as "context".
  context = sample(c(TRUE,FALSE), size=n_trials, prob = c(0.5,0.5), replace = T)
  for (t in 1:n_trials){
    Q_inital[t, 1:2] = Q
    dv[t] = Q_inital[t, 1] - Q_inital[t, 2] # PE
    
    prob_choose_left[t] = 1 - (1/(1+exp(beta*dv[t])))
    choose_left[t] = as.numeric(runif(1)<=prob_choose_left[t])
    
    outcome[t, 1] = as.numeric(runif(1)<=0.75)
    outcome[t, 2] = as.numeric(runif(1)<=0.25)
  
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
    # condition on learning rate #Here, for now, I've made the lr for False feedback zero. 
    if(context[t]==FALSE){
      positive_lr_local <- 0
      negative_lr_local <- 0
    }
    if(PE>0){
      # positive_lr_local <- +1*lr_local
      Q[1] = (1-positive_lr_local)*Q[1] + positive_lr_local*outcome[t, 1]
      Q[2] = (1-positive_lr_local)*Q[2] + positive_lr_local*outcome[t, 2]
    }else{
      # negative_lr_local <- -1*lr_local
      Q[1] = (1-negative_lr_local)*Q[1] + negative_lr_local*outcome[t, 1]
      Q[2] = (1-negative_lr_local)*Q[2] + negative_lr_local*outcome[t, 2]
    }
    #store new (updated) Q value
    Q_new[t, ] = Q
  }
  
  divide_trials <- 3 #I also tried to implement the accuracy rating, but later realized that it's not as simple as this, so for now, it just calculates the mean accuracy every three trials
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
  return (dat_comp)
}

n_sims = 20
n_trials = 27
beta = 3
positive_lr_local = 0.3
negative_lr_local = 0.3

for (sim in 1:n_sims){
    # context = context_options[sim]
    dat_comp = session_simulate(n_trials, beta, positive_lr_local, negative_lr_local)
    dat_comp$sim = sim
    # dat_comp$context = context
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
#setnames(dat_comp_all, "V1", "high_prob")
#setnames(dat_comp_all, "V2", "low_prob")

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


  
