#clear screen
rm(list = ls())

#load packages (install if have not already)
library(data.table)
library(ggplot2)
library(cowplot)

session_simulate <- function(context, n_trials, beta, lr_local) {
  
  #inital Q of each bandit
  Q = c(0, 0)
  
  #initalise these - store trial by trial variables
  Q_inital = matrix(as.numeric(NA), n_trials, 2)
  Q_new = matrix(as.numeric(NA), n_trials, 2)
  
  outcome = matrix(as.numeric(NA), n_trials, 2)
  
  dv = matrix(as.numeric(NA), n_trials, 1)
  
  prob_choose_left = matrix(as.numeric(NA), n_trials, 1)
  choose_left = matrix(as.numeric(NA), n_trials, 1)
  correct_choice = matrix(as.numeric(NA), n_trials, 1)
   
for (t in 1:n_trials){
  
    Q_inital[t, 1:2] = Q
    dv[t] = Q_inital[t, 1] - Q_inital[t, 2]
    prob_choose_left[t] = 1 - (1/(1+exp(beta*dv[t])))
    choose_left[t] = as.numeric(runif(1)<=prob_choose_left[t])
    
    outcome[t, 1] = as.numeric(runif(1)<=0.75)
    outcome[t, 2] = as.numeric(runif(1)<=0.25)
  
    if (context=="negative"){
      outcome[t, 1] = -1*outcome[t, 1]
      outcome[t, 2] = -1*outcome[t, 2]
    }
  
    if (choose_left[t]==1 & context=="negative"){
      correct_choice[t] = 0
    } else if (choose_left[t]==0 & context=="positive"){
      correct_choice[t] = 0
    } else {
      correct_choice[t] = 1
    }
  
    #updates
    Q[1] = (1-lr_local)*Q[1] + lr_local*outcome[t, 1]
    Q[2] = (1-lr_local)*Q[2] + lr_local*outcome[t, 2]
    
    #store new (updated) Q value
    Q_new[t, 1:2] = Q
  
}
  
    dat_comp = data.table(Q_inital)
    dat_comp$trials = seq(1, n_trials)
    dat_comp$dv = dv
    dat_comp$outcome_l = outcome[, 1]
    dat_comp$outcome_r = outcome[, 2]
    dat_comp$choose_l = choose_left
    dat_comp$correct_choice = correct_choice
    
    return (dat_comp)
  
}

n_sims = 20

context_options = rep(c("positive", "negative"), n_sims/2)

n_trials = 25
beta = 3
lr_local = 0.3

for (sim in 1:n_sims){
  
    context = context_options[sim]
    dat_comp = session_simulate(context, n_trials, beta, lr_local)
    dat_comp$sim = sim
    dat_comp$context = context
    
    if (sim==1){
      dat_comp_all = dat_comp
    } else {
      dat_comp_all = rbind(dat_comp_all, dat_comp)
    }
}

#setnames(dat_comp_all, "V1", "high_prob")
#setnames(dat_comp_all, "V2", "low_prob")

### plot average Q values over trials
#average
bandit_plot = dat_comp_all[, .(high_prob = mean(V1), low_prob = mean(V2)), by=.(trials, context)]

#convert to long form
bandit_plot = melt(bandit_plot,  id.vars = c('trials', 'context'), variable.name = 'bandit')

"plot"
ggplot(dat = bandit_plot, aes(x=trials, y=value, colour = bandit, shape=context, size=5)) + geom_point() +theme_cowplot()+ylab("Qvalue")+ylim(-1,1)

### plot proportion of correct choices over time
correct_plot = dat_comp_all[, .(percent_correct = sum(correct_choice)/.N), by=.(trials, context)]
ggplot(dat = correct_plot, aes(x=trials, y=percent_correct, colour = context)) + geom_line()+theme_cowplot()+ylim(0,1)


  
  