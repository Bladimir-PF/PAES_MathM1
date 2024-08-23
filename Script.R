# 1. Original dataset ====
raw_resp_math2024 <- readr::read_delim("11. PAES 2024 - Psychometric evaluation and DIF/0. Data/Corregidas_MATE1_ADM2024.csv",
                                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
raw_resp_math2024 <- as.data.frame(raw_resp_math2024)
# View(raw_resp_math2024)

# New dataset ====
dat1 = raw_resp_math2024
dat1[dat1 == 7 | dat1 == 8 | dat1 == 9] = NA

full_items = dat1[,-c(1:2)]
full_items = full_items[!apply(full_items, 1, function(x) all(is.na(x))),]

# write.csv(full_items, file = "12. IDPS 2023 - IFA and MI for Selfcare among students/0. Data/Fullitems_data.csv", row.names = FALSE)

# Specific datasets for each pair of equal forms ====
form13 = dat1[dat1$FORMA == 113 | dat1$FORMA == 115,][,3:ncol(dat1)]
form24 = dat1[dat1$FORMA == 114 | dat1$FORMA == 116,][,3:ncol(dat1)]

# Valid items per form
v_form13 = form13[, apply(form13, 2, function(x) !all(is.na(x)))]
v_form24 = form24[, apply(form24, 2, function(x) !all(is.na(x)))]

common_items <- intersect(colnames(v_form13), colnames(v_form24))

# 2. Concurrent item calibration ====
# 2.2 Full items ====
# 1PL model ====
library(mirt)
mod_1PL_fullitems = mirt(data = full_items,
                         itemtype = 'Rasch',
                         # SE = T,
                         verbose = F)
# Global model fit indices
#M2(mod_1PL_fullitems, na.rm=TRUE)

# 2PL model ====
mod_2PL_full_items = mirt(data = full_items,
                          itemtype = '2PL',
                          #SE = T,
                          verbose = F)
# Global model fit indices
#M2(mod_2PL_full_items, na.rm=TRUE)

# Relative model fit
anova(mod_1PL_fullitems, mod_2PL_full_items)

# Partial 3PL model ====
# LRT to evaluate if item i needs a c parameter or not
item_parameters = rep('2PL', ncol(full_items))
model_comparison_full = data.frame(Item = colnames(full_items),
                                   x2_diff = NA,
                                   pvalue = NA)

for (v in 1:ncol(full_items)) {
  model_list = item_parameters
  model_list[v] = '3PL'
  partial_3PL = mirt(data = full_items,
                     itemtype = model_list,
                     verbose = F)
  model_comparison_full[v,2:3] = anova(mod_2PL_full_items, partial_3PL)[2, c(6,8)]
}

model_comparison_full[order(model_comparison_full$x2_diff),]

partial3PL_items = as.numeric(rownames(model_comparison_full[which(model_comparison_full$pvalue < .01),]))
item_parameters[partial3PL_items] = '3PL'

# Partial 3PL model results
mod_partial3PL = mirt(data = full_items,
                      itemtype = item_parameters,
                      verbose = F)

coef(mod_partial3PL, IRTpars = T, printSE = T, simplify = T)
itemplot(mod_partial3PL, 2, type = 'info')

anova(mod_2PL_full_items, mod_partial3PL)

# 3. Analysis of model results ====
# Test per model ====
scale_theta <- matrix(seq(-3,3,.1))

tinfo_partial3PL <- testinfo(mod_partial3PL, scale_theta)
tinfo_2PL <- testinfo(mod_2PL_full_items, scale_theta)
tinfo_Rasch <- testinfo(mod_1PL_fullitems, scale_theta)

models_info = data.frame(
  Theta_scale = scale_theta,
  info_Rasch = tinfo_Rasch,
  info_2PL = tinfo_2PL,
  info_P3PL = tinfo_partial3PL)

models_info_long <- reshape(models_info,
                            varying = c('info_Rasch', 'info_2PL', 'info_P3PL'),
                            v.names = "Info_score",
                            timevar = "Source",
                            times = c('Rasch', '2PL', 'P3PL'),
                            new.row.names = 1:1000,
                            direction = "long")

aggregate(models_info_long$Info_score ~ models_info_long$Source, FUN = which.max)

library(ggplot2)

test_info_models = ggplot(models_info_long, aes(x = Theta_scale, y = Info_score, linetype = Source))+
  geom_line()+
  labs(x = 'Ability Level', y = 'Test Information')+
  theme_bw(base_size = 12)+
  #scale_x_continuous(breaks = NULL)+
  scale_linetype_discrete('Model', labels = c("Traditional 2 PL", "Partial 3PL", 'Rasch'))+
  theme(
    #panel.grid.major.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(linewidth=.1, color="grey" ),
    #plot.title = element_text(size = 10),
    #axis.text.x=element_blank(),
    axis.text.x=element_text(size = 12),
    axis.ticks.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.text.y=element_text(size = 12),
    axis.ticks.y=element_blank(),
    legend.title = element_blank(),
    legend.position= "bottom",
    #legend.text=element_text(size=10)
  )

ggsave(
  filename = 'test_info_models.png',
  path = 'C:/Users/geral/OneDrive - University of Iowa/PhD portfolio/1.-Research/11. PAES 2024 - Psychometric evaluation and DIF/1. Paper/Figures',
  plot = test_info_models,
  device = png,
  width = 8,
  height = 5,
  dpi = 550)

# plot(scale_theta, tinfo_partial3PL, type = 'l')
# plot(scale_theta, tinfo_2PL, type = 'l')
# plot(scale_theta, tinfo_Rasch, type = 'l')
# 
# plot(scale_theta, tinfo_2PL, type = 'l')

# Test reliability
rel_partial3PL = tinfo_partial3PL / (tinfo_partial3PL + 1)
rel_2PL = tinfo_2PL / (tinfo_2PL + 1)
rel_Rasch = tinfo_Rasch / (tinfo_Rasch + 1)

models_rel = data.frame(
  Theta_scale = scale_theta,
  rel_Rasch = rel_partial3PL,
  rel_2PL = rel_2PL,
  rel_P3PL = rel_Rasch)

models_rel_long <- reshape(models_rel,
                           varying = c('rel_Rasch', 'rel_2PL', 'rel_P3PL'),
                           v.names = "Rel_score",
                           timevar = "Source",
                           times = c('Rasch', '2PL', 'P3PL'),
                           new.row.names = 1:1000,
                           direction = "long")

test_rel_models = ggplot(models_rel_long, aes(x = Theta_scale, y = Rel_score , linetype = Source))+
  geom_line()+
  labs(x = 'Ability Level', y = 'Test Reliability')+
  theme_bw(base_size = 12)+
  #scale_x_continuous(breaks = NULL)+
  scale_linetype_discrete('Model', labels = c("Traditional 2 PL", "Partial 3PL", 'Rasch'))+
  theme(
    #panel.grid.major.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(linewidth=.1, color="grey" ),
    #plot.title = element_text(size = 10),
    #axis.text.x=element_blank(),
    axis.text.x=element_text(size = 12),
    axis.ticks.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.text.y=element_text(size = 12),
    axis.ticks.y=element_blank(),
    legend.title = element_blank(),
    legend.position= "bottom",
    #legend.text=element_text(size=10)
  )

ggsave(
  filename = 'test_rel_models.png',
  path = 'C:/Users/geral/OneDrive - University of Iowa/PhD portfolio/1.-Research/11. PAES 2024 - Psychometric evaluation and DIF/1. Paper/Figures',
  plot = test_rel_models,
  device = png,
  width = 8,
  height = 5,
  dpi = 550)

# plot(scale_theta, rel_partial3PL, type = 'l')
# plot(scale_theta, rel_2PL, type = 'l')
# plot(scale_theta, rel_Rasch, type = 'l')

# Ability estimates
theta_partial3PL <- as.data.frame(fscores(mod_partial3PL, full.scores.SE = T))
theta_2PL <- as.data.frame(fscores(mod_2PL_full_items, full.scores.SE = T))
theta_Rasch <- as.data.frame(fscores(mod_1PL_fullitems, full.scores.SE = T))

theta_partial3PL$ID = 'Partial3PL'
theta_2PL$ID = 'Full2PL'
theta_Rasch$ID = 'Rasch'

theta_models = rbind(theta_partial3PL, theta_2PL, theta_Rasch)

# write.csv(theta_models, file = 'C:/Users/geral/OneDrive - University of Iowa/PhD portfolio/1.-Research/11. PAES 2024 - Psychometric evaluation and DIF/0. Data/factor_scores.csv',
#           row.names = F)

temp_theta = rbind(
  theta_partial3PL[1:200,],
  theta_2PL[1:200,],
  theta_Rasch[1:200,])

library(ggplot2)

# Standard Errors
ggplot(theta_models, aes(x = F1, y = SE_F1, linetype = ID))+
  geom_smooth(method = 'loess', se = F, color = 'black')+
  labs(x = 'Ability Level', y = 'Standard Error')+
  theme_bw(base_size = 12)+
  #scale_x_continuous(breaks = NULL)+
  scale_linetype_discrete('Model', labels = c("Traditional 2 PL", "Partial 3PL", 'Rasch'))+
  theme(
    #panel.grid.major.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(size=.1, color="grey" ),
    #plot.title = element_text(size = 10),
    #axis.text.x=element_blank(),
    axis.text.x=element_text(size = 12),
    axis.ticks.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.text.y=element_text(size = 12),
    axis.ticks.y=element_blank(),
    legend.title = element_blank(),
    legend.position= "bottom",
    #legend.text=element_text(size=10)
  )

# Factor Scores
ggplot(theta_models, aes(x = F1, linetype = ID))+
  geom_density()+
  labs(y = 'Density', x = 'Ability Level')+
  theme_bw(base_size = 12)+
  #scale_x_continuous(breaks = NULL)+
  scale_linetype_discrete('Model', labels = c("Traditional 2 PL", "Partial 3PL", 'Rasch'))+
  theme(
    #panel.grid.major.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(size=.1, color="grey" ),
    #plot.title = element_text(size = 10),
    #axis.text.x=element_blank(),
    axis.text.x=element_text(size = 12),
    axis.ticks.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.text.y=element_text(size = 12),
    axis.ticks.y=element_blank(),
    legend.title = element_blank(),
    legend.position= "bottom",
    #legend.text=element_text(size=10)
  )

# Forms per model ====
items_form13 <- colnames(v_form13)
items_form24 <- colnames(v_form24)

# Rasch
item_pars_Rasch = data.frame(coef(mod_1PL_fullitems, simplify = T, IRTpars = T))[,1:2]

item_pars_form13R <- item_pars_Rasch[rownames(item_pars_Rasch) %in% items_form13,]
item_pars_form24R <- item_pars_Rasch[rownames(item_pars_Rasch) %in% items_form24,]

library(plink)
theta_scale = seq(-3, 3, .1)
info_item_pars_form13R <- plink::drm(item_pars_form13R, theta = theta_scale, information = T)
form13R_info <- info_item_pars_form13R@info

info_item_pars_form24R <- plink::drm(item_pars_form24R, theta = theta_scale, information = T)
form24R_info <- info_item_pars_form24R@info

form13R_info$form_info = rowSums(form13R_info[,-1])
form24R_info$form_info = rowSums(form24R_info[,-1])

formsR_info = data.frame(
  Theta_scale = theta_scale,
  Info_13R = form13R_info$form_info,
  Info_24R = form24R_info$form_info)

formsR_info_long <- reshape(formsR_info,
                            varying = c('Info_13R', 'Info_24R'),
                            v.names = "Info_score",
                            timevar = "Source",
                            times = c('Form13R', 'Form24R'),
                            new.row.names = 1:1000,
                            direction = "long")

library(ggplot2)

# Test info
test_info_Rasch = ggplot(formsR_info_long, aes(x = Theta_scale, y = Info_score, linetype = Source ))+
  geom_line()+
  labs(x = 'Ability Level', y = 'Test Information')+
  theme_bw(base_size = 12)+
  #scale_x_continuous(breaks = NULL)+
  scale_linetype_discrete('Forms', labels = c("A", "B"))+
  theme(
    #panel.grid.major.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(linewidth =.1, color="grey" ),
    #plot.title = element_text(size = 10),
    #axis.text.x=element_blank(),
    axis.text.x=element_text(size = 12),
    axis.ticks.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.text.y=element_text(size = 12),
    axis.ticks.y=element_blank(),
    legend.title = element_blank(),
    legend.position= "bottom",
    #legend.text=element_text(size=10)
  )

ggsave(
  filename = 'test_info_Rasch.png',
  path = 'C:/Users/geral/OneDrive - University of Iowa/PhD portfolio/1.-Research/11. PAES 2024 - Psychometric evaluation and DIF/1. Paper/Figures',
  plot = test_info_Rasch,
  device = png,
  width = 8,
  height = 5,
  dpi = 550)

form13R_probs <- info_item_pars_form13R@prob
form24R_probs <- info_item_pars_form24R@prob

form13R_probs$form_probs = rowSums(form13R_probs[,-1])
form24R_probs$form_probs = rowSums(form24R_probs[,-1])

formsR_probs = data.frame(
  Theta_scale = theta_scale,
  Probs_13R = form13R_probs$form_probs,
  Probs_24R = form24R_probs$form_probs)

formsR_probs_long <- reshape(formsR_probs,
                             varying = c('Probs_13R', 'Probs_24R'),
                             v.names = "Probability",
                             timevar = "Source",
                             times = c('Form13', 'Form24'),
                             new.row.names = 1:1000,
                             direction = "long")

test_ETS_Rasch = ggplot(formsR_probs_long, aes(x = Theta_scale, y = Probability, linetype = Source ))+
  geom_line()+
  labs(x = 'Ability Level', y = 'Expected True Score')+
  theme_bw(base_size = 12)+
  #scale_x_continuous(breaks = NULL)+
  scale_linetype_discrete('Forms', labels = c("A", "B"))+
  theme(
    #panel.grid.major.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(linewidth =.1, color="grey" ),
    #plot.title = element_text(size = 10),
    #axis.text.x=element_blank(),
    axis.text.x=element_text(size = 12),
    axis.ticks.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.text.y=element_text(size = 12),
    axis.ticks.y=element_blank(),
    legend.title = element_blank(),
    legend.position= "bottom",
    #legend.text=element_text(size=10)
  )

ggsave(
  filename = 'test_ETS_Rasch.png',
  path = 'C:/Users/geral/OneDrive - University of Iowa/PhD portfolio/1.-Research/11. PAES 2024 - Psychometric evaluation and DIF/1. Paper/Figures',
  plot = test_ETS_Rasch,
  device = png,
  width = 8,
  height = 5,
  dpi = 550)

# 2PL

item_pars_2PL = data.frame(coef(mod_2PL_full_items, simplify = T, IRTpars = T))[,1:2]

item_pars_form13 <- item_pars_2PL[rownames(item_pars_2PL) %in% items_form13,]
item_pars_form24 <- item_pars_2PL[rownames(item_pars_2PL) %in% items_form24,]

library(plink)
theta_scale = seq(-3, 3, .1)
info_item_pars_form13 <- plink::drm(item_pars_form13, theta = theta_scale, information = T)
form13_info <- info_item_pars_form13@info

info_item_pars_form24 <- plink::drm(item_pars_form24, theta = theta_scale, information = T)
form24_info <- info_item_pars_form24@info

form13_info$form_info = rowSums(form13_info[,-1])
form24_info$form_info = rowSums(form24_info[,-1])

forms_info = data.frame(
  Theta_scale = theta_scale,
  Info_13 = form13_info$form_info,
  Info_24 = form24_info$form_info)

forms_info_long <- reshape(forms_info,
                           varying = c('Info_13', 'Info_24'),
                           v.names = "Info_score",
                           timevar = "Source",
                           times = c('Form13', 'Form24'),
                           new.row.names = 1:1000,
                           direction = "long")

library(ggplot2)

test_info_2PL = ggplot(forms_info_long, aes(x = Theta_scale, y = Info_score, linetype = Source ))+
  geom_line()+
  labs(x = 'Ability Level', y = 'Test Information')+
  theme_bw(base_size = 12)+
  #scale_x_continuous(breaks = NULL)+
  scale_linetype_discrete('Forms', labels = c("A", "B"))+
  theme(
    #panel.grid.major.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(linewidth =.1, color="grey" ),
    #plot.title = element_text(size = 10),
    #axis.text.x=element_blank(),
    axis.text.x=element_text(size = 12),
    axis.ticks.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.text.y=element_text(size = 12),
    axis.ticks.y=element_blank(),
    legend.title = element_blank(),
    legend.position= "bottom",
    #legend.text=element_text(size=10)
  )

ggsave(
  filename = 'test_info_2PL.png',
  path = 'C:/Users/geral/OneDrive - University of Iowa/PhD portfolio/1.-Research/11. PAES 2024 - Psychometric evaluation and DIF/1. Paper/Figures',
  plot = test_info_2PL,
  device = png,
  width = 8,
  height = 5,
  dpi = 550)

form13_probs <- info_item_pars_form13@prob
form24_probs <- info_item_pars_form24@prob

form13_probs$form_probs = rowSums(form13_probs[,-1])
form24_probs$form_probs = rowSums(form24_probs[,-1])

forms_probs = data.frame(
  Theta_scale = theta_scale,
  Probs_13 = form13_probs$form_probs,
  Probs_24 = form24_probs$form_probs)

forms_probs_long <- reshape(forms_probs,
                            varying = c('Probs_13', 'Probs_24'),
                            v.names = "Probability",
                            timevar = "Source",
                            times = c('Form13', 'Form24'),
                            new.row.names = 1:1000,
                            direction = "long")

test_ETS_2PL = ggplot(forms_probs_long, aes(x = Theta_scale, y = Probability, linetype = Source ))+
  geom_line()+
  labs(x = 'Ability Level', y = 'Expected True Score')+
  theme_bw(base_size = 12)+
  #scale_x_continuous(breaks = NULL)+
  scale_linetype_discrete('Forms', labels = c("A", "B"))+
  theme(
    #panel.grid.major.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(linewidth =.1, color="grey" ),
    #plot.title = element_text(size = 10),
    #axis.text.x=element_blank(),
    axis.text.x=element_text(size = 12),
    axis.ticks.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.text.y=element_text(size = 12),
    axis.ticks.y=element_blank(),
    legend.title = element_blank(),
    legend.position= "bottom",
    #legend.text=element_text(size=10)
  )

ggsave(
  filename = 'test_ETS_2PL.png',
  path = 'C:/Users/geral/OneDrive - University of Iowa/PhD portfolio/1.-Research/11. PAES 2024 - Psychometric evaluation and DIF/1. Paper/Figures',
  plot = test_ETS_2PL,
  device = png,
  width = 8,
  height = 5,
  dpi = 550)

save.image('3. Code/Results.RData')
