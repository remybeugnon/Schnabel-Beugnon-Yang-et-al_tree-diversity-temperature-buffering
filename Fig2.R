# R version 4.3.1 (2023-06-16)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Ventura 13.4.1
#
# Package: 
# - dplyr_1.1.2
# - stringr_1.5.0
# - readxl_1.4.2
# - nlme_3.1-162
# - emmeans_1.8.7
# - mgcv_1.8-42
# - piecewiseSEM_2.3.0
# - car_3.1-2
# - ggplot2_3.4.2
# - ggeffects_1.2.3
# - ggpubr_0.6.0  

# Cleaning workspace
rm(list = ls())

#### > 1. Packages ####
libs <- c(
  # Data handling
  'dplyr', 'stringr', 'readxl',
  # Statistical analyses
  'nlme', 'emmeans', "mgcv", 'car',
  # Data visualization
  'ggplot2', 'ggpubr', 'ggeffects'
)
invisible(lapply(libs, library, character.only = T))

#### > 2. Data ####
unzip('climate-data.csv.zip')
df.clim = read.csv('climate-data.csv')

d.1.2 = df.clim %>% 
  group_by(site, plot, year, TreeDiv) %>%
  filter(!is.na(temp) & TreeDiv > 0) |> 
  # Calculate buffering
  summarise(T.buff = mean(temp) / sd(temp))

d.1.2$year = as.factor(d.1.2$year)

d.2.2 = df.clim %>% 
  filter(!is.na(temp) &  TreeDiv > 0) |> 
  group_by(site, plot, year, month, TreeDiv) %>%
  summarise(T.buff = mean(temp) / sd(temp)) %>%
  mutate(month.f = factor(month)) |>
  mutate(year.month = factor(paste(year, month))) 

df.macro.T = read.csv("macroclimate.csv") %>%
  filter(year %in% 2015:2020) %>% 
  dplyr::select(year, month, avg.t = tmp_celsius, min.t = tmn_celsius , max.t = tmx_celsius)

df.macro.T.year = df.macro.T %>%
  group_by(year) %>%
  summarise(mean.avg.t = mean(avg.t),
            sd.avg.t = sd(avg.t), 
            min.t = mean(min.t),
            max.t = mean(max.t)
  )

df.macro.plot =
  df.macro.T %>%
  filter(year %in% 2015:2020) %>%
  group_by(month) %>%
  summarise(
    avg = mean(avg.t),
    int.pos = mean(avg.t) + 1.96 * sd(avg.t),
    int.neg = mean(avg.t) - 1.96 * sd(avg.t)
  ) %>%
  mutate(group = as.factor(month))

df.macro.spei = read.csv("spei.csv") %>%
  dplyr::select(year, spei = SPEI12)

#### > 3. Statistical analyses ####
#### >> 3.1 Monthly buffering ####
mod.2.2 = lme(T.buff ~ log(TreeDiv, base = 2) * month.f,
              random = ~ 1|site/plot/year,
              data = d.2.2,
              correlation=corCAR1(form = ~year.month),
              na.action=na.exclude)
summary(mod.2.2)
anova(mod.2.2)

df.out <- matrix(nrow = 1, ncol = 6) %>% as.data.frame() 
colnames(df.out) <- c("Value", "Std.Error", "DF", "t-value", "p-value", "month")
for (m in 1:12) {
  mod.month <- lme(T.buff ~ log(TreeDiv, base = 2),
                   random = ~ 1|site/plot/year,
                   data = d.2.2 %>% filter(month == m),
                   correlation=corCAR1(form = ~year),
                   na.action=na.exclude)
  
  df <- summary(mod.month)$tTable[2,] %>% 
    matrix(nrow=1) %>% 
    as.data.frame()
  colnames(df) <- c("Value", "Std.Error", "DF", "t-value", "p-value")
  df$month <- m
  df.out <- rbind(df.out,df)
}

df.out$month <- as.factor(df.out$month)
df.out <- df.out[-1,]

# Calculate random effect residuals
mod.2.2.res = lme(T.buff ~ 1,
                  random = ~ 1|site/plot/year,
                  data = d.2.2,
                  correlation=corCAR1(form = ~year.month),
                  na.action=na.exclude)

d.2.2$res = residuals(mod.2.2.res) + mod.2.2.res$coefficients$fixed
d.2.2$group = d.2.2$month.f
pred.2.2   = ggpredict(model = mod.2.2,   terms = c("TreeDiv", 'month.f'))

ann_text <- data.frame(
  lab = c(paste0("p ", 
                 ifelse(round(df.out$`p-value` <.001), 
                        "< 0.001", paste0("= ", 
                                          round(df.out$`p-value`, 3))))),
  group = factor(1:12, levels=c(1:12)))

#### >> 3.2 Yearly buffering ####
mod.1.2 = lme(T.buff ~ log(TreeDiv, base = 2) * year, 
              random = ~ 1|site/plot,
              data = d.1.2,
              correlation=corCAR1(form = ~year),
              na.action=na.exclude)
summary(mod.1.2)
anova(mod.1.2)

mod.1.2.3 = lme(T.buff ~ log(TreeDiv) , 
                random = ~ 1|site/plot/year,
                data = d.1.2,
                correlation=corCAR1(),
                na.action=na.exclude)

summary(mod.1.2.3)

mod.1.2.3.res = lme(T.buff ~ 1, 
                    random = ~ 1|site/plot,
                    data = d.1.2,
                    correlation=corCAR1(),
                    na.action=na.exclude)
mod.1.2.3.res
d.1.2$res = residuals(mod.1.2.3.res) + mod.1.2.3.res$coefficients$fixed 
d.1.2$group = d.1.2$year

pred.1.2   = ggpredict(model = mod.1.2,   terms = c("TreeDiv", 'year'))
pred.1.2.3 = ggpredict(model = mod.1.2.3, terms = c("TreeDiv", 'year'))

# Test tree species richness - SPEI interaction
mod.1.2.spei = lme(T.buff ~ log(TreeDiv, base = 2) * spei, 
                   random = ~ 1|site/plot,
                   data = d.1.2 |> 
                     left_join(df.macro.spei |> 
                                 mutate(year = as.factor(year))),
                   correlation = corCAR1(form = ~year),
                   na.action=na.exclude)
summary(mod.1.2.spei)
anova(mod.1.2.spei)

#### > 4. Figure 2 ####
#### >> 4.1 Figure 2.A. ####
# Macroclimate second axis conversion factors
coef = (9/30)
int = 0

p.month =
  ggplot(data = NULL) + 
  # Macroclimate
  geom_point(data = df.macro.plot, 
             aes(x = 4, y = (avg-int) * coef),
             fill = 'blue', alpha = 1) + 
  geom_linerange(data = df.macro.plot, 
                 aes(x = 4, ymin = (int.neg-int)*coef, 
                     ymax = (int.pos-int)*coef),
                 color = 'blue', alpha = 1) + 
  # Buffering
  geom_jitter(data = d.2.2, 
              aes(x = TreeDiv, y = res), 
              alpha = .05) + 
  geom_line(data = pred.2.2, 
            aes(x, predicted),
            col = 'black') +
  geom_ribbon(data = pred.2.2, 
              aes(x = x,
                  ymin = conf.low, 
                  ymax = conf.high), 
              alpha = .2) +
  scale_x_continuous(trans = 'log2', breaks = c(1,2,4,8, 24)) + 
  scale_y_continuous(
    sec.axis = sec_axis(~(./ coef) + int ,
                        name = expression(
                          paste('Mean monthly macroclimatic temperature [',
                                ~degree,'C]',sep='')))) +
  labs(x = 'Tree species richness x month', 
       y = "Monthly temperature\nbuffering (1/CV)", 
       title = "Monthly temperature buffering") +
  facet_grid(cols = vars(group)) +
  theme_bw() + 
  theme(axis.ticks.y.right = element_line(color = 'blue'),
        axis.text.y.right  = element_text(color = 'blue'),
        axis.title.y.right =  element_text(color = 'blue'),
        panel.grid = element_blank()) + 
  geom_text(data = ann_text,
            aes(x = 1.5, y = 11.2, 
                label = lab, angle = 90),
            size = 3)
p.month

#### >> 4.2 Figure 2.B. ####
df.macro.T = read.csv("macroclimate.csv") %>%
  filter(year %in% 2015:2020) %>% 
  select(year, month, avg.t = tmp_celsius, min.t = tmn_celsius , max.t = tmx_celsius)

df.macro.spei.1 = read.csv("spei1.csv")

# Montly df 
d.1 = df.clim %>% 
  filter(TreeDiv > 0 & !is.na(temp)) %>%
  group_by(site, plot, year, month) %>%
  summarise(Buff = mean(temp)/ sd(temp) , 
            T.max = median(temp[temp>quantile(temp,probs=c(.95))]),
            T.med = median(temp),
            T.min = median(temp[temp<quantile(temp,probs=c(.05))]),
            TreeDiv = mean(TreeDiv))

df.out <- matrix(nrow = 1, ncol = 7) %>% as.data.frame() 
colnames(df.out) <- c("Value", "Std.Error", "DF", "t-value", "p-value", "month", 'year')
for (y in 2015:2020) {
  for (m in 1:12) {
    mod.month <- lme(Buff~ log(TreeDiv, base = 2),
                     random = ~ 1|site/plot,
                     data = d.1 %>% filter(month == m, year == y),
                     na.action=na.exclude)
    
    df <- summary(mod.month)$tTable[2,] %>% matrix(nrow=1) %>% as.data.frame()
    colnames(df) <- c("Value", "Std.Error", "DF", "t-value", "p-value")
    df$month = m
    df$year = y
    
    df.out <- rbind(df.out,df)
  }
}
df.out <- df.out[-1,]

df.out = 
  df.out |> 
  left_join(df.macro.T, by = c('year', 'month')) |> 
  left_join(df.macro.spei.1, by = c('year', 'month'))

p.SPEI = 
  ggplot(data = df.out, aes(y = Value, x = spei)) + 
  geom_point(data = df.out, aes(y = Value, x = spei, color = spei)) + 
  geom_errorbar(data = df.out, aes(ymin = Value - 1.96 * Std.Error,
                                   ymax = Value + 1.96 * Std.Error,
                                   x = spei)) + 
  geom_smooth(data = df.out, aes(y = Value, x = spei), 
              color = 'black', method = 'lm') +
  scale_color_gradient(low = 'red', high = 'blue',
                       name = "SPEI 1", breaks = seq(-2,2,1), lim = c(-2.5,2.5)) + 
  labs(x = "Monthly SPEI1", y = 'Diversity effect\n on temperature buffering', 
       title = '') +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 10),
        text = element_text(size = 15), 
        legend.position = 'none')

p.SPEI

#### >> 4.3 Figure 2.C. ####
df.macro.T.year$group = df.macro.T.year$year %>% factor
df.macro.spei$group = df.macro.spei$year %>% factor
pred.1.2.p = left_join(pred.1.2  %>% data.frame(), 
                       df.macro.spei %>% dplyr::select(group, spei),
                       by = c('group'))
df.macro.spei$year = df.macro.spei$year %>% factor
d.1.2 = left_join(d.1.2  %>% data.frame(), 
                  df.macro.spei %>% 
                    dplyr::select(year, spei),
                  by = c('year'))

# Second axis conversion coefficients
coef.1 = 6
int.1 = 16.5

p.year =
  ggplot(data = pred.1.2.3, 
         aes(x, predicted)) +
  geom_jitter(data = d.1.2, aes(x = TreeDiv, y = res, 
                                color = spei), 
              alpha = .2, width = 0.05, size = .5) +
  geom_line(data = pred.1.2.3,
            aes(x, predicted, group = group),
            color = 'black', linewidth= 2) +
  geom_line(data = pred.1.2.p,
            aes(x, predicted, group = group, 
                color = spei),
            lty = 2, linewidth = 1) +
  geom_label(data = data.frame(
    x = 27.5,
    y = c(2.22, 2.03, 2.095, 1.97, 2.155, 2.28),
    label = seq(2015,2020)
  ),
  aes(x = x, y = y, label = label),
  size = 3,
  color = 'gray50') +
  annotate(geom = 'text', x = 20 , y = 1.75, label = 'p < 0.001') + 
  scale_x_continuous(trans = 'log2', breaks = c(1,2,4,8,16, 24)) + 
  scale_color_gradient(low = 'red', high = 'blue',
                       breaks = seq(-2,2,1), lim = c(-2.5,2.5)) + 
  labs(x = 'Tree species richness', y = "Annual temperature\nbuffering (1/CV)", 
       title = "Yearly temperature buffering",
       color = "SPEI value") +
  lims(y = c(1.7,2.3)) + 
  theme_bw() + 
  theme(axis.ticks.y.right = element_line(color = 'blue'),
        axis.text.y.right  = element_text(color = 'blue'),
        axis.title.y.right =  element_text(color = 'blue'),
        panel.grid = element_blank(),
        axis.text = element_text(size = 10),
        text = element_text(size = 15), 
        legend.position = 'bottom')
p.year

#### >> 4.3 Figure ####
p = ggarrange(p.month,
              p.SPEI,
              p.year, 
              nrow = 3, 
              heights = c(.4,.3,.4),
              labels = paste0(LETTERS[1:3],'.'),
              align = 'hv') 
p

# Saving figure
ggsave(filename = 'Figure-2.png', 
       height = 25, width = 20, 
       units = 'cm')