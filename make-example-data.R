library(tidyverse)
dat <- read_csv("data/mouse.csv")
## adding simpons paradox
dat <- mutate(dat, 
              fragment = recode(DNA, "1"="141G6", "2"="152F7", "3"="230E8", "4"="285E6")) %>%
  mutate(fragment = ifelse(tg == 1, fragment, "No trisomy")) %>%
  mutate(sex = ifelse(sex == 1, "Male", "Female"))
## remove sex effect
resids <- lm(weight ~ sex, data = dat)$resid
resids <- resids/sd(resids)
indexes <- with(dat,split(1:nrow(dat), list(fragment, sex)))

set.seed(2015)
bp_sd <- 10
bp_avg <- 125
bp <- vector("numeric", nrow(dat))
for(i in seq_along(indexes)){
  ind <- indexes[[i]]
  avg <- mean(resids[ind])
  ## create a negative correlation
  new_avg <- bp_avg - 1*avg*bp_sd
  ## now create within group positive correlation
  bp[ind] <- new_avg + resids[ind] - mean(resids[ind]) + 
    rnorm(length(ind), 0, 1)
  print(c(length(ind), cor(bp[ind], dat$weight[ind])))
}
print(cor(bp, dat$weight))

dat <- select(dat, -bp)
dat <- mutate(dat, bp = bp)

dat %>% filter(sex == "Female") %>%
  ggplot(aes(weight, bp)) + #, color = fragment)) + 
  geom_point()

dat %>% filter(sex == "Female") %>%
  ggplot(aes(weight, bp, color = fragment)) + 
  geom_point()

dat %>% group_by(fragment, sex) %>%
  summarize(avg = mean(weight)) %>% ungroup() %>%
  spread(fragment, avg)


### adding residual
ind <- sample(which(dat$tg==0), 1)
dat$weight[ind] <- dat$weight[ind]*100

## adding simpons paradox
dat <- mutate(dat, 
              fragment = recode(DNA, "1"="141G6", "2"="152F7", "3"="230E8", "4"="285E6")) %>%
  mutate(fragment = ifelse(tg == 1, fragment, "No trisomy")) 

res <- dat %>% group_by(fragment) %>% 
  summarize(avg = mean(weight),
            se = sd(weight)/n())

res %>% ggplot(aes(fragment, avg)) + 
  geom_errorbar(aes(ymin = avg - se, ymax = avg+se), width = 0.25)+
  geom_bar(stat = "identity", width=0.5, fill=4, col = 1) +
  xlab("") + ylab("Outcome")
