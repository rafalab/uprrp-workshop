library(tidyverse)
dat <- read_csv("data/mouse.csv")
## adding simpons paradox
dat <- mutate(dat, 
              fragment = recode(DNA, "1"="141G6", "2"="152F7", "3"="230E8", "4"="285E6")) %>%
  mutate(fragment = ifelse(tg == 1, fragment, "No trisomy")) 
## remove sex effect
resids <- lm(weight ~ sex, data = dat)$resid
resids <- resids/sd(resids)
indexes <- with(dat,split(1:nrow(dat), list(sex,fragment)))

set.seed(2015)
bp_sd <- 10
bp_avg_m <- 126
bp_avg_f <- 124
bp <- vector("numeric", nrow(dat))
for(i in seq_along(indexes)){
  bp_avg <- ifelse(grepl("0\\.",names(indexes)[i]), bp_avg_f, bp_avg_m)
  ind <- indexes[[i]]
  avg <- mean(resids[ind])
  ## create a negative correlation
  new_avg <- bp_avg - 0.75*avg*bp_sd
  ## now create within group positive correlation
  bp[ind] <- new_avg + resids[ind] - mean(resids[ind]) + 
    rnorm(length(ind), 0, 1.5)
  #print(c(length(ind), cor(bp[ind], dat$weight[ind])))
}
dat <- mutate(dat, bp = round(bp, 1))



### jagged arrays
indexes <- with(dat,split(1:nrow(dat), list(DNA)))

n_max <- max(sapply(indexes, length))
## first weight
w_tabs <- lapply(seq(along=indexes), function(i){
  ind <- indexes[[i]]
  res <- select(dat, -c("bp","fragment","DNA")) %>% slice(ind)
  frag <- c("141G6", "152F7", "230E8", "285E6")[as.numeric(names(indexes)[i])]
  names(res) <- paste(frag, names(res), sep=".")
  res <- mutate_all(res, as.character)
  tmp <- matrix("", n_max-nrow(res), ncol(res))
  colnames(tmp) <- colnames(res)
  res <- bind_rows(res, as_data_frame(tmp))
  res
})
weight <- do.call(cbind, w_tabs)

## now bp
## we add an extra line.
n <- 15
bp <- dat %>% select(c("line","bp")) %>% 
  separate(line, c("line.1", "line.2", "line.3"), sep="-") %>%
  mutate(line.1 = gsub("#", "", line.1)) %>%
  bind_rows(data_frame(line.1 = rep("44", n), 
                       line.2 = rep("22", n), 
                       line.3 = as.character(1:n), 
                       bp = rnorm(n, mean(dat$bp), sd(dat$bp)))) %>%
  arrange(line.1, line.2)


write.csv(weight, file = "data/weight.csv", quote = FALSE, row.names = FALSE)
write.csv(bp, file = "data/blood-pressure.csv", quote = FALSE, row.names = FALSE)

### add outlien then save RDA save the rda
### adding outlier
dat <- bind_rows(dat,
                data_frame(DNA = 1, line = paste0("#85-12-",1:5), 
                           tg = 0, sex = 0, age = 101,
                           weight = 999, bp = 999, cage = 1))

dat <- select(dat, DNA,line,tg,sex,age,weight,bp,cage)
save(dat, file = "rdas/mouse.rda")


