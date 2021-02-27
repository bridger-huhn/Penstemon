library(RJSONIO)
library(tidyverse)
j<- fromJSON("penstemon-light-curve (1).json")

d<- t(data.frame(rep(NA,length(j[[1]]$sample[[1]]))))
colnames(d)<-names(j[[1]]$sample[[1]])
d <- data.frame(d[-1,])
d$order <- NULL

ivals <- setdiff(c(1:length(j[[1]]$sample[[1]])),55) 

for (k in 1:(length(j)-1)) {
  for (i in ivals) {
    item <- j[[k]]$sample[[1]][[i]]
    
    d[k,ifelse(i<55,i,i-1)] <- item
  }
  d$name[k] <- j[[k]]$user_answers[[4]]
}
d2 <- d %>% 
  select(contains("Phi2")) %>% 
  mutate(name = d$name) %>% 
  filter(str_detect(name,'p')) 

for (i in 1:nrow(d2)) {
  ifelse(str_detect(d$name[i], pattern = "pen"),
         d2$sp[i] <- "pen", d2$sp[i] <- "pso")
  ifelse(str_detect(d$name[i], pattern = "veg"),
         d2$veg[i] <- "veg", d2$veg[i] <- "non")
  
}

d2 <- d2 %>% 
  pivot_longer(!c(name,sp, veg),names_to = 'Phi2', values_to = "phi") %>%
  separate(col = Phi2, into = c("trash", "Phi2"), sep = "_", remove = FALSE) %>% 
  mutate(Phi2 = as.numeric(Phi2)) %>% 
  select(!trash)



ggplot(data = filter(d2, veg != "veg"), aes(x = Phi2, y = phi, color = sp, size = veg))+
  geom_point()
