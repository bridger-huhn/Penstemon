library(RJSONIO)
library(tidyverse)
setwd("/Users/bridgerhuhn/Documents/School/Database Management/Repositories/Penstemon")
j<- fromJSON("penstemon-light-curve (1).json")

#### first protocol#####
samp <- 1
d<- t(data.frame(rep(NA,length(j[[1]]$sample[[samp]]))))
colnames(d)<-names(j[[1]]$sample[[samp]])
d <- data.frame(d[-1,])
d$order <- NULL
ord <- which(names(j[[1]]$sample[[1]])=="order")

ivals <- setdiff(c(1:length(j[[1]]$sample[[1]])),ord) 


for (k in 1:(length(j)-1)) {
  for (i in ivals) {
    item <- j[[k]]$sample[[samp]][[i]]
    
    d[k,ifelse(i<ord,i,i-1)] <- item
  }
  d$name[k] <- j[[k]]$user_answers[[4]]
}

#### pulls spacial data out #####
for (i in 1:(length(j)-1)) {
  for (k in 1:2) {
    ifelse(k == 1, lat<-j[[i]]$location[[k]],
           lon<- j[[i]]$location[[k]]) 
    tempDf <- data.frame(lat = lat , lon = lon)
  }
  spacial <- rbind(spacial, tempDf)
}
d <- cbind(d, spacial)

for (i in 1:nrow(d)) {
  ifelse(d$lon[i] <= -107.1025, d$group[i] <- "blowout", d$group[i] <- "topOfDune") 
    
}

mapview(d,
        ycol = "lat",
        xcol = "lon",
        crs = 4269,
        cex = 4,
        alpha = .75,
        zcol = "group")





d2 <- d %>% 
  select(contains("Phi2"), group) %>% 
  mutate(name = d$name) %>% 
  filter(str_detect(name,'p')) 

for (i in 1:nrow(d2)) {
  ifelse(str_detect(d$name[i], pattern = "pen"),
         d2$sp[i] <- "pen", d2$sp[i] <- "pso")
  ifelse(str_detect(d$name[i], pattern = "veg"),
         d2$veg[i] <- "veg", d2$veg[i] <- "non")
  
}

d2 <- d2 %>% 
  pivot_longer(!c(name,sp, group, veg),names_to = 'Phi2', values_to = "phi") %>%  
  separate(col = Phi2, into = c("trash", "Phi2"), sep = "_", remove = FALSE) %>% 
  mutate(Phi2 = as.numeric(Phi2)) %>% 
  select(!trash)



ggplot(data = filter(d2, group != "blowout"), aes(x = Phi2, y = phi, color = sp, shape = name))+
  geom_point()

#### for the second protocol ####
samp <- 2

d<- t(data.frame(rep(NA,length(j[[1]]$sample[[samp]]))))
colnames(d)<-names(j[[1]]$sample[[samp]])
d <- data.frame(d[-1,])
d$order <- NULL
d$recall <- NULL
rec <- which(names(j[[1]]$sample[[samp]])=="recall")
ord<- which(names(j[[1]]$sample[[samp]])=="order")
json <- j[[1]]$sample[[samp]]
ivals <- setdiff(c(1:length(j[[1]]$sample[[samp]])),c(ord, rec))
i <- 40
for (k in 1:(length(j)-1)) {
  for (i in ivals) {
    item <- j[[k]]$sample[[samp]][[i]]
    d[k,ifelse(i<=rec,i,
               ifelse(i<=ord,i-1,i-2))]<- item
  }
  d$name[k] <- j[[k]]$user_answers[[4]]
}


d3 <- d %>% 
  select(contains("Phi2")) %>% 
  mutate(name = d$name) %>% 
  filter(str_detect(name,'p')) 

for (i in 1:nrow(d3)) {
  ifelse(str_detect(d$name[i], pattern = "pen"),
         d3$sp[i] <- "pen", d3$sp[i] <- "pso")
  ifelse(str_detect(d$name[i], pattern = "veg"),
         d3$veg[i] <- "veg", d3$veg[i] <- "non")
  
}





d3 <- d3 %>% 
  pivot_longer(!c(name,sp, veg),names_to = 'Phi2', values_to = "phi") %>%
  separate(col = Phi2, into = c("trash", "Phi2"), sep = "_", remove = FALSE) %>% 
  mutate(Phi2 = as.numeric(Phi2)) %>% 
  select(!trash)
dtot <-rbind(d2,d3)


ggplot(data = filter(d2, veg == "veg"), aes(x = Phi2, y = phi, color = sp, size = veg))+
  geom_point()+
  ylab("PHIPS2")+
  xlab("PAR")

####geospacial data ####
spacial <- data.frame(lat = NA, long = NA)
spacial <- spacial[-1,]
i <- 1
k <- 1
for (i in 1:(length(j)-1)) {
  for (k in 1:2) {
    ifelse(k == 1, lat<-j[[i]]$location[[k]],
           lon<- j[[i]]$location[[k]]) 
    tempDf <- data.frame(lat = lat , lon = lon)
  }
  spacial <- rbind(spacial, tempDf)
}

library(mapview)
mapview(d,
        ycol = "lat",
        xcol = "lon",
        crs = 4269,
        cex = 4,
        alpha = .75,
        zcol = "group")

