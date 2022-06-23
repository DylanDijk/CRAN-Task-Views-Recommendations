library(cranlogs)
library(data.table)
library(lubridate)
library(RWsearch)
library(cranly)
library(igraph)

source("code/new_functions/load_snapshots.R")

tvdb_load("code/tvdb-2021-0411.rda")
tvdb_load("code/tvdb-2022-1602.rda")
tvdb_load("code/tvdb-2022-2002.rda")
tvdb_load("code/tvdb-2022-1603.rda")
tvdb_load("code/tvdb-2022-0404.rda")

task_views = tvdb_vec()

date = "2022-02-20"
date = "2022-03-16"
date = "2022-04-04"

All_data = load_snapshots(dates = date)


#### Or if running using todays snapshot. Not the snapshots on Ioannis Kosmidis onedrive ####
today_snap = TRUE
All_data = clean_CRAN_db()

aut_network <- build_network(All_data, perspective = 'author')
pac_network <- build_network(All_data, perspective = 'package')
All_data = list("aut_network" = aut_network, "pac_network" = pac_network)


All_data_igraph = as.igraph(All_data$pac_networks[[date]])
All_data_igraph = as.igraph(All_data$pac_network)

#### Or using CRAN snapshots that I have saved ####
#load(file = "C:/Users/Dylan Dijk/Dropbox/Dylan Dijk - Networks of software and developers/Data/saved_CRAN_snapshots/built_networks_04_04_2022.rda")
All_data_igraph = as.igraph(All_data$pac_network)




date = ymd("2021-12-27")
date = ymd("2022-02-20")
date = ymd("2022-03-16")
date = ymd("2022-04-04")




#####

##### Experimenting for one Task View -  CRAN Downloads for Bayesian  #####
total_dec_21 <- cran_downloads(from = "2021-12-01", to = "2021-12-31", packages = tvdb_pkgs("Bayesian"))
total_dec_21$package = factor(total_dec_21$package)

library(dplyr)
mnth_bayes = total_dec_21 %>%
  group_by(package) %>%
  summarise( sum = sum(count))

hist(mnth_bayes$sum)
summary(mnth_bayes$sum)

mnth_bayes[which.max(mnth_bayes$sum), ]

















######


##### CRAN downloads for All task views #####

mnth_dwnloads = vector(length = length(task_views), mode = "list")
names(mnth_dwnloads) = tvdb_vec()

# creates list with number of monthly downloads for each package within Task View
for(i in tvdb_vec()){
  # i = "Cluster"
  # pkg = tvdb_vec()[i]
  total_dec_21 <- cran_downloads(from = date %m-% months(1), to = date, packages = tvdb_pkgs(i))
  total_dec_21$package = factor(total_dec_21$package)
  

  mnth_dwnloads[[i]] = total_dec_21 %>%
    group_by(package) %>%
    summarise(sum = sum(count))
  
}


lapply(mnth_dwnloads, function(x){median(x$sum)})
lapply(mnth_dwnloads, function(x){median(x$sum)})
unique(as.data.frame(rbindlist(mnth_dwnloads)))




# Decided on using 75th Percentile as threshold
summary(unique(as.data.frame(rbindlist(mnth_dwnloads)))$sum)
quantile(unique(as.data.frame(rbindlist(mnth_dwnloads)))$sum, 0.75)
quantile(log(unique(as.data.frame(rbindlist(mnth_dwnloads)))$sum), 0.75)

# Creating histogram of past month Task View package downloads with line showing decided threshold
hist(log(unique(as.data.frame(rbindlist(mnth_dwnloads)))$sum), breaks = 50, main = "Past month downloads\nof Task View packages", xlab = "log of month downloads")
abline(v = log(2500), lty = 2, col = "red", lwd = 3)





#####

#### Finding packages that have no assigned Task View that mee the decided download threshold ####
# Vector of package names that do not have a Task View
no_tsk_view_packages = V(All_data_igraph)$name[!(V(All_data_igraph)$name %in% Reduce(c,tvdb_pkgs(tvdb_vec())))]
# Remove R package, as it cannot be queried with other packages with cran_downloads
no_tsk_view_packages = no_tsk_view_packages[-which(no_tsk_view_packages == "R")]



# Can not query too many at same time, so need to chunk
library(cranlogs)
# Can query 800 at a time, maybe more
#past_mnth_CRAN_logs <- cran_downloads(from = date %m-% months(1), to = date, packages = no_tsk_view_packages[sample(1:15863, 800)])



# splitting into chunks
chunk_size = 500
n_chunks = ceiling(length(no_tsk_view_packages)/chunk_size)
no_tsk_downloads_ls = vector(mode = "list", length = n_chunks)

# first (n_chunks - 1)
for(i in 1:(n_chunks - 1)){

  print(i)
    j = (1 + (chunk_size*(i - 1))):(chunk_size + (chunk_size*(i - 1)))
  
    no_tsk_downloads <- cran_downloads(from = date %m-% months(1), to = date, packages = no_tsk_view_packages[j])
    
    no_tsk_downloads_ls[[i]] = no_tsk_downloads %>%
      group_by(package) %>%
      summarise(sum = sum(count)) %>%
      filter(sum > 2500)
}

# final chunk
j = (((n_chunks -1) * chunk_size) + 1):length(no_tsk_view_packages)
no_tsk_downloads <- cran_downloads(from = date %m-% months(1), to = date, packages = no_tsk_view_packages[j])

no_tsk_downloads_ls[[n_chunks]] = no_tsk_downloads %>%
  group_by(package) %>%
  summarise(sum = sum(count)) %>%
  filter(sum > 2500)


length(rbindlist(no_tsk_downloads_ls)$package)
length(unique(rbindlist(no_tsk_downloads_ls)$package))

# Number of not assigned packages that meet threshold
nrow(rbindlist(no_tsk_downloads_ls))

# Checking if R meets threshold
cran_downloads(from = date %m-% months(1), to = date, packages = "R") %>%
  summarise(sum = sum(count)) %>%
  filter(sum > 2500)

# It does, so add to list
nrow(rbindlist(no_tsk_downloads_ls)) + 1

no_tsk_pckgs_meet_threshold = c(rbindlist(no_tsk_downloads_ls)$package, "R")
#save(no_tsk_pckgs_meet_threshold, file = paste0("Code/Multinomial_models/Predictors/",date,"/no_taskview_pckgs_that_meet_threshold.RData"))









#### Creating figure of downloads in each Task View ####
library(cranlogs)
library(lubridate)
date = ymd("2022-02-20")
tvdb_load("code/tvdb-2022-2002.rda")
task_views = tvdb_vec()

mnth_dwnloads = vector(length = length(task_views), mode = "list")
names(mnth_dwnloads) = tvdb_vec()

# creates list with number of monthly downloads for each Task View
for(i in tvdb_vec()){
  # i = "Cluster"
  # pkg = tvdb_vec()[i]
  print(i)
  total_dec_21 <- cran_downloads(from = date %m-% months(1), to = date, packages = tvdb_pkgs(i))
  total_dec_21$package = factor(total_dec_21$package)
  
  
  mnth_dwnloads[[i]] = total_dec_21 %>%
    summarise(sum = sum(count))
  
}

total_mnth_downloads_per_task_view = sort(unlist(mnth_dwnloads))
names(total_mnth_downloads_per_task_view) = gsub(x = names(total_mnth_downloads_per_task_view), pattern = ".sum","")
total_mnth_downloads_per_task_view = sort(total_mnth_downloads_per_task_view)
ratio_downloads_per_package = total_mnth_downloads_per_task_view/number_of_pckgs_Task_View[names(total_mnth_downloads_per_task_view),]


barplot(total_mnth_downloads_per_task_view, las = 2)
barplot(sort(ratio_downloads_per_package), las = 2)

dat <- data.frame(`Task Views` = names(sort(ratio_downloads_per_package)),
                  `Number of Downloads in last month` = sort(ratio_downloads_per_package))
                  
dat$Task.Views = factor(names(sort(ratio_downloads_per_package)), levels = names(sort(ratio_downloads_per_package)))


ggplot(data=dat, aes(x=Task.Views, y=Number.of.Downloads.in.last.month)) +
  geom_bar(stat = "identity", fill = "blue")  +
  xlab("Task Views") +
  ylab("Number of Downloads in last month\ndivide by number of packages") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)
  )  +
  scale_y_continuous(limits = c(0,110000), expand = c(0, 0), breaks = round(seq(0, 110000, by = 10000),1))



