library(cranlogs)
library(data.table)
library(lubridate)
library(RWsearch)
library(cranly)
library(igraph)
library(dplyr)

task_views = tvdb_vec()

date = today()


#All_data_igraph = as.igraph(All_data$pac_network)


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


# lapply(mnth_dwnloads, function(x){median(x$sum)})
# lapply(mnth_dwnloads, function(x){median(x$sum)})
# unique(as.data.frame(rbindlist(mnth_dwnloads)))




# Decided on using 75th Percentile as threshold
# summary(unique(as.data.frame(rbindlist(mnth_dwnloads)))$sum)
# quantile(unique(as.data.frame(rbindlist(mnth_dwnloads)))$sum, 0.75)
# quantile(log(unique(as.data.frame(rbindlist(mnth_dwnloads)))$sum), 0.75)

# Creating histogram of past month Task View package downloads with line showing decided threshold
# hist(log(unique(as.data.frame(rbindlist(mnth_dwnloads)))$sum), breaks = 50, main = "Past month downloads\nof Task View packages", xlab = "log of month downloads")
# abline(v = log(2500), lty = 2, col = "red", lwd = 3)





#####

#### Finding packages that have no assigned Task View that mee the decided download threshold ####
# Vector of package names that do not have a Task View
no_tsk_view_packages = V(pac_network_igraph)$name[!(V(pac_network_igraph)$name %in% Reduce(c,tvdb_pkgs(tvdb_vec())))]
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

board %>% pin_write(no_tsk_downloads_ls, "no_tsk_downloads_ls", type = "rds")



# Number of not assigned packages that meet threshold
length(unique(rbindlist(no_tsk_downloads_ls)$package))

# Checking if R meets threshold
cran_downloads(from = date %m-% months(1), to = date, packages = "R") %>%
  summarise(sum = sum(count)) %>%
  filter(sum > 2500)

# It does, so add to list
nrow(rbindlist(no_tsk_downloads_ls)) + 1

no_tsk_pckgs_meet_threshold = c(rbindlist(no_tsk_downloads_ls)$package, "R")
#save(no_tsk_pckgs_meet_threshold, file = paste0("Code/Multinomial_models/Predictors/",date,"/no_taskview_pckgs_that_meet_threshold.RData"))

board %>% pin_write(no_tsk_pckgs_meet_threshold, "no_tsk_pckgs_meet_threshold", type = "rds")




