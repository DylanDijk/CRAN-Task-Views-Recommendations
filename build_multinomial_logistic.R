source("code/new_functions/load_snapshots.R")

# Data

# Response matrix
library(igraph)
library(bc3net)
library(cranly)
library(RWsearch)
library(lubridate)

tvdb_load("Code/tvdb-2022-2002.rda") 
tvdb_load("Code/tvdb-2022-1603.rda") 
tvdb_load("code/tvdb-2022-0404.rda")


date = "2021-12-27"
date = "2022-02-20"
date = "2022-03-16"
date = "2022-04-04"


load(paste0("Data/taskviews_sub_graphs/",date,"/Entire_network_",date,".RData"))


# loading Task views of packages from generating_taskviews script
load(paste0("Data/taskviews_of_pckgs/", date ,"/taskviews_of_pckgs_", date ,".RData"))



## loading CRAN snapshots that I have saved
#     load(file = "C:/Users/Dylan Dijk/Dropbox/Dylan Dijk - Networks of software and developers/Data/saved_CRAN_snapshots/built_networks_04_04_2022.rda")


##### Creating the response matrix ####

# all_pks is all of the current packages available in CRAN
all_pks = All_data$pac_networks[[date]]$nodes$package
# if using today snapshot, need to remove date layer from list. And s from pac_networks
all_pks = All_data$pac_network$nodes$package

response_matrix = matrix(0, nrow = length(all_pks), ncol = length(tvdb_vec()) + 1)
colnames(response_matrix) = c(tvdb_vec(), "none")

# Creating matrix that denotes which Task View(s) each package belongs to
for(i in 1:length(all_pks)){
  #i = 6214
  #i = 7
  
  if(is.null(taskviews_of_pckgs[[all_pks[i]]])){
    
    response_matrix[i,"none"] = 1
    
  } else {
    
    
    response_matrix[i,taskviews_of_pckgs[[all_pks[i]]]] = 1
    
  }
}

rownames(response_matrix) = all_pks

response_matrix["rjags",]



# Creating equivalent dataframe form 
taskviews_of_pckgs_none = taskviews_of_pckgs[all_pks]
names(taskviews_of_pckgs_none) = all_pks
taskviews_of_pckgs_none = lapply(taskviews_of_pckgs_none, function(x){ x = ifelse(is.null(x),"none", x)})

response_df = data.frame(Packages = names(taskviews_of_pckgs_none), TaskViews = unlist(taskviews_of_pckgs_none))
head(response_df)
response_df["rjags",]
#save(response_df, file = paste0("Code/Multinomial_models/Models/",date,"/Data/response_df.RData"))


################# Creating features/predictors ####





##### Creating  Proportion of neighboring packages feature matrix ####
# creating graph object removing soft dependencies
All_data_igraph = as.igraph(All_data$pac_networks[[date]])
# if using today snapshot, need to remove date layer from list. And s from pac_networks
All_data_igraph = as.igraph(All_data$pac_network)

# packages in Task Views that are not in CRAN
not_in_CRAN = Reduce(c,tvdb_pkgs(tvdb_vec()))[!(Reduce(c,tvdb_pkgs(tvdb_vec())) %in% V(All_data_igraph)$name)]
packages_assigned_Task_View = Reduce(c,tvdb_pkgs(tvdb_vec()))
packages_assigned_Task_View = packages_assigned_Task_View[!(packages_assigned_Task_View %in% not_in_CRAN)]
packages_assigned_Task_View = unique(packages_assigned_Task_View)

All_taskviews_igraph = induced.subgraph(All_data_igraph, packages_assigned_Task_View)
All_data_igraph = set_vertex_attr(All_data_igraph, name = "taskview", index = V(All_data_igraph)[V(All_taskviews_igraph)$name], taskviews_of_pckgs[V(All_data_igraph)[V(All_taskviews_igraph)$name]$name])

# check:
# V(All_data_igraph)$taskview[V(All_data_igraph)$name == "ggplot2"]

dep_imp_edges = which(!is.element(E(All_data_igraph)$type, c("depends","imports", "linking_to"))) 
All_taskviews_rem_edges_igraph = delete.edges(All_data_igraph, dep_imp_edges)



# Creating matrix where for each package it gives the proportion of immediate dependencies
feature_matrix_all_neighbour_pkgs = matrix(0, nrow = length(all_pks), ncol = length(tvdb_vec()) + 1)
colnames(feature_matrix_all_neighbour_pkgs) = c(tvdb_vec(), "none")



for(i in 1:length(all_pks)){
  #i = 1106
  #i = 1029
  
  print(i)
  neigh = neighbors(All_taskviews_rem_edges_igraph, all_pks[i], mode = c("all"))$taskview
  n_none = sum(unlist(lapply(neigh, function(x){is.null(x)})))
  props = (prop.table(table(c(unlist(neigh), rep("none", n_none)))))
  
  
  feature_matrix_all_neighbour_pkgs[i,names(props)] = as.vector(props)
  
}

# named all as we are looking at in and out edges. could just look at 
rownames(feature_matrix_all_neighbour_pkgs) = all_pks
#save(feature_matrix_all_neighbour_pkgs, file = paste0("Code/Multinomial_models/Predictors/",date,"/feature_matrix_all_neighbour_pkgs.RData"))





#### load from pre-saved ####
load(file = paste0("Code/Multinomial_models/Predictors/",date,"/feature_matrix_all_neighbour_pkgs.RData"))
colnames(feature_matrix_all_neighbour_pkgs) = paste0(colnames(feature_matrix_all_neighbour_pkgs), ".Prop_neighbour")

feature_matrix_all_neighbour_pkgs["rjags",]
feature_matrix_all_neighbour_pkgs["DEEPR",]








##### Load Features using text data of packages ####
# This has been created in the NLP R script. Reliant on the number of package available on GitHub page.
# library(lubridate)
# date = today()

load(file = paste0("Code/Multinomial_models/Predictors/",date,"/feature_matrix_titles_descriptions_packages_cosine.RData"))

length(unique(names(feature_matrix_titles_descriptions_packages_cosine)))
feature_matrix_titles_descriptions_packages_cosine = feature_matrix_titles_descriptions_packages_cosine[!duplicated(names(feature_matrix_titles_descriptions_packages_cosine))]


feature_matrix_titles_descriptions_packages_cosine_df = as.data.frame(do.call(rbind, feature_matrix_titles_descriptions_packages_cosine))
colnames(feature_matrix_titles_descriptions_packages_cosine_df) = paste0(colnames(feature_matrix_titles_descriptions_packages_cosine_df), ".text")







##### Proportion of other packages that Author worked on  ####

# calculated by taking the authors of the package, getting the packages that they worked on.
# Then looking at the proportion of the assignmnet to Task Views of these packages

authors_of_packages = All_data$pac_networks[[date]]$nodes$author
names(authors_of_packages) = All_data$pac_networks[[date]]$nodes$package
# if using today snapshot, need to remove date layer from list. And s from pac_networks
authors_of_packages = All_data$pac_network$nodes$author
names(authors_of_packages) = All_data$pac_network$nodes$package


packages_of_authors = All_data$aut_networks[[date]]$nodes$package
names(packages_of_authors) = All_data$aut_networks[[date]]$nodes$author 
# if using today snapshot, need to remove date layer from list. And s from pac_networks
packages_of_authors = All_data$aut_network$nodes$package
names(packages_of_authors) = All_data$aut_network$nodes$author 

fun1 = function(x){
  
  # when given a vector of Authors, returns a vector of the proportion of the Task Views of the packages qorked on by the authors 
  z = unlist(taskviews_of_pckgs[unique(unlist(packages_of_authors[x]))])
  
  if(is.null(z)){
    author_tsk_view_prop = c(rep(0,length(tvdb_vec())),1)
    names(author_tsk_view_prop) = c(tvdb_vec(), "none")
    
    
  }else{
    author_tsk_view_prop = prop.table(table(z))
    mat_fill = matrix(0, nrow = 1, ncol = length(tvdb_vec()) + 1)
    colnames(mat_fill) = c(tvdb_vec(), "none")
    mat_fill[1,names(author_tsk_view_prop)] = as.vector(author_tsk_view_prop)
    author_tsk_view_prop = mat_fill
  }
  
  return(author_tsk_view_prop)
  
  }

library(pbapply)
feature_matrix_author_task_views = pblapply(authors_of_packages, fun1)

feature_matrix_author_task_views = do.call(rbind, feature_matrix_author_task_views)
row.names(feature_matrix_author_task_views) = names(authors_of_packages)

colnames(feature_matrix_author_task_views) = paste0(colnames(feature_matrix_author_task_views), ".Author_props")
#save(feature_matrix_author_task_views, file = paste0("Code/Multinomial_models/Predictors/",date,"/feature_matrix_author_task_views.RData"))











#### load from pre-saved ####
load(file = paste0("Code/Multinomial_models/Predictors/",date,"/feature_matrix_author_task_views.RData"))



#### Creating training and testing data sets ####
# I am going to split the labeled data with 80:20 ratio
# The labeled data consists of:
#             > Packages with assigned Task Views
#             > Packages with no Task View that meet assigned download Threshold

# Loading vector of packages that do not meet threshold
load(file = paste0("Code/Multinomial_models/Predictors/",date,"/no_taskview_pckgs_that_meet_threshold.RData"))

# merging feature matrices
feature_matrix_all_neighbour_pkgs_df = as.data.frame(feature_matrix_all_neighbour_pkgs)
feature_matrix_author_task_views_df = as.data.frame(feature_matrix_author_task_views)


features = merge(feature_matrix_titles_descriptions_packages_cosine_df, feature_matrix_all_neighbour_pkgs_df, by="row.names", all.x = TRUE)
rownames(features) = features[,"Row.names"]
features = features[,colnames(features) != "Row.names"]

features = merge(features, feature_matrix_author_task_views_df, by="row.names", all.x = TRUE)
# features = as.numeric(as.matrix(features))
nrow(features)
rownames(features) = features[,"Row.names"]
features = features[,colnames(features) != "Row.names"]





head(no_tsk_pckgs_meet_threshold)
response_matrix[response_matrix[,"none"] == 0,]
which(response_matrix[no_tsk_pckgs_meet_threshold,][,"none"] == 0)





intersect(no_tsk_pckgs_meet_threshold, which(response_matrix[,"none"] == 0))

(response_matrix[no_tsk_pckgs_meet_threshold,])    # Packages that have no assigned Task View that meet threshold
(response_matrix[response_matrix[,"none"] == 0,])  # Packages that have assigned Task View
# combining the two sets
labelled_data_res = (rbind(response_matrix[response_matrix[,"none"] == 0,],    response_matrix[no_tsk_pckgs_meet_threshold,])) 
labelled_data_features = features[rownames(labelled_data_res),]

labelled_data_res_df = rbind(response_df[!(response_df[,"TaskViews"] == "none"),], response_df[no_tsk_pckgs_meet_threshold,])


set.seed(3)
split1<- sample(c(rep(0, 0.8 * nrow(labelled_data_res)), rep(1, 0.2 * nrow(labelled_data_res))))
table(split1)
train_res = labelled_data_res[split1 == 0,]
train_features = labelled_data_features[split1 == 0,]
test_res = labelled_data_res[split1 == 1,]
test_res_df = labelled_data_res_df[split1 == 1,]
test_feature = labelled_data_features[split1 == 1,]


#save(test_res,test_feature,test_res_df, file = paste0("Code/Multinomial_models/Models/",date,"/Data/test_data.RData"))
# train_features = as.matrix(train_features)
# test_feature = as.matrix(test_feature)

#### fitting model ####
library(nnet)
# This model is using the proportion of packages assigned to task views as a predictor. Have used all data.
#model1 = multinom(train_res ~ train_features, MaxNWts=10000)
#model1 = multinom(train_res ~ Databases.text, data = train_features, MaxNWts=10000)

# This model is using the packages that were available in CRAN on 2021-12-27.
# And used the `tvdb-2022-2002.rda` Task View snapshot
# This model used the text features created in 2022-2002. This only included the text data features of 7 Task Views.
# Databases Econometrics Hydrology ModelDeployment SpatioTemporal WebTechnologies OfficialStatistics
#model_2022_03_16 = multinom(train_res ~ ., data = train_features, MaxNWts=10000)
#save(model_2022_03_16, file = paste0("Code/Multinomial_models/Models/",date,"/model_2022_03_16.RData"))

model_2022_04_04 = multinom(train_res ~ ., data = train_features, MaxNWts=10000)
#save(model_2022_04_04, file = paste0("Code/Multinomial_models/Models/",date,"/model_2022_04_04.RData"))
















# features from network dependencies, without the "none" features
network_prop_features = rbind(train_features, test_feature)[,!grepl(pattern = "none.",colnames(train_features)) & !grepl(pattern = "text",colnames(train_features))]
network_prop_features_sum = apply(network_prop_features, 1, sum)

hist(network_prop_features_sum, breaks = 50)

pkgs_for_suggestions = all_pks[!(all_pks %in% no_tsk_pckgs_meet_threshold) & response_matrix[,"none"] == 1]
pkgs_for_suggestions_features = features[pkgs_for_suggestions,]

network_prop_features_suggest = pkgs_for_suggestions_features[,!grepl(pattern = "none.",colnames(pkgs_for_suggestions_features)) & !grepl(pattern = "text",colnames(pkgs_for_suggestions_features))]
network_prop_features_suggest_sum = apply(network_prop_features_suggest, 1, sum)

hist(network_prop_features_suggest_sum, breaks = 50)



##### LASSO #####
library(glmnet)

# removing row that has missing features
train_res = train_res[!apply(as.matrix(train_features),1, function(x){any(is.na(x))}),]
train_features = train_features[!apply(as.matrix(train_features),1, function(x){any(is.na(x))}),]
train_res = as.matrix(train_res)
train_features = as.matrix(train_features)


library(Matrix)

train_sparse <- sparse.model.matrix(~., as.data.frame(train_features))
train_res_sparse <- sparse.model.matrix(~0 + ., as.data.frame(train_res))


#glmnet(x = train_features, y = train_res, family = "multinomial", alpha = 1)
glmnet(x = train_sparse, y = train_res, family = "multinomial", alpha = 1)

set.seed(3)
#cv_glmnet_model_2022_03_16 = cv.glmnet(x = train_sparse, y = train_res, family = "multinomial", alpha = 1)
#cv_glmnet_model_2021_12_27 = cv.glmnet(x = train_features,  y = train_res, family = "multinomial", alpha = 1)
cv_glmnet_model_2022_04_04 = cv.glmnet(x = train_sparse,  y = train_res, family = "multinomial", alpha = 1)

# save(cv_glmnet_model_2022_04_04, file = paste0("Code/Multinomial_models/Models/",date,"/cv_glmnet_model_2022_04_04.RData"))

cv_glmnet_model_2022_04_04_miss_class = cv.glmnet(x = train_sparse,  y = train_res, family = "multinomial", alpha = 1, type.measure = "class")

model = cv.glmnet(x = train_sparse,  y = train_res, family = "multinomial", alpha = 1, maxit = 1000, type.measure = "class")

model = cv_glmnet_model_2022_04_04
model = cv_glmnet_model_2022_04_04_miss_class

plot(model)

cv_glmnet_model_2022_03_16$lambda.min
cv_glmnet_model_2022_03_16$lambda.1se

cv_glmnet_model_2022_04_04$lambda.1se
cv_glmnet_model_2022_04_04$lambda.min


coef(cv_glmnet_model_2022_03_16, s = "lambda.1se")
predict_class = predict(cv_glmnet_model_2022_03_16, newx = cbind(rep(1, nrow(test_feature)),as.matrix(test_feature)), s = "lambda.min",  type = "class")


coef(cv_glmnet_model_2022_04_04, s = "lambda.1se")
predict_class = predict(model, newx = cbind(rep(1, nrow(test_feature)),as.matrix(test_feature)), s = "lambda.min",  type = "class")



# Getting accuracy of model after applying lasso with min Lambda
predict_class = factor(predict_class[,1], levels = c(tvdb_vec(), "none"))


# 77.5% accuracy
mean(test_res[cbind(1:nrow(test_res), predict_class)], na.rm = T)

apply(test_res[which(test_res[cbind(1:nrow(test_res), predict_class)] == 0),],2,sum)
View(train_features)



train_features[!apply(as.matrix(train_features),1, function(x){any(is.na(x))}),]

train_features["showtextdb",]






##### top 3 recommendations from model


predict_class = predict(model, newx = cbind(rep(1, nrow(test_feature)),as.matrix(test_feature)), s = "lambda.min",  type = "class")
predict_prob = predict(model, newx = cbind(rep(1, nrow(test_feature)),as.matrix(test_feature)), s = "lambda.min", type = "response")



load(file = paste0("Code/Multinomial_models/Predictors/",date,"/no_taskview_pckgs_that_meet_threshold.RData"))
pkgs_for_suggestions = all_pks[!(all_pks %in% no_tsk_pckgs_meet_threshold) & response_matrix[,"none"] == 1]
pkgs_for_suggestions_features = features[pkgs_for_suggestions,]

pkgs_for_suggestions_features = pkgs_for_suggestions_features[!apply(as.matrix(pkgs_for_suggestions_features),1, function(x){any(is.na(x))}),]



predicted_probs_for_suggestions = predict(model, newx = cbind(rep(1, nrow(pkgs_for_suggestions_features)),as.matrix(pkgs_for_suggestions_features)), s = "lambda.min", type = "response")
predicted_probs_for_suggestions = as.data.frame(predicted_probs_for_suggestions)

suggestions_for_Task_View = function(TaskView = "Hydrology", n = 5){
  
  suggestions = row.names(predicted_probs_for_suggestions[,paste0(TaskView,".1"), drop = F][order(predicted_probs_for_suggestions[,paste0(TaskView,".1"), drop = F], decreasing = T),, drop = F])[1:n]
  return(suggestions)
}


suggestions_for_Task_View(TaskView = "Environmetrics")
suggestions_for_Task_View(TaskView = "NaturalLanguageProcessing")
predicted_probs_for_suggestions["multxpert",]


##### Create LaTeX table
suggestions_for_tsk_views = list()

for(i in tvdb_vec()){
  
  # i = "Environmetrics"
  
  suggestions_for_tsk_views[[paste(i)]] =  suggestions_for_Task_View(TaskView = i, n = 5)
  
}

suggestions_for_tsk_views = t(as.data.frame(suggestions_for_tsk_views))

CRAN_summary_table_2022_04_04 = data.frame(total_number_of_packages_in_CRAN_0404, total_number_of_authors_in_CRAN_0404,
                                           number_of_task_views_0404, n_packages_assigned_task_view_0404, median_number_packages_per_author_0404, median_number_authors_per_package_0404)

colnames(CRAN_summary_table_2022_04_04) = c("Packages", "Authors", "Task Views", "Packages assigned to Task View", "Median packages per author", "Median author per package")

library(memisc)
booktabs_suggestions_for_tsk_views_2022_04_04 = toLatex(suggestions_for_tsk_views, useDcolumn = FALSE, digits = 0, booktabs = TRUE)
write(booktabs_suggestions_for_tsk_views_2022_04_04, file = "C:/Users/Dylan Dijk/Dropbox/Apps/Overleaf/Dissertation 21-22 structure v2/Tables/Task_View_suggestions_2022_04_04.tex")



