library(RWsearch)
tvdb_down()
tvdb_dfr()

load("Code/tvdb-2021-0411.rda")

p_page("ggplot2")


######## Experimenting ##########

library("ctv")
x <- read.ctv(system.file("ctv", "Econometrics.md", package = "ctv"))
system.file("ctv", "Econometrics.md", package = "ctv")

read.ctv(system.file("ctv", "Bayesian.md", package = "ctv"))
system.file("ctv", "Bayesian.md", package = "ctv")

library(xml)
library(devtools)
# version
#install_version("ctv", version = "0.8.5", repos = "http://cran.us.r-project.org")

Bayes = read.ctv(system.file("ctv", "Bayesian.ctv", package = "ctv"))
# ctv2html(Bayes$info)

library("ctv")
gR = read.ctv(system.file("ctv", "gR.ctv", package = "ctv"))



Bayes$info
Bayes_cln = gsub("<.*?>", " ", Bayes$info)
Bayes_cln = gsub("\n", " ", Bayes_cln)
Bayes_cln = gsub("\t", " ", Bayes_cln)
Bayes_cln

library(stringr)

gR$info
gR_cln = gsub("\\s+", " ", str_trim(gR$info))
gR_cln = gsub("\\\\", " ", gR_cln)
gR_cln = gsub("<.*?>", " ", gR_cln)
gR_cln = gsub("\n", " ", gR_cln)
gR_cln = gsub("\t", " ", gR_cln)
gR_cln = gsub("&quot", " ", gR_cln)
gR_cln = gsub("\\s+", " ", str_trim(gR_cln))
gR_cln = gsub("http.*?\\s"," ",gR_cln)
gR_cln = gsub("doi:.*?\\s", " ", gR_cln)
gR_cln = gsub("\\s\\w\\s", " ", gR_cln, ignore.case = T)
gR_cln = gsub("\\s\\w\\.", " ", gR_cln, ignore.case = T)
gR_cln = gsub("\\s\\w\\.", " ", gR_cln, ignore.case = T)
gR_cln = gsub("\\(\\d+\\)", " ", gR_cln, ignore.case = T)
gR_cln = gsub("\\d+\\)", " ", gR_cln, ignore.case = T)
gR_cln = gsub("\\(\\w\\.", " ", gR_cln, ignore.case = T)
gR_cln = gsub("\\s\\w\\.", " ", gR_cln, ignore.case = T)
gR_cln = gsub("-", "_", gR_cln, ignore.case = T)



to_replace <- paste0("\\s", gR$packagelist$name, "\\s")
replace_with <- rep(" ", length(to_replace))
target_text <- gR_cln

names(replace_with) <- to_replace
gR_cln = str_replace_all(target_text, regex(replace_with,ignore_case = TRUE))

to_replace <- paste0("\\(", gR$packagelist$name, "\\)")
replace_with <- rep(" ", length(to_replace))
target_text <- gR_cln

names(replace_with) <- to_replace
gR_cln = str_replace_all(target_text, regex(replace_with,ignore_case = TRUE))

to_replace <- paste0(gR$packagelist$name, ":")
replace_with <- rep(" ", length(to_replace))
target_text <- gR_cln

names(replace_with) <- to_replace
gR_cln = str_replace_all(target_text, regex(replace_with,ignore_case = TRUE))


# library(koRpus)
# 
# 
# treetag(pp$word, treetagger="manual", format="obj",
#         TT.tknz=FALSE , lang="en",
#         TT.options=list(path="./TreeTagger", preset="en"))


library(textstem)
library(dplyr)
library(tidytext)

corpus = data.frame(text = c(Bayes_cln, gR_cln) , task_view = c("Bayesian", "gr"))
corpus = data.frame(text = c(gR_cln) , task_view = c("gr"))
corpus$text = lemmatize_words(corpus$text)

pp = unnest_tokens(corpus, word, text)

pp$word = lemmatize_words(pp$word)

pp = count(pp, task_view, word, sort = TRUE)

pp = unnest_tokens(corpus, word, text) %>%
  count(task_view, word, sort = TRUE)

pp

library(textstem)


vector <- c("run", "ran", "running")
vector <- c("analyze", "analysis", "analyse")
lemmatize_words(vector)
lemmatize_words(pp$word)



gR$packagelist$name







gsub("doi:\\d+|doi:\\d+.", " ", "Link: doi:10.18637/jss.v023.i06")
gsub("doi:.*?\\s", " ", "Link: doi:10.18637/jss.v023.i06 ")

gsub("http.*?\\s"," ","t http://www.burns-stat.com/pages/spoetry.html n http://www.burns-stat.com/pages/spoetry.html p")
str_subset("Link: doi:10.18637/jss.v023.i06", "^doi", negate = TRUE)


gR$info
# gR_cln = gsub("<pkg>.*?</pkg>", " ", gR$info)
gR_cln = gsub("<.*?>", " ", gR$info)
gR_cln = gsub("doi:.*?", " ", gR_cln)
gR_cln = gsub("http:.*", " ", gR_cln)
gR_cln = gsub("\n", " ", gR_cln)
gR_cln = gsub("\t", " ", gR_cln)
gR_cln = gsub("&quot", " ", gR_cln)
gR_cln



# library("XML")
# library("methods")
# 
# # Give the input file name to the function.
# result <- xmlParse(file = system.file("ctv", "gR.ctv", package = "ctv"))


corpus = data.frame(text = c(Bayes_cln, gR_cln) , task_view = c("Bayesian", "gr"))
corpus = data.frame(text = c(gR_cln) , task_view = c("gr"))

unnest_tokens(corpus, word, text) %>%
  count(task_view, word, sort = TRUE)





library(dplyr)
library(janeaustenr)
library(tidytext)

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)













library("XML")
library("methods")

result <- xmlParse(file = system.file("ctv", "gR.ctv", package = "ctv"))

rootnode = xmlRoot(result)
info = xmlRoot(rootnode[["info"]])

getNodeSet(info, "//a")









library(rvest)
pitcherlinks <- list()
pitcherlinks[[1]] <- 
  '<td class="left " data-append-csv="abadfe01" data-stat="player" csk="Abad,Fernando0.01">
  <a href="/players/a/abadfe01.shtml">FernandoÂ Abad</a>*
    </td>'

pitcherlinks[[2]] <- 
  '<td class="left " data-append-csv="adlemti01" data-stat="player" csk="Adleman,Tim0.01">
    <a href="/players/a/adlemti01.shtml">TimÂ Adleman</a>
      </td>'

names <- sapply(pitcherlinks, function(x) {x %>% read_html() %>% html_nodes("a") %>% html_text()})
links <- sapply(pitcherlinks, function(x) {x %>% read_html() %>% html_nodes("a") %>% html_attr("href")})

s = "Hi<friends>and<family>";
System.out.println(s.replaceAll("<.*?>", ""))

















############# Clean gR pipeline  ########################

# install_version("ctv", version = "0.8.5", repos = "http://cran.us.r-project.org")


library("ctv")
library(stringr)

library(textstem)
library(dplyr)
library(tidytext)

gR = read.ctv(system.file("ctv", "gR.ctv", package = "ctv"))


gR$info
gR_cln = gsub("\\s+", " ", str_trim(gR$info))
gR_cln = gsub("\\\\", " ", gR_cln)
gR_cln = gsub("<.*?>", " ", gR_cln)
gR_cln = gsub("\n", " ", gR_cln)
gR_cln = gsub("\t", " ", gR_cln)
gR_cln = gsub("&quot", " ", gR_cln)
gR_cln = gsub("\\s+", " ", str_trim(gR_cln))
gR_cln = gsub("http.*?\\s"," ",gR_cln)
gR_cln = gsub("doi:.*?\\s", " ", gR_cln)
gR_cln = gsub("\\s\\w\\s", " ", gR_cln, ignore.case = T)
gR_cln = gsub("\\s\\w\\.", " ", gR_cln, ignore.case = T)
gR_cln = gsub("\\s\\w\\.", " ", gR_cln, ignore.case = T)
gR_cln = gsub("\\(\\d+\\)", " ", gR_cln, ignore.case = T)
gR_cln = gsub("\\d+\\)", " ", gR_cln, ignore.case = T)
gR_cln = gsub("\\(\\w\\.", " ", gR_cln, ignore.case = T)
gR_cln = gsub("\\s\\w\\.", " ", gR_cln, ignore.case = T)
gR_cln = gsub("-", "_", gR_cln, ignore.case = T)



to_replace <- paste0("\\s", gR$packagelist$name, "\\s")
replace_with <- rep(" ", length(to_replace))
target_text <- gR_cln

names(replace_with) <- to_replace
gR_cln = str_replace_all(target_text, regex(replace_with,ignore_case = TRUE))

to_replace <- paste0("\\(", gR$packagelist$name, "\\)")
replace_with <- rep(" ", length(to_replace))
target_text <- gR_cln

names(replace_with) <- to_replace
gR_cln = str_replace_all(target_text, regex(replace_with,ignore_case = TRUE))

to_replace <- paste0(gR$packagelist$name, ":")
replace_with <- rep(" ", length(to_replace))
target_text <- gR_cln

names(replace_with) <- to_replace
gR_cln = str_replace_all(target_text, regex(replace_with,ignore_case = TRUE))







corpus = data.frame(text = c(Bayes_cln, gR_cln) , task_view = c("Bayesian", "gr"))
corpus = data.frame(text = c(gR_cln) , task_view = c("gr"))
corpus$text = lemmatize_words(corpus$text)

pp = unnest_tokens(corpus, word, text)

pp$word = lemmatize_words(pp$word)

pp = count(pp, task_view, word, sort = TRUE)

pp = unnest_tokens(corpus, word, text) %>%
  count(task_view, word, sort = TRUE)

pp







############# For all Task Views ###############

library(ctv)
library(stringr)
library(tidyr)
library(tidytext)
library(dplyr)


info_list = list()
package_list = list()



for(i in 1:length(list.files("C:/Users/Dylan Dijk/Documents/R/win-library/4.0/ctv/ctv", full.names = TRUE))){
  print(i)
  x = list.files("C:/Users/Dylan Dijk/Documents/R/win-library/4.0/ctv/ctv", full.names = TRUE)[i]
  info_list[str_extract(pattern = "(?<=/ctv/ctv/).*?(?=\\.ctv)", string = x)] = (read.ctv(x)$info)
  package_list[[str_extract(pattern = "(?<=/ctv/ctv/).*?(?=\\.ctv)", string = x)]] = (read.ctv(x)$packagelist$name)

  
}


i = 1

corpus = data.frame(number = 1:1)

for(i in 1:length(list.files("C:/Users/Dylan Dijk/Documents/R/win-library/4.0/ctv/ctv", full.names = TRUE))){
  print(i)

    x = list.files("C:/Users/Dylan Dijk/Documents/R/win-library/4.0/ctv/ctv", full.names = TRUE)[i]
    z = info_list[[str_extract(pattern = "(?<=/ctv/ctv/).*?(?=\\.ctv)", string = x)]]
    
    pkgss = package_list[[str_extract(pattern = "(?<=/ctv/ctv/).*?(?=\\.ctv)", string = x)]]
    
    z = gsub("\\s+", " ", str_trim(z))
    z = gsub("\\\\", " ", z)
    z = gsub("<.*?>", " ", z)
    z = gsub("\n", " ", z)
    z = gsub("\t", " ", z)
    z = gsub("&quot", " ", z)
    z = gsub("\\s+", " ", str_trim(z))
    z = gsub("http.*?\\s"," ",z)
    z = gsub("doi:.*?\\s", " ", z)
    z = gsub("\\s\\w\\s", " ", z, ignore.case = T)
    z = gsub("\\s\\w\\.", " ", z, ignore.case = T)
    z = gsub("\\s\\w\\.", " ", z, ignore.case = T)
    z = gsub("\\(\\d+\\)", " ", z, ignore.case = T)
    z = gsub("\\d+\\)", " ", z, ignore.case = T)
    z = gsub("\\(\\w\\.", " ", z, ignore.case = T)
    z = gsub("\\s\\w\\.", " ", z, ignore.case = T)
    z = gsub("-", "_", z, ignore.case = T)
    
    to_replace <- paste0("\\s", pkgss, "\\s")
    replace_with <- rep(" ", length(to_replace))
    target_text <- z
    
    names(replace_with) <- to_replace
    z = str_replace_all(target_text, regex(replace_with,ignore_case = TRUE))
    
    to_replace <- paste0("\\(", pkgss, "\\)")
    replace_with <- rep(" ", length(to_replace))
    target_text <- z
    
    names(replace_with) <- to_replace
    z = str_replace_all(target_text, regex(replace_with,ignore_case = TRUE))
    
    to_replace <- paste0(pkgss, ":")
    replace_with <- rep(" ", length(to_replace))
    target_text <- z
    
    names(replace_with) <- to_replace
    z = str_replace_all(target_text, regex(replace_with,ignore_case = TRUE))
    
    corpus[str_extract(pattern = "(?<=/ctv/ctv/).*?(?=\\.ctv)", string = x)] = z
    
}

corpus = corpus[-1]
corpus_long <- gather(corpus, task_view, text, Bayesian:WebTechnologies, factor_key=TRUE)
data_long

corpus_long$text = lemmatize_words(corpus_long$text)

pp = unnest_tokens(corpus_long, word, text)

View(pp)

pp$word = lemmatize_words(pp$word)

pp = count(pp, task_view, word, sort = TRUE)












##### Reading text from CTV GitHub page   #####

## First code for seperate view ##

Econometrics = readLines("https://raw.githubusercontent.com/cran-task-views/Econometrics/main/Econometrics.md")
Databases = readLines("https://raw.githubusercontent.com/cran-task-views/Databases/main/Databases.md")
SpatioTemporal = readLines("https://raw.githubusercontent.com/cran-task-views/SpatioTemporal/main/SpatioTemporal.md")
ModelDeployment = readLines("https://raw.githubusercontent.com/cran-task-views/ModelDeployment/main/ModelDeployment.md")

# storing YAML info
Databases_YAML = Databases[(min(which(Databases == "---")) + 1):(max(which(Databases == "---")) - 1)] 
# removing YAML info
Databases_cln =  Databases[(max(which(Databases == "---")) + 1):length(Databases)]

# removing links section
Databases_cln =  Databases_cln[1:(which(Databases_cln == "### Links") - 1)]


# storing subsection titles
Databases_sec = Databases_cln[grepl(x = Databases_cln , pattern = "###")]


# 
# # merging individual elements into one object
# Databases_cln_merge = paste(Databases_cln, sep = " ", collapse = " ")
# 

# removing names of r task views and packages
Databases_cln = gsub(x = Databases_cln, pattern = "`r[^`]*`", " ")
# removing function names
Databases_cln = gsub(x = Databases_cln, pattern = "`[^`]*`", " ")

# remove links
Databases_cln = gsub(x = Databases_cln, pattern = "\\(https:[^()]*\\)", "")
Databases_cln = gsub(x = Databases_cln, pattern = "\\(http:[^()]*\\)", "")

# storing intro section
Databases_intro = Databases_cln[1:(min(which(grepl(Databases_cln, pattern ="###"))) - 1)]

# removing subsection titles
Databases_cln = Databases_cln[!grepl(x = Databases_cln , pattern = "###")]


# need to remove special case link: (https://msdn.microsoft.com/en-us/library/ms710252(v=vs.85).aspx),"
# need to decide what punctuation to delete. for example square brackets

# absorb multiple spaces into single space
Databases_cln = str_squish(Databases_cln)










#######

#### Cleaning for all of the possible available Task Views ####

library(stringr)

TaskViews = c("Databases", "Econometrics", "Hydrology", "ModelDeployment", "SpatioTemporal", "WebTechnologies")

TaskView_sources_text = vector(length = length(TaskViews), mode = "list")
names(TaskView_sources_text) = TaskViews

for(TaskView in TaskViews){
  
  #TaskView = "WebTechnologies"
  #TaskView = "Databases"

  raw = readLines(paste0("https://raw.githubusercontent.com/cran-task-views/",TaskView,"/main/",TaskView,".md"))
  
  
  YAML = raw[(min(which(raw == "---")) + 1):(max(which(raw == "---")) - 1)] 
  #TaskView_references = gsub(x = raw_cln, pattern = "`r [^`]*`", " ")
  raw_cln =  raw[(max(which(raw == "---")) + 1):length(raw)]
  raw_cln =  raw_cln[1:(which(raw_cln == "### Links") - 1)]
  
  sections = raw_cln[grepl(x = raw_cln , pattern = "##")]
  
  
  raw_cln = gsub(x = raw_cln, pattern = "`r[^`]*`", " ")
  raw_cln = gsub(x = raw_cln, pattern = "`[^`]*`", " ")
  
  raw_cln = gsub(x = raw_cln, pattern = "\\(https:[^()]*\\)", "")
  raw_cln = gsub(x = raw_cln, pattern = "\\(http:[^()]*\\)", "")
  raw_cln = gsub(x = raw_cln, pattern = "\\(http:.*", "")
  raw_cln = gsub(x = raw_cln, pattern = "\\(https:.*", "")
  raw_cln = gsub(x = raw_cln, pattern = "\\([^()]*\\)", "")
  
  intro = raw_cln[1:(min(which(grepl(raw_cln, pattern ="##"))) - 1)]
  intro = str_squish(intro)
  intro = paste(intro, collapse = " ")
  
  raw_cln = raw_cln[!grepl(x = raw_cln , pattern = "##")]

  raw_cln = str_squish(raw_cln)
  
  raw_cln = paste(raw_cln, collapse = " ")
  raw_cln = gsub(x = raw_cln, pattern = "\\([^()]*\\)", "")
  
  
  TaskView_sources_text[[TaskView]] = list(YAML = YAML, sections = sections, intro = intro, clean = raw_cln)
  
  
}

# To clean names from Econometrics. Deleting matching brackets has not worked because of line breaks
# Need to apply again after merging lines



# Lemmatization, stemming
# First coding for single pair

library(stringr)
library(tidyr)
library(tidytext)
library(dplyr)
library(textstem)

library(lexicon)
data("hash_lemmas")
#View(hash_lemmas)



text = paste(TaskView_sources_text$Databases$clean, collapse = " ")
Databases_txt = data.frame(txt = text)
# unnest_tokens splits the text into individual words. One word per row
Databases_txt = unnest_tokens(Databases_txt, word, txt)
# stems words using the "hash_lemmas" dictionary
Databases_txt$word = lemmatize_words(Databases_txt$word)
# remove numbers
Databases_txt = data.frame(word = Databases_txt$word[is.na(as.numeric(Databases_txt$word))])
# Aggregate into word frequencies
Databases_txt = count(Databases_txt, word, sort = TRUE, name = "Databases")



text = paste(TaskView_sources_text$Econometrics$clean, collapse = " ")
Econometrics_txt = tibble(txt = text)
Econometrics_txt = unnest_tokens(Econometrics_txt, word, txt)
Econometrics_txt$word = lemmatize_words(Econometrics_txt$word)
Econometrics_txt =data.frame(word = Econometrics_txt$word[is.na(as.numeric(Econometrics_txt$word))])
Econometrics_txt = count(Econometrics_txt, word, sort = TRUE, name = "Econometrics")


text = paste(TaskView_sources_text$Hydrology$clean, collapse = " ")
Hydrology_txt = tibble(txt = text)
Hydrology_txt = unnest_tokens(Hydrology_txt, word, txt)
Hydrology_txt$word = lemmatize_words(Hydrology_txt$word)
Hydrology_txt =data.frame(word = Hydrology_txt$word[is.na(as.numeric(Hydrology_txt$word))])
Hydrology_txt = count(Hydrology_txt, word, sort = TRUE, name = "Hydrology")

text = paste(TaskView_sources_text$WebTechnologies$clean, collapse = " ")
WebTechnologies_txt = tibble(txt = text)
WebTechnologies_txt = unnest_tokens(WebTechnologies_txt, word, txt)
WebTechnologies_txt$word = lemmatize_words(WebTechnologies_txt$word)
WebTechnologies_txt =data.frame(word = WebTechnologies_txt$word[is.na(as.numeric(WebTechnologies_txt$word))])
WebTechnologies_txt = count(WebTechnologies_txt, word, sort = TRUE, name = "WebTechnologies")

# number of unique words in each Task View source
n = c(nrow(Databases_txt), nrow(Econometrics_txt), nrow(Hydrology_txt), nrow(WebTechnologies_txt))


corpus_word_matrix = merge(x = Databases_txt, y = Econometrics_txt, by = "word", all.x = TRUE)
corpus_word_matrix = merge(x = corpus_word_matrix, y = Hydrology_txt, by = "word", all.x = TRUE)
corpus_word_matrix = merge(x = corpus_word_matrix, y = WebTechnologies_txt, by = "word", all.x = TRUE)

# changing NA values into zeroes
corpus_word_matrix[is.na(corpus_word_matrix)] = 0
# Creating column that gives the number of documents that a word appears across the corpus
corpus_word_matrix$df = apply(corpus_word_matrix[,c(2:ncol(corpus_word_matrix))], 1, function(x){sum(x > 0)})

#### Calculating TF-IDF  ####
# Getting Term frequencies, number of times word occurs divided by number of unique words in each View
TF = data.frame(word = corpus_word_matrix$word, t(apply(corpus_word_matrix[,c(2:(4 + 1))], 1, function(x){x/n})))

# calcualting IDF
# 4 is length(TaskViews)
idf = log(base = 2, 4/(corpus_word_matrix$df))


TF_IDF = data.frame(word = corpus_word_matrix$word, TF[2:(4 + 1)]*t(idf))



# pairwise cosine similarity

library(lsa)
cosine_mat = as.matrix(TF_IDF[,-1])
cosine(cosine_mat)
cosine(TF_IDF$Databases, TF_IDF$Hydrology)
cosine(TF_IDF$Databases, TF_IDF$Econometrics)
cosine(TF_IDF$Databases, TF_IDF$WebTechnologies)

View(TF_IDF)























#######

##### Now need to automate for all TaskViews  #####

TaskViews = c("Databases", "Econometrics", "Hydrology",
              "ModelDeployment", "SpatioTemporal", "WebTechnologies",
              "OfficialStatistics", "ClinicalTrials", "Tracking",
              "TimeSeries", "Pharmacokinetics", "TeachingStatistics",
              "Spatial", "Cluster", "MachineLearning",
              "MissingData", "Bayesian", "ChemPhys",
              "Finance", "HighPerformanceComputing", "DifferentialEquations",
              "ExperimentalDesign", "Environmetrics", "MetaAnalysis",
              "ReproducibleResearch", "GraphicalModels", "Distributions",
              "MedicalImaging", "NumericalMathematics", "Optimization",
              "FunctionalData", "Survival", "ExtremeValue",
              "NaturalLanguageProcessing", "Psychometrics", "Robust")




library(stringr)
library(tidyr)
library(tidytext)
library(dplyr)
library(textstem)

library(lexicon)
TaskView_sources_text = vector(length = length(TaskViews), mode = "list")
names(TaskView_sources_text) = TaskViews

for(TaskView in TaskViews){
  print(TaskView)
  #TaskView = "WebTechnologies"
  #TaskView = "Databases"
  #TaskView = "OfficialStatistics" 
  #TaskView = "MachineLearning"
  #TaskView = "HighPerformanceComputing"
  #TaskView = "ReproducibleResearch"
  
  if(TaskView == "HighPerformanceComputing"){
    raw = readLines(paste0("https://raw.githubusercontent.com/cran-task-views/",TaskView,"/master/",TaskView,".md"))
  } else{
    raw = readLines(paste0("https://raw.githubusercontent.com/cran-task-views/",TaskView,"/main/",TaskView,".md"))
  }
  
  YAML = raw[(min(which(raw == "---")) + 1):(max(which(raw == "---")) - 1)] 
  #TaskView_references = gsub(x = raw_cln, pattern = "`r [^`]*`", " ")
  raw_cln =  raw[(max(which(raw == "---")) + 1):length(raw)]
  
  if(any(grepl(pattern = "# Links", x = raw_cln))){
  raw_cln =  raw_cln[1:(which(grepl(pattern = "# Links", x = raw_cln)) - 1)]
  }
  
  sections = raw_cln[grepl(x = raw_cln , pattern = "#")]
  
  if(TaskView == "ReproducibleResearch"){
    positions_of_sections_ReproducibleResearch = which(grepl(x = raw_cln , pattern = "===") | grepl(x = raw_cln , pattern = "---"))
    positions_of_sections_ReproducibleResearch = positions_of_sections_ReproducibleResearch - 1
    
    sections = raw_cln[positions_of_sections_ReproducibleResearch]
  }
  
  raw_cln = gsub(x = raw_cln, pattern = "`r[^`]*`", " ")
  raw_cln = gsub(x = raw_cln, pattern = "`[^`]*`", " ")
  
  raw_cln = gsub(x = raw_cln, pattern = "\\(https:[^()]*\\)", "")
  raw_cln = gsub(x = raw_cln, pattern = "\\(http:[^()]*\\)", "")
  raw_cln = gsub(x = raw_cln, pattern = "\\(http:.*", "")
  raw_cln = gsub(x = raw_cln, pattern = "\\(https:.*", "")
  raw_cln = gsub(x = raw_cln, pattern = "\\([^()]*\\)", "")
  
  # Some Task View text does not have separate topics for example the MachineLearning Task View
  if(any(grepl(pattern = "#", x = raw_cln))){
      intro = raw_cln[1:(min(which(grepl(raw_cln, pattern ="#"))) - 1)]
  } else {
      intro = raw_cln[1:(min(which(grepl(raw_cln, pattern ="\\*"))) - 1)]
  }
  
  intro = str_squish(intro)
  intro = paste(intro, collapse = " ")
  
  raw_cln = raw_cln[!grepl(x = raw_cln , pattern = "##")]
  
  raw_cln = str_squish(raw_cln)
  
  raw_cln = paste(raw_cln, collapse = " ")
  raw_cln = gsub(x = raw_cln, pattern = "\\([^()]*\\)", "")
  
  
  TaskView_sources_text[[TaskView]] = list(YAML = YAML, sections = sections, intro = intro, clean = raw_cln)
  
  
}








n = vector(length = length(TaskViews))
names(n) = TaskViews

first = TaskViews[1]

# 
for(i in TaskViews){
  # i= TaskViews[3]
  text = paste(TaskView_sources_text[[i]]$clean, collapse = " ")
  TaskViews_txt = tibble(txt = text)
  # unnest_tokens converts the character string to separate words
  TaskViews_txt = unnest_tokens(TaskViews_txt, word, txt)
  # absorbs words into lemma word
  TaskViews_txt$word = lemmatize_words(TaskViews_txt$word)
  # remove numbers
  TaskViews_txt = data.frame(word = TaskViews_txt$word[is.na(as.numeric(TaskViews_txt$word))])
  # count number of times each word appears 
  TaskViews_txt = count(TaskViews_txt, word, sort = TRUE, name = i)
  
  # this is the number of unique words in each document
  n[i] = nrow(TaskViews_txt)
  
  if(i == first){
    
    corpus_word_matrix = TaskViews_txt
    
  }else{
    
    corpus_word_matrix = merge(x = corpus_word_matrix, y = TaskViews_txt, by = "word", all = TRUE)
    
  }
  
}


# changing NA values into zeroes
corpus_word_matrix[is.na(corpus_word_matrix)] = 0
# Creating column that gives the number of documents that a word appears across the corpus
corpus_word_matrix$df = apply(corpus_word_matrix[,c(2:ncol(corpus_word_matrix))], 1, function(x){sum(x > 0)})



#### Calculating TF-IDF  ####
# Getting Term frequencies, number of times word occurs divided by number of unique words in each View
TF = data.frame(word = corpus_word_matrix$word, t(apply(corpus_word_matrix[,c(2:(length(TaskViews) + 1))], 1, function(x){x/n})))

# calcualting IDF
# 4 is length(TaskViews)
idf = log(base = 2, length(TaskViews)/(corpus_word_matrix$df))


TF_IDF = data.frame(word = corpus_word_matrix$word, TF[2:(length(TaskViews) + 1)]*t(idf))



# pairwise cosine similarity

library(lsa)
cosine_mat = as.matrix(TF_IDF[,-1])
cosine(cosine_mat)


# Task_View_cosine_sim_mat = cosine(cosine_mat)
# library(lubridate)
# date = today()
# save(Task_View_cosine_sim_mat, file = paste0("Data/distance_matrices/",date,"/Task_View_cosine_sim_mat.RData"))





#### Trying with sections included ####
# Acctually the way I have done it has included the introduction anyway

n = vector(length = length(TaskViews))
names(n) = TaskViews

first = TaskViews[1]

for(i in TaskViews){
  # i= TaskViews[3]
  text = paste(TaskView_sources_text[[i]]$clean, TaskView_sources_text[[i]]$sections, collapse = " ")
  TaskViews_txt = tibble(txt = text)
  TaskViews_txt = unnest_tokens(TaskViews_txt, word, txt)
  TaskViews_txt$word = lemmatize_words(TaskViews_txt$word)
  TaskViews_txt = data.frame(word = TaskViews_txt$word[is.na(as.numeric(TaskViews_txt$word))])
  TaskViews_txt = count(TaskViews_txt, word, sort = TRUE, name = i)
  
  n[i] = nrow(TaskViews_txt)
  
  if(i == first){
    
    corpus_word_matrix = TaskViews_txt
    
  }else{
    
    corpus_word_matrix = merge(x = corpus_word_matrix, y = TaskViews_txt, by = "word", all.y = TRUE)
    
  }
  
}


# changing NA values into zeroes
corpus_word_matrix[is.na(corpus_word_matrix)] = 0
# Creating column that gives the number of documents that a word appears across the corpus
corpus_word_matrix$df = apply(corpus_word_matrix[,c(2:ncol(corpus_word_matrix))], 1, function(x){sum(x > 0)})



### Calculating TF-IDF  
# Getting Term frequencies, number of times word occurs divided by number of unique words in each View
TF = data.frame(word = corpus_word_matrix$word, t(apply(corpus_word_matrix[,c(2:(length(TaskViews) + 1))], 1, function(x){x/n})))

# calcualting IDF
# 4 is length(TaskViews)
idf = log(base = 2, length(TaskViews)/(corpus_word_matrix$df))


TF_IDF = data.frame(word = corpus_word_matrix$word, TF[2:(length(TaskViews) + 1)]*t(idf))



# pairwise cosine similarity

library(lsa)
cosine_mat = as.matrix(TF_IDF[,-1])
cosine(cosine_mat)










#### Individual packages to Task Views ####

# This code takes the titles and description texts of each package, converts to TF vector then takes cosine similarity with TF-IDF Task View vectors
require("tools")

# This function accesses package descriptions and titles. Dirk Eddilbettel code
# It takes the most up to date information from CRAN
# library(lubridate)
# date = today()
getPackagesWithTitle <- function() {
  contrib.url(getOption("repos")["CRAN"], "source") 
  description <- sprintf("%s/web/packages/packages.rds", 
                         getOption("repos")["CRAN"])
  con <- if(substring(description, 1L, 7L) == "file://") {
    file(description, "rb")
  } else {
    url(description, "rb")
  }
  on.exit(close(con))
  db <- readRDS(gzcon(con))
  rownames(db) <- NULL
  db[, c("Package", "Title", "Description")]
}


# Using function, get an object with descriptions and titles of all packages
titles_descriptions_packages_data = getPackagesWithTitle()


length(titles_descriptions_packages_data[,"Package"])
titles_descriptions_packages_data = titles_descriptions_packages_data[!duplicated(titles_descriptions_packages_data[,"Package"]), ]
length(titles_descriptions_packages_data[,"Package"])

# Creating dataframe object, Titles and Description columns
titles_descriptions_packages = data.frame(Package = titles_descriptions_packages_data[,"Package"],
                                          text = paste(Title = titles_descriptions_packages_data[,"Title"],
                                                       Description = titles_descriptions_packages_data[,"Description"]))

#write.csv(titles_descriptions_packages, file = "Data/titles_descriptions_packages")

# converting to list
titles_descriptions_packages_ls = as.list(titles_descriptions_packages$text)
names(titles_descriptions_packages_ls) = titles_descriptions_packages_data[,"Package"]

# Quick text cleaning
titles_descriptions_packages_ls_cln = lapply(titles_descriptions_packages_ls, function(x){gsub(x, pattern = "[\n]", replacement = " ")})
titles_descriptions_packages_ls_cln = lapply(titles_descriptions_packages_ls_cln, function(x){gsub(x, pattern = "<[^>]+>", replacement = " ")})


library(stringr)
library(tidyr)
library(tidytext)
library(dplyr)
library(textstem)
library(lexicon)
library(pbapply)

# To take cosine similarity of a package with Task View I need to convert the package vector to same length of Task View vector.
word_selector = TF_IDF[,"word", drop = FALSE]

# cleaning and converting package text to term frequencies
fun1 = function(x){
text_ls_cln = tibble(txt = x)
text_ls_cln = unnest_tokens(text_ls_cln, word, txt)
text_ls_cln$word = lemmatize_words(text_ls_cln$word)
text_ls_cln = data.frame(word = text_ls_cln$word[is.na(as.numeric(text_ls_cln$word))])
text_ls_cln = count(text_ls_cln, word, sort = TRUE, name = "test")
return(text_ls_cln)
}

titles_descriptions_packages_freq = pblapply(titles_descriptions_packages_ls_cln, fun1)


# Merging package vectors with Task View vectors and then taking cosine similarity
fun2 = function(x){
  
  pkg_tsk_text_comb = merge(x = x, y = TF_IDF, by = "word", all.y = TRUE)
  pkg_tsk_text_comb[is.na(pkg_tsk_text_comb)] = 0
  
  # Have included here weighting the package vectors by IDF
  pkg_tsk_text_comb$test = pkg_tsk_text_comb$test*(idf)
  
  
  cosine = cosine(as.matrix(pkg_tsk_text_comb[,-1]))
  cosine = cosine[1,-1]
  
  return(cosine)
  
}

titles_descriptions_packages_cosine = pblapply(titles_descriptions_packages_freq, fun2)

head(titles_descriptions_packages_cosine)
which.max(unlist(lapply(titles_descriptions_packages_cosine, function(x){x["OfficialStatistics"]})))
unlist(lapply(titles_descriptions_packages_cosine, function(x){x["OfficialStatistics"]}))[which.max(unlist(lapply(titles_descriptions_packages_cosine, function(x){x["OfficialStatistics"]})))]

feature_matrix_titles_descriptions_packages_cosine = titles_descriptions_packages_cosine

# The date will be whatever date that the getPackagesWithTitle() function was run
# library(lubridate)
# date = today()

#save(feature_matrix_titles_descriptions_packages_cosine, file = paste0("Code/Multinomial_models/Predictors/",date,"/feature_matrix_titles_descriptions_packages_cosine.RData"))





