# Automatically Topic modeling of Schwab data
# Cheng-Jun Wang, 20130829@Weblab, Hong Kong


# ------------------------------------------------------------------------#
# I convert the raw html files to csv using BeautifulSoup in Python       #
# The csv are separated by '|' premarily.                                 #
# In ashoka_data_cleaningSep it's also separated with semi-colons.        #
#-------------------------------------------------------------------------#


###############################################################################
## ------------------1. load data---------------------------------------------#
###############################################################################
setwd("./social_entrepreneur/data/")

dat2 = read.csv("./ashoka_data_cleaningSep.csv",
                header = F, sep = "|", quote = "", stringsAsFactors=F)
dim(dat2)
names(dat2) =  c('name', 'category', 'subsectors', 'targets', 'organization', 'location1',
                 'location2', 'profileIntro', 'year_fellowship', 'introduction', 'idea',
                 'problem', 'strategy', 'person', 'rnames', 'rorgs')    


###################
#  spatial patterns
###################
library(rworldmap)
library(ggmap)
data(countryExData)
country = unlist(strsplit(dat2$location1, split = ", "))
# dat2$name[which(nchar(country) > 50)]
country[which(nchar(country) > 50)]  =c("United States", "Thailand", "United Kingdom", "Brazil", "Italy")
uname = unique(country)[which(!unique(country) %in% countryExData$Country)]
# [1] "Singapore"             "Palestinian Territory" "Czech Republic"        "Ivory Coast"          
# [5] "Gambia"                "Afghanistan"           "Timor-Leste"           "Hong Kong S.A.R."  
country[country =="Palestinian Territory"] = "palestine"
# table(country[which(country %in% uname)])
country_iso3 = unlist(lapply(country, rwmGetISO3))

mmap = data.frame(table(country_iso3))

mmaps = joinCountryData2Map(mmap, joinCode = "ISO3",
                              nameJoinColumn = "country_iso3")

png("./map_social_entrepreneur.png",
    width=10, height=7, 
    units="in", res=700)
mapCountryData(mmaps, nameColumnToPlot="Freq", #mapRegion="Eurasia", 
               colourPalette="terrain",
               #oceanCol="lightblue",
               mapTitle = "",
               lwd = 0.5)
dev.off()
# http://nominatim.openstreetmap.org/search.php?q=israel&viewbox=-126.57%2C51.79%2C126.57%2C-51.79

png("./map_entrepreneur_bubbles4.png",
    width=10, height=8, 
    units="in", res=700)
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapBubbles(dF=mmaps, nameZSize="Freq", #legendBg = "NA", legendPos = "center", 
           #nameZColour = "Freq",
           maxZVal = 350,
           catMethod =  "categorical", # "pretty", 
           numCats = 12, symbolSize =1,
           oceanCol='lightblue', landCol = "beige",
           borderCol=rgb(50,50,50,maxColorValue=255),
           lwd=0.5,lwdSymbols=1, colourLegendTitle = "Numbers", legendTitle = "Numbers",
           legendHoriz = TRUE, legendPos = "bottom", addLegend=TRUE)
dev.off()
#----------barplot-------------#
#default uses popn data in the default map
barplotCountryData( mmaps, nameColumnToPlot="Freq", nameCountryColumn = "country_iso3")

#-----Join country data--------------------~#
library(plyr)
library(rworldmap)
data(countryExData)

sPDF = joinCountryData2Map( countryExData
                               ,joinCode = "ISO3"
                               ,nameJoinColumn = "ISO3V10")

which(!mmap[,1]%in%countryExData$ISO3V10)
names(mmap) = c("ISO3V10", "Numbers")
data = join(countryExData, mmap, by = "ISO3V10")
names(data)

data = data[which(data$Numbers!="NA"),]
# download word bank country data
library(WDI)
?WDI
a = WDI(country="all", indicator=c("AG.AGR.TRAC.NO","TM.TAX.TCOM.BC.ZS"),
    start=2012, end=2012)

?WDIsearch

plot(Numbers~Population2005, data = data)
plot(Numbers~GDP_capita.MRYA, data = data)
# data$gdp = data$GDP_capita.MRYA/data$Population2005
# plot(Numbers~gdp, data = data)

reg = lm(Numbers~Population2005 +GDP_capita.MRYA +AGRICULTURE+ CLIMATE, data = data)
summary(reg)

cor.test(data$Numbers, data$Population2005) # 0.47
# dtmt comes from the "topiclabel-person networks"
colnames(dtmt) = country[match(colnames(dtmt), dat2$name)]
#-----------country_cluster_by_co-strategies----------------#

mt = t(dtmt) 
mt = t(sapply(by(mt,rownames(mt),colMeans),identity) )

hc = hclust(dist(scale(mt)))
plot(hc, hang = -1)


library(ape)
plot(as.phylo(hc), type = "cladogram", cex = 1, label.offset = 0)
# unrooted
plot(as.phylo(hc), type = "unrooted")

plot(as.phylo(hc), type = "fan")


png("./country_cluster_by_co-strategies_cladogram.png", 
    width=10, height=15, 
    units="in", res=700)
plot(as.phylo(hc), type = "fan")

dev.off()
#-----------strategy_cluster_by_co-location----------------#

mt = t(dtmt) 
mt = sapply(by(mt,rownames(mt),colSums),identity) 

hc = hclust(dist(scale(mt)))
plot(hc, hang = -1)


library(ape)
plot(as.phylo(hc), type = "cladogram", cex = 1, label.offset = 0)
# unrooted
plot(as.phylo(hc), type = "unrooted")

plot(as.phylo(hc), type = "fan")


png("./strategy_cluster_by_colocation.png", 
    width=10, height=10, 
    units="in", res=700)
plot(as.phylo(hc), type = "fan")

dev.off()
###################################
# topic models
###################################

# Run the function you need to set the text first.
library(topicmodels)
require(rJava)
library(tm)
library(slam)
library(igraph)
library(openNLP)
require(NLP)
require(SnowballC)
require(tmcn)

# load("D:/chengjun/crystal/strategy_sentence_cleaning_workstation44_1_en.RData") # dss
# load("D:/chengjun/crystal/strategy_sentence_cleaning_workstation44_2_ce.RData") # dss
# load("D:/chengjun/crystal/strategy_sentence_cleaning_workstation44_3le.RData") # dss
# load("D:/chengjun/crystal/strategy_sentence_cleaning_workstation44_4hr.RData") # dss
# load("D:/chengjun/crystal/strategy_sentence_cleaning_workstation44_5he.RData") # dss
load("./strategy_sentence_cleaning_workstation44_6ed.RData") # dss

get_dtm = function(dss){
  names(dss) = c("id", "name", "org", "sentence", "pick")
  dx = dss[,4]
  
  corpus = Corpus(   VectorSource( dx)  )  # corpus[[1]] ## inspect the first corpus
  # make each letter lowercase
  corpus = tm_map(corpus, tolower) 
  
  # remove generic and custom stopwords
  human_find_special_cases = c("Funda??¨¬??¨ºo Pr?3-Cerrado")  # to delete "social" etc.
  
  my_stopwords = c(dat2$location1, # combine an index
                    dat2$organization,
                    dat2$name,            # delete both the first name and last name, if only they appears, rather than combined together.
                    human_find_special_cases)
  
  my_stopwords = Corpus( VectorSource(my_stopwords)  )
  my_stopwords = tm_map(my_stopwords, tolower) # to lowercase my_stopwords here
  my_stopwords = tm_map(my_stopwords, removePunctuation)
  
  # Finally we can delete the country/org/person names
  # However, when my_stopwords are too large, regx does not work, I have to split the corpus of my_stopwords
  # for dat2, this takes about 3 minutes
  mylist = unlist(my_stopwords)
  len = length(mylist)/1000
  
  
  for (i in c(0:(len-2), len-1)){
    cat("R is running for my_stopwords", i, 
        as.character(as.POSIXlt(Sys.time(), "Asia/Shanghai")), sep = "\n") 
    mywords = Corpus( VectorSource(mylist[1+1000*i:1000+1000*i])  ) 
    corpus = tm_map(corpus, removeWords, mywords)  
  }
  
  corpus = tm_map(corpus, removePunctuation)
  
  mywords.add = Corpus( VectorSource(c("will", "almost", "already"))  ) 
  mywords.add = c("will", "almost", "already", "also")
  corpus = tm_map(corpus, removeWords, mywords.add)  
  
  
  # install.packages("SnowballC")
  Sys.setlocale("LC_COLLATE", "C") # set this for reproducible results
  # corpus = tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte")) # very important to convert encoding
  dtm = DocumentTermMatrix(corpus, control = list(stemming = FALSE, # TRUE,  
                                                   stopwords = TRUE,
                                                   wordLengths=c(4, Inf), # to allow long words
                                                   bounds = list(global = c(5,Inf)), # each term appears in at least 5 docs
                                                   removeNumbers = TRUE, 
                                                   removePunctuation  = list(preserve_intra_word_dashes = FALSE)
                                                   ,encoding = "UTF-8" 
  )
  )
  return(dtm)
}


# colnames(dtm)  # findFreqTerms(dtm, lowfreq=0) ##  inspect all the words for errors
# term_tfidf =tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
# l1=term_tfidf >= quantile(term_tfidf, 0.5)       # second quantile, ie. median
# dtm = dtm[,l1]

dtm = get_dtm(dss)
dtm = dtm[row_sums(dtm)>0, ]; dim(dtm) # 2246 6210
summary(col_sums(dtm))
rowid = as.numeric(rownames(dtm))
nameid = dss[rowid,2]
#########################################
#  Topic modeling specification  
#########################################
SEED = 2003
jss_TM = list(Gibbs = LDA(dtm, k = 30, method = "Gibbs", 
                          control = list(seed = SEED, 
                                            burnin = 1000, 
                                            thin = 100, 
                                            iter = 1000))    )
  tfs= terms(jss_TM[["Gibbs"]], 30)


rs = posterior(jss_TM$Gibbs, dtm)
dtmt = (rs$topics)
rownames(dtmt) = nameid
# aggregate by rownames
dtmt = t(sapply(by(dtmt,rownames(dtmt),colSums),identity))
topic = read.csv("./human coding_Ashoka_combined_label_20140404.csv", header = FALSE, stringsAsFactors = FALSE)

# topic1 = topic[,2][topic[,1]=="Environment"]
# topic1 = topic[,2][topic[,1]=="Civic Engagement"]
# topic1 = topic[,2][topic[,1]=="Learning/Education"]
# topic1 = topic[,2][topic[,1]=="Human Rights"]
# topic1 = topic[,2][topic[,1]=="Health"]
topic1 = topic[,2][topic[,1]=="Economic Development"]

colnames(dtmt) = topic1
dtmt = t(dtmt)
dtmt = t(sapply(by(dtmt,rownames(dtmt),colSums), identity))

# save(dtmt, file="./person-topic-matrix-1en.RData") # dtmt
# save(dtmt, file="./person-topic-matrix-2ce.RData") # dtmt
# save(dtmt, file="./person-topic-matrix-3le.RData") # dtmt
# save(dtmt, file="./person-topic-matrix-4hr.RData") # dtmt
# save(dtmt, file="./person-topic-matrix-5he.RData") # dtmt
# save(dtmt, file="./person-topic-matrix-6ed.RData") # dtmt

##############################
# topiclabel_terms matrix
##############################

for(i in 1:6){
  file = paste("./results20140513/perplexity_", i, ".RData", sep = "")
  load(file) # dtmt
  print(mean(ctmK[[1]]))
}

# [1] 1592.383
# [2] 1747.174
# [3] 1487.149
# [4] 1683.958
# [5] 1578.841
# [6] 1748.231

topic = read.csv("./human coding_Ashoka_combined_label_20140404.csv", 
                 header = FALSE, stringsAsFactors = FALSE)

for(i in 1:6){
  file = paste("./results20140513/dtm_terms_", i, ".RData", sep = "")
  load(file) # dtm_terms
  cat_names = c("Environment", "Civic Engagement", "Learning/Education", 
                "Human Rights", "Health", "Economic Development" )
  rownames(dtm_terms) =  topic[,2][topic[,1]== cat_names[i]]
  assign(paste("dtm_terms", i, sep = ""), dtm_terms)
}


require(plyr)
join_matrix = function(m1, m2){
  m3 = rbind.fill.matrix(m1, m2)
  gc()
  rownames(m3) = c(rownames(m1), rownames(m2))
  m3[is.na(m3)]=0
  m3 = t(sapply(by(m3,rownames(m3),colSums),identity))
  gc()
  return(m3)
}

dtmt = join_matrix(dtm_terms1, dtm_terms2)
dtmt = join_matrix(dtmt, dtm_terms3)
dtmt = join_matrix(dtmt, dtm_terms4)
dtmt = join_matrix(dtmt, dtm_terms5)
dtmt = join_matrix(dtmt, dtm_terms6)
dtmt = dtmt[rownames(dtmt)[-1],]

topic_term_ratio = data.frame(term_ratio = rowSums(dtmt)/sum(rowSums(dtmt)))
write.csv(topic_term_ratio, "./topic_term_ratio.csv")


##############################
"topiclabel-person networks"
##############################
require(plyr)

load("./person-topic-matrix-1en.RData") # dtmt
dtmt1 = t(dtmt)
load("./person-topic-matrix-2ce.RData") # dtmt
dtmt2 = dtmt

load("./person-topic-matrix-3le.RData") # dtmt
dtmt3 = dtmt

load("./person-topic-matrix-4hr.RData") # dtmt
dtmt4 = dtmt

load("./person-topic-matrix-5he.RData") # dtmt
dtmt5 = dtmt

load("./person-topic-matrix-6ed-new.RData") # dtmt
dtmt6 = dtmt

join_matrix = function(m1, m2){
  m3 = rbind.fill.matrix(m1, m2)
  rownames(m3) = c(rownames(m1), rownames(m2))
  m3[is.na(m3)]=0
  m3 = t(sapply(by(m3,rownames(m3),colSums),identity))
  return(m3)
}

dtmt = join_matrix(dtmt1, dtmt2)
dtmt = join_matrix(dtmt, dtmt3)
dtmt = join_matrix(dtmt, dtmt4)
dtmt = join_matrix(dtmt, dtmt5)
dtmt = join_matrix(dtmt, dtmt6)
dtmt = dtmt[rownames(dtmt)[-1],]

####################
# co-adoption network
####################
# aggregae again
mat = t(dtmt)
get_common_person = function(mat){
  com = combn(colnames(mat),2)
  n = ncol(com)
  min_i = NULL
  colids = lapply(1:n, function(x) com[,x])
  for (i in 1:n){
    mat_i = mat[,colids[[i]]]
    min_i[[i]] = sum(pmin(mat_i[,1], mat_i[,2]))
  }
  colids = do.call(rbind, colids)
  colids = data.frame(colids, stringsAsFactors = FALSE)
  colids = data.frame(colids, min_i, stringsAsFactors = FALSE)
  return(colids)
}

net = get_common_person(mat)
net = net[which(net[,3]!=0), ]

# nets = subset(net, net[,3]>= mean(net[,3]))

#~~~~~~~~~~~~PLOT NETWORK~~~~~~~~~~~~~~~~#
library(igraph)
g =graph.data.frame(net[,1:2],directed=FALSE )
E(g)$weight = net[,3]

set.seed(34)   ## to make this reproducable
l=layout.fruchterman.reingold(g)

# set.seed(2008) ; colors = sample(colors()[20:150], 39);pal(colors)
colors = colors()[sample(seq(400, 600, by = 5), 39)];pal(colors)
nodesize = centralization.degree(g)$res 
V(g)$size = ( centralization.degree(g)$res )/3
V(g)$degree = centralization.evcent(g)$vector 

closeness = centralization.closeness(g)[[1]]
betweenness = centralization.betweenness(g)[[1]]
evcent = centralization.evcent(g)[[1]]
degree = centralization.degree(g)[[1]]
topic_score = rowMeans(dtmt)

position = rank(-V(g)$size, ties.method = "first")

V(g)$color = colors[position] 

E(g)$width = (E(g)$weight-min(E(g)$weight))/500+1
nodeLabel = V(g)$name

# 保存图片格式
png("./topic_label_networks_color_size_good3.png",
    width=10, height=10, 
    units="in", res=700)

plot(g, vertex.label= nodeLabel,  edge.curved = FALSE, vertex.frame.color="#FFFFFF",
     vertex.label.cex =1.2,  edge.arrow.size=0.02, layout=l )

# 结束保存图片
dev.off()

'Community detection for semantic network'


fc = walktrap.community(g); sizes(fc)
mfc = membership(fc)
node_cluster = data.frame(names(mfc), mfc)
for (i in 1:max(mfc)) cat("\n", i, names(mfc[mfc==i]), "\n")


V(g)$color[which(V(g)$name%in%names(mfc[mfc==1]))]="purple"
V(g)$color[which(V(g)$name%in%names(mfc[mfc==2]))]="blue"
V(g)$color[which(V(g)$name%in%names(mfc[mfc==3]))]="green"
V(g)$color[which(V(g)$name%in%names(mfc[mfc==4]))]="red"
V(g)$color[which(V(g)$name%in%names(mfc[mfc==5]))]="orange"
V(g)$color[which(V(g)$name%in%names(mfc[mfc==6]))]="brown"

# 保存图片格式
png("./topic_label_networks_color_six_communities_same.png",
    width=8, height=8, 
    units="in", res=700)

plot(g, vertex.label= nodeLabel,  edge.curved = FALSE, vertex.frame.color="#FFFFFF",
     vertex.label.cex =1,  edge.arrow.size=0.02, layout=l )

# 结束保存图片
dev.off()

################################################
# Add community indicating background colors
#

# http://rpubs.com/kaz_yos/igraph-individuals


png("./topic_label_networks_color_six_communities.png",
    width=8, height=8, 
    units="in", res=700)

plot(g, mark.groups = by(seq_along(mfc), mfc, invisible), layout=l)
dev.off()


topic = read.csv("./human coding_Ashoka_combined_label_20140404.csv", 
                 header = FALSE, stringsAsFactors = FALSE)

topic = topic[!duplicated(topic),]
topic = subset(topic, topic[,2]!=0)

for (i in 1:6){
  topic[,1][topic[,1] == unique(topic[,1])[i]] = i
}

for (i in 1:39){
  topic[,2][ topic[,2] == V(g)$name[i] ] = i
}

topic[,1] = as.numeric(topic[,1])
topic[,2] = as.numeric(topic[,2])

marks= by(seq_along(mfc), mfc, invisible)

# 保存图片格式
png("./topic_label_networks_color_six_communities_by_overplapped_categories.png",
    width=8, height=8, 
    units="in", res=700)
plot(g, layout=l, mark.groups = lapply(1:6, function(i) c(subset(topic[,2], topic[,1] ==i)))   )
dev.off()



####################
# robustness check
####################

con1=file("./mass_elite.txt")
linn = readLines(con1)
close(con1)

con2=file("./material_symbolic.txt")
limm = readLines(con2)
close(con2)

con3=file("./general_specific.txt")
ligs = readLines(con3)
close(con3)

trim.leading = function (x)  sub("^\\s+", "", x)
clean.str = function(x) trim.leading(unlist(strsplit(x, ',')))

general = unique(clean.str(ligs[1]))
specific = unique(clean.str(ligs[2]))
# general%in%V(g)$name; specific%in%V(g)$name
material = unique(clean.str(limm[1]))
symbolic = unique(clean.str(limm[2]))
# material%in%V(g)$name; symbolic%in%V(g)$name
mass = unique(clean.str(linn[1]))
elite = unique(clean.str(linn[2]))

topic_score= rowMeans(dtmt)

######################
"mass-elite robustness test"
######################
topic = read.csv("./human coding_Ashoka_combined_label_20140404.csv", 
                 header = FALSE, stringsAsFactors = FALSE)
cat_names = c("Environment", "Civic Engagement", "Learning/Education", 
              "Human Rights", "Health", "Economic Development" )

# section_id = 3

get_section_scores = function(section_id){
  # load raw data
  load( paste("./dtm_topics_", section_id, ".RData", sep = "") ) # dtmt
  dtmt = dtm_topics
  colnames(dtmt) =  topic[,2][topic[,1]== cat_names[section_id]]
  
  
  # load mass_elite labels
  con=file("./mass_elite.txt")
  limm = readLines(con); 
  close(con)
  
  trim.leading = function (x)  sub("^\\s+", "", x)
  clean.str = function(x) trim.leading(unlist(strsplit(x, ',')))
  
  mass = unique(clean.str(limm[1]))
  elite = unique(clean.str(limm[2]))
  ## test 
  # mass%in%unique(topic[,2])
  # elite%in%unique(topic[,2])
  # colnames(dtmt)
  
  # Recode colnames
  for (i in 1:length(colnames(dtmt))){
    if (colnames(dtmt)[i] %in% mass & !(colnames(dtmt)[i] %in% elite)){
      colnames(dtmt)[i] = "mass"
    }else if (!(colnames(dtmt)[i] %in% mass) & colnames(dtmt)[i] %in% elite){
      colnames(dtmt)[i] = "elite"
    }else if(!(colnames(dtmt)[i] %in% mass) & !(colnames(dtmt)[i] %in% elite)){
      colnames(dtmt)[i] = "none"
    }else{
      colnames(dtmt)[i] = "both"
    } 
  }
  
  # colnames(dtmt)
  
  # step 1: aggregate matrix by column
  df = t(dtmt)
  df = t(sapply(by(df,rownames(df),colSums),identity))
  # step 2: drop unused rows
  df = df[2:3,]
  # step3: calculate aggregated weights for each person
  
  person = unique(colnames(df))
  
  get_person_score = function(person_name){
    person_colid = which(person_name==colnames(df))
    dfp = df[,person_colid]
    weight_score = 1:ncol(dfp)
    elite_score = mean(dfp['elite',]*weight_score)
    mass_score = mean(dfp['mass',]*weight_score)
    person_score = c(mass_score, elite_score)
    return(person_score)
  }
  
  person_scores = lapply(person, get_person_score)
  person_scores = do.call(rbind, person_scores)
  
  return(person_scores)
}

section_scores = lapply(1:6, get_section_scores)
section_scores = do.call(rbind, section_scores)
section_scores = as.data.frame(section_scores)
colnames(section_scores)= c("Mass", "Elite")

t.test(section_scores$Mass, section_scores$Elite)

ss = melt(section_scores)
names(ss) = c("Category", "Sequence")
ss$Category = as.factor(ss$Category)

# draw the box plot
library(reshape2)
library(ggplot2)
pmes = ggplot(ss, aes(x=Category, y=Sequence, fill=Category)) + 
  geom_boxplot() +  ggtitle("F") +
  ylab("Sentence sequential number") +
  guides(fill=FALSE) + scale_fill_manual(values = c("yellow", "orange"))


# 保存图片格式
png("./mass_elite_sequential_number_boxplot.png",
    width=5, height=5, 
    units="in", res=700)

pmes

# 结束保存图片
dev.off()

###################
"mass-elite"
###################
V(g)$mass= V(g)$name


for (i in 1:39){
  if (V(g)$mass[i] %in% mass & !(V(g)$mass[i] %in% elite)){
    V(g)$mass[i] = "Mass"
  }else if (!(V(g)$mass[i] %in% mass) & V(g)$mass[i] %in% elite){
    V(g)$mass[i] = "Elite"
  }else if(!(V(g)$mass[i] %in% mass) & !(V(g)$mass[i] %in% elite)){
    V(g)$mass[i] = "none"
  }else{
    V(g)$mass[i] = "both"
  } 
}



data_me = data.frame(Degree = degree, Category = V(g)$mass, 
                     Topic_score = topic_score, Closeness = closeness,
                     Betweenness = betweenness, Evcent = evcent)
data_me = subset(data_me, data_me$Category %in%c("Mass", "Elite"))
# change the order of data_me$Category
class(data_me$Category)
levels(data_me$Category)
data_me$Category = factor(data_me$Category,levels(data_me$Category)[c(3, 2, 1, 4)])
  
t.test(Degree~ Category, data_me)
t.test(Closeness~ Category, data_me)
t.test(Betweenness~ Category, data_me)
t.test(Evcent~ Category, data_me)
t.test(Topic_score~ Category, data_me)

t.test(Degree~ Category, data_me, var.equal=TRUE)
plot(Degree~ Category, data = data_me)  
plot(Topic_score~ Category, data = data_me)  

library(reshape2)
library(ggplot2)
pmed = ggplot(data_me, aes(x=Category, y=Degree, fill=Category)) + 
  geom_boxplot() +  ggtitle("E") +
  ylab("Degree centrality") +
  guides(fill=FALSE)+ scale_fill_manual(values = c("yellow", "orange"))

pmet = ggplot(data_me, aes(x=Category, y=Topic_score, fill=Category)) + 
  geom_boxplot() + 
  ylab("Topic score") +
  guides(fill=FALSE)
# 保存图片格式
png("./general_specific_degree_boxplot.png",
    width=5, height=5, 
    units="in", res=700)

pmed

# 结束保存图片
dev.off()

###################
"material-symbolic"
###################
V(g)$material = V(g)$name



for (i in 1:39){
  if (V(g)$material[i] %in% material & !(V(g)$material[i] %in% symbolic)){
    V(g)$material[i] = "Material"
  }else if (!(V(g)$material[i] %in% material) & V(g)$material[i] %in% symbolic){
    V(g)$material[i] = "Symbolic"
  }else if(!(V(g)$material[i] %in% material) & !(V(g)$material[i] %in% symbolic)){
    V(g)$material[i] = "none"
  }else{
    V(g)$material[i] = "both"
  } 
}



data_ms = data.frame(Degree = degree, Category = V(g)$material, 
                     Topic_score = topic_score, Closeness = closeness,
                     Betweenness = betweenness, Evcent = evcent)
data_ms = subset(data_ms, data_ms$Category %in%c("Material", "Symbolic"))


t.test(Degree~ Category, data_ms)
t.test(Closeness~ Category, data_ms)
t.test(Betweenness~ Category, data_ms)
t.test(Evcent~ Category, data_ms)
t.test(Topic_score~ Category, data_ms)

t.test(Degree~ Category, data_ms, var.equal=TRUE)
plot(Degree~ Category, data = data_ms)  
plot(Topic_score~ Category, data = data_ms)  

library(reshape2)
library(ggplot2)
pmsd = ggplot(data_ms, aes(x=Category, y=Degree, fill=Category)) + 
  geom_boxplot() +  ggtitle("C") +
  ylab("Degree centrality") +
  guides(fill=FALSE)+ scale_fill_manual(values = c("blue", "purple"))

pmst = ggplot(data_ms, aes(x=Category, y=Topic_score, fill=Category)) + 
  geom_boxplot() +  ggtitle("D") +
  ylab("Topic score") +
  guides(fill=FALSE)+ scale_fill_manual(values = c("blue", "purple"))
# 保存图片格式
png("./general_specific_degree_boxplot.png",
    width=5, height=5, 
    units="in", res=700)

pmsd

# 结束保存图片
dev.off()

################
"general-specific"
################
V(g)$general= V(g)$name

for (i in 1:39){
  if (V(g)$general[i] %in% general & !(V(g)$general[i] %in% specific)){
    V(g)$general[i] = "General"
  }else if (!(V(g)$general[i] %in% general) & V(g)$general[i] %in% specific){
    V(g)$general[i] = "Specific"
  }else if(!(V(g)$general[i] %in% general) & !(V(g)$general[i] %in% specific)){
    V(g)$general[i] = "none"
  }else{
    V(g)$general[i] = "both"
  } 
}



data_gs= data.frame(Degree = degree, Category = V(g)$general, Topic_score = topic_score)
data_gs = subset(data_gs, data_gs$Category %in%c("Specific", "General"))
t.test(Degree~ Category, data_gs)
t.test(Topic_score~ Category, data_gs)

t.test(Degree~ Category, data_gs, var.equal=TRUE)
plot(Topic_score~ Category, data = data_gs)  

library(reshape2)
library(ggplot2)
pgsd = ggplot(data_gs, aes(x=Category, y=Degree, fill=factor(Category))) + 
  geom_boxplot() + 
  ggtitle("A") +
  ylab("Degree centrality") +
  guides(fill=FALSE)+ scale_fill_manual(values = c("red", "green"))

pgst = ggplot(data_gs, aes(x=Category, y=Topic_score, fill=Category)) + 
  geom_boxplot() +  ggtitle("B") +
  ylab("Topic score") +
  guides(fill=FALSE)+ scale_fill_manual(values = c("red", "green"))
# 保存图片格式
png("./general_specific_degree_boxplot.png",
    width=5, height=5, 
    units="in", res=700)

pgsd

# 结束保存图片
dev.off()

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# 保存图片格式
png("./Robustness_check_3_dimensions_boxplot_title_colors.png",
    width=8, height=10, 
    units="in", res=700)

multiplot(pgsd, pmsd,
          pmed,  pgst, 
           pmst, pmes, cols=2)


dev.off()

#############
# topic-category matrix
#############
position = grep('<a', dat2$category)

for (i in position){
  dat2$category[i] = strsplit(dat2$category[i], split= ">")[[1]][2]
}

dat2$category[position] = gsub("</a", "", dat2$category[position])

# [7] "<a href=\"http://ashoka.org/taxonomy/term/7\" rel=\"tag\">Environment</a>"          
# [8] "<a href=\"http://ashoka.org/taxonomy/term/6\" rel=\"tag\">Health</a>"               
# [9] "<a href=\"http://ashoka.org/taxonomy/term/32\" rel=\"tag\">Civic Engagement</a>"    
# [10] "<a href=\"http://ashoka.org/taxonomy/term/34\" rel=\"tag\">Learning/Education</a>"  
# [11] "None"                                                                               
# [12] "<a href=\"http://ashoka.org/taxonomy/term/10\" rel=\"tag\">Economic Development</a>"

colnames(dtmt) = dat2$category[match(colnames(dtmt), dat2$name)]
m3 = t(aggregate(t(dtmt),list((colnames(dtmt))), sum))
write.csv(m3, "./strategy-category-matrix.csv")
m4 = data.frame(m3)
unique(dat2$category)

# document percentage
# term percentage
ttp = read.table("./topic_term_percentage.txt", header = F, sep = "\t", stringsAsFactors=F)
unique(ttp[,1])
ttp= aggregate(ttp$V2, by = list(ttp$V1), FUN = "sum")
names(ttp) = c("name", "term_percentage")
ttp$document_percentage = rowSums(dtmt)/sum(dtmt)

# write.csv(ttp, "./topic_percentage.csv")

#############################
"longitudinal topic clusting"
#############################
## Use the posterior method to get topic-term matrix and topic-document matrix 
year = dat2$year_fellowship
years = lapply(1:length(year), function(i) strsplit(year[i], " in ")[[1]][2])
years = unlist(years)
years = gsub(".", "", years, fixed = T )
years = as.numeric(years)
colnames(dtmt)  = years[match(colnames(dtmt), dat2$name)]

tdata = t(dtmt)
tdat = t(sapply(by(tdata,rownames(tdata),colSums),identity))
year = as.numeric(rownames(tdat))
plot(tdat[,2]~year, type = "l")

# weight by the number of fellows every year first!
# for longitudinal comparison. 
tdat = tdat/as.numeric(table(colnames(dtmt)))

# weight by minimum value
# If( Max([E]) = Min([E]), 0.5, ([E] – Min([E]) / (Max([E]) – Min([E])) )


# get_topic_ratio = function(i){
#   # i = 1
#   tt = data.frame(year= year, topic = tdat[,i])
# #     topic = tt[,2]/sum(tt[,2])
#    topic = (tt[,2]- min(tt[,2]))/(max(tt[,2]) - min(tt[,2]))
#   return(topic)
# }

# tp = lapply(1:39, get_topic_ratio)
# tp = do.call(rbind, tp)
# rownames(tp) = colnames(tdat)
# colnames(tp) = year
# require(colorspace)
# pal = function(col, border = "light gray", ...)
# {
#   n = length(col)
#   plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
#        axes = FALSE, xlab = "", ylab = "", ...)
#   rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
# }
# 
# colors = terrain_hcl(39, c = c(65, 0), l = c(45, 90), power = c(1/2, 1.5))
# plot(tdat[,1]~year, ylim = c(0, max(tdat)), type = "n", ylab = "Topic")
# for (i in  1:39) lines(tdat[,i]~year, col = colors[i])
# start plot with 6 curves in one plot

save(tdat, file = "./tdat.RData")
load("./tdat.RData")

# tdat = cbind(1:10, 21:30, 31:40, 11:20)
# rownames(tdat) = 2001:2010
# colnames(tdat) = c("A", "C", "D", "B")
# sequences = names(sort(colSums(tdat), decreasing = TRUE))

library(reshape2)
require(ggplot2)

#Create a custom color scale
require(RColorBrewer)

plot_evolution = function(m, n){
  mdf = melt(tdat[,sequences[m:n]])
  names(mdf) = c("Year", "Topic", "Topic_score")
  mdf$Topic = factor(mdf$Topic, levels = sequences[m:n])
  p = ggplot(data=mdf, aes(x=Year, y=Topic_score, group = Topic, 
                            colour = Topic)) +geom_line() +ylab("Topic Score")
  return(p)
}

p1 = plot_evolution(1, 7)
p2 = plot_evolution(8, 14)
p3 = plot_evolution(15, 21)
p4 = plot_evolution(22, 28)
p5 = plot_evolution(29, 35)
p6 = plot_evolution(36, 39)

source("./multipleplot.R")
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

png("./topic_evolution_ggplot2_good.png", 
    width=10, height=15, 
    units="in", res=700)
multiplot(p1, p2, p3, p4, p5, p6, cols = 1)
dev.off()

##################
# cluster by bursts
##################

get_peak = function(i){
  peak = max(tp[i,])/sum(tp[i,])
  return(peak)
}

get_peak_time = function(i){
  peak_time = which(tp[i,]==max(tp[i,]))
  return(peak_time)
}

get_cv = function(i){
  cv= sd(tp[i,])/mean(tp[i,])
  cv = cv/(cv+1)
  return (cv)
}

peak_year = unlist(lapply(1:39, get_peak_time))
peak = unlist(lapply(1:39, get_peak))
cv = unlist(lapply(1:39, get_cv))
plot(peak~cv)

p1 = which(peak_year <=10)
p2 = which(peak_year >10 & peak_year <=20)
p3 = which(peak_year >20 & peak_year <=30)
p4 = which(peak_year >30)


rownames(tp)[p1]
rownames(tp)[p2]
rownames(tp)[p3]


# tp = data.frame(tp)
# names(tp) = c(1978:2013)

png("./topic_evolution.png", 
    width=8, height=8, 
    units="in", res=700)
par(mfrow=c(2,2))
#
plot(tp[1,]~year, ylim = c(0, max(tp)), type = "n", ylab = "Topic")
for (i in which(peak_year <=30)) lines(tp[i,]~year)
title("1982-2012")
# 
plot(tp[1,]~year, ylim = c(0, max(tp)), type = "n", ylab = "Topic")
for (i in which(peak_year <=10)) lines(tp[i,]~year)
abline(v = 1982, col = "red", lty = 3)
abline(v = 1992, col = "red", lty = 3)
title("1982-1992")
# 
plot(tp[1,]~year, ylim = c(0, max(tp)), type = "n", ylab = "Topic")
for (i in which(peak_year >10 & peak_year <=20)) lines(tp[i,]~year)
abline(v = 1993, col = "red", lty = 3)
abline(v = 2002, col = "red", lty = 3)
title("1993-2002")
# 
plot(tp[1,]~year, ylim = c(0, max(tp)), type = "n", ylab = "Topic")
for (i in which(peak_year >20 & peak_year <=30)) lines(tp[i,]~year)
abline(v = 2003, col = "red", lty = 3)
abline(v = 2012, col = "red", lty = 3)
title("2003-2012")
# 
dev.off()

####################################
# cluster strategy-person matrix
####################################

hc = hclust(dist(scale(dtmt)))
# very simple dendrogram
plot(hc, hang = -1)
# using dendrogram objects
hcd = as.dendrogram(hc)
# alternative way to get a dendrogram
plot(hcd)
# using dendrogram objects
plot(hcd, type = "triangle")

library(ape)
plot(as.phylo(hc), type = "cladogram", cex = 1, label.offset = 0)
# unrooted
plot(as.phylo(hc), type = "unrooted")

plot(as.phylo(hc), type = "fan")


png("./topic_cluster_by_coadoption_fanplot.png", 
    width=10, height=10, 
    units="in", res=700)

dev.off()

mydata = t(tp)
# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:60) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:60, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


# K-Means Cluster Analysis
fit <- kmeans(mydata, 5) # 5 cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)


# rs$topics # topics and documents
# rs$terms  # topic and terms

"Method 1"
dc = t(cbind(rs$terms, t(rs$topics)))
dc = cor(dc)


"Method 2"
d1 <- dist( scale(rs$terms),  method = "euclidean"); dim(d) # distance matrix
d2 <- dist(scale(t(rs$topics)), method = "euclidean"); dim(d) # distance matrix
dc = d1*d2

d <- dist( scale(dc),  method = "euclidean"); dim(d) # distance matrix

names = NULL
for (i in 1:dim(tfs)[2]) names[[i]] = paste(tfs[1:5,i], collapse = " ")
names(d) = names

fit <- hclust(d, method="ward")

plot(fit) # display dendogram?
groups <- cutree(fit, k=20) # cut tree into 5 clusters
rect.hclust(fit, k=20, border="red")


png(paste(getwd(), "/cluster_3cor.png", sep = ''), 
    width=10, height=10, 
    units="in", res=300)
"run here something for save"
dev.off()