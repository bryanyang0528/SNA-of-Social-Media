
library("XML")
library("httr")
library("stringr")
library("igraph")
library("dplyr")

##這部分是爬網，先從網頁版ＰＴＴ中抓下每篇文章的聯結
rm(data)
data <- list()
for( i in 1628:2628){
  url <- paste('bbs/Wanted/index', i, '.html', sep='')
  html <- content(GET("https://www.ptt.cc/", path = url),as = 'parsed')
  url.list <- xpathSApply(html, "//div[@class='title']/a[@href]", xmlAttrs)
  data <- rbind(data, url.list[[1]])
}

##把一些空值刪掉
data = Filter(function(x) x != "www.ptt.cc",data)

##根據每篇文章代碼進去抓作者和推文的資料
popu <- function(link){
  html <- content(GET('https://www.ptt.cc', path = data[[link]]), as = 'parsed')
  poster <- xpathApply(html,"//div[@class='article-metaline']/span[@class='article-meta-value']",xmlValue)[[1]]

  #這邊特別說明一下，因為作者這個欄位很討厭，除了ＩＤ之外還有暱稱，要把暱稱刪掉
  poster <- str_split_fixed(poster, "\\(",2)[,1]
  puller <- xpathApply(html,"//div[@class='push']/span[@class='f3 hl push-userid']",xmlValue)
  
  #這個if是排除那些沒人有推文的作者（例如我）
  if (length(puller) >0 ){
    puller <- as.data.frame(matrix(unlist(puller),nrow = length(puller),T))
    edgelist <- merge(poster,puller,all = TRUE)
    return (edgelist)
  }
}

##我在這個步驟卡了半天  每次抓ＩＤ和推文都很容易抓漏
##原本用sapply做但是遇到抓錯後面就全部錯，所以改用for回圈
edge = list()
for (l in 1:length(data)){
  edge <- rbind(edge, popu(l))
}

#依照推文和被推者分組，計算每個pair的數量
edge2 <- group_by(edge,x,V1)
edge2 <- summarise(edge2,weight=n())

#from 表示推文者 in 表示被推文的
edge2 <- data.frame(from = edge2$V1, to = edge2$x,weight = edge2$weight)

#使用igraph套件將edge list的data轉成graph 物件，這是有方向性的
g <- graph.data.frame(edge2,directed = TRUE)

#用被推文的數量“in"來決定大小
V(g)$size <- degree(g,mode = ("in"))/2

#接下來就是畫圖輸出了
png(filename="org_network.png", height=1080, width=1080) 
plot(g, layout = layout.fruchterman.reingold,
     vertex.label=NA, 
     edge.arrow.size = 1
     )
dev.off()


