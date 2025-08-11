Sys.setlocale('LC_ALL', 'C')
movie <- read.csv('pre_movie.csv', encoding = 'UTF-8', stringsAsFactors = F)
staff <- read.csv('pre_actor.csv', encoding = 'UTF-8', stringsAsFactors = F)
Sys.setlocale('LC_ALL', 'korean')

library(tidyverse)
library(igraph)

movie <- movie %>% mutate(genre = strsplit(genre, ',')) %>% unnest(genre)

actor <- staff %>% filter(person_category == 'actor')
director <- staff %>% filter(person_category == 'director')

director_data <- merge(director[c('movie_id', 'person_id')], actor[c('movie_id', 'person_id')], by='movie_id')
director_data['person_id.x'] <- paste0('D', staff_data[['person_id.x']])
director_data['person_id.y'] <- paste0('A', staff_data[['person_id.y']])

movie_temp <- unique(actor[1])[[1]]
actor_comb <- data.frame()
for (j in 1:length(movie_temp)) {
  tmp <- actor[actor$movie_id==movie_temp[j],4]
  if (length(tmp) > 1) {
    actor_comb <- rbind(actor_comb, data.frame(movie_id=movie_temp[j], t(combn(tmp,2))))
  }
}

graph_names <- c('d_animation', 'a_animation',
               'd_action', 'a_action',
               'd_comedy', 'a_comedy',
               'd_fanta', 'a_fanta',
               'd_melo', 'a_melo',
               'd_drama', 'a_drama',
               'd_mystery', 'a_mystery',
               'd_adventure', 'a_adventure',
               'd_thriller', 'a_thriller',
               'd_crime', 'a_crime',
               'd_family', 'a_family',
               'd_horor', 'a_horor',
               'd_scifi', 'a_scifi',
               'd_musical', 'a_musical',
               'd_war', 'a_war',
               'd_doc', 'a_doc',
               'd_hist', 'a_hist',
               'd_ero', 'a_ero',
               'd_show', 'a_show',
               'd_western', 'a_western')

categories <- c("애니메이션",
                "액션",
                "코미디",
                "판타지",
                "멜로/로맨스",
                "드라마",
                "미스터리",
                "어드벤처",
                "스릴러",
                "범죄",
                "가족",
                "공포(호러)",
                "SF",
                "뮤지컬",
                "전쟁",
                "다큐멘터리",
                "사극",
                "성인물(에로)",
                "공연",
                "서부극(웨스턴)")

# 그래프 생성
for (i in 1:length(categories)) {
  staff_temp <- director_data[staff_data$movie_id %in% movie[movie$genre == categories[i], ][['code']],]
  graph_temp <- graph_from_data_frame(staff_temp[2:3], directed=F)
  assign(graph_names[i*2-1], graph_temp)
  
  actor_temp <- actor_comb[actor_comb$movie_id %in% movie[movie$genre == categories[i],][['code']],]
  graph_temp2 <- graph_from_data_frame(actor_temp[2:3], directed=F)
  assign(graph_names[i*2], graph_temp2)
}

# 전체 장르 그래프 생성
staff_temp <- director_data
d_all <- graph_from_data_frame(staff_temp[2:3], directed=F)

actor_temp <- actor_comb
a_all <- graph_from_data_frame(actor_temp[2:3], directed=F)

graph_names <- c(graph_names, 'd_all', 'a_all')

# 그래프에 대한 통계
graph_stat <- data.frame()
graph_comp <- list()
for (s in graph_names) {
  g <- get(s)
  print(s)
  graph_stat <- rbind(graph_stat, data.frame(graph=s, vcount=vcount(g), ecount=ecount(g),density=edge_density(g, loops=F), 
                                             dyad_mut=dyad_census(g)$mut, dyad_null=dyad_census(g)$null, triad_003=triad_census(g)[1], 
                                             triad_201=triad_census(g)[11], transitivity=transitivity(g), centr=centr_degree(g)$centralization))
  graph_comp[s] <- components(g)
}

# 시각화

for (s in graph_names) {
  g <- get(s)
  
  gubun <- str_sub(V(g)$name, 1, 1)
  colors <- ifelse(gubun=='D', 'yellow', 'blue')
  sizes <- ifelse(gubun=='D', 2, 1)
  plot(g, main=s, layout=layout.kamada.kawai, vertex.size=sizes, vertex.color=colors, edge.arrow.size=0.01, vertex.label=NA)
  legend("bottomleft",c("배우","감독"),cex=0.7,fill=colors,col=colors,bty="n")
}

# for (s in graph_names) {
#   g <- get(s)
#   print(s)
#
#   gubun <- str_sub(V(g)$name, 1, 1)
#   colors <- ifelse(gubun=='D', 'yellow', 'blue')
#   sizes <- ifelse(gubun=='D', 2, 1)
#   pdf(paste0('plot/', s, '.pdf'))
#   pdf.options(family='Korea1deb')
#   plot(g, main=s, layout=layout.kamada.kawai, vertex.size=sizes, vertex.color=colors, edge.arrow.size=0.01, vertex.label=NA)
#   legend("bottomleft",c("배우","감독"),cex=0.7,fill=color,col=color,bty="n")
#   dev.off()
# }

# --------------------------------------------------------------------------------------------------



# --------------------------------------------------------------------------------------------------

# 통계 데이터
graph_stat

# 칼럼명 'density'만 다른 칼럼으로 바꾸면 됨
graph_stat[order(graph_stat['density_a'], decreasing = T),][['graph']]
graph_stat[order(graph_stat['density_d'], decreasing = T),][['graph']]

# g에 원하는 그래프 넣고
result <- data.frame()
topn <- 3
for (j in 1:(length(graph_names)/2)) {
  g <- get(graph_names[j*2-1])
  counts <- degree(g)[order(degree(g), decreasing = T)]
  ordered_names <- names(counts)
  # D는 가장 많은 배우와 함께 일한 감독, A는 가장 많은 감독과 일한 배우
  top_d <- ordered_names[str_detect(ordered_names, 'D')][1:topn] 
  top_a <- ordered_names[str_detect(ordered_names, 'A')][1:topn]
  id_d <- str_sub(top_d, 2)
  id_a <- str_sub(top_a, 2)
  counts_d <- counts[top_d]
  counts_a <- counts[top_a]
  names_d <- unique(staff[staff[['person_id']] %in% id_d,]$person_name)
  names_a <- unique(staff[staff[['person_id']] %in% id_a,]$person_name)
  
  g2 <- get(graph_names[j*2])
  counts2 <- degree(g2)[order(degree(g2), decreasing = T)]
  id2 <- names(counts2)
  top2 <- id2[1:topn]
  names2 <- unique(staff[staff[['person_id']] %in% top2,]$person_name)
  
  for (i in 1:topn) {
    result <- rbind(result, data.frame('장르'=str_sub(graph_names[j*2], 3),
                                       '순위'=i,
                                       '감독 with 배우'=paste0('(',counts_d[i], ') ', names_d[i]), 
                                       '배우 with 감독'=paste0('(',counts_a[i], ') ', names_a[i]), 
                                       '배우 with 배우'=paste0('(',counts2[top2[i]], ') ', names2[i])))
  } 
}


# g에 원하는 그래프 넣고 실행하면 가장 많은 배우와 함께 일한 배우 나옴
g <- a_action
counts <- degree(g)[order(degree(g), decreasing = T)]
test <- names(counts)
top <- test[1:5]
test <- top
counts <- counts[top]
test <- unique(staff[staff[['person_id']] %in% test,]$person_name)
for (i in 1:length(test)) print(paste0('(',counts[i], ') ', test[i]))

g2 <- a_action
counts2 <- degree(g2)[order(degree(g2), decreasing = T)]
id2 <- names(counts2)
top2 <- id2[1:5]
names2 <- unique(staff[staff[['person_id']] %in% top2,]$person_name)
for (i in 1:5) print()


s <- 'a_action'
g <- get(s)
comp <- graph_comp[s]
table(comp)

# 'density'가 최대인 그래프 3개 추출
graph_stat[order(graph_stat['transitivity'], decreasing = T),]

# 'density'가 최소인 그래프 3개 추출
graph_stat[order(graph_stat['density'])[1:3],]

# d_action의 components 확인 방법
table(graph_comp['d_action'])

# 필요한 그래프 이름 넣고, 실행하면 데이터 가장 큰 컴포넌트와 비율 나옴
final <- c('d_drama', 'a_drama', 'd_melo', 'a_melo', 'd_action', 'a_action')

comp_stat <- data.frame()
for (s in graph_names) {
  g <- get(s)
  comp <- graph_comp[s]
  print(s)
  top <- table(comp)[order(table(comp), decreasing = T)][1]
  comp_stat <- rbind(comp_stat, data.frame(graph=s, top=top, top_rate=top/vcount(g)))
}
comp_stat[order(comp_stat$top_rate),]

# 특정 그래프 하나만 확인할 때.
g <- get(s)
comp <- graph_comp[s]
print(s)
top <- table(comp)[order(table(comp), decreasing = T)][1:3]


# 감독 같이 있는 데이터에 대해 density 다른 방법으로 계산한거
dens <- c('d_drama', 'd_melo', 'd_action')
for (s in dens) {
  g <- get(s)
  edge_density(g)
  tmp <- ecount(g)/(sum(str_detect(names(V(g)), 'D'))*sum(str_detect(names(V(g)), 'A')))
  print(tmp)
}

# 장르별 속한 영화 수
top <- movie %>% group_by(genre) %>% summarise(n())
top[order(top$`n()`, decreasing = T),]

# 개별 플랏
s <- 'd_drama'
g <- get(s)
print(s)
gubun <- str_sub(V(g)$name, 1, 1)
colors <- ifelse(gubun=='D', 'yellow', 'blue')
sizes <- ifelse(gubun=='D', 2, 1)
#pdf(paste0('plot/', s, '.pdf'))
#pdf.options(family='Korea1deb')
plot(g, main=s, layout=layout.kamada.kawai, vertex.size=sizes, vertex.color=colors, edge.arrow.size=0.01, vertex.label=NA)
legend("bottomleft",c("배우","감독"),cex=0.7,fill=color,col=color,bty="n")
dev.off()