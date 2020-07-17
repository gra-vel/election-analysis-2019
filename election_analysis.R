### Introduction

### Background

### Exploration
options(warn=-1)

# loading libraries
library(tidyverse)
library(plotly)

# loading data for political parties
load("parties.Rdata")
partidos_alc <- partidos_alc %>%
  mutate(OP_SIGLAS = as.factor(toupper(as.character(OP_SIGLAS)))) #upper characters for OP_SIGLAS
head(partidos_alc)

# number of candidates
partidos_alc %>%
  select(CANDIDATO_CODIGO) %>%
  summarise(total.count = n())

# number of parties
partidos_alc %>%
  select(OP_CODIGO) %>%
  unique() %>%
  tally()

### Number of victories by party or alliance
# getting 10 top individual parties and alliances
top_parties <- partidos_alc %>%
  filter(CANDIDATO_ESTADO == "Electos") %>% #filtering the elected candidates
  group_by(OP_SIGLAS) %>%
  count(OP_SIGLAS) %>%
  arrange(desc(n)) %>%
  head(10) %>% droplevels()
round(sum(top_parties$n)/221, 3)

# plotting overall results
top_plot <- plot_ly(top_parties, x = ~reorder(OP_SIGLAS, -n), y = ~n, 
                    marker = list(color = 'rgb(158,202,255)', opacity = rep(0.8, 10),
                                  line = list(color = 'rgb(8,48,107)', width = 1.5)),
                    type = "bar") %>%
  layout(title = "Ten best performing parties/alliances by number of mayors elected", showlegend = FALSE,
         yaxis = list(title = 'Count'),
         xaxis = list(title = 'Parties and alliances'))
top_plot
#embed_notebook(top_plot)

# top_plot <- ggplot(top_parties, aes(x = reorder(OP_SIGLAS, -n), y = n)) +
#   geom_bar(stat = 'identity', alpha = 1, fill='lightblue') +
#   ggtitle('Ten best performing parties/alliances by number of mayors elected') +
#   xlab('Parties and alliances') +
#   ylab('Count') +
#   theme(plot.title = element_text(hjust = 0.5),
#         axis.text.x = element_text(angle = 25, vjust = 1, hjust=1),
#         panel.background = element_rect(fill = 'white'),
#         panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', color = 'gray'),
#         panel.grid.minor.y = element_line(size = 0.05, linetype = 'solid', color = 'gray'))
# top_plot

### Number of victories by number of parties in alliance
# all winning parties and alliances
alliances <- partidos_alc %>%
  mutate(parties_n = str_count(OP_SIGLAS, "/") + 1) %>% #how many parties are in the alliance
  filter(CANDIDATO_ESTADO == "Electos")

alliances %>%
  group_by(parties_n) %>%
  mutate(total_allliance = n()) %>%
  select(parties_n, total_allliance) %>% unique() %>% 
  arrange(parties_n) %>% ungroup() %>%
  mutate(perc = total_allliance/sum(total_allliance)*100)

# parties per alliance
alliances <- as_tibble(str_split(alliances$OP_SIGLAS, "/", simplify = TRUE))

# number of victories of each party running alone or as a part of an alliance
alliances <- alliances %>% 
  mutate(parties_n = rowSums(alliances != "")) %>% #sums the number of parties in each election
  gather("V1","V2","V3","V4","V5","V6", key = "V", value = "party") %>% #reshape data
  filter(party != "") %>% #removes NA
  count(party, parties_n) %>% #number of times a party appears per number of alliance
  group_by(party) %>% 
  mutate(total = sum(n)) %>% 
  arrange(desc(total), desc(parties_n), desc(n)) %>% 
  ungroup()

# identifying top ten parties
alliances <- alliances %>%
  left_join(alliances[!duplicated(alliances$party),] %>% #modify alliances by deleting party duplicates
              select(party) %>% #takes only one variable
              mutate(id_party = row_number())) %>% #creates id to join
  filter(id_party <= 10) %>% #filters by id
  mutate(parties_n = case_when(
    .$parties_n == 1 ~ 'A single party',
    .$parties_n == 2 ~ 'Alliance of 2',
    .$parties_n == 3 ~ 'Alliance of 3',
    .$parties_n == 4 ~ 'Alliance of 4',
    .$parties_n == 5 ~ 'Alliance of 5',
    .$parties_n == 6 ~ 'Alliance of 6'
  ))

# plotting alliances
alliances_plot <- plot_ly(alliances, x = ~reorder(party, -total), y = ~n,
                          color = ~as.factor(parties_n),
                          type = 'bar',
                          text = ~total,
                          textposition = 'inside') %>%
  layout(title = 'Top ten parties by number of alliances in mayor races', showlegend = TRUE,
         barmode = 'stack',
         legend = list(orientation = 'h',
                       xanchor = 'center',
                       x = 0.5),
         yaxis = list(title = 'Count'),
         xaxis = list(title = ''))
alliances_plot
#embed_notebook(alliances_plot)

# alliances_plot <- ggplot() +
#   geom_bar(data = alliances[order(alliances$parties_n, decreasing = FALSE),],
#            aes(reorder(party, -total), n, fill = as.factor(parties_n),),
#            stat = 'identity', alpha = 0.8, position = position_stack(reverse = TRUE)) +
#   geom_bar(data = alliances[which(alliances$parties_n == 1),],
#            aes(reorder(party, -total), n, fill = as.factor(parties_n),),
#            stat = 'identity', position = 'dodge') +
#   scale_fill_brewer(palette = "Paired", direction = -1) +
#   guides(fill = guide_legend(reverse = TRUE)) +
#   ggtitle('Top ten parties by number of alliances in mayor races') +
#   xlab('') +
#   ylab('Count') +
#   theme(plot.title = element_text(hjust = 0.5),
#         panel.background = element_rect(fill = 'white'),
#         panel.grid.major.y = element_line(size = 0.05, linetype = 'solid', color = 'gray'),
#         panel.grid.minor.y = element_line(size = 0.05, linetype = 'solid', color = 'gray'),
#         legend.position = 'bottom',
#         legend.title = element_blank())
# alliances_plot

### Parties in second place
# loading data of total results
load("results.Rdata")
head(resultados_alc)

# finding parties in second place for each victory of top parties
second_party <- resultados_alc %>%
  arrange(CANTON_CODIGO, desc(VOTOS)) %>% #sorting
  group_by(CANTON_CODIGO) %>%
  mutate(order = row_number()) %>% #creating index
  filter(order <= 2) %>% select(-VOTOS, -PROVINCIA_CODIGO, -PROVINCIA_NOMBRE) %>%  #filtering out first and second party
  left_join(partidos_alc %>% #getting info on parties
              select(CANDIDATO_CODIGO, OP_SIGLAS), by = "CANDIDATO_CODIGO") %>%
  rename(OPS = OP_SIGLAS) %>%
  mutate(OP_SIGLAS = ifelse(order == 1, as.character(OPS), "")) %>%
  left_join(top_parties, by = "OP_SIGLAS") %>% #it can't be right join because it would remove order == 2
  mutate(n = ifelse(is.na(n) == TRUE, 0, n),
         temp = sum(n)) %>% ungroup() %>% 
  filter(temp != 0) %>%
  mutate(sec = ifelse(order == 1, as.character(lead(OPS)), NA)) %>%
  select(-OPS, -temp) %>%
  filter(!is.na(sec)) %>%
  count(OP_SIGLAS, n, sec)

second_party %>%
  group_by(OP_SIGLAS) %>%
  filter(nn == max(nn)) %>%
  arrange(desc(n), nn)

# parties that appear the most in second place
second_party %>%
  group_by(sec) %>%
  mutate(total_sec = sum(nn)) %>%
  select(-OP_SIGLAS, -n, -nn) %>% unique() %>%
  arrange(desc(total_sec)) %>%
  head(20)

### Difference between first and second parties
# getting difference between first and second parties when top parties won
fsp_results <- resultados_alc %>%
  select(-PROVINCIA_CODIGO, -PROVINCIA_NOMBRE) %>%
  group_by(CANTON_CODIGO) %>% 
  mutate(total = sum(VOTOS),
         perc = (VOTOS/total)*100) %>%
  arrange(CANTON_CODIGO, desc(VOTOS)) %>%
  mutate(order = row_number()) %>%
  filter(order <= 2) %>%
  mutate(dif_seg = perc - lead(perc),
         CANTON_NOMBRE = as.character(CANTON_NOMBRE)) %>%
  filter(!is.na(dif_seg)) %>%
  left_join(partidos_alc %>% select(-OP_NOMBRE, -CANDIDATO_ESTADO), by = "CANDIDATO_CODIGO") %>%
  right_join(top_parties %>% ungroup() %>% mutate(party_n = row_number()), by = "OP_SIGLAS") %>%
  select(-order, -perc, -VOTOS, -total, -CANDIDATO_CODIGO) %>%
  arrange(desc(n), desc(dif_seg))

fsp_plot <- plot_ly(fsp_results, x = ~reorder(CANTON_NOMBRE, party_n), y = ~round(dif_seg,2), type = 'bar', 
                    color = ~as.factor(OP_SIGLAS), 
                    colors = 'Set2') %>%
  layout(title = 'Difference  between first and second parties in each canton for top parties',
         legend = list(orientation = 'v',
                       xanchor = 'left',
                       x = 1.00,
                       y = 0.35,
                       traceorder = 'normal'),
         yaxis = list(title = 'Percent'),
         xaxis = list(title = ''))
fsp_plot
#embed_notebook(fsp_plot)

# fsp_plot <- ggplot(fsp_results, aes(reorder(CANTON_NOMBRE, party_n), dif_seg)) +
#   geom_bar(aes(fill = reorder(OP_SIGLAS, party_n)), stat = "identity") +
#   scale_fill_brewer(palette = 'Paired') +
#   guides(fill = guide_legend(reverse = FALSE)) +
#   ggtitle('Difference between first and second parties in each canton for top parties') +
#   xlab('') +
#   ylab('Difference (%)') +
#   theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1),
#         plot.title = element_text(hjust = 0.5),
#         panel.background = element_rect(fill = 'white'),
#         panel.grid.major.y = element_line(size = 0.05, linetype = 'solid', color='gray'),
#         panel.grid.minor.y = element_line(size = 0.05, linetype = 'solid', color='gray'),
#         legend.position = 'bottom',
#         legend.title = element_blank())
# fsp_plot

# Canton: A. Baquerizo Moreno
ABM <- arrange(resultados_alc[which(resultados_alc$CANTON_NOMBRE=='A.baquerizo Moreno'),], desc(VOTOS)) %>%
  select(-PROVINCIA_CODIGO, -PROVINCIA_NOMBRE, -CANTON_CODIGO)
print(ABM)
round((max(ABM$VOTOS)/sum(ABM$VOTOS))*100,3)

# Canton: Daule
DAU <- arrange(resultados_alc[which(resultados_alc$CANTON_NOMBRE=='Daule'),], desc(VOTOS)) %>%
  select(-PROVINCIA_CODIGO, -PROVINCIA_NOMBRE, -CANTON_CODIGO)
print(DAU)
round((max(DAU$VOTOS)/sum(DAU$VOTOS))*100,3)

# Canton: Duran
DUR <- arrange(resultados_alc[which(resultados_alc$CANTON_NOMBRE=='Duran'),], desc(VOTOS)) %>%
  select(-PROVINCIA_CODIGO, -PROVINCIA_NOMBRE, -CANTON_CODIGO)
print(DUR)
round((max(DUR$VOTOS)/sum(DUR$VOTOS))*100,3)