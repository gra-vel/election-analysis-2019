---
title: "2019-election-analysis-plotly"
author: "Gabriel Velastegui"
date: "17/7/2020"
output:
  html_document:
    fig_height: 7
    fig_width: 9
    keep_md: yes
    toc: yes
    toc_float: yes
---




# 2019 Ecuador election analysis - Mayoral elections

## Introduction

The focus of this analysis is to identify party performance in mayoral elections. Specifically, the analysis focuses on the parties that had won the most in these elections. Local elections took place on March 24, 2019. Some local authorities won with less than a third of support of the electorate due to the sheer number of candidates running for a single seat, which points out to a deep party fragmentation in the political system.

## Background

Ecuador has a multi-party system with almost no long-standing party tradition. Multiple parties emerge every few years in a sort of political cycle, even though most politicians have been around for several decades, but with different political organizations. General elections and local elections take place every four years but with a lag of two years between each other. The last general election was in 2017, whereas the last local election was in 2019. Next general election is in 2021.

Even though the multi-party system is hailed as an expression of democracy, it is clear that there is a significant fragmentation in the political system. There were more than 40 000 candidates for the 5 661 available seats for local authorities. That is an average of seven candidates for each seat. The following analysis focuses only on the elections for mayor in the 221 city councils in the country.

## Analysis

### Exploration

```r
# loading libraries
library(tidyverse)
library(plotly)

# loading data for political parties
load("parties.Rdata")
partidos_alc <- partidos_alc %>%
  mutate(OP_SIGLAS = as.factor(toupper(as.character(OP_SIGLAS)))) #OP_SIGLAS - acronyms for parties
head(partidos_alc)
```

```
## # A tibble: 6 x 5
##   OP_CODIGO CANDIDATO_CODIGO CANDIDATO_ESTADO OP_NOMBRE                OP_SIGLAS
##       <dbl>            <dbl> <fct>            <chr>                    <fct>    
## 1         3                2 Electos          Partido Sociedad Patrio~ PSP      
## 2        37                3 No Electos       Movimiento Sociedad Uni~ SUMA     
## 3        31                9 Electos          Movimiento Peninsular C~ MPCNG    
## 4        12               10 No Electos       Movimiento De Unidad Pl~ MUPP     
## 5         3               16 No Electos       Partido Sociedad Patrio~ PSP      
## 6       210               17 No Electos       Movimiento De Accion Ci~ MACF
```

```r
# number of candidates
partidos_alc %>%
  select(CANDIDATO_CODIGO) %>% #CANDIDATO_CODIGO - id for each candidate
  summarise(total.count = n())
```

```
## # A tibble: 1 x 1
##   total.count
##         <int>
## 1        1875
```

```r
# number of parties
partidos_alc %>%
  select(OP_CODIGO) %>% #OP_CODIGO - id for each party
  unique() %>%
  tally()
```

```
## # A tibble: 1 x 1
##       n
##   <int>
## 1   380
```

There are 1 875 candidates from 380 parties for the mayoral elections. That means that there was an average of almost 8.5 candidates for each of the 221 cantones (city council) of the country. It is important to note that the number of parties differ if I consider either their names or their acronyms. Some parties use the same acronym, despite having different names. Some names in the data set use the generic word 'Alianza' (alliance, in Spanish), meaning that they do not differentiate between one another.

The variable 'OP_CODIGO' uses a unique id for each party. There are some cases where the same party or alliance will have a different id based on the location in which they are participating. However, this happens when the party or alliance is local, instead of regional or national. The assumption is that these parties or alliances do not have a strong performance; therefore, it will not affect the analysis of larger parties. This is confirmed by getting the same results for top parties when using both the acronyms and the unique id.

### Number of victories by party or alliance


```r
# getting 10 top individual parties and alliances
top_parties <- partidos_alc %>%
  filter(CANDIDATO_ESTADO == "Electos") %>% #filtering the elected candidates
  group_by(OP_SIGLAS) %>%
  count(OP_SIGLAS) %>%
  arrange(desc(n)) %>%
  head(10) %>% droplevels()
(round(sum(top_parties$n)/221, 2))*100
```

```
## [1] 40
```

```r
top_plot <- plot_ly(top_parties, x = ~reorder(OP_SIGLAS, -n), y = ~n, 
                    marker = list(color = 'rgb(158,202,255)', opacity = rep(0.8, 10),
                                  line = list(color = 'rgb(8,48,107)', width = 1.5)),
                    type = "bar") %>%
  layout(title = "Ten best performing parties/alliances by number of mayors elected", showlegend = FALSE,
         yaxis = list(title = 'Count'),
         xaxis = list(title = 'Parties and alliances'))
top_plot
```

<!--html_preserve--><div id="htmlwidget-89a8cf9caaa8278ad9ef" style="width:864px;height:672px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-89a8cf9caaa8278ad9ef">{"x":{"visdat":{"1450133e6a6e":["function () ","plotlyVisDat"]},"cur_data":"1450133e6a6e","attrs":{"1450133e6a6e":{"x":{},"y":{},"marker":{"color":"rgb(158,202,255)","opacity":[0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8],"line":{"color":"rgb(8,48,107)","width":1.5}},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Ten best performing parties/alliances by number of mayors elected","showlegend":false,"yaxis":{"domain":[0,1],"automargin":true,"title":"Count"},"xaxis":{"domain":[0,1],"automargin":true,"title":"Parties and alliances","type":"category","categoryorder":"array","categoryarray":["MUPP","PSC/MMDG","MPAIS","CREO","PSC","MDSI","SUMA","PSC/MPUP","CREO/MCUP/PAEA/FE/UE/MNP","MEU/MDSI"]},"hovermode":"closest"},"source":"A","config":{"showSendToCloud":false},"data":[{"x":["MUPP","PSC/MMDG","MPAIS","CREO","PSC","MDSI","SUMA","PSC/MPUP","CREO/MCUP/PAEA/FE/UE/MNP","MEU/MDSI"],"y":[15,14,10,9,9,8,8,6,5,5],"marker":{"color":"rgb(158,202,255)","opacity":[0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8],"line":{"color":"rgb(8,48,107)","width":1.5}},"type":"bar","error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

Based on the number of victories, the MUPP could be considered the most successful party, as it managed to win in 15 cantones. However, there is a problem with this assertion, as it does not take into account how powerful alliances can be. Looking at the second, fifth and eight position, it appears that the PSC beats the MUPP, as it has won in more places by forming alliances. The victories of these 10 parties and alliances represent about 40% of the cantones in the country. It is also noteworthy the alliance between six parties, which has five victories.

### Number of victories by number of parties in alliance

In theory, an alliance would entail that the parties involved are sharing power. However, in practice that is not always the case. A single individual and not a council take the mayor seat. An alliance may be formed by party x and y, where the candidate for mayor comes from party x. Voters may expect that the alliance will achieve some sort of balance between two parties when governing, but it is highly likely that the candidate will almost always favor party x policies. Another issue has to do with agreements between parties, where the support for the alliance can be explicitly divided between the parties involved. As an example, an agreement may give the major party 70% of the control of local government, as it invests more resources and has a wider support in the electorate.

It is difficult to determine how each particular agreement works and it may take a whole different analysis and approach to accurately represent the balance of power in alliances. However, for the purposes of this analysis, the idea is to show how much the involvement of alliances can change the performance of parties in local elections. For this reason, the next step does not assume a division of power, but only accounts for the presence of a party in an alliance. This means that if party x and y share an alliance, both parties are registered to have won an election. This would inflate the real number of victories for each party. Nonetheless, it works as a representation of how much the party system can change and how important and widespread alliances are.


```r
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
```

```
## # A tibble: 6 x 3
##   parties_n total_allliance  perc
##       <dbl>           <int> <dbl>
## 1         1             107 48.4 
## 2         2              78 35.3 
## 3         3              17  7.69
## 4         4               6  2.71
## 5         5               8  3.62
## 6         6               5  2.26
```

Single parties won in 107 cantones in the country, which is little under half of all the city councils in the country. The majority of victories for mayor belong to alliances, being the alliance of two parties the most used (35.3%).


```r
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

# identifying top ten individual parties
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
```

<!--html_preserve--><div id="htmlwidget-0710145d31197329d2e2" style="width:864px;height:672px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-0710145d31197329d2e2">{"x":{"visdat":{"14504db3416":["function () ","plotlyVisDat"]},"cur_data":"14504db3416","attrs":{"14504db3416":{"x":{},"y":{},"text":{},"textposition":"inside","color":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Top ten parties by number of alliances in mayor races","showlegend":true,"barmode":"stack","legend":{"orientation":"h","xanchor":"center","x":0.5},"yaxis":{"domain":[0,1],"automargin":true,"title":"Count"},"xaxis":{"domain":[0,1],"automargin":true,"title":"","type":"category","categoryorder":"array","categoryarray":["PSC","CREO","MDSI","MPAIS","MUPP","MNP","MEU","MMDG","SUMA","PSE"]},"hovermode":"closest"},"source":"A","config":{"showSendToCloud":false},"data":[{"x":["PSC","CREO","MDSI","MPAIS","MUPP","MNP","MEU","SUMA","PSE"],"y":[9,9,8,10,15,3,3,8,1],"text":[42,34,29,28,21,20,16,15,14],"textposition":["inside","inside","inside","inside","inside","inside","inside","inside","inside"],"type":"bar","name":"A single party","marker":{"color":"rgba(102,194,165,1)","line":{"color":"rgba(102,194,165,1)"}},"textfont":{"color":"rgba(102,194,165,1)"},"error_y":{"color":"rgba(102,194,165,1)"},"error_x":{"color":"rgba(102,194,165,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["PSC","CREO","MDSI","MPAIS","MUPP","MNP","MEU","MMDG","SUMA","PSE"],"y":[30,15,10,3,4,6,6,14,5,3],"text":[42,34,29,28,21,20,16,15,15,14],"textposition":["inside","inside","inside","inside","inside","inside","inside","inside","inside","inside"],"type":"bar","name":"Alliance of 2","marker":{"color":"rgba(252,141,98,1)","line":{"color":"rgba(252,141,98,1)"}},"textfont":{"color":"rgba(252,141,98,1)"},"error_y":{"color":"rgba(252,141,98,1)"},"error_x":{"color":"rgba(252,141,98,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["PSC","CREO","MDSI","MPAIS","MNP","MEU","SUMA","MMDG","PSE"],"y":[2,4,3,8,1,2,2,1,3],"text":[42,34,29,28,20,16,15,15,14],"textposition":["inside","inside","inside","inside","inside","inside","inside","inside","inside"],"type":"bar","name":"Alliance of 3","marker":{"color":"rgba(141,160,203,1)","line":{"color":"rgba(141,160,203,1)"}},"textfont":{"color":"rgba(141,160,203,1)"},"error_y":{"color":"rgba(141,160,203,1)"},"error_x":{"color":"rgba(141,160,203,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["PSC","MDSI","MPAIS","MUPP","MNP","MEU"],"y":[1,1,3,2,1,2],"text":[42,29,28,21,20,16],"textposition":["inside","inside","inside","inside","inside","inside"],"type":"bar","name":"Alliance of 4","marker":{"color":"rgba(231,138,195,1)","line":{"color":"rgba(231,138,195,1)"}},"textfont":{"color":"rgba(231,138,195,1)"},"error_y":{"color":"rgba(231,138,195,1)"},"error_x":{"color":"rgba(231,138,195,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["CREO","MDSI","MPAIS","MNP","MEU","PSE"],"y":[1,7,4,4,3,7],"text":[34,29,28,20,16,14],"textposition":["inside","inside","inside","inside","inside","inside"],"type":"bar","name":"Alliance of 5","marker":{"color":"rgba(166,216,84,1)","line":{"color":"rgba(166,216,84,1)"}},"textfont":{"color":"rgba(166,216,84,1)"},"error_y":{"color":"rgba(166,216,84,1)"},"error_x":{"color":"rgba(166,216,84,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["CREO","MNP"],"y":[5,5],"text":[34,20],"textposition":["inside","inside"],"type":"bar","name":"Alliance of 6","marker":{"color":"rgba(255,217,47,1)","line":{"color":"rgba(255,217,47,1)"}},"textfont":{"color":"rgba(255,217,47,1)"},"error_y":{"color":"rgba(255,217,47,1)"},"error_x":{"color":"rgba(255,217,47,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

By taking into account the number of alliances, the order of the most successful parties changes significantly. The PSC could be considered the most successful party with 42 victories, 30 of them come from an alliance with one more party. The performance of the MUPP falls to fifth place, as it is the party with the less number victories in alliances (only 6), whereas the MMDG has no victory on its own, but 15 in alliances. Noteworthy are the parties with victories in alliances of five or six parties, as the implications of power sharing could be complex.

### Parties in second place

Turning back to the parties and alliances in the first part of this analysis, the next question was to find out if there is a trend for the parties that came in second place. The idea is to find out which parties came in second, when one of these 10 parties or alliances won the mayor race in a specific canton. In this point, I use a second dataset with the total results in each city council in the country to identify the party that came in second. It is important to note that each political party or alliance present one candidate in each canton. Therefore, I use the id for candidates to identify the number of votes to later join the data with the id for parties.


```r
# loading data of total results
load("results.Rdata")
head(resultados_alc)
```

```
## # A tibble: 6 x 6
##   PROVINCIA_CODIGO PROVINCIA_NOMBRE CANTON_CODIGO CANTON_NOMBRE CANDIDATO_CODIGO
##              <dbl> <fct>                    <dbl> <fct>                    <dbl>
## 1                1 Azuay                      260 Cuenca                   13581
## 2                1 Azuay                      260 Cuenca                   15411
## 3                1 Azuay                      260 Cuenca                   15881
## 4                1 Azuay                      260 Cuenca                   22636
## 5                1 Azuay                      260 Cuenca                   24094
## 6                1 Azuay                      260 Cuenca                   24778
## # ... with 1 more variable: VOTOS <dbl>
```

```r
# finding parties in second place for each victory of top parties
second_party <- resultados_alc %>%
  arrange(CANTON_CODIGO, desc(VOTOS)) %>% #CANTON_CODIGO - id for city council
  group_by(CANTON_CODIGO) %>%
  mutate(order = row_number()) %>% #creating index 
  filter(order <= 2) %>% select(-VOTOS, -PROVINCIA_CODIGO, -PROVINCIA_NOMBRE) %>%  #filtering out first and second party
  left_join(partidos_alc %>% #getting info on parties using shared variable (id for candidate)
              select(CANDIDATO_CODIGO, OP_SIGLAS), by = "CANDIDATO_CODIGO") %>%
  rename(OPS = OP_SIGLAS) %>%
  mutate(OP_SIGLAS = ifelse(order == 1, as.character(OPS), "")) %>%
  left_join(top_parties, by = "OP_SIGLAS") %>% #it can't be right join because it would remove order == 2
  mutate(n = ifelse(is.na(n) == TRUE, 0, n),
         temp = sum(n)) %>% ungroup() %>% 
  filter(temp != 0) %>%
  mutate(sec = ifelse(order == 1, as.character(lead(OPS)), NA)) %>% #creates variable with party in second place
  select(-OPS, -temp) %>%
  filter(!is.na(sec)) %>%
  count(OP_SIGLAS, n, sec)

second_party %>%
  group_by(OP_SIGLAS) %>%
  filter(nn == max(nn)) %>%
  arrange(desc(n), nn)
```

```
## # A tibble: 19 x 4
## # Groups:   OP_SIGLAS [10]
##    OP_SIGLAS                    n sec           nn
##    <chr>                    <dbl> <chr>      <int>
##  1 MUPP                        15 MPFA/MPAIS     3
##  2 PSC/MMDG                    14 MPAIS          4
##  3 MPAIS                       10 PSC            2
##  4 PSC                          9 MPAIS          2
##  5 PSC                          9 UP             2
##  6 CREO                         9 MPAIS          3
##  7 MDSI                         8 PSC            2
##  8 SUMA                         8 MUPP           2
##  9 PSC/MPUP                     6 CREO/MSP       3
## 10 CREO/MCUP/PAEA/FE/UE/MNP     5 APLA/MPQ       1
## 11 CREO/MCUP/PAEA/FE/UE/MNP     5 ID             1
## 12 CREO/MCUP/PAEA/FE/UE/MNP     5 MJS/PSE        1
## 13 CREO/MCUP/PAEA/FE/UE/MNP     5 PSC            1
## 14 CREO/MCUP/PAEA/FE/UE/MNP     5 SUMA           1
## 15 MEU/MDSI                     5 MCR            1
## 16 MEU/MDSI                     5 MGG            1
## 17 MEU/MDSI                     5 MNP/MPDR       1
## 18 MEU/MDSI                     5 MPAIS          1
## 19 MEU/MDSI                     5 MUPP           1
```

There is no single clear contender for the parties with the best performance. For instance, the MPFA/MPAIS came in second three times when the MUPP won, which represents 20% of the victories for the MUPP. Most of these parties came in second less than 50% of the times when the top parties won. This gives further evidence of the level of fragmentation in the political system.


```r
# parties that appear the most in second place
second_party %>%
  group_by(sec) %>%
  mutate(total_sec = sum(nn)) %>%
  select(-OP_SIGLAS, -n, -nn) %>% unique() %>%
  arrange(desc(total_sec)) %>%
  head(20)
```

```
## # A tibble: 20 x 2
## # Groups:   sec [20]
##    sec                      total_sec
##    <chr>                        <int>
##  1 MPAIS                           12
##  2 PSC                              6
##  3 MUPP                             5
##  4 CREO                             4
##  5 PSC/MMDG                         3
##  6 UP                               3
##  7 MPFA/MPAIS                       3
##  8 CREO/MSP                         3
##  9 SUMA                             2
## 10 MNP/MPDR                         2
## 11 CD                               2
## 12 CREO/MCUP/PAEA/FE/UE/MNP         2
## 13 SUMA/PSC                         2
## 14 PSC/SUMA                         2
## 15 MFCS                             2
## 16 MLC                              1
## 17 MUPP/PRIMIZA                     1
## 18 PPA                              1
## 19 APLA/MPQ                         1
## 20 ID                               1
```

The table shows the 20 first parties that came in second when the top ten parties won. MPAIS is the party that came in second the most. It would appear that single parties, in this specific case, had a better performance than alliances, since they are represented by the first four positions with the highest values.

### Difference between parties in first and second place

```r
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
         CANTON_NOMBRE = as.character(CANTON_NOMBRE)) %>% #CANTON_NOMBRE - name of city council
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
```

<!--html_preserve--><div id="htmlwidget-04e28c968cc65284a487" style="width:864px;height:672px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-04e28c968cc65284a487">{"x":{"visdat":{"145077bddb":["function () ","plotlyVisDat"]},"cur_data":"145077bddb","attrs":{"145077bddb":{"x":{},"y":{},"color":{},"colors":"Set2","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Difference  between first and second parties in each canton for top parties","legend":{"orientation":"v","xanchor":"left","x":1,"y":0.35,"traceorder":"normal"},"yaxis":{"domain":[0,1],"automargin":true,"title":"Percent"},"xaxis":{"domain":[0,1],"automargin":true,"title":"","type":"category","categoryorder":"array","categoryarray":["Aguarico","Cañar","Cayambe","El Pangui","Gualaquiza","Guamote","Guaranda","Huamboya","Limon Indanza","San Juan Bosco","Santiago","Suscal","Taisha","Yacuambi","Yanzatza","A.baquerizo Moreno","Balao","Balzar","Colimes","Crnl Marcelino Maridueñas","Daule","Duran","Guayaquil","Lomas De Sargentillo","Pedro Carbo","Samborondon","Santa Lucia","Simon Bolivar","Yaguachi","Baba","Calvas","Deleg","El Triunfo","Gonzanama","Isabela","Las Naves","Puyango","San Fernando","Sigsig","Arenillas","Bolivar","Espejo","La Troncal","Morona","Playas","Pueblo Viejo","San Cristobal","Zamora","Azogues","Babahoyo","Mocache","Pindal","Puerto Quito","Quininde","Rio Verde","Valencia","Ventanas","Biblian","Cuyabeno","Eloy Alfaro","Gral. A. Elizalde","Pablo Sexto","Pallatanga","Palora","San Lorenzo","Cascales","Logroño","Pelileo","Pujili","Quero","Saquisili","Shushufindi","Sigchos","Chone","El Carmen","Flavio Alfaro","Jipijapa","Montecristi","San Vicente","Chaguarpamba","Macara","Olmedo","Quilanga","Sozoranga","Cuenca","Guachapala","Nabon","Oña","Sevilla De Oro"]},"hovermode":"closest","showlegend":true},"source":"A","config":{"showSendToCloud":false},"data":[{"x":["Pueblo Viejo","Bolivar","San Cristobal","Morona","Arenillas","Espejo","Playas","Zamora","La Troncal"],"y":[12.41,11.06,9.63,9.6,6.73,5.72,3.71,2.75,0.87],"type":"bar","name":"CREO","marker":{"color":"rgba(212,164,122,1)","line":{"color":"rgba(212,164,122,1)"}},"textfont":{"color":"rgba(212,164,122,1)"},"error_y":{"color":"rgba(212,164,122,1)"},"error_x":{"color":"rgba(212,164,122,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["Sozoranga","Olmedo","Macara","Quilanga","Chaguarpamba"],"y":[30.59,12.67,8.45,5.59,1.74],"type":"bar","name":"CREO/MCUP/PAEA/FE/UE/MNP","marker":{"color":"rgba(238,150,107,1)","line":{"color":"rgba(238,150,107,1)"}},"textfont":{"color":"rgba(238,150,107,1)"},"error_y":{"color":"rgba(238,150,107,1)"},"error_x":{"color":"rgba(238,150,107,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["San Lorenzo","Gral. A. Elizalde","Cuyabeno","Pablo Sexto","Pallatanga","Palora","Biblian","Eloy Alfaro"],"y":[13.64,11.35,8.58,8.04,3.23,2.07,2.01,1.45],"type":"bar","name":"MDSI","marker":{"color":"rgba(164,156,201,1)","line":{"color":"rgba(164,156,201,1)"}},"textfont":{"color":"rgba(164,156,201,1)"},"error_y":{"color":"rgba(164,156,201,1)"},"error_x":{"color":"rgba(164,156,201,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["Sevilla De Oro","Nabon","Cuenca","Oña","Guachapala"],"y":[15.06,5.89,5.85,0.93,0.12],"type":"bar","name":"MEU/MDSI","marker":{"color":"rgba(192,150,199,1)","line":{"color":"rgba(192,150,199,1)"}},"textfont":{"color":"rgba(192,150,199,1)"},"error_y":{"color":"rgba(192,150,199,1)"},"error_x":{"color":"rgba(192,150,199,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["El Triunfo","Baba","Deleg","Sigsig","Puyango","Gonzanama","Isabela","San Fernando","Las Naves","Calvas"],"y":[25.19,20.75,15.56,9.65,9.46,9.23,5.92,5.11,1.68,0.56],"type":"bar","name":"MPAIS","marker":{"color":"rgba(191,196,121,1)","line":{"color":"rgba(191,196,121,1)"}},"textfont":{"color":"rgba(191,196,121,1)"},"error_y":{"color":"rgba(191,196,121,1)"},"error_x":{"color":"rgba(191,196,121,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["Aguarico","Taisha","Cayambe","Limon Indanza","Guaranda","Santiago","Cañar","Yanzatza","Yacuambi","El Pangui","Guamote","San Juan Bosco","Suscal","Huamboya","Gualaquiza"],"y":[33.45,30.76,18.76,16.64,13.47,8.3,7.79,6.64,6.06,5.63,5.38,5.38,4.01,3.74,1.58],"type":"bar","name":"MUPP","marker":{"color":"rgba(253,215,61,1)","line":{"color":"rgba(253,215,61,1)"}},"textfont":{"color":"rgba(253,215,61,1)"},"error_y":{"color":"rgba(253,215,61,1)"},"error_x":{"color":"rgba(253,215,61,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["Valencia","Puerto Quito","Ventanas","Pindal","Azogues","Rio Verde","Quininde","Babahoyo","Mocache"],"y":[21.84,19.72,12.46,9.07,3.95,2.74,1.77,1.38,0.88],"type":"bar","name":"PSC","marker":{"color":"rgba(237,201,128,1)","line":{"color":"rgba(237,201,128,1)"}},"textfont":{"color":"rgba(237,201,128,1)"},"error_y":{"color":"rgba(237,201,128,1)"},"error_x":{"color":"rgba(237,201,128,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["A.baquerizo Moreno","Samborondon","Daule","Guayaquil","Simon Bolivar","Pedro Carbo","Duran","Crnl Marcelino Maridueñas","Colimes","Balzar","Balao","Lomas De Sargentillo","Santa Lucia","Yaguachi"],"y":[37.29,26.06,22.05,20.8,18.13,13.18,10.36,8.37,6.83,5.92,5.82,5.21,4.96,2.93],"type":"bar","name":"PSC/MMDG","marker":{"color":"rgba(235,200,133,1)","line":{"color":"rgba(235,200,133,1)"}},"textfont":{"color":"rgba(235,200,133,1)"},"error_y":{"color":"rgba(235,200,133,1)"},"error_x":{"color":"rgba(235,200,133,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["San Vicente","Montecristi","Flavio Alfaro","Chone","El Carmen","Jipijapa"],"y":[16.7,13.61,12.32,10.45,5.18,4],"type":"bar","name":"PSC/MPUP","marker":{"color":"rgba(233,199,137,1)","line":{"color":"rgba(233,199,137,1)"}},"textfont":{"color":"rgba(233,199,137,1)"},"error_y":{"color":"rgba(233,199,137,1)"},"error_x":{"color":"rgba(233,199,137,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["Cascales","Saquisili","Pujili","Shushufindi","Pelileo","Logroño","Sigchos","Quero"],"y":[11.44,8.64,8.31,7.49,6.62,6.04,3.31,2.67],"type":"bar","name":"SUMA","marker":{"color":"rgba(208,188,162,1)","line":{"color":"rgba(208,188,162,1)"}},"textfont":{"color":"rgba(208,188,162,1)"},"error_y":{"color":"rgba(208,188,162,1)"},"error_x":{"color":"rgba(208,188,162,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

From the 89 victories from the top parties, ten had a difference over 20% of the votes with the second party. These cases would suggest a wide support from the electorate. However, by looking at individual cases, it is possible to identify how party performance is related to the number of candidates running for mayor at each city council.


```r
# Canton: A. Baquerizo Moreno
ABM <- arrange(resultados_alc[which(resultados_alc$CANTON_NOMBRE=='A.baquerizo Moreno'),], desc(VOTOS)) %>%
  select(-PROVINCIA_CODIGO, -PROVINCIA_NOMBRE, -CANTON_CODIGO)
ABM
```

```
## # A tibble: 9 x 3
##   CANTON_NOMBRE      CANDIDATO_CODIGO VOTOS
##   <fct>                         <dbl> <dbl>
## 1 A.baquerizo Moreno             3576  7793
## 2 A.baquerizo Moreno             6576  1981
## 3 A.baquerizo Moreno            30928  1937
## 4 A.baquerizo Moreno            34716  1492
## 5 A.baquerizo Moreno            22648  1157
## 6 A.baquerizo Moreno             9839   522
## 7 A.baquerizo Moreno            21276   418
## 8 A.baquerizo Moreno             9487   184
## 9 A.baquerizo Moreno            29092   103
```

```r
round((max(ABM$VOTOS)/sum(ABM$VOTOS))*100,3)
```

```
## [1] 49.997
```

```r
# Canton: Daule
DAU <- arrange(resultados_alc[which(resultados_alc$CANTON_NOMBRE=='Daule'),], desc(VOTOS)) %>%
  select(-PROVINCIA_CODIGO, -PROVINCIA_NOMBRE, -CANTON_CODIGO)
DAU
```

```
## # A tibble: 18 x 3
##    CANTON_NOMBRE CANDIDATO_CODIGO VOTOS
##    <fct>                    <dbl> <dbl>
##  1 Daule                     1162 28472
##  2 Daule                    21019 10686
##  3 Daule                    32938  7253
##  4 Daule                    12817  6000
##  5 Daule                    17590  4881
##  6 Daule                    20742  4847
##  7 Daule                     6387  3880
##  8 Daule                    34926  3105
##  9 Daule                     7717  2447
## 10 Daule                    24656  2337
## 11 Daule                     4718  1895
## 12 Daule                    20016  1390
## 13 Daule                    23086  1373
## 14 Daule                    38020   598
## 15 Daule                    29178   490
## 16 Daule                    33543   444
## 17 Daule                    36569   324
## 18 Daule                    24318   255
```

```r
round((max(DAU$VOTOS)/sum(DAU$VOTOS))*100,3)
```

```
## [1] 35.291
```

```r
# Canton: Duran
DUR <- arrange(resultados_alc[which(resultados_alc$CANTON_NOMBRE=='Duran'),], desc(VOTOS)) %>%
  select(-PROVINCIA_CODIGO, -PROVINCIA_NOMBRE, -CANTON_CODIGO)
DUR
```

```
## # A tibble: 21 x 3
##    CANTON_NOMBRE CANDIDATO_CODIGO VOTOS
##    <fct>                    <dbl> <dbl>
##  1 Duran                     3455 47621
##  2 Duran                     9466 33230
##  3 Duran                     4423 30987
##  4 Duran                    16883  7264
##  5 Duran                    11645  4931
##  6 Duran                    20092  2511
##  7 Duran                    32630  2142
##  8 Duran                    20490  1762
##  9 Duran                    21873  1513
## 10 Duran                    35634  1264
## # ... with 11 more rows
```

```r
round((max(DUR$VOTOS)/sum(DUR$VOTOS))*100,3)
```

```
## [1] 34.297
```

In the first case, the party that won in A. Baquerizo Moreno got 49.99% of the votes. Eight other parties were participating in this mayor race. However, in Daule, the winning party got 35.29% of the vote in an election with 18 candidates. Another canton, Duran, had 21 candidates, where the winner got little more than a third of the total votes (34,29%).

These few examples point to the existence of fragmentation in the political system. Moreover, the widespread use of alliances has not diminished the appearance of wide range of political organizations. In this case, the main issue with fragmentation has to do with the legitimacy of the party in power. Being elected with less than half of support in a scenario with more than ten political parties could potentially constrained the party performance once in power.

### Future Work

This is small exploratory analysis that could serve as a basis for further research. In this sense, it would be interesting to explore difference at geographical level to check if party fragmentation is concentrated over certain regions. The analysis only took into account the number of victories, but it would also be interesting to determine performance based on the number of votes, since this would also control for population size. The dataset has the number of votes as well. Finally, it would be possible to establish the likelihood of victory of each party, based on its participation in certain locations and measuring its performance compared to other parties.
