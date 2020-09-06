library(tidyverse)
library(haven)




########### - Funções
########### 
gen_p201 <- function(df){
  df %>%
    arrange(Ano, Trimestre) %>%
    mutate(p201 = (first(na.omit(n_p)) - 1) * 100 + first(na.omit(V2003))) %>%
    group_by(V1014, UF, UPA, V1008, p201) %>%
    arrange(Ano, Trimestre) %>%
    mutate(
      back = ifelse(row_number() > 1 & !is.na(p201), 1, 0),
      forw = ifelse(row_number() < n() & !is.na(p201), 1, 0)
    )
}  

match0 <- function(i, df) {
  df <- as.data.frame(df)
  j <- 1:nrow(df)
  
  if (!is.na(df$p201[i])) {
    l <- i
  } else {
    
    ### Para lidar com o caso de gêmeos de mesmo sexo,
    ### lidar com 3 casos se p201 != NA:
    g <- 
      df$V2008[i] != 99 &
        df$V20081[i] != 99 &
        df$V20082[i] != 9999 
    
    k <-   
      df$Ano[i] == df$Ano[j] &
      df$Trimestre[i] == df$Trimestre[j] &
      i != j
    
    
    ### Se há gêmeos - mesmas características encontradas para observações
    ### no mesmo ano e trimestre - parear utilizando uma característica adicional,
    ### o número de ordem
    
    if (any(g) & !any(k)) {
      l <- which(g)[1]
      
    } else if(any(k)){
      k <- df$V2003[i] == df$V2003[j] &
        all(!(df$Ano[i] == df$Ano[j] &
                df$Trimestre[i] == df$Trimestre[j] &
                i != j)) | i == j
      l <- which(k)[1]
      
      } else {
        l <- i
      }
    }
  return(l)
}

match1 <- function(i, df) {
  df <- as.data.frame(df)
  j <- 1:nrow(df)
  
  
  if (!is.na(df$p201[i]) & (df$aux[i] == TRUE)) {
    l <- df$pairs0[i]
  } else {
    g <- (df$V2008[i] != 99 &
            df$V20081[i] != 99) |
      df$pairs0[i] == df$pairs0[j]
    
    g1 <- which(g)
    
    ##### Achando aqueles que j� haviam sido antes a g1 
    
    h <- which(df$pairs0[j] %in% df$pairs0[g1])
    
    #### Se k  == TRUE, i s� pode seria pareado a outros invidiv�duos
    #### no mesmo trimestre e ano que ele, ou a aqueles j� pareados
    #### a individuos desse tipo - poss�vel caso de g�meos de mesmo sexo
    
    k <- 
      all(df$Ano[i] == df$Ano[h] &
            df$Trimestre[i] == df$Trimestre[h] &
            i != h)
    
    if (any(g) & k == FALSE) {
      ##### �ndice que geraria problema por
      ##### pareamento ser de observa��es na mesma data
      
      k1 <- h[df$Ano[i] == df$Ano[h] &
                df$Trimestre[i] == df$Trimestre[h] &
                i != h]
      
      ###### Procurando por observa��es j� pareadas a k1 - 
      ###### n�o podemos parear nem a elas nem a k1
      
      k2 <- h[df$pairs0[h] %in% df$pairs0[k1]]
      
      ##### Potenciais pare�veis que n�o est�o em k2
      
      g2 <- setdiff(g1,k2) 
      
      ###### Exemplo: se algu�m (i1) em 1/2012 j� for pareado com algu�m em 2/2012, 
      ###### i em 2/2012 n�o pode for�ar i1 a se juntar a ele - crit�rio mais forte
      ###### tem prefer�ncia:
      
      ####### Procurando quem j� estava pareado antes a i:
      
      h1 <- which(df$pairs0[j] == df$pairs0[i] & i != j)
      
      ##### Elementos de g2 n�o podem ter mesmo ano e trimestre que os de h1
      
      g3 <- g2[!(df$Ano[g2] %in% df$Ano[h1] & 
                   df$Trimestre[g2] %in% df$Trimestre[h1])]
      
      ###### Para parear mais observações, escolher elemento de g3 que i não tenha
      ###### pareado em alguma etapa anterior, sempre que possível
      
      g4 <- setdiff(g3,c(h1,i)) %>%
        ifelse(length(.) > 0 , . , g3[1])
      
      
      
      
      l <- df$pairs0[g4[1]]
    } else if (k == TRUE) {
      k <- df$V2003[i] == df$V2003[j] &
        all(!(df$Ano[i] == df$Ano[j] &
                df$Trimestre[i] == df$Trimestre[j] &
                i != j)) | i == j
      
      l <- df$pairs0[which(k)[1]]
    } else {
      l <- df$pairs0[i]
    }
  }
  return(l)
}

match2 <- function(i, df) {
  j <- 1:nrow(df)
  if (!is.na(df$p201[i]) & (df$aux[i] == TRUE)) {
    l <- df$pairs1[i]
  } else {
    g <-
      #######
      (abs(df$V2009[i] - df$V2009[j]) <= df$ager[i] &
        df$V2009[i] != 999 &
        (
          (
            df$V2005[i] <= 3 & df$V2005[j] <= 3
          ) |
            (
              df$V2009[i] >= 25 & df$V2009[j] >= 25 & df$V2005[i] == 4 &
                df$V2005[j] == 4
            )
        ) &
        (
          (
            abs(df$V2008[i] - df$V2008[j]) <= 4 & abs(df$V20081[i] - df$V20081[j]) <= 2 &
              df$V2008[i] != 99 & df$V20081[i] != 99
          ) |
            (
              abs(df$VD3004[i] - df$VD3004[j]) <= 1 &
                (
                  (
                    abs(df$V20081[i] - df$V20081[j]) <= 2 & df$V20081[i] != 99 &
                      (df$V2008[i] == 99 | df$V2008[j] == 99)
                  ) |
                    (
                      abs(df$V2008[i] - df$V2008[j]) <= 4 & df$V2008[i] != 99 &
                        (
                          df$V20081[i] == 99 | df$V20081[j] == 99
                        )
                    ) |
                    (
                      (
                        df$V2008[i] == 99 | df$V2008[j] == 99) &
                        (df$V20081[i] == 99 | df$V20081[j] == 99)))))) |
        df$pairs1[i] == df$pairs1[j]

    #######
    #######
    g1 <- which(g)
    
    ##### Achando aqueles que j� haviam sido antes a g1 
    
      h <- which(df$pairs1[j] %in% df$pairs1[g1])
    
    #### Se k  == TRUE, i s� pode seria pareado a outros invidiv�duos
    #### no mesmo trimestre e ano que ele, ou a aqueles j� pareados
    #### a individuos desse tipo - poss�vel caso de g�meos de mesmo sexo
    
    k <- 
      all(df$Ano[i] == df$Ano[h] &
            df$Trimestre[i] == df$Trimestre[h] &
            i != h)
    
    if (any(g) & k == FALSE) {
      ##### �ndice que geraria problema por
      ##### pareamento ser de observa��es na mesma data
      
      k1 <- h[df$Ano[i] == df$Ano[h] &
                df$Trimestre[i] == df$Trimestre[h] &
                i != h]
      
      ###### Procurando por observa��es j� pareadas a k1 - 
      ###### n�o podemos parear nem a elas nem a k1
      
        k2 <- h[df$pairs1[h] %in% df$pairs1[k1]]
      
      ##### Potenciais pare�veis que n�o est�o em k2
      
      g2 <- setdiff(g1,k2) 
      
      ###### Exemplo: se algu�m (i1) em 1/2012 j� for pareado com algu�m em 2/2012, 
      ###### i em 2/2012 n�o pode for�ar i1 a se juntar a ele - crit�rio mais forte
      ###### tem prefer�ncia:
      
      ####### Procurando quem j� estava pareado antes a i:
      
      h1 <- which(df$pairs1[j] == df$pairs1[i] & i != j)
      
      ##### Elementos de g2 n�o podem ter mesmo ano e trimestre que os de h1
      
      g3 <- g2[!(df$Ano[g2] %in% df$Ano[h1] & 
                   df$Trimestre[g2] %in% df$Trimestre[h1])]
      
      ###### Para parear mais observações, escolher elemento de g3 que i não tenha
      ###### pareado em alguma etapa anterior sempre que possível
      
      g4 <- setdiff(g3,c(h1,i)) %>%
        ifelse(length(.) > 0 , . , g3[1])
      
      l <- df$pairs1[g4[1]]
    
      } else if (k == TRUE) {
      k <- df$V2003[i] == df$V2003[j] &
        all(!(df$Ano[i] == df$Ano[j] &
                df$Trimestre[i] == df$Trimestre[j] &
                i != j)) | i == j
      
        l <- df$pairs1[which(k)[1]]
    } else {
        l <- df$pairs1[i]
      }
    }
  return(l)
}

match3 <- function(i, df, m) {
  j <- 1:nrow(df)
  w <- list(0, 1, 2, df$ager2[i])

  df$aux[i] <- (df$forw[i] == 1 & (df$n_p[i] == 1 | df$back[i] == 1)) |
    (df$back[i] == 1 & df$n_p[i] == 5) | (df$dom[i] == 0 & df$n_p[i] == i + 1)

  if (!is.na(df$p201[i]) & (df$aux[i] == TRUE)) {
    if(m == 1){
      l <- df$pairs2[i]
    } else {
      l <- df$index[i]
    }
  } else {
    
     if(m == 1){
      f <- df$pairs2[i] == df$pairs2[j]
     } else {
      f <- df$index[i] == df$index[j]
    }
    
    
    ### Achando pareáveis
    ### 
    
    g <- (df$dom[i] > 0 & !is.na(df$dom[i]) &
      (
        abs(df$V2009[i] - df$V2009[j]) <= w[m] &
          df$V2009[i] != 999) |
      (
        (
          df$VD3004[i] == df$VD3004[j] &
            df$V2005[i] == df$V2005[j] &
            (
              df$V2009[i] == 999 | df$V2009[j] == 999
            )
        )
      )) | f
    
    g1 <- which(g)
    
    ##### Achando aqueles que já haviam sido antes a g1 
    
    if(m == 1){
      h <- which(df$pairs2[j] %in% df$pairs2[g1])
    } else{
      h <- which(df$index[j] %in% df$index[g1])
    }
    

    
    #### Se k  == TRUE, i só pode seria pareado a outros invidivíduos
    #### no mesmo trimestre e ano que ele, ou a aqueles já pareados
    #### a individuos desse tipo - possível caso de gêmeos de mesmo sexo
    
    k <- 
      all(df$Ano[i] == df$Ano[h] &
      df$Trimestre[i] == df$Trimestre[h] &
      i != h)
    
   
  
  if (any(g) & k == FALSE) {
    ##### Índice que geraria problema por
    ##### pareamento ser de observações na mesma data
    
    k1 <- h[df$Ano[i] == df$Ano[h] &
               df$Trimestre[i] == df$Trimestre[h] &
               i != h]
    
    ###### Procurando por observações já pareadas a k1 - 
    ###### não podemos parear nem a elas nem a k1
    
    if(m == 1){
      k2 <- h[df$pairs2[h] %in% df$pairs2[k1]]
    } else{
      k2 <- h[df$index[h] %in% df$index[k1]]
    }
  
    ##### Potenciais pareáveis que não estão em k2
    
    g2 <- setdiff(g1,k2) 
    
    ###### Exemplo: se alguém (i1) em 1/2012 já for pareado com alguém em 2/2012, 
    ###### i em 2/2012 não pode forçar i1 a se juntar a ele - critério mais forte
    ###### tem preferência:
    
    ####### Procurando quem já estava pareado antes a i:
    
    if(m == 1){
      h1 <- which(df$pairs2[j] == df$pairs2[i] & i != j)
    } else{
      h1 <- which(df$index[j] == df$index[i] & i != j)
    }
    
    ##### Elementos de g2 não podem ter mesmo ano e trimestre que os de h1
    
   g3 <- g2[!(df$Ano[g2] %in% df$Ano[h1] & 
                   df$Trimestre[g2] %in% df$Trimestre[h1])]
   
   ###### Para parear mais observações, escolher elemento de g3 que i não tenha
   ###### pareado em alguma etapa anterior
  
  # g4 <- setdiff(g3,c(h1,i)) %>%
   #  ifelse(length(.) > 0 , . , g3[1])
   
   if (m == 1) {
    
    l <- df$pairs2[g4[1]]
  } else {
    l <- df$index[g4[1]]
  }
} else if (k == TRUE) {
  k <- df$V2003[i] == df$V2003[j] &
    all(!(df$Ano[i] == df$Ano[j] &
            df$Trimestre[i] == df$Trimestre[j] &
            i != j)) | i == j
    

  if (m == 1) {
    l <- df$pairs2[which(k)[1]]
  } else {
    l <- df$index[which(k)[1]]
  }
} else {
  if(m == 1){
    l <- df$pairs2[i]
    } else {
    l <- df$index[i]
    }
}
    }
  return(l)
  }

loop <- function(data,
                 n = 1) {
  data <- data %>%
    mutate(index = map_dbl(seq(n()), ~ match3(.x, df = cbind(
      cur_group()[rep(1, n()), ],
      cur_data()),
      m = n
      )))
  if (n == 4) {
    return(data)
  } else {
    return(loop(data, n + 1))
  }
}

############

dados <- read_dta("C:/Users/arthu/Desktop/base.dta") 

gab_total <- read_dta("C:/Users/arthu/Desktop/gab_total.dta") %>%
  mutate(idind = ifelse(idind == "", NA, idind)) %>%
  arrange(UF, UPA, V1008, V1014, V2007, V2008, V20081, V20082) %>%
  mutate(
    UPA = str_pad(UPA, 9, pad = "0"),
    V1008 = str_pad(V1008, 3, pad = "0"),
    p201 = str_pad(p201, 3, pad = "0"),
    back = as.numeric(back),
    forw = as.numeric(forw)
  ) 

painel1 <- dados %>% 
  group_by(UF, UPA, V1008, V1014, V2007, V2008, V20081, V20082) %>%
  # etapa 1
  arrange(
    UF, UPA, V1008, V1014, V2007, V2008, V20081, V20082,
    Ano, Trimestre, V2003
  ) %>%
  mutate(index = map_dbl(seq(n()), ~ match0(.x, df = cbind(
    cur_group()[rep(1, n()), ],
    cur_data()
  )))) %>%
  ungroup()

painel_1.1 <- painel1 %>%
  bind_cols(pairs0 = group_indices(
    painel1, UF, UPA, V1008, V1014, V2007, V2008,
    V20081, V20082, index
  )) %>%
  group_by(pairs0) %>%
  gen_p201() %>%
  mutate(
    aux = (forw == 1 & (n_p == 1 | back == 1)) | (back == 1 & n_p == 5)
  ) %>%
  ungroup() %>%
  group_by(UF, UPA, V1008, V1014, V2008, V20081, V2003) %>%
  arrange(
    UF, UPA, V1008, V1014, V2007,
    V2008, V20081, V2003, Ano, Trimestre
  ) %>%
  mutate(index = map_dbl(seq(n()), ~ match1(.x, df = cbind(
    cur_group()[rep(1, n()), ],
    cur_data()
  )))) %>%
  ungroup()

painel_1.2 <- painel_1.1 %>%
  bind_cols(pairs1 = group_indices(painel_1.1, UF, UPA, V1008, V1014, V2008,
    V20081, V2003, index
  )) %>%
  group_by(pairs0) %>%
  mutate(pairs1 = min(pairs1)) %>%
  group_by(pairs1) %>%
  gen_p201()

painel2 <- painel_1.2 %>% 
  mutate(
    ager = ifelse(V2009 >= 25 & V2009 < 999, exp(V2009 / 30), 2),
    aux = (forw == 1 & (n_p == 1 | back == 1)) | (back == 1 & n_p == 5)
  ) %>%
  group_by(UF, UPA, V1008, V1014, V2007) %>%
  arrange(
    desc(aux), UF, UPA, V1008, V1014, V2007, Ano, Trimestre,
    V2009, VD3004, V2003
  ) %>% 
  mutate(index = map_dbl(seq(n()), ~ match2(.x, df = cbind(
    cur_group()[rep(1, n()), ],
    cur_data()
  )))) %>%
  ungroup()

painel_2.1 <- painel2 %>%
  bind_cols(pairs2 = group_indices(
    painel2, UF, UPA, V1008, V1014,
    V2007, index
  )) %>%
  group_by(pairs1) %>%
  mutate(pairs2 = min(pairs2)) %>%
  group_by(pairs2) %>%
  gen_p201()

  df <- painel_2.1 %>% 
  group_by(Ano, Trimestre, UF, UPA, V1008, V1014) %>%
  mutate(
    dom = sum(back),
    ager2 = ifelse(V2009 >= 25, 2 * ager, ager)
  ) %>%
  mutate(
    aux = (forw == 1 & (n_p == 1 | back == 1)) | (back == 1 & n_p == 5)
  ) %>%
  group_by(UF, UPA, V1008, V1014, V2007) %>%
  arrange(
    desc(aux), UF, UPA, V1008, V1014, V2007, Ano, Trimestre,
    V2009, VD3004, V2003
  ) %>% filter(id_dom == 1010 & V2009 %in% 7:8)
  loop() %>%
  ungroup()
  
  painel_2.3 <- painel_2.2 %>%
  bind_cols(pairs3 = group_indices(
    painel_2.2, UF, UPA, V1008, V1014,
    V2007, index
  )) %>%
  group_by(pairs2) %>%
  mutate(pairs3 = min(pairs3)) %>%
  group_by(pairs3) %>%
  gen_p201()
  

painel_final <- painel_2.3 %>%
  mutate_at(c("UF", "UPA", "V1008", "p201"), as.character) %>%
  mutate(
    UPA = str_pad(UPA, 9, pad = "0"),
    V1008 = str_pad(V1008, 3, pad = "0"),
    p201 = str_pad(p201, 3, pad = "0"),
    back = as.numeric(back),
    forw = as.numeric(forw)
  ) %>%
  mutate(idind = ifelse(is.na(p201),
                        NA,
                        paste0(V1014, UF, UPA, V1008, p201)
                        ))



painel_final <- painel_final %>%
  ungroup() %>%
  arrange(Ano, Trimestre, id_dom, id_chefe, V2003)

gab_total <- gab_total %>%
  ungroup() %>%
  arrange(Ano, Trimestre, id_dom, id_chefe, V2003)




comp <- data.frame(
  dom = painel_final$id_dom,
  idind = painel_final$idind,
  idind_gab = gab_total$idind,
  p201 = painel_final$p201
) %>%
  ungroup() %>%
  filter((idind != idind_gab | (is.na(idind) & !is.na(idind_gab)) |
            (!is.na(idind) & is.na(idind_gab)))) %>% 
  filter(!(dom %in% c(223,590, 643, 2015)))
  
  


  


