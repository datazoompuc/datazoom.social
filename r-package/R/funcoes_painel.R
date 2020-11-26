#' @importFrom dplyr %>%




#### Create auxiliary variables and p201 identifiers
create_p201 <- function(dados) {

  dados %>%
    dplyr::bind_cols(
      ### Adiciona identificadores
      id_dom = dplyr::group_indices(., UPA, V1008, V1014),
      id_chefe = dplyr::group_indices(., UPA, V1008, V1014, V2005)
    ) %>%
    dplyr::mutate(id_chefe = ifelse(V2005 != 1, NA, id_chefe)) %>%
    dplyr::group_by(id_chefe) %>%
    dplyr::mutate(n_p_aux = ifelse(V2005 != 1,
      NA,
      dplyr::row_number()
    )) %>%
    dplyr::group_by(id_dom, ANO, TRIMESTRE) %>%
    dplyr::mutate(n_p = mean(n_p_aux, na.rm = T)) %>%
    dplyr::ungroup() %>%
    ### Transforma NaN em NA's
    dplyr::mutate(n_p = ifelse(is.na(n_p), NA, n_p)) %>%
    dplyr::mutate(p201 = ifelse(n_p == 1, as.numeric(as.character(V2003)), NA))
}


### Prepare matches by removing for each observation units impossible to
### be matched: units already matched (same prev_id) or on the same date
### (same n_p)

prep_matches <- function(x, prev_id) {
  if (nrow(x) == 1) {
    tmp <- NA
  } else {
    id <- rlang::as_name(rlang::enquo(prev_id))
    j <- seq(nrow(x))

    id_eq <- outer(x[[id]], x[[id]], `==`)
    n_p_eq <- outer(x$n_p, x$n_p, `==`)



    tmp <- purrr::map(j, function(i) {
      if (all(id_eq[, i, drop = FALSE] == TRUE) |
        all(n_p_eq[, i, drop = FALSE] == TRUE)
      ) {
        out <- NA
      }
      else {
        id <- setdiff(j[id_eq[, i]], i)
        n_p <- setdiff(j[n_p_eq[, i]], i)
        rm_id <- which(rowSums(n_p_eq[, id, drop = FALSE]) > 0)
        rm_n_p <- which(rowSums(id_eq[, n_p, drop = FALSE]) > 0)

        out <- setdiff(j, dplyr::union_all(rm_id, rm_n_p, i))

        if (purrr::is_empty(out)) {
          out <- NA
        } else {
          out <- out
        }
      }
      return(out)
    })
  }
  return(tmp)
}

### Functions that match according to different criteria,
### match0 for basic panel, the rest for the advanced

match0 <- function(df, prev_id, j) {
  id <- rlang::as_name(rlang::enquo(prev_id))

  out <- purrr::map2(
    seq(nrow(df)), j,
    function(x, y) {
      if (!is.na(df$p201[x]) | all(is.na(y)) | purrr::is_empty(x)) {
        out <- df[[x, id]]
      } else {
        y <- y[!is.na(y)]
        g1 <- df$V2008[x] != 99 & df$V20081[x] != 99 &
          df$V20082[x] != 9999

        out <- ifelse(purrr::is_empty(g1) | all(g1 == FALSE), df[[x, id]],
          df[y[g1], ] %>%
            dplyr::pull({{ prev_id }}) %>%
            unique(.)
        )
      }
      return(out)
    }
  )
  return(out)
}
match1 <- function(df, prev_id, j) {
  id <- rlang::as_name(rlang::enquo(prev_id))

  V2008_99 <- df$V2008 == 99
  V20081_99 <- df$V20081 == 99
  forw <- df$forw == 1

  out <- purrr::map2(seq(nrow(df)), j, function(x, y) {
    if (!is.na(df$p201[x]) | df$aux[x] == TRUE |
      purrr::is_empty(y) | all(is.na(y)) | forw[x]) {
      out <- df[[x, id]]
    } else {
      y <- y[!is.na(y)]
      g1 <- (!V2008_99[y] & !V20081_99[x] & !forw[y])

      out <- ifelse(purrr::is_empty(g1) | all(g1 == FALSE), df[[x, id]],
        df[y[g1], ] %>%
          dplyr::pull({{ prev_id }}) %>%
          unique(.)
      )
    }
    return(out)
  })

  return(out)
}
match2 <- function(df, prev_id, j) {
  id <- rlang::as_name(rlang::enquo(prev_id))

  V2009_ager <- outer(
    df$V2009, df$V2009,
    function(x, y) {
      abs(x - y) <= df$ager
    }
  )
  V2009_999 <- df$V2009 == 999
  V2005_3 <- df$V2005 <= 3
  V2009_25 <- df$V2009 >= 25
  V2005_4 <- df$V2005 == 4

  abs_V2008_4 <- outer(
    df$V2008, df$V2008,
    function(x, y) {
      abs(x - y) <= 4
    }
  )

  abs_V20081_2 <- outer(
    df$V20081, df$V20081,
    function(x, y) {
      abs(x - y) <= 2
    }
  )

  V2008_99 <- df$V2008 == 99
  V20081_99 <- df$V20081 == 99

  abs_VD3004_1 <- outer(
    df$VD3004, df$VD3004,
    function(x, y) {
      abs(x - y) <= 1
    }
  )

  forw <- df$forw == 1

  out <- purrr::map2(seq(nrow(df)), j, function(x, y) {
    if (!is.na(df$p201[x]) |
      purrr::is_empty(y) | all(is.na(y)) | df$forw[x] == 1) {
      out <- df[[x, id]]
    } else {
      y <- y[!is.na(y)]
      g1 <-
        #######
        (V2009_ager[x, y] &
          !V2009_999[x] &
          (
            (V2005_3[x] & V2005_3[y]) |
              (V2009_25[x] & V2009_25[y] &
                V2005_4[x] & V2005_4[y]
              )
          ) &
          ((
            abs_V2008_4[x, y] &
              abs_V20081_2[x, y] &
              !V2008_99[x] &
              !V20081_99[x]
          ) |
            (abs_VD3004_1[x, y] &
              (
                (
                  abs_V20081_2[x, y] & !V20081_99[x] &
                    (V2008_99[x] | V2008_99[y])
                ) |
                  (
                    abs_V2008_4[x, y] & !V2008_99[x] &
                      (V20081_99[x] | V20081_99[y])
                  ) |
                  (
                    (V2008_99[x] | V2008_99[y]) &
                      (V20081_99[x] |
                        V20081_99[y])
                  )
              )))) & !forw[y]

      #########
      out <- ifelse(purrr::is_empty(g1) | all(g1 == FALSE),
        df[[x, id]],
        df[y[g1], ] %>% dplyr::pull({{ prev_id }}) %>% unique(.)
      )
    }
    return(out)
  })

  return(out)
}
match3 <- function(df, prev_id, j, m) {
  id <- rlang::as_name(rlang::enquo(prev_id))

  abs_V2009_m <- outer(
    df$V2009, df$V2009,
    function(x, y) {
      w <- c(0, 1, 2, df$ager2)
      abs(x - y) <= w[m]
    }
  )

  VD3004_equal <- outer(df$VD3004, df$VD3004, `==`)
  V2005_equal <- outer(df$V2005, df$V2005, `==`)
  V2009_999 <- df$V2009 == 999
  forw <- df$forw == 1

  out <- purrr::map2(seq(nrow(df)), j, function(x, y) {
    df$aux[x] <-
      (df$forw[x] == 1 & (df$n_p[x] == 1 | df$back[x] == 1)) |
        (df$back[x] == 1 & df$n_p[x] == 5) | df$dom[x] == 0

    if (!is.na(df$p201[x]) |
      !(df$dom[x] > 0 & !is.na(df$dom[x])) |
      purrr::is_empty(y) | all(is.na(y)) | forw[x] == 1) {
      out <- df[[x, id]]
    } else {
      y <- y[!is.na(y)]

      g1 <- ((abs_V2009_m[x, y] & !V2009_999[x]) |
        (VD3004_equal[x, y] & V2005_equal[x, y] &
          V2009_999[x] & V2009_999[y]) &
          !forw[y]
      )

      out <- ifelse(purrr::is_empty(g1) | all(g1 == FALSE),
        df[[x, id]],
        df[y[g1], ] %>%
          dplyr::pull({{ prev_id }}) %>%
          unique(.)
      )
    }
    return(out)
  })

  return(out)
}

### Deals with the case of within a matching process observations
### with the same n_p being matched to the same unit

remove_duplicates <- function(df, prev_id, new_id, matchables) {
  prev_id <- df %>%
    dplyr::select({{ prev_id }}) %>%
    dplyr::pull(.)

  j <- which(purrr::map_lgl({{ matchables }}, ~ all(!is.na(.))))

  x <- purrr::pmap(
    list({{ new_id }}, {{ matchables }}, seq(length({{ new_id }})), prev_id),
    function(x, y, w, z) {
      if (x != z) {
        h1 <- which({{ new_id }} %in% x)
        h1 <- h1[h1 != w]
        f <- purrr::map({{ matchables }}[h1], ~ !(w %in% .x)) %>% unlist()
        out <- dplyr::case_when(
          purrr::is_empty(h1) ~ x,
          all(w > h1[f]) ~ x,
          TRUE ~ z
        )
      } else {
        out <- x
      }
      return(out)
    }
  )
  return(x)
}


### Equalizes observations that are indirectly connected in the same matching
### process:
### if x is matched to y, and y to z, x should be matched to
### z as well

eq_index <- function(matches, df, prev_id) {
  z <- purrr::map2(
    matches, df,
    function(x, y) {
      if (length(x) == 1) {
        out <- y %>% dplyr::pull({{ prev_id }})
      } else {
        w <- purrr::map2(x, seq(length(x)), ~ c(
          .x,
          y %>% dplyr::slice(.y) %>%
            dplyr::pull({{ prev_id }})
        ) %>% unique(.))
        z <- find_connections(w)
        y$tmp <- z
        out <- y %>%
          dplyr::group_by(tmp) %>%
          dplyr::mutate(out = dplyr::first({{ prev_id }})) %>%
          dplyr::pull(out)
      }
      return(out)
    }
  )

  return(z)
}

find_connections <- function(x) {
  dt <- data.table::rbindlist(purrr::map(x, ~ .x %>%
    data.table::data.table(
      from = ., to = data.table::shift(., -1, fill = .[1])
    )))

  dg <- igraph::graph_from_data_frame(dt, directed = FALSE)

  purrr::map_dbl(x, ~ igraph::components(dg)$membership[as.character(.x[1])])
}

### Bind groups of rows together and then adds column with ids corresponding
### to new wave of matches

bind <- function(old_df, id, new_id) {
  a <- dplyr::bind_rows(old_df)
  b <- purrr::map(id, unlist) %>% unlist(.)

  dplyr::bind_cols(a, "{{new_id}}" := b)
}

### Connects observations that are indirectly matched by different
### matching functions

eq_index_across <- function(df, id1, id2, new_id) {
  df <- df %>% dplyr::mutate(from = paste0("id1", "_", {{ id1 }}), to = paste0("id2", "_", {{ id2 }}))
  dg <- igraph::graph_from_data_frame(df %>% dplyr::select(from, to), directed = FALSE)
  df <- df %>% dplyr::mutate("{{new_id}}" := igraph::components(dg)$membership[from])
  df %>% dplyr::select(-c(from, to))
}



update_back_forw <- function(df, id) {
  df %>%
    dplyr::group_by({{ id }}) %>%
    dplyr::arrange(ANO, TRIMESTRE) %>%
    dplyr::mutate(
      back = ifelse(dplyr::n() > 1 & n_p > min(n_p), 1, 0),
      forw = ifelse(dplyr::n() > 1 & n_p < max(n_p), 1, 0)
    )
}

### wrapper around a matching stage

wrapper <- function(df, prev_id, new_id, M, f) {
  painel <- df %>%
    dplyr::group_split()

  isItMatch3 <- deparse(substitute(f)) == "match3"

  painel <- purrr::map(painel, dplyr::ungroup)

  prep <- purrr::map(painel, ~ prep_matches(., prev_id = {{ prev_id }}))

  if (isItMatch3) {
    matches <- purrr::map2(painel, prep, ~ match3(
      df = .x, prev_id = {{ prev_id }},
      j = .y, m = M
    ))
  } else {
    matches <- purrr::map2(painel, prep, ~ f(
      df = .x, prev_id = {{ prev_id }},
      j = .y
    ))
  }

  matches_adjusted <- purrr::pmap(
    list(painel, matches, prep),
    function(x, y, z) {
      remove_duplicates(
        df = x,
        new_id = y,
        prev_id = {{ prev_id }},
        matchables = z
      )
    }
  )

  isItMatch0 <- deparse(substitute(f)) == "match0"

  if (isItMatch0) {
    panel_matched <- eq_index(matches_adjusted, painel, prev_id = {{ prev_id }}) %>%
      bind(painel, ., id_1)

    out <- panel_matched
  } else {
    panel_matched <- eq_index(matches_adjusted, painel, prev_id = {{ prev_id }}) %>%
      bind(painel, ., tmp)

    out <- eq_index_across(panel_matched, {{ prev_id }}, tmp, {{ new_id }}) %>%
      dplyr::select(-tmp)
  }

  return(out)
}


### same as wrapper, but considers the 4 cycles using
### function match3

wrapper3 <- function(df, N) {
  D <- df %>%
    dplyr::group_by(UF, UPA, V1008, V1014, V2007) %>%
    dplyr::arrange(
      desc(aux), UF, UPA, V1008, V1014, V2007, ANO, TRIMESTRE,
      V2009, VD3004, V2003
    )
  df <- wrapper(
    df = D, prev_id = id_1,
    new_id = id_1, f = match3, M = N
  )
  return(df)
}

create_idind <- function(df, id, is_basic = TRUE) {
  df <- df %>%
    dplyr::group_by({{ id }}) %>%
    dplyr::arrange(ANO, TRIMESTRE) %>%
    dplyr::mutate(p201 = (dplyr::first(na.omit(n_p)) - 1) * 100 +
                    dplyr::first(na.omit(as.numeric(as.character(V2003))))
           ) %>%
    dplyr::mutate(dplyr::across(c(UF, UPA, V1008, p201), as.character)) %>%
    dplyr::mutate(
      UPA = stringr::str_pad(UPA, 9, pad = "0"),
      V1008 = stringr::str_pad(V1008, 3, pad = "0"),
      p201 = stringr::str_pad(p201, 3, pad = "0"),
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(idind = paste0(V1014, UF, UPA, V1008, p201))

  if (is_basic) {
    df <- df %>%
      dplyr::mutate(idind = ifelse(is.na(p201) | V2008 == 99 |
        V20081 == 99 | V20082 == 9999,
      NA, idind
      ))
  }
  df %>% dplyr::select(-c(
    p201, id_dom, id_chefe, n_p,
    aux, ager, ager2, dom, {{ id }}
  ))
}


build_panel_basic_aux <- function(original_data, add_idind = TRUE,
                                  do_nothing = FALSE) {

  separated_data <- original_data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(id_0 = dplyr::row_number()) %>%
    dplyr::select(-c(
      ANO, TRIMESTRE, UF, UPA, V1008, V1014, V2003, V2005, V2007, V2008,
      V20081, V20082, V2009, VD3004
    ))

  if (do_nothing) {
    return(separated_data)
  }

  dados <- original_data %>%
    dplyr::mutate(id_0 = dplyr::row_number()) %>%
    dplyr::select(
      ANO, TRIMESTRE, UF, UPA, V1008, V1014, V2003, V2005, V2007, V2008,
      V20081, V20082, V2009, VD3004, id_0
    ) %>%
    create_p201()

  dados <- dados %>%
    dplyr::mutate(
      aux = NA,
      ager = NA,
      ager2 = NA,
      dom = NA
    )

  dados <- dados %>%
    dplyr::group_by(UF, UPA, V1008, V1014, V2007, V2008, V20081, V20082) %>%
    dplyr::arrange(
      UF, UPA, V1008, V1014, V2007, V2008, V20081, V20082,
      ANO, TRIMESTRE, V2003
    )

  painel_basico <- wrapper(
    df = dados, prev_id = id_0,
    new_id = id_1, f = match0
  )
  if (add_idind) {
    painel_basico <- create_idind(painel_basico, id_1, is_basic = TRUE)

    painel_basico <- dplyr::left_join(painel_basico, separated_data) %>%
      dplyr::select(-c(id_0, n_p_aux))
  }

  return(painel_basico)
}

build_panel_adv_aux <- function(original_data) {

  separated_data <- build_panel_basic_aux(original_data, do_nothing = TRUE)
  dados <- build_panel_basic_aux(original_data, add_idind = FALSE)

  dados <- dados %>%
    update_back_forw(id = id_1) %>%
    dplyr::mutate(
      aux = (forw == 1 & (n_p == 1 | back == 1)) | (back == 1 & n_p == 5)
    ) %>%
    dplyr::group_by(UF, UPA, V1008, V1014, V2008, V20081, V2003) %>%
    dplyr::arrange(
      aux, UF, UPA, V1008, V1014, V2007,
      V2008, V20081, V2003, ANO, TRIMESTRE
    )
  dados <- wrapper(
    df = dados, prev_id = id_1,
    new_id = id_1, f = match1
  )

  dados <- dados %>%
    update_back_forw(id = id_1) %>%
    dplyr::mutate(
      ager = ifelse(as.numeric(as.character(V2009)) >= 25 &
                      as.numeric(as.character(V2009)) < 999,
                    exp(as.numeric(as.character(V2009))/ 30), 2),
      ager2 = ifelse(as.numeric(as.character(V2009)) >= 25, 2 * ager, ager),
      aux = (forw == 1 & (n_p == 1 | back == 1)) | (back == 1 & n_p == 5)
    ) %>%
    dplyr::group_by(UF, UPA, V1008, V1014, V2007) %>%
    dplyr::arrange(
      desc(aux), UF, UPA, V1008, V1014, V2007, ANO, TRIMESTRE,
      V2009, VD3004, V2003
    )

  dados <- wrapper(
    df = dados, prev_id = id_1,
    new_id = id_1, f = match2
  )

  dados <- dados %>%
    dplyr::group_by(ANO, TRIMESTRE, UF, UPA, V1008, V1014) %>%
    dplyr::mutate(dom = sum(back)) %>%
    update_back_forw(id = id_1) %>%
    dplyr::mutate(aux = (forw == 1 & (n_p == 1 | back == 1)) | (back == 1 & n_p == 5))

  dados <- wrapper3(dados, N = 1) %>%
    wrapper3(N = 2) %>%
    wrapper3(N = 3) %>%
    wrapper3(N = 4)

  painel_avancado <- create_idind(dados, id_1, is_basic = FALSE)

  painel_avancado <- dplyr::left_join(painel_avancado, separated_data) %>%
    dplyr::select(-c(id_0, back, forw, n_p_aux))

  return(painel_avancado)
}

##########################


build_panel <- function(..., basic = TRUE) {
  database <- list(...)

  purrr::map(database, ~
  if (basic) {
    build_panel_basic_aux(original_data = ., add_idind = TRUE)
  } else {
    build_panel_adv_aux(original_data = .)
  })
}




