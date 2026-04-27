#' Build PNADc Panel
#'
#' This function builds a panel dataset from PNADC data, identifying households and individuals
#'
#' @param dat Data frame with PNADC data, sorted into a single panel.
#' @param panel A \code{character} with the type of panel identification. Use "none" for no paneling, "basic" for basic paneling, and "advanced" for advanced paneling.
#'
#' @return A modified dataset with added identifiers for household (\code{id_dom}) and individual (\code{id_ind} or \code{id_rs}) based on the chosen panel algorithm.
#'
#' @examplesIf interactive()
#' # Example usage:
#' 
#' panel_data <- build_pnadc_panel(dat = pnad_sample, panel = "basic")
#'
#' @export
build_pnadc_panel_rs_strict <- function(dat, panel = c("basic", "advanced")) {
  
  panel <- match.arg(panel)
  
  # ------------------------------------------------------------------
  # 1. ORDENAÇÃO INICIAL (igual Stata)
  # ------------------------------------------------------------------
  dat <- dat[order(dat$UF, dat$UPA, dat$V1008, dat$V1014,
                   dat$Ano, dat$Trimestre, dat$V2003), ]
  
  dat$row_id <- seq_len(nrow(dat))
  
  # ------------------------------------------------------------------
  # 2. ID DOMICÍLIO + n_p (REPLICA STATA)
  # ------------------------------------------------------------------
  dat$id_dom <- interaction(dat$UPA, dat$V1008, dat$V1014, drop = TRUE)
  dat$id_chefe <- interaction(dat$UPA, dat$V1008, dat$V1014, dat$V2005, drop = TRUE)
  dat$id_chefe[dat$V2005 != 1] <- NA
  
  dat <- dat[order(dat$id_chefe, dat$id_dom, dat$Ano, dat$Trimestre), ]
  
  dat$n_p_aux <- ave(dat$id_chefe, dat$id_chefe, FUN = seq_along)
  dat$n_p_aux[is.na(dat$id_chefe)] <- NA
  
  dat$n_p <- ave(dat$n_p_aux, dat$id_dom, dat$Ano, dat$Trimestre, FUN = function(x) mean(x, na.rm = TRUE))
  
  dat <- dat[order(dat$row_id), ]
  
  # ------------------------------------------------------------------
  # 3. INICIALIZAÇÃO
  # ------------------------------------------------------------------
  dat$p201 <- ifelse(dat$n_p == 1, as.numeric(dat$V2003), NA)
  dat$back <- 0
  dat$forw <- 0
  
  # ------------------------------------------------------------------
  # 4. LOOP TEMPORAL (1 → 4)
  # ------------------------------------------------------------------
  for (i in 1:4) {
    
    # ================================================================
    # 4A. MATCHING BÁSICO (SEQUENCIAL)
    # ================================================================
    
    dat <- dat[order(dat$UF, dat$UPA, dat$V1008, dat$V1014,
                     dat$V2007, dat$V2008, dat$V20081, dat$V20082,
                     dat$Ano, dat$Trimestre, dat$V2003), ]
    
    repeat {
      old <- dat$p201
      
      for (j in 1:nrow(dat)) {
        
        for (idx in which(dat$n_p == (i + 1) & is.na(dat$p201))) {
          
          prev <- idx - j
          if (prev < 1) next
          
          cond <- (
            dat$UF[idx] == dat$UF[prev] &&
            dat$UPA[idx] == dat$UPA[prev] &&
            dat$V1008[idx] == dat$V1008[prev] &&
            dat$V1014[idx] == dat$V1014[prev] &&
            dat$n_p[idx] == i + 1 &&
            dat$n_p[prev] == i &&
            is.na(dat$p201[idx]) &&
            dat$forw[prev] != 1 &&
            dat$V2007[idx] == dat$V2007[prev] &&
            dat$V2008[idx] == dat$V2008[prev] &&
            dat$V20081[idx] == dat$V20081[prev] &&
            dat$V20082[idx] == dat$V20082[prev] &&
            dat$V2008[idx] != 99 &&
            dat$V20081[idx] != 99 &&
            dat$V20082[idx] != 9999
          )
          
          if (cond) {
            dat$p201[idx] <- dat$p201[prev]
            dat$back[idx] <- 1
            dat$forw[prev] <- 1
          }
        }
      }
      
      if (identical(old, dat$p201)) break
    }
    
    # ================================================================
    # 4B. MATCHING AVANÇADO (PARTE ESSENCIAL RS)
    # ================================================================
    
    if (panel == "advanced") {
      
      # Loop de tolerância de idade (igual Stata)
      w_list <- list(0, 1, 2)
      
      for (w in w_list) {
        
        repeat {
          old <- dat$p201
          
          for (j in 1:nrow(dat)) {
            
            for (idx in which(dat$n_p == (i + 1) & is.na(dat$p201))) {
              
              prev <- idx - j
              if (prev < 1) next
              
              cond <- (
                dat$UF[idx] == dat$UF[prev] &&
                dat$UPA[idx] == dat$UPA[prev] &&
                dat$V1008[idx] == dat$V1008[prev] &&
                dat$V1014[idx] == dat$V1014[prev] &&
                dat$n_p[idx] == i + 1 &&
                dat$n_p[prev] == i &&
                is.na(dat$p201[idx]) &&
                dat$forw[prev] != 1 &&
                dat$V2007[idx] == dat$V2007[prev] &&
                abs(as.numeric(dat$V2009[idx]) - as.numeric(dat$V2009[prev])) <= w
              )
              
              if (cond) {
                dat$p201[idx] <- dat$p201[prev]
                dat$back[idx] <- 1
                dat$forw[prev] <- 1
              }
            }
          }
          
          if (identical(old, dat$p201)) break
        }
      }
    }
    
    # ================================================================
    # 4C. FALLBACK
    # ================================================================
    idx_new <- which(dat$n_p == (i + 1) & is.na(dat$p201))
    dat$p201[idx_new] <- (i * 100) + as.numeric(dat$V2003[idx_new])
  }
  
  # ------------------------------------------------------------------
  # 5. LOOP RETROSPECTIVO (SIMPLIFICADO MAS FIEL)
  # ------------------------------------------------------------------
  if (panel == "advanced") {
    
    for (i in 4:2) {
      
      repeat {
        old <- dat$p201
        
        for (j in 1:nrow(dat)) {
          
          for (idx in which(dat$p201 > (i * 100) & dat$back == 0)) {
            
            prev <- idx - j
            if (prev < 1) next
            
            cond <- (
              dat$UF[idx] == dat$UF[prev] &&
              dat$UPA[idx] == dat$UPA[prev] &&
              dat$V1008[idx] == dat$V1008[prev] &&
              dat$V1014[idx] == dat$V1014[prev] &&
              dat$V2007[idx] == dat$V2007[prev]
            )
            
            if (cond) {
              dat$p201[idx] <- dat$p201[prev]
              dat$back[idx] <- 1
              dat$forw[prev] <- 1
            }
          }
        }
        
        if (identical(old, dat$p201)) break
      }
    }
  }
  
  # ------------------------------------------------------------------
  # 6. ID FINAL
  # ------------------------------------------------------------------
  dat$id_ind <- ifelse(
    is.na(dat$p201),
    NA,
    paste0(dat$UPA, "_", dat$V1008, "_", dat$V1014, "_", dat$p201)
  )
  
  # limpeza
  dat <- dat[, !names(dat) %in% c("row_id", "back", "forw", "n_p_aux")]
  
  return(dat)
}
