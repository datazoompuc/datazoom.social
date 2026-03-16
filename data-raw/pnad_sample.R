## code to prepare `pnad_sample` dataset goes here

## code to prepare the dataset

set.seed(123)

Ano <- 2022L
Trimestre <- 2L

# UF codes (as used in treat_pnadc mapping)
uf_levels <- c(11L, 12L, 13L, 14L, 15L, 16L, 17L,
               21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L,
               31L, 32L, 33L, 35L,
               41L, 42L, 43L,
               50L, 51L, 52L, 53L)

# create a handful of "households" (same keys -> same id_dom in build_pnadc_panel)
n_dom <- 8
dom <- data.frame(
  UF    = sample(uf_levels, n_dom, replace = TRUE),
  UPA   = sample(100000L:100200L, n_dom, replace = FALSE),
  V1008 = sample(1L:9999L, n_dom, replace = TRUE),
  V1014 = sample(1L:20L, n_dom, replace = TRUE),
  stringsAsFactors = FALSE
)

# build_pnadc_panel basic id_ind groups by:
#   (id_dom, V20082, V20081, V2008, V2007)
# So we keep those constant for the same person across repeated rows.

tracked_people <- data.frame(
  dom_id = c(1, 1, 3, 5, 5),     # which household they belong to
  V20082 = c(1990L, 1978L, 2002L, 1965L, 2010L),  # birth year
  V20081 = c(7L,    11L,   2L,    3L,    9L),     # birth month
  V2008  = c(15L,   4L,    20L,   9L,    1L),     # birth day
  V2007  = c(1L,    2L,    1L,    2L,    1L),     # sex
  stringsAsFactors = FALSE
)

# For each tracked person, choose which interviews (V1016) they appear in
# (some show up multiple times, not all 5)
tracked_appearances <- list(
  c(1L, 2L, 3L),     # person 1 appears 3 times
  c(1L, 3L, 5L),     # person 2 appears 3 times (skips 2 and 4)
  c(2L, 3L, 4L, 5L), # person 3 appears 4 times (skips 1)
  c(1L, 2L),         # person 4 appears 2 times
  c(4L)              # person 5 appears once (still "trackable", just not repeated)
)

tracked_rows <- do.call(
  rbind,
  lapply(seq_len(nrow(tracked_people)), function(i) {
    ap <- tracked_appearances[[i]]
    data.frame(
      Ano = Ano,
      Trimestre = Trimestre,
      UF = dom$UF[tracked_people$dom_id[i]],
      UPA = dom$UPA[tracked_people$dom_id[i]],
      V1008 = dom$V1008[tracked_people$dom_id[i]],
      V1014 = dom$V1014[tracked_people$dom_id[i]],
      V1016 = ap,  # interview number (1..5) used by attrition code; OK to include here too
      V20082 = tracked_people$V20082[i],
      V20081 = tracked_people$V20081[i],
      V2008  = tracked_people$V2008[i],
      V2007  = tracked_people$V2007[i],
      stringsAsFactors = FALSE
    )
  })
)

# Add some one-off individuals (noise)

n_noise <- 18

noise <- data.frame(
  Ano = Ano,
  Trimestre = Trimestre,
  dom_id = sample.int(n_dom, n_noise, replace = TRUE),
  V1016 = sample(1L:5L, n_noise, replace = TRUE),
  V20082 = sample(1950L:2015L, n_noise, replace = TRUE),
  V20081 = sample(1L:12L, n_noise, replace = TRUE),
  V2008  = sample(1L:28L, n_noise, replace = TRUE),
  V2007  = sample(c(1L, 2L), n_noise, replace = TRUE),
  stringsAsFactors = FALSE
)

noise$UF    <- dom$UF[noise$dom_id]
noise$UPA   <- dom$UPA[noise$dom_id]
noise$V1008 <- dom$V1008[noise$dom_id]
noise$V1014 <- dom$V1014[noise$dom_id]

noise <- noise[, c("Ano","Trimestre","UF","UPA","V1008","V1014","V1016","V20082","V20081","V2008","V2007")]


# Add minimal treat_pnadc vars (optional)

make_treat_vars <- function(n) {
  data.frame(
    V2009  = pmax(0L, Ano - sample(1950L:2015L, n, replace = TRUE)),
    VD3004 = sample(1L:10L, n, replace = TRUE),
    VD4001 = sample(c(1L, 2L, 3L), n, replace = TRUE),
    VD4002 = sample(c(1L, 2L), n, replace = TRUE),
    VD4005 = sample(c(1L, 2L, 3L, 4L), n, replace = TRUE),
    VD4009 = sample(c(1L, 2L, 3L, 4L, 5L), n, replace = TRUE),
    VD4019 = round(exp(rnorm(n, log(2500), 0.6)), 2),
    V4010  = sample(1111L:9999L, n, replace = TRUE),
    V4012  = sample(1L:7L, n, replace = TRUE),
    V4013  = sample(1L:99L, n, replace = TRUE),
    V4022  = sample(1L:999999L, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

pnad_sample <- rbind(tracked_rows, noise)

# attach optional vars expected by treat_pnadc()
pnad_sample <- cbind(pnad_sample, make_treat_vars(nrow(pnad_sample)))

# shuffle rows
pnad_sample <- pnad_sample[sample.int(nrow(pnad_sample)), ]
row.names(pnad_sample) <- NULL

usethis::use_data(pnad_sample, overwrite = TRUE)
