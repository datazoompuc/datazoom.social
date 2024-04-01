#' Load PNADc Data
#'
#' This function downloads PNADC data and applies panel identification algorithms
#'
#' @param save_to A \code{character} with the directory in which to save the downloaded files.
#' @param years A \code{numeric} indicating for which years the data will be loaded, in the format YYYY. Can be any vector of numbers, such as 2010:2012.
#' @param quarter The quarters within those years to be downloaded. Can be a numeric vector or a list of vectors, for different quarters per year.
#' @param panel A \code{character} choosing the panel algorithm to apply ("none", "basic", or "advanced").
#' @param raw_data A \code{logical} setting the return of raw (\code{TRUE}) or processed (\code{FALSE}) data.
#'
#' @return A message indicating the successful save of panel files.
#' @import PNADcIBGE
#' @import dplyr
#' @import purrr
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' load_pnadc(
#'   save_to = "Directory/You/Would/like/to/save/the/files",
#'   year = 2016,
#'   quarter = 1:4,
#'   panel = "basic",
#'   raw_data = FALSE
#' )
#' }
#' @export

load_pnadc <- function(save_to = getwd(), year,
                       quarter = 1:4, panel = "advanced",
                       raw_data = FALSE, language = "pt") {
  # Check if PNADcIBGE namespace is already attached
  if (!"PNADcIBGE" %in% loadedNamespaces()) {
    # If not attached, attach it
    attachNamespace("PNADcIBGE") # without this, an error appears
    # I believe this is a problem with the PNADcIBGE package
    # If you run PNADcIBGE::get_pnad(...) without library(PNADcIBGE)
    # you get the same error
  }

  ###########################
  ## Bind Global Variables ##
  ###########################

  . <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  # The param list contains the various objects that will be used as parameters for this function
  param <- list()
  param$year <- years # the years the user would like to download
  param$quarter <- quarter # the quarters within those years to be downloaded
  param$panel <- panel # which panel algorithm (none, basic or advanced) should be applied to this data, check our READ-ME for greater explanation
  param$raw_data <- raw_data # A command to define if the user would like to download the raw data from the IBGE website directly
  param$save_to <- save_to # the directory in which the user desires to save the files downloaded
  param$language <- language
  
  # Check if quarter is a list; if not, wrap it in a list and repeat it for each year
  if (!is.list(quarter)) {
    param$quarter <- rep(list(quarter), length(year))
  }

  # Calculate the lengths of quarters for each year
  n_quarters <- lapply(param$quarter, length)

  # Map2: Repeat each year based on the corresponding lengths in n_quarters, so we can have two parallel vectors of years and quarters to loop over
  param$year <- purrr::map2(
    year, n_quarters,
    function(year, n) {
      rep(year, n)
    }
  )

  # generaring these two paralell vectors of years and quarter to loop over

  param$year <- unlist(param$year)
  param$quarter <- unlist(param$quarter)

  ##################
  ## Loading data ##
  ##################

  # store info on all panels and column names

  panel_list <- c()
  cnames <- NULL

  # download to the saving directory

  source_files <- purrr::map2(
    param$year, param$quarter, # looping over the two parallel vector of years and quarters (this was previoulsy done in a "for" structure, but qwe optimized it)
    function(year, quarter) {
      base::message(
        paste0("Downloading PNADC ", year, " Q", quarter, "\n") # just generating a message so the user knows which file is being downloaded now
      )
      df <- get_pnadc(
        year = year, quarter = quarter, labels = FALSE, design = FALSE # downloading the file, design= FALSE returns to us just the dataframe with all variables in the PNADc
      )

      # turns everything into numeric
      df <- df %>%
        mutate(across(everything(), as.numeric))
      
      panel_list <<- c(panel_list, unique(df$V1014)) # registering, for every quarter, the panel's which the quarter's observations are included (every OBS is just included in one panel, but there should be OBS inserted in 2 to 3 panels for every quarter, check our READ-ME or the IBGE's website about the rotation scheme for PNADc surveys)
      #<<- stabilishing a variable inside the function that continues to exist outside the function, it is not just local to the function's current context
      cnames <<- names(df)

      file_path <- file.path(
        param$save_to, paste0("pnadc_", year, "_", quarter, ".rds") # defining the file's names to a certain format: year= 2022, quarter=3, file -> pnadc_2022_3.rds
      )
      
      # runs data cleaning if desired
      if (!param$raw_data) {
        df <- treat_pnadc(df)
      }
      
      # download each quarter to a separate file

      readr::write_rds(df, file_path, compress = "gz") # saving the file into the user's computer

      return(file_path)
    }
  )

  ## Return Raw Data

  if (param$raw_data) {
    return(paste("Raw Data saved to", param$save_to))
  }

  #################
  ## Panel Files ##
  #################

  if (param$panel != "none") {
    ## Split data into panels

    panel_list <- unique(panel_list) # listing all the panels included in the quarters downloaded

    # set up .csv file paths for each panel such as "pnadc_panel_2.csv"

    panel_files <- purrr::map(
      panel_list,
      function(panel) {
        file_path <- file.path(
          param$save_to, paste0("pnadc", "_panel_", panel, ".csv")
        )

        file_path
      }
    )

    # write an empty dataframe into each

    purrr::map(
      panel_files,
      function(path) {
        readr::write_csv(data.frame(), path, col_names = cnames)
      }
    )

    # read each of the source files, split into panels, and append
    # to their corresponding .csv files

    # we use the .csv files because they have a appending propriety, meaning that they can receive new information without having the older one deleted
    # for the R users, you can simply think as literally doing a rbind() into those files, but in a much more efficient way

    purrr::map(
      source_files, # source_files= the .rds files with the data that were downloaded way before in this function before
      function(file) {
        dat <- readr::read_rds(file) %>%
          split(.$V1014)

        dat %>%
          purrr::imap(
            function(df, panel) {
              file_path <- file.path(
                param$save_to, paste0("pnadc", "_panel_", panel, ".csv")
              )
              readr::write_csv(df, file_path, append = TRUE) # append=TRUE allows us to add new info without deleting the older one, as comented above
            }
          )
      }
    )

    ##########################
    ## Panel Identification ##
    ##########################

    # read each file in panel_files and apply the identification algorithms defined in the build_pnadc_panel.R

    purrr::map(
      panel_files,
      function(path) {
        df <- readr::read_csv(path, col_names = cnames) %>%
          build_pnadc_panel(panel = param$panel)

        readr::write_csv(df, path)
      }
    )
  }
  
  ####################
  ## Returning Data ##
  ####################

  return(paste("Panel files saved to", param$save_to))
}

######################
## Data Engineering ##
######################

# define a data cleaning function which is run for each quarter separately

treat_pnadc <- function(df) {
  
  # regions
  
  df <- df %>%
    dplyr::mutate(
      regiao = substr(UF, 1, 1),
      regiao = dplyr::case_match(
        regiao,
        "1" ~ "Norte",
        "2" ~ "Nordeste",
        "3" ~ "Sudeste",
        "4" ~ "Sul",
        "5" ~ "Centro-Oeste"
      )
    )
  
  # states
  
  df <- df %>%
    dplyr::mutate(
      sigla_uf = dplyr::case_match(
        UF,
        11 ~ "RO",
        12 ~ "AC",
        13 ~ "AM",
        14 ~ "RO",
        15 ~ "PA",
        16 ~ "AP",
        17 ~ "TO",
        21 ~ "MA",
        22 ~ "PI",
        23 ~ "CE",
        24 ~ "RN",
        25 ~ "PB",
        26 ~ "PE",
        27 ~ "AL",
        28 ~ "SE",
        29 ~ "BA",
        31 ~ "MG",
        32 ~ "ES",
        33 ~ "RJ",
        35 ~ "SP",
        41 ~ "PR",
        42 ~ "SC",
        43 ~ "RS",
        50 ~ "MS",
        51 ~ "MT",
        52 ~ "GO",
        53 ~ "DF"
      )
    )
  
  # sex
  
  df <- df %>%
    dplyr::mutate(
      sexo = dplyr::case_match(
        V2007,
        1 ~ "Homem",
        2 ~ "Mulher"
      )
    )
  
  # age groups
  
  df <- df %>%
    dplyr::mutate(
      faixa_idade = dplyr::case_when(
        V2009 >= 14 & V2009 <= 17 ~ "Entre 14 e 17 anos",
        V2009 >= 18 & V2009 <= 24 ~ "Entre 18 e 24 anos",
        V2009 >= 25 & V2009 <= 29 ~ "Entre 25 e 29 anos",
        V2009 >= 30 & V2009 <= 39 ~ "Entre 30 e 39 anos",
        V2009 >= 40 & V2009 <= 49 ~ "Entre 40 e 49 anos",
        V2009 >= 50 & V2009 <= 59 ~ "Entre 50 e 59 anos",
        V2009 >= 60 ~ "60 anos ou mais"
      )
    )
  
  # education levels
  
  df <- df %>%
    dplyr::mutate(
      faixa_educ = dplyr::case_match(
        VD3004,
        1 ~ "Sem instru\u00e7\u00a3o",
        2 ~ "1 a 7 anos de estudo",
        3 ~ "8 a 11 anos de estudo",
        4:6 ~ "9 a 14 anos de estudo",
        7 ~ "15 ou mais anos de estudo"
      )
    )
  
  # Labor Market definitions taken from:
  # https://github.com/datazoompuc/datazoom_labour_amazon/blob/main/Labour_Market/code/_definicoes_pnadcontinua_trimestral.do
  
  # habitual income from all occupations
  
  df <- df %>%
    dplyr::mutate(
      rendimento_habitual = VD4019,
      rendimento_habitual_real = VD4019 * Habitual
      )
  
  # occupied status
  
  df <- df %>%
    dplyr::mutate(
      ocupado = ifelse(VD4002 == 1, 1, 0),
      desocupado = ifelse(VD4002 == 2, 1, 0)
    )
  
  # formal vs. informal
  
  df <- df %>%
    dplyr::mutate(
      formal = dplyr::case_when(
        ocupado == 1 & VD4009 %in% c(1, 3, 5, 7) ~ 1,
        ocupado == 1 & VD4009 == 9 & VD4012 == 1 ~ 1,
        .default = 0
      ),
      informal = case_when(
        ocupado == 1 & VD4009 %in% c(2, 4, 6, 10) ~ 1,
        ocupado == 1 & VD4009 == 9 & VD4012 == 2 ~ 1,
        .default = 0
      )
    )
  
  # public or private sector
  
  df <- df %>%
    dplyr::mutate(
      publico = ifelse(V4012 %in% c(2,4), 1, 0),
      privado = ifelse(V4012 %in% c(1, 3, 5, 6, 7), 1, 0)
    )
  
  # labor force
  
  df <- df %>%
    dplyr::mutate(
      fora_forca_trab = ifelse(VD4001 == 2, 1, 0),
      forca_trab = ifelse(VD4001 == 1, 1, 0)
    )
  
  # active population
  
  df <- df %>%
    dplyr::mutate(
      pia = ifelse(V2009 >= 14, 1, 0),
      idade_de_trabalhar = ifelse(V2009 >= 15 & V2009 <= 64, 1, 0),
      pea = ocupado + desocupado
    )
  
  # unemployed
  
  df <- df %>%
    dplyr::mutate(
      desempregado = forca_trab * desocupado,
      desalentado = ifelse(VD4005 == 1, 1, 0)
    )
  
  # neet
  
  df <- df %>%
    dplyr::mutate(
      nem_nem = dplyr::case_when(
        desocupado == 1 & forca_trab == 1 & V3002 == 2 & 
          (V4074 != 6 | is.na(V4074)) & (V4074A != 8 | is.na(V4074A)) ~ 1,
        fora_forca_trab == 1 & V3002 == 2 &
          (V4074 != 6 | is.na(V4074)) & (V4074A != 8 | is.na(V4074A)) ~ 1,
        .default = 0
      )
    )
  
  # positions in occupation
  
  df <- df %>%
    dplyr::mutate(
      empregado_sc = ifelse(VD4009 %in% c(2, 4, 6, 10), 1, 0),
      empregado_cc = ifelse(VD4009 %in% c(1, 3, 5), 1, 0),
      conta_propria = ifelse(VD4009 == 9, 1, 0),
      conta_propria_contrib = ifelse(VD4009 == 9 & VD4012 == 1, 1, 0),
      conta_propria_nao_contrib = ifelse(VD4009 == 9 & VD4012 == 2, 1, 0),
      empregador = ifelse(VD4009 == 8, 1, 0),
      militar_estatutario = ifelse(VD4009 == 7, 1, 0),
      home_office = ifelse(V4022 %in% c(4, 5), 1, 0)
    )
  
  # translating sector codes
  
  df <- df %>%
    dplyr::mutate(
      cnae_2dig = substr(V4013, 1, 2),
      cnae_2dig = dplyr::case_match(
        cnae_2dig,
        '00' ~ "Outros",
        '01' ~ "Agricultura",
        '02' ~ "Extração florestal",
        '03' ~ "Pesca, caça e aquicultura",
        '05' ~ "Extração mineral e de carvão, petróleo e gás",
        '06' ~ "Extração mineral e de carvão, petróleo e gás",
        '07' ~ "Extração mineral e de carvão, petróleo e gás",
        '08' ~ "Extração mineral e de carvão, petróleo e gás",
        '09' ~ "Extração mineral e de carvão, petróleo e gás",
        '10' ~ "Alimentos, bebidas e fumo",
        '11' ~ "Alimentos, bebidas e fumo",
        '12' ~ "Pecuária e criação de animais",
        '13' ~ "Têxtil, vestuário, couro e calçados",
        '14' ~ "Pecuária e criação de animais",
        '15' ~ "Pesca, caça e aquicultura",
        '16' ~ "Madeira, celulose e papel",
        '17' ~ "Madeira, celulose e papel",
        '18' ~ "Madeira, celulose e papel",
        '19' ~ "Químicos, farmacêuticos, borracha e plástico",
        '20' ~ "Químicos, farmacêuticos, borracha e plástico",
        '21' ~ "Químicos, farmacêuticos, borracha e plástico",
        '22' ~ "Químicos, farmacêuticos, borracha e plástico",
        '23' ~ "Produtos de metal, minerais não-metálicos e metalurgia",
        '24' ~ "Produtos de metal, minerais não-metálicos e metalurgia",
        '25' ~ "Produtos de metal, minerais não-metálicos e metalurgia",
        '26' ~ "Serviços jurídicos",
        '27' ~ "Eletrônicos, máquinas e equipamentos",
        '28' ~ "Eletrônicos, máquinas e equipamentos",
        '29' ~ "Automóveis e equipamentos de transporte",
        '30' ~ "Automóveis e equipamentos de transporte",
        '31' ~ "Móveis",
        '32' ~ "Outros",
        '34' ~ "Serviços jurídicos",
        '33' ~ "Eletrônicos, máquinas e equipamentos",
        '35' ~ "Eletrônicos, máquinas e equipamentos",
        '36' ~ "Eletrônicos, máquinas e equipamentos",
        '37' ~ "Eletrônicos, máquinas e equipamentos",
        '38' ~ "Eletrônicos, máquinas e equipamentos",
        '39' ~ "Eletrônicos, máquinas e equipamentos",
        '41' ~ "Construção",
        '42' ~ "Construção",
        '43' ~ "Construção",
        '45' ~ "Comércio",
        '48' ~ "Comércio",
        '49' ~ "Transporte e correio",
        '50' ~ "Transporte e correio",
        '51' ~ "Transporte e correio",
        '52' ~ "Transporte e correio",
        '53' ~ "Transporte e correio",
        '55' ~ "Estadia e turismo",
        '56' ~ "Serviços de alimentação",
        '58' ~ "Serviços de informação e comunicação",
        '59' ~ "Serviços de informação e comunicação",
        '60' ~ "Serviços de informação e comunicação",
        '61' ~ "Serviços de informação e comunicação",
        '62' ~ "Serviços de informação e comunicação",
        '63' ~ "Serviços de informação e comunicação",
        '64' ~ "Serviços financeiros e de seguros",
        '65' ~ "Serviços financeiros e de seguros",
        '66' ~ "Serviços financeiros e de seguros",
        '68' ~ "Atividades profissionais, científicas e técnicas",
        '69' ~ "Atividades profissionais, científicas e técnicas",
        '70' ~ "Atividades profissionais, científicas e técnicas",
        '71' ~ "Atividades profissionais, científicas e técnicas",
        '72' ~ "Atividades profissionais, científicas e técnicas",
        '73' ~ "Atividades profissionais, científicas e técnicas",
        '74' ~ "Atividades profissionais, científicas e técnicas",
        '75' ~ "Atividades profissionais, científicas e técnicas",
        '78' ~ "Terceirização de mão-de-obra",
        '79' ~ "Estadia e turismo",
        '80' ~ "Segurança e edifícios",
        '81' ~ "Segurança e edifícios",
        '82' ~ "Segurança e edifícios",
        '84' ~ "Administração pública, defesa e seguridade social",
        '85' ~ "Educação",
        '86' ~ "Saúde e assistência social",
        '87' ~ "Saúde e assistência social",
        '88' ~ "Saúde e assistência social",
        '90' ~ "Artes, cultura, esportes e recreação",
        '91' ~ "Artes, cultura, esportes e recreação",
        '92' ~ "Artes, cultura, esportes e recreação",
        '93' ~ "Artes, cultura, esportes e recreação",
        '94' ~ "Organizações religiosas, sindicais e patronais",
        '95' ~ "Serviços de informação e comunicação",
        '96' ~ "Serviços pessoais (cabelereiros, lavanderias, etc.)",
        '97' ~ "Serviços domésticos",
        '99' ~ "Outros"
      ),
      cnae_2dig = dplyr::case_match(
        cnae_2dig,
        c(paste0(0, 1201:1209), paste0(0, 1402:1409), "01999") ~ "Pecuária e criação de animais"
      )
    )
  
  # translating occupation codes
  
  df <- df %>%
    dplyr::mutate(
      cod_2dig = substr(V4010, 1, 2),
      cod_2dig = dplyr::case_match(
        cod_2dig,
        '01' ~ "Policiais, bombeiros e forças armadas",
        '02' ~ "Policiais, bombeiros e forças armadas",
        '04' ~ "Policiais, bombeiros e forças armadas",
        '05' ~ "Policiais, bombeiros e forças armadas",
        '11' ~ "Trabalhadores no governo",
        '12' ~ "Dirigentes e gerentes",
        '13' ~ "Dirigentes e gerentes",
        '14' ~ "Dirigentes e gerentes",
        '21' ~ "Cientistas e engenheiros",
        '22' ~ "Profissionais da saúde",
        '23' ~ "Profissionais do ensino",
        '24' ~ "Administradores e especialista em gestão",
        '25' ~ "Serviços de TI e comunicação",
        '26' ~ "Serviços jurídicos",
        '31' ~ "Cientistas e engenheiros",
        '32' ~ "Profissionais da saúde",
        '33' ~ "Serviços financeiros e administrativos",
        '34' ~ "Serviços jurídicos",
        '35' ~ "Serviços de TI e comunicação",
        '41' ~ "Escriturários",
        '42' ~ "Atendimento direto ao público",
        '43' ~ "Apoio administrativo",
        '44' ~ "Apoio administrativo",
        '51' ~ "Serviços e cuidados pessoais",
        '52' ~ "Vendedores",
        '53' ~ "Serviços e cuidados pessoais",
        '54' ~ "Profissionais de segurança",
        '61' ~ "Pecuaristas e criadores de animais",
        '62' ~ "Pecuaristas e criadores de animais",
        '71' ~ "Operários da construção, metalurgia e indústria",
        '72' ~ "Operários da construção, metalurgia e indústria",
        '73' ~ "Artesões e artes gráficas",
        '74' ~ "Técnicos de eletricidade e eletrônica",
        '75' ~ "Operários de processamento e instalações",
        '81' ~ "Operários de processamento e instalações",
        '82' ~ "Montadores e condutores de veículos",
        '83' ~ "Montadores e condutores de veículos",
        '91' ~ "Domésticos",
        '92' ~ "Pecuaristas e criadores de animais",
        '93' ~ "Operários da construção, metalurgia e indústria",
        '94' ~ "Profissionais em alimentação",
        '95' ~ "Ambulantes",
        '96' ~ "Coletores de lixo"
      ),
      cod_2dig = ifelse(V4010 == 9215, "Extrativistas florestais", cod_2dig)
    )
    

  return(df)
}
