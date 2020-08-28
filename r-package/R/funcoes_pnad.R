#' @importFrom dplyr %>% arrange bind_cols filter group_by group_indices mutate mutate_at select summarise ungroup
#' @importFrom labelled set_variable_labels var_label
#' @importFrom purrr map
#' @importFrom readr read_tsv
#' @importFrom readxl read_excel cell_cols
#' @importFrom stringr str_pad
#' @importFrom tidyr fill separate

NULL



#' Carregando dados brutos
#'
#' @param diretorio_dados Diretório onde os microdados originais em formato de texto estão armazenados
#'
#' @param ... vetores com datas das pesquisas de interesse no  formato \code{c('trimestre', 'ano')}
#'
#' @return Lista de dataframes, sendo cada entrada um trimestre/ano em \code{...}
#' @encoding UTF-8
#' @export
#'
#' @examples
#' datazoom_pnadc("./Desktop", "./Desktop", c(1, 2000), c(2, 2000))
datazoom_pnadc <- function(diretorio_dados,
                           ...) {
  datas <- list(...)

  if (any(map(datas, length) != 2)) {
    stop("Escolha o mesmo número de anos e trimestres", call. = FALSE)
  }
  trimestre <- datas %>% map(~ .x[[1]])
  ano <- datas %>% map(~ .x[[2]])

  if (sum(trimestre %in% 1:4 == F) > 0) {
    stop("Escolha trimestres entre 1 e 4", call. = FALSE)
  }
  if (sum(ano %in% 2012:2020 == F) > 0) {
    stop("Escolha anos entre 2012 e 2020", call. = FALSE)
  }


  trimestre <- str_pad(trimestre, width = 2, pad = 0)
  key <- data.frame(trimano = paste0(trimestre, ano) %>% as.character())
  paths <- key %>% mutate(
    date = ifelse(
      trimyear %in% c("022019", "032019", "042019"),
      paste0("PNADC_", trimyear),
      paste0("PNADC_", trimyear, "_20190729")
    ),
    filepath_in = file.path(diretorio_dados, paste0(date, ".txt"), sep = "")
  )
  filepath_in <- paths$filepath_in

  last <- c(
    4, 5, 7, 9, 11, 20,
    27, 29, 31, 32, 33,
    34, 49, 64, 73, 76,
    78, 80, 82, 83, 85,
    87, 91, 94, 95, 96,
    97, 98, 100, 102, 103,
    104, 105, 107, 108, 109,
    110, 112, 114, 115, 116,
    117, 118, 120, 121, 122,
    123, 124, 125, 126, 127,
    128, 129, 130, 131, 132,
    134, 136, 138, 139, 143,
    144, 145, 150, 151, 152,
    153, 154, 155, 156, 158,
    159, 160, 162, 164, 165,
    166, 167, 168, 169, 171,
    173, 174, 175, 176, 177,
    178, 179, 180, 181, 182,
    183, 184, 185, 186, 187,
    195, 196, 197, 205, 206,
    207, 208, 209, 210, 218,
    219, 220, 228, 231, 234,
    235, 237, 239, 241, 245,
    246, 247, 252, 253, 254,
    255, 256, 257, 258, 259,
    260, 268, 269, 270, 278,
    279, 280, 281, 282, 283,
    291, 292, 293, 301, 304,
    307, 308, 309, 310, 311,
    319, 320, 321, 329, 330,
    331, 332, 333, 334, 335,
    343, 344, 345, 353, 356,
    359, 360, 361, 362, 363,
    364, 366, 367, 368, 369,
    371, 372, 374, 375, 377,
    379, 381, 382, 383, 384,
    385, 387, 389, 390, 391,
    393, 394, 395, 396, 397,
    398, 399, 400, 401, 402,
    404, 406, 408, 409, 410,
    411, 412, 420, 428, 429,
    437, 445, 446, 447, 450,
    453, 456, 459, 462, 463
  )

  vname <- c(
    "Ano", "Trimestre", "UF", "Capital", "RM_RIDE",
    "UPA", "Estrato", "V1008", "V1014", "V1016",
    "V1022", "V1023", "V1027", "V1028", "V1029",
    "posest", "V2001", "V2003", "V2005", "V2007",
    "V2008", "V20081", "V20082", "V2009", "V2010",
    "V3001", "V3002", "V3002A", "V3003A",
    "V3004", "V3005", "V3005A", "V3006", "V3006A",
    "V3007", "V3008", "V3009", "V3009A", "V3010",
    "V3011", "V3011A", "V3012", "V3013", "V3013A",
    "V3013B", "V3014", "V4001", "V4002", "V4003",
    "V4004", "V4005", "V4006", "V4006A", "V4007",
    "V4008", "V40081", "V40082", "V40083", "V4009",
    "V4010", "V4012", "V40121", "V4013", "V40132",
    "V40132A", "V4014", "V4015", "V40151", "V401511",
    "V401512", "V4016", "V40161", "V40162", "V40163",
    "V4017", "V40171", "V401711", "V4018", "V40181",
    "V40182", "V40183", "V4019", "V4020", "V4021",
    "V4022", "V4024", "V4025", "V4026", "V4027",
    "V4028", "V4029", "V4032", "V4033", "V40331",
    "V403311", "V403312", "V40332", "V403321", "V403322",
    "V40333", "V403331", "V4034", "V40341", "V403411",
    "V403412", "V40342", "V403421", "V403422", "V4039",
    "V4039C", "V4040", "V40401", "V40402", "V40403",
    "V4041", "V4043", "V40431", "V4044", "V4045",
    "V4046", "V4047", "V4048", "V4049", "V4050",
    "V40501", "V405011", "V405012", "V40502", "V405021",
    "V405022", "V40503", "V405031", "V4051", "V40511",
    "V405111", "V405112", "V40512", "V405121", "V405122",
    "V4056", "V4056C", "V4057", "V4058", "V40581",
    "V405811", "V405812", "V40582", "V405821", "V405822",
    "V40583", "V405831", "V40584", "V4059", "V40591",
    "V405911", "V405912", "V40592", "V405921", "V405922",
    "V4062", "V4062C", "V4063", "V4063A", "V4064",
    "V4064A", "V4071", "V4072", "V4072A", "V4073",
    "V4074", "V4074A", "V4075A", "V4075A1", "V4076",
    "V40761", "V40762", "V40763", "V4077", "V4078",
    "V4078A", "V4082", "VD2002", "VD2003", "VD2004",
    "VD3004", "VD3005", "VD3006", "VD4001", "VD4002",
    "VD4003", "VD4004", "VD4004A", "VD4005", "VD4007",
    "VD4008", "VD4009", "VD4010", "VD4011", "VD4012",
    "VD4013", "VD4014", "VD4015", "VD4016", "VD4017",
    "VD4018", "VD4019", "VD4020", "VD4023", "VD4030",
    "VD4031", "VD4032", "VD4033", "VD4034", "VD4035",
    "VD4036", "VD4037"
  )

  fatores <- c(
    "Ano", "Trimestre", "UF", "Capital", "RM_RIDE",
    "V1008", "V1014", "V1016", "V1022", "V1023",
    "posest", "V2005", "V2007", "V2008", "V20081",
    "V20082", "V2010", "V3001", "V3002", "V3002A",
    "V3003", "V3003A", "V3004", "V3005", "V3005A",
    "V3006", "V3006A", "V3007", "V3008", "V3009",
    "V3009A", "V3010", "V3011", "V3011A", "V3012",
    "V3013A", "V3013B", "V3014", "V4001", "V4002",
    "V4003", "V4004", "V4005", "V4006", "V4006A",
    "V4007", "V4008", "V4009", "V4010", "V4012",
    "V40121", "V4013", "V40132", "V40132A", "V4014",
    "V4015", "V40151", "V4016", "V4017", "V40171",
    "V4018", "V4019", "V4020", "V4021", "V4022",
    "V4024", "V4025", "V4026", "V4027", "V4028",
    "V4029", "V4032", "V4033", "V40331", "V40331",
    "V40332", "V403321", "V40333", "V403331", "V4034",
    "V40341", "V403411", "V40342", "V403421", "V4040",
    "V40403", "V4043", "V40431", "V4044", "V4045",
    "V4046", "V4047", "V4048", "V4049", "V4050",
    "V40501", "V405011", "V40502", "V405021", "V40503",
    "V405031", "V4051", "V40511", "V405111", "V40512",
    "V405121", "V4057", "V4058", "V40581", "V405811",
    "V40582", "V405821", "V40583", "V405831", "V40584",
    "V4059", "V40591", "V405911", "V40592", "V405921",
    "V4063", "V4063A", "V4064", "V4064A", "V4071",
    "V4072", "V4072A", "V4073", "V4074", "V4074A",
    "V4075A", "V4076", "V4077", "V4078", "V4078A",
    "V4082", "VD2002", "VD2004", "VD3004", "VD3005",
    "VD3006", "VD4001", "VD4002", "VD4003", "VD4004",
    "VD4004A", "VD4005", "VD4007", "VD4008", "VD4009",
    "VD4010", "VD4011", "VD4012", "VD4013", "VD4014",
    "VD4015", "VD4018", "VD4023", "VD4030", "VD4036",
    "VD4037"
  )

  caracteres <- c("UPA", "Estrato", "V4041")
  numerico <- c(
    "V1027", "V1028", "V1029", "V2001", "V2003",
    "V2009", "V3013", "V40081", "V40082", "V40083",
    "V401511", "V401512", "V40161", "V40162", "V40163",
    "V401711", "V40181", "V40182", "V40183", "V403312",
    "V403322", "V403412", "V403422", "V4039", "V4039C",
    "V40401", "V40402", "V405012", "V405022", "V405112",
    "V405122", "V4056", "V4056C", "V405812", "V405822",
    "V405912", "V405922", "V4062", "V4062C", "V4075A1",
    "V40761", "V40762", "V40763", "VD2003", "VD4016",
    "VD4017", "VD4019", "VD4020", "VD4031", "VD4032",
    "VD4033", "VD4034", "VD4035"
  )

  Descricoes <- data.frame(
    var = vname,
    descricao_pt =
      c(
        "Ano de referência", "Trimestre de referência",
        "Unidade da Federação", "Município da Capital",
        "Reg. Metr. e Reg. Adm. Int. Des.", "Unidade Primária de Amostragem",
        "Estrato", "Número de seleção do domicílio", "Painel",
        "Número da entrevista no domicílio", "Tipo de área",
        "Peso SEM pós estratificação", "Peso COM pós estratificação",
        "Projeção da população", "Domínios de projeção",
        "Número de pessoas no domicílio", "Número de ordem",
        "Condição no domicílio", "Sexo",
        "Dia de nascimento", "Mês de nascimento", "Ano de nascimento",
        "Idade na data de referência", "Cor ou raça",
        "Sabe ler e escrever", "Frequenta escola",
        "A escola que ... frequenta é de",
        "Qual é o curso que frequenta", "Duração deste curso que requenta",
        "Curso que frequenta é seriado", "Curso que freq é organizado em:",
        "Qual é o ano/série que frequenta", "Qual é a etapa que frequenta",
        "Concluiu outro curso de graduação",
        "Anteriormente frequentou escola",
        "Curso mais elevado que frequentou",
        "Curso mais elevado que frequentou",
        "Duração do curso que frequentou",
        "Curso que frequentou era seriado",
        "Curso que freq é organizado em:",
        "Aprovado na prim. série do curso",
        "Último ano/série que concluiu",
        "Qual é a etapa que frequentou",
        "Cursou os anos iniciais deste curso",
        "Concluiu o curso que frequentou",
        "Trabalhou 1 hr em ativ. remunerd.",
        "Trabalhou 1 hr em produtos etc...",
        "Fez algum bico pelo menos de 1 hr",
        "Ajudou sem receber no domic. 1 hr",
        "Afastado de trabalho remunerado",
        "Motivo de estar afastado",
        "Motivo de estar afastado",
        "Durante afastamento recebia pagam.",
        "Quanto tempo que estava afastado",
        "Tempo de afastamenento até 1 ano",
        "Tempo de afastamen. de 1 a 2 anos",
        "Tempo de afastamen. mais de 2 anos",
        "Quantos trabalhos tinha na semana",
        "Ocupação no trabalho principal",
        "Posição na ocupação",
        "Tipo trabalhador não remunerado",
        "Atividade no trab. principal",
        "Seção da atividade",
        "Seção da atividade",
        "Esse trabalho era na área",
        "Teve ajuda de pelo menos um trabalhador não remunerado",
        "Qnts trabalhadores não remunerados",
        "1 a 5 trabalhadores não remunerados",
        "6 a 10 trabalhadores não remunerados",
        "Qnts empregados trabalhavam nesse negócio/empresa",
        "1 a 5 empregados",
        "6 a 10 empregados",
        "11 a 50 empregados",
        "Tinha pelo menos um sócio que trab. nesse negócio/empresa",
        "Quantos sócios",
        "1 a 5 sócios",
        "Qnts pessoas trabalhavam nesse negócio/empresa",
        "1 a 5 pessoas",
        "6 a 10 pessoas",
        "11 a 50 pessoas",
        "Negócio/empresa registrado no CNPJ",
        "Em que tipo de local funcionava esse negócio/empresa",
        "Exercia o trabalho em estabelecimento desse negócio/empresa",
        "Onde exercia normalmente esse serviço",
        "Serv. domést. em mais de 1 domic.",
        "Contratado como empregado temporário",
        "Era contratado somente por pessoa responsável pelo negócio",
        "Era contratado somente por intermediário",
        "Servidor público estatutário",
        "Carteira de trabalho assinada",
        "Contribuinte de instit. d previd.",
        "Rendimento habitual var. auxil.",
        "Rendimento habitual em dinheiro",
        "Faixa do valor do rendimento hab.",
        "Valor do rend. hab. em dinheiro",
        "Rendimento habitual em produtos",
        "Faixa do valor do rendimento hab.",
        "Valor do rend. hab. em produtos",
        "Rendimento habitual em benefícios",
        "Tipo rend. habitual em benefícios",
        "Rendimento efetivo var. auxil.",
        "Rendimento efetivo em dinheiro",
        "Faixa do valor do rendimento efe.",
        "Valor do rend. efe. em dinheiro",
        "Rendimento efetivo em produtos",
        "Faixa do valor do rendimento efe.",
        "Valor do rend. efe. em produtos",
        "Hrs habituais no trab. princ.",
        "Hrs efetivas no trab. princ.",
        "Tempo que estava nesse trabalho",
        "De 1 mês a menos de 1 ano",
        "De 1 ano a menos de 2 anos",
        "De 2 anos ou mais tempo",
        "Ocupação no trab. secundário",
        "Posição na ocupação",
        "Tipo trabalhador não remunerado",
        "Atividade no trab. secundário",
        "Esse trabalho era na área",
        "Negócio/empresa registrado no CNPJ",
        "Servidor público estatutário",
        "Carteira de trabalho assinada",
        "Contribuinte de instit. d previd.",
        "Rendimento habitual var. auxil.",
        "Rendimento habitual em dinheiro",
        "Faixa do valor do rendimento hab.",
        "Valor do rend. hab. em dinheiro",
        "Rendimento habitual em produtos",
        "Faixa do valor do rendimento hab.",
        "Valor do rend. hab. em produtos",
        "Rendimento habitual em benefícios",
        "Tipo rend. habitual em benefícios",
        "Rendimento efetivo var. auxil.",
        "Rendimento efetivo em dinheiro",
        "Faixa do valor do rendimento efe.",
        "Valor do rend. efe. em dinheiro",
        "Rendimento efetivo em produtos",
        "Faixa do valor do rendimento efe.",
        "Valor do rend. efe. em produtos",
        "Hrs habituais no trab. secun.",
        "Hrs efetivas no trab. secun.",
        "Contribuinte de instit. d previd.",
        "Rendimento habitual var. auxil.",
        "Rendimento habitual em dinheiro",
        "Faixa do valor do rendimento hab.",
        "Valor do rend. hab. em dinheiro",
        "Rendimento habitual em produtos",
        "Faixa do valor do rendimento hab.",
        "Valor do rend. hab. em produtos",
        "Rendimento habitual em benefícios",
        "Tipo rend. habitual em benefícios",
        "Não remunerado",
        "Rendimento efetivo var. auxil.",
        "Rendimento efetivo em dinheiro",
        "Faixa do valor do rendimento efe.",
        "Valor do rend. efe. em dinheiro",
        "Rendimento efetivo em produtos",
        "Faixa do valor do rendimento efe.",
        "Valor do rend. efe. em produtos",
        "Hrs habituais no(s) outro(s) trab.",
        "Hrs efetivas no(s) outro(s) trab.",
        "Gostaria trabalhar + hrs efetivas",
        "Gostaria trabalhar + hrs habituais",
        "Dispon. trabalhar + hrs efetivas",
        "Dispon. trabalhar + hrs habituais",
        "Providência p/ conseg. trab. (30d)",
        "Principal provid. p/conseg. trab",
        "Principal provid. p/conseg. trab.",
        "Gostaria de ter trabalhado",
        "Motivo de não ter tomado provid.",
        "Motivo de não ter tomado provid.",
        "Tempo em que irá começar o trab.",
        "Meses em que irá começar o trab.",
        "Tempo tentando conseguir trabalho",
        "Tempo tentando trab. 1 mes-1 ano",
        "Tempo tentando trab. 1 ano-2 anos",
        "Tempo tentando trab. + de 2 anos",
        "Poderia ter começado a trabalhar",
        "Motivo p/ñ querer/começar a trab.",
        "Motivo p/ñ querer/começar a trab.",
        "Trab por pelo menos 1 hora em 1 ano",
        "Condição no domicílio",
        "Número de componentes do domic.",
        "Espécie da unidade doméstica",
        "Nível de instrução mais elevado alcançado
                  (5 anos ou mais de idade)",
        "Anos de estudo (5 anos ou mais de idade) para fundamental
                  de 9 anos",
        "Grupamento de anos de estudo (pessoas de 5 anos ou mais de idade)
                  para fundamental de 9 anos",
        "Condição em relação a força d trab.",
        "Condição de ocupação",
        "Força de trabalho potencial",
        "Subocupação por insuficiên. de hrs efet",
        "Subocupação por insuficiên. de hrs hab",
        "Pessoas desalentadas",
        "Posição na ocupação trab. princ.",
        "Posição na ocupação trab. princ.",
        "Posição na ocupação trab. princ.",
        "Grupamen. d ativid. trab. princ.",
        "Grupamen. ocupacion. trab. princ.",
        "Contrib. instit. previd. qq trab.",
        "Faixa hrs habituais em todos trab.",
        "Faixa hrs efetivas em todos trab.",
        "Tipo d remuneração trab. princ.",
        "Rendim. habitual trab. princ.",
        "Rendim. efetivo trab. princ.",
        "Tipo d remuneração em qq trabalho",
        "Rendim. habitual qq trabalho",
        "Rendim. efetivo qq trabalho",
        "Pq ñ proc./ñ gost./ñ disp.p/trab.",
        "Pq ñ proc./ñ gost./ñ disp.p/trab.",
        "Hrs habituais em todos trab.",
        "Hrs efetivas no trab. princ.",
        "Hrs efetivas no trab. secun.",
        "Hrs efetivas no(s) outro(s) trab.",
        "Hrs efetivas em todos trab.",
        "Faixa hrs habituais trab. princ.",
        "Faixa hrs efetivas trab. princ."
      ),
    descricao_en =
      c(
        "Year",
        "Quarter",
        "State",
        "Capital",
        "Metropolitan Region",
        "District borders",
        "Stratum",
        "Household selection number",
        "Panel",
        "household interview number",
        "Area type",
        "Weight without after stratification",
        "Weight with after stratification",
        "Population projecting",
        "Projecting domain",
        "Number of people in household",
        "Order number",
        "Role in the household",
        "Gender",
        "Day of birth",
        "Month of birth",
        "Year of birth",
        "Resident's age in the ref. date",
        "Skin color or race",
        "Knows how to read and write",
        "Attend school",
        "School is public or private?",
        " Type of course attended (education)",
        "Course lenght",
        "The course is divided into",
        "Year/Grade which is attended by the surveyed",
        "Finished another graduation course",
        "Attended school before",
        "Highest level of education later completed",
        "Lenght of the last course attended by the surveyed",
        "The course was divided into",
        "Finished at least the first grade",
        "Educational attainment",
        "Finished the course",
        "Worked/Was a intern in paid activity in cash",
        "Worked/Was a intern in activity remunerated in goods",
        "Occasional remunerated job (at least 1 hr)",
        "Worked without remuneration for household",
        "Was temporarily absent from its remunerated work",
        "Motives for being temporarily absent from work",
        "Period of absence from the workplace",
        "Time from absence in the workplace (1 to 11 months)",
        "Time of absence in the workplace (12 to 23 months)",
        "Time of absence in the workplace (2 years or more)",
        "# of jobs in the week of ref.",
        "Occupation code",
        "Occupation type",
        "Unpaid worker",
        "Code of the main activity from this business",
        "Section of economic activity",
        "this job was in the area",
        "business registered in CNPJ",
        "worked in household services for more than 1 hh",
        "hired as temporary worker",
        "Public servant",
        "Formally employed",
        "Contributed to a pension plan",
        "Monthly gross income (norm. received) (auxiliary)",
        "normally received income in cash ",
        "Income (in cash) bracket (norm. rec.)",
        "Gross income (in cash) (norm. rec.)",
        " normally received income in goods",
        "Income (in goods) bracket (norm. rec.)",
        "Monthly gross income (in goods) (norm. rec.)",
        "Norm. rec. payment only in the form of benefits",
        " type of remuneration in benefits ",
        "Gross income in the month of ref. (auxiliary)",
        "Received income in cash in the month of ref",
        "Income (in cash) bracket (in the month of ref)",
        "Gross income (in cash) (in month of ref)",
        "Received income (in goods) in the month of ref",
        "Income (in goods) bracket in the month of ref",
        "Gross income (in goods) in the month of ref",
        "Usual working hours (main job)",
        "Effective working hours (main job)",
        "Lenght of time that you are in this main job",
        "From 1 month to less than a year",
        "From 1 year to 2 years",
        "2 years or more",
        "Occupation in the 2nd job",
        "Occupation type",
        "Non-paid worker",
        "Activity in the 2nd job",
        "This job was in the area",
        "business registered in CNPJ",
        "Public servant",
        "Formally employed",
        "Contributed to a pension plan",
        "Monthly gross income (2nd job)",
        "Norm. received income (in cash) (2nd job)",
        "Income (in cash) bracket (2nd job)",
        "Monthly income in cash (2nd job)",
        "Received income in goods (2nd job)",
        "Income (in goods) bracket (2nd job)",
        "Monthly gross income (in goods)(2nd job",
        "Norm. received remuneration only in benefits",
        "type of remuneration in benefits ",
        "Gross income in month of ref.(2nd job)(auxil.)",
        "Received income in cash in the month of ref",
        "Income (in cash) bracket in the month of ref",
        "Income (in cash) in the month of ref",
        "Received income (in goods) in the month of ref",
        "Income (in goods) bracket in the month of ref",
        "Gross income (in goods) in the month of ref",
        "Usual working hours (2nd job)",
        "Effective working hours (2nd job)",
        "Contributed to a pension plan",
        "Monthly gross income (norm. received other jobs)",
        "Norm. received income in cash (other jobs)",
        "Income (in cash) bracket (norm.rec.-other jobs)",
        "Monthly income in cash (norm. rec. - other jobs)",
        "Received income in goods (normally-other jobs)",
        "Income (in goods) bracket (other jobs)",
        "Monthly gross income in goods (norm. rec. - other jobs)",
        "Received remuneration only in benefits",
        "type of remuneration in benefits ",
        "Unpaid in other jobs",
        "Gross income in month of ref. (other jobs)(aux.)",
        "Received income (in cash) in the month of ref",
        "Income (in cash) bracket in the month of ref",
        "Gross income (in cash) in the month of ref",
        "Received income in  goods in the month of ref",
        "Income (in goods) bracket in the month of ref",
        "Gross income in goods in the month of ref",
        "Usual working hours (other jobs)",
        "Effective working hours (other jobs)",
        "Willing to have more usual working hours",
        "Able to have more usual working hours",
        "Made an effort to get a job (30d)",
        "Main measure taken to get a new job",
        "Was willing to work (no efforts to work tough)",
        "Motives for not start seeking for work",
        " how much time to start working in the new job",
        " # of months to start working in new job",
        "lenght of time trying to get a job",
        "time trying to get a job 1 month to 1 year",
        "time trying to get a job 1 year to 2 years",
        "time trying to get a job more than 2 years",
        "Able to start working in the week of ref",
        "Motives for not be willing to work in the week of ref",
        "# of individuals in the household",
        "Highest level of instruction completed (Work)",
        "Condition in the workforce",
        "Occupation's condition",
        "Potential workforce",
        "Sub-occupation due to lack of hours",
        "Position in the main job",
        "Position in the main job(2)",
        "Position in the occupation (main job)",
        "Main activity groups in the workplace",
        "Occupational groups in the main job",
        "Contributed to a pension plan (any job)",
        "Bracket - Usual working hours in all jobs",
        "Bracket - Effective working hours in all jobs",
        "Remuneration type in the main job",
        "Norm. received monthly income in the main job",
        "Effective monthly income in the main job",
        "Type of remuneration",
        "Norm. received monthly income for all jobs",
        "Effective monthly income for all jobs",
        "Motives for not be looking for a new job",
        "Usual working hours in all jobs",
        "Effective working hours in all jobs",
        "Bracket - Usual working hours in the main job",
        "Bracket - Effective working hours in the main job"
      )
  )

  DF <- filepath_in %>% map(
    ~ .x %>%
      read_tsv(
        .,
        n_max = 1000,
        col_names = "a"
      ) %>%
      separate(a, into = vname, sep = last)
  )

  names(DF) <- paste0("PNADC_", trimestre, ano)

  DF <- DF %>% map(
    ~ .x %>%
      mutate_at(caracteres, as.character) %>%
      mutate_at(numerico, as.numeric) %>%
      mutate_at(fatores, as.factor) %>%
      mutate(
        hous_id = paste0(UPA, V1008, V1014),
        ind_id = paste0(UPA, V1008, V1014, V2003)
      ) %>%
      select(hous_id, ind_id, everything())
  )

  DF <- DF %>% map(~ .x %>% set_variable_labels(.labels = lista))
  return(DF)
}

#' Painel básico
#'
#' @param build_data Default \code{TRUE}.
#' Se \code{TRUE}, implementa primeiro \code{\link{datazoom_pnadc}} e depois
#' monta paineis de indiv??duos. Se \code{FALSE}, a função constrói paneis a partir de dados já carregados no R
#'
#' @param dados_prontos Bases de dados para diferentes trimestres da PNAD cont??nua.
#' Necessário se \code{build_data = FALSE}
#'
#' @param local_dados Diretório onde os microdados originais em formato de texto estão armazenados
#' caso \code{build_data = TRUE}
#'
#' @param periodos Lista de vetores com per??odos de interesse no formato
#' \code{periodos = list(c(trimestre1, ano1), c(trimestre2, ano2), ...)}
#'
#' @encoding UTF-8
#'
#' @return Lista de dataframes, sendo cada entrada um trimestre/ano
#'
#' @examples
#' PNADC_2012 <- datazoom_pnadc(
#'   diretorio_dados = "./Desktop",
#'   c(1, 2012), c(2, 2012)
#' )
#'
#' teste <- pnadc_painel_basico(
#'   build_data = FALSE,
#'   dados_prontos = PNADC_2012
#' )
#'
#' teste2 <- pnadc_painel_basico(
#'   build_data = TRUE,
#'   local_dados = "./pnadcontinua",
#'   periodos = list(c(1, 2012), c(2, 2012))
#' )
#' @export
pnadc_painel_basico <- function(build_data = TRUE, ...) {
  argumentos <- list(...)

  if (!(build_data) & is.null(argumentos$dados_prontos)) {
    stop("Se build_data == FALSE, definir dados_prontos.
       Ver help para detalhes")
  }
  if (build_data == TRUE &
    (is.null(argumentos$local_dados))
  ) {
    stop("Se build_data == TRUE, definir local_dados,
          Ver help para detalhes")
  }

  if (!build_data) {
    if (is.list(argumentos$dados_prontos) == FALSE) {
      dados_prontos <- as.list(argumentos$dados_prontos)
    } else {
      dados_prontos <- argumentos$dados_prontos
    }
  } else {
    local_dados <- argumentos$local_dados

    argumentos$local_pastas <- NULL

    dados_prontos <- do.call(datazoom_pnadc, c(
      local_dados,
      argumentos$periodos
    ))
  }

  ###### Inserir função do painel


  return(paineis)
}
