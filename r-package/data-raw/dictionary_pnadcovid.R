
#### Generating PNAD COVID DICTIONARY
read_dictionary_pnadcovid <- function(directory_dictionary) {
  raw.dic <- readxl::read_xls(file.path(directory_dictionary, "Dicionario_PNAD_COVID_112020.xls"),
    skip = 2
  )
  raw.dic %>%
    janitor::clean_names() %>%
    dplyr::select(x2, tipo, descricao_4, descricao_6) %>%
    dplyr::rename(
      codigo_variavel = x2,
      descricao = descricao_4,
      valor = tipo,
      valor_descricao = descricao_6
    ) %>%
    dplyr::filter(!is.na(codigo_variavel) | !is.na(valor) | !is.na(descricao) |
      !is.na(valor_descricao)) %>%
    tidyr::fill(codigo_variavel, descricao, .direction = "down")
}

dictionary <- read_dictionary_pnadcovid("C:/users/arthu/Downloads") %>%
  dplyr::filter(!codigo_variavel %in% c("Ano", "Estrato", "Mes"))


description_en <-
  #######
  c(
    "Ano" = "Year",
    "UF" = "State",
    "CAPITAL" = "Capital",
    "RM_RIDE" = "Metropolitan Region",
    "UPA" = "District borders",
    "ESTRATO" = "Stratum",
    "V1008" = "Household selection number",
    "V1012" = "Week of the month",
    "V1013" = "Month of the survey",
    "V1016" = "Household interview number",
    "UPA" = "District borders",
    "V1013" = 'Month of the survey',
    "V1022" = "Situation in the household",
    "V1023" = "Area type",
    "V1030" = "Population projection",
    "V1031" = "Sample weight with correction of no-interview,
  without post-stratification by population projection",
    "V1032" = "Sample weight with correction of no-interview,
  with post-stratification by population projection",
    "posest" = "Projecting domain",
    "A001" = "Order number",
    "A001A" = "Condition in the household",
    "A001B1" = "Day of birth",
    "A001B2" = "Month of birth",
    "A001B3" = "Year of birth",
    "A002" = "Resident's age",
    "A003" = "Gender",
    "A004" = "Skin color or race",
    "A005" = "Schooling",
    "A006" = "Goes to school",
    "A006A" = "Is the school or college you attend public or private?	",
    "A006B" = "Are you taking face-to-face classes?	",
    "A007" = "In the last week, were school activities available to do at home?	",
    "A007A" = "Why didn't you carry out the activities made available last week?",
    "A008" = "In the last week, how many days did you dedicate to school activities?",
    "A009" = "In the last week how much time per day did you spend doing school activities?",
    "B0011" = "In the last week, did you have fever?",
    "B0012" = "In the last week, did you have a cough?",
    "B0013" = "In the last week, did you have a sore throat?",
    "B0014" = "In the last week, did you have difficulty breathing?",
    "B0015" = "In the last week, did you have a headache?",
    "B0016" = "In the last week, did you have chest pain?",
    "B0017" = "In the last week, did you have nausea?",
    "B0018" = "In the last week, did you have stuffy or runny nose?",
    "B0019" = "In the last week, did you have fatigue?",
    "B00110" = "In the last week, did you have eye pain?",
    "B00111" = "In the last week, did you have loss of smell or taste?",
    "B00112" = "In the last week, did you have muscle pain?",
    "B00113" = "In the last week, did you have diarrhea?",
    "B002" = "Because of that, did you go to any health establishment?",
    "B0031" = "Providence taken to recover from symptoms was staying at home",
    "B0032" = "Providence taken to recover from symptoms was to call some healthcare professional",
    "B0033" = "Providence taken to recover from symptoms was to buy and/or take medicine on their own",
    "B0034" = "Providence taken to recover from symptoms was to buy and/or take medicine by medical advice",
    "B0035" = "Providence taken to recover from symptoms was a visit from some public healthcare professional",
    "B0036" = "Providence taken to recover from symptoms was a visit from some private healthcare professional",
    "B0037" = "Providence taken to recover from symptoms was another",
    "B0041" = "The place where you sought care was a health post/basic
  health unit/Family Health Team (doctor, nurse, nursing technician or community health worker)",
    "B0042" = "The place where you sought care was the SUS/UPA emergency room",
    "B0043" = "The place where you sought care was a SUS hospital",
    "B0044" = "The place where you sought care was an outpatient or private practice or linked to the armed forces",
    "B0045" = "Place that sought care was a private emergency room or linked to the armed forces",
    "B0046" = "Place where he sought care was a private hospital or linked to the armed forces",
    "B005" = "When looking for the hospital, had to stay in the hospital for a day or more",
    "B006" = "During hospitalization, was sedated, intubated and placed in artificial respiration with ventilator",
    "B007" = "Do you have a medical health plan, whether private, company or public agency",
    "B008" = "Did you do any tests to find out if you were infected with the coronavirus?",
    "B009A" = "Did you have the test collected with a swab in your mouth and / or nose (SWAB)?",
    "B009B" = "What was the result?",
    "B009C" = "Did you have the blood collection test through a hole in your finger?",
    "B009D" = "What was the result?",
    "B009E" = "Did you do the blood collection test through a vein in your arm?",
    "B009F" = "What was the result?",
    "B0101" = "Has a doctor ever diagnosed you with diabetes?",
    "B0102" = "Has any doctor ever diagnosed you with hypertension?",
    "B0103" = "Has any doctor ever diagnosed you with asthma/bronchitis/emphysema /chronic respiratory disease or lung disease?",
    "B0104" = "Has any doctor ever diagnosed you with heart disease (heart attack, angina, heart failure, arrhythmia)?",
    "B0105" = "Has a doctor ever diagnosed you with depression?",
    "B0106" = "Has a doctor ever diagnosed you with cancer?",
    "B011" = "What was the test result? Last week, due to the Coronavirus pandemic, to what extent did you restrict contact with people?",
    "C001" = "In the last week, for at least an hour, did you work in any occupational activity?",
    "C002" = "Last week, were you temporarily away from work?",
    "C003" = "What is the main reason for this temporary removal?",
    "C004" = "Continued to be paid (even partially) for this work",
    "C005" = "How long have you been away from that job?",
    "C0051" = "Time away (From 1 month to less than 1 year)",
    "C0052" = "Time away (from 1 Year to less than 2 Years)",
    "C0053" = "Time away (from 2 Year to 98 Years)",
    "C006" = "Has more than one job",
    "C007" = "At work (single or main) I had that week, it was:",
    "C007A" = "This work was in the area:",
    "C007B" = "Do you have a formal contract and are you a public servant?",
    "C007C" = "What kind of job, role or function do you do in your job (single or main)?",
    "C007D" = "What is the main activity of local or company in which you work?",
    "C007E" = "In the last week, how many employees worked in that firm/company that ... you had?",
    "C007E1" = "1 to 5 employees",
    "C007E2" = "6 to 10 employees",
    "C007F" = "In the work (single or main one) that you had in that week, did you have the employment contract suspended?",
    "C008" = "How many hours a week did you normally work?",
    "C009" = "How many hours, in the last week, did you actually work?",
    "C009A" = "Would you like to have worked more hours last week than you actually did?",
    "C010" = "How much did he receive (or withdraw) normally in all your jobs",
    "C0101" = "Received/withdrawn normally in cash",
    "C01011" = "Cash/withdrawal band number",
    "C01012" = "Cash value",
    "C0102" = "Usually received on products and merchandise",
    "C01021" = "Income/withdrawal range number on products and goods",
    "C01022" = "Value in products and goods",
    "C0103" = "Normally received only in benefits",
    "C0104" = "Was unpaid",
    "C011A" = "How much did you normally receive (or withdraw) in all your jobs",
    "C011A1" = "Received/withdrawn normally in cash",
    "C011A11" = "Cash/withdrawal band number",
    "C011A12" = "Cash value",
    "C011A2" = "Usually received on products and merchandise",
    "C011A21" = "Income/withdrawal range number on products and goods",
    "C011A22" = "Value in products and goods",
    "C012" = "Most of the time, in the last week, was this (single or main) job performed in the same place you usually work?",
    "C013" = "In the last week, were you  in remote work (home office or telework)?",
    "C014" = "Do you contribute to the INSS?",
    "C015" = "In the last week ___ have you taken any effective steps to get a job?",
    "C016" = "What is the main reason for not looking for work in the last week?",
    "C017A" = "Did any other resident of this household work in a paid way in the last week?",
    "D0011" = "Income received from retirement and pension by all residents",
    "D0013" = "Sum of received values",
    "D0021" = "Income from alimony, donation or cash allowance of person who did not live at home",
    "D0023" = "Sum of received values",
    "D0031" = "Income from Bolsa Família program",
    "D0033" = "Sum of received values",
    "D0041" = "On the Month of ... (Reference month), ... received income from the assistance benefit of Continued Provision - BPC-LOAS?",
    "D0043" = "Sum of received values",
    "D0051" = "Emergency aid related to coronavirus",
    "D0053" = "Sum of received values",
    "D0061" = "Unemployment insurance",
    "D0063" = "Sum of received values",
    "D0071" = "Other income, with rent, lease, private pension, scholarship, income from financial investments, etc.",
    "D0073" = "Sum of received values",
    "E001" = "During the pandemic period, did anyone in this household apply for a loan?",
    "E0021" = "This loan was acquired with a bank or financial institution",
    "E0022" = "This loan was acquired from a relative or friend",
    "E0023" = "This loan was acquired from employees or an employer",
    "E0024" = "This loan was acquired from some other location or person",
    "F001" = "This address is:",
    "F0021" = "What was the monthly rent paid, or that should have been paid, in the reference month?",
    "F0022" = "Track number of paid rent",
    "F002A1" = "In your home there are the following basic cleaning and protection items: soap or detergent",
    "F002A2" = "In your home there are the following basic cleaning and protection items: 70% alcohol or higher (gel or liquid)",
    "F002A3" = "In your home there are the following basic cleaning and protection items: masks",
    "F002A4" = "In your home there are the following basic cleaning and protection items: disposable gloves",
    "F002A5" = "In your home there are the following basic cleaning and protection items: bleach or disinfectant",
    "F0061" = "Who answered the questionnaire?",
    "F006" = "Order number of the resident who provided the information"
  )

#######

description_en <- data.frame(
  label = description_en,
  variable = names(description_en)
)

dictionary <- dplyr::left_join(
  dictionary %>%
    dplyr::rename(
      variable = codigo_variavel,
      value = valor,
      value_label = valor_descricao
    ),
  description_en
)


label_description_en <-
  ######
  c(
    "Rondônia" = "Rondônia",
    "Acre" = "Acre",
    "Amazonas" = "Amazonas",
    "Roraima" = "Roraima",
    "Pará" = "Pará",
    "Amapá" = "Amapá",
    "Tocantins" = "Tocantins",
    "Maranhão" = "Maranhão",
    "Piauí" = "Piauí",
    "Ceará" = "Ceará",
    "Rio Grande do Norte" = "Rio Grande do Norte",
    "Paraíba" = "Paraíba",
    "Pernambuco" = "Pernambuco",
    "Alagoas" = "Alagoas",
    "Sergipe" = "Sergipe",
    "Bahia" = "Bahia",
    "Minas Gerais" = "Minas Gerais",
    "Espírito Santo" = "Espírito Santo",
    "Rio de Janeiro" = "Rio de Janeiro",
    "São Paulo" = "São Paulo",
    "Paraná" = "Paraná",
    "Santa Catarina" = "Santa Catarina",
    "Rio Grande do Sul" = "Rio Grande do Sul",
    "Mato Grosso do Sul" = "Mato Grosso do Sul",
    "Mato Grosso" = "Mato Grosso",
    "Goiás" = "Goiás",
    "Distrito Federal" = "Distrito Federal",
    "Município de Porto Velho (RO)" = "Porto Velho (RO) municipality",
    "Município de Rio Branco (AC)" = "Rio Branco (AC) municipality",
    "Município de Manaus (AM)" = "Manaus (AM) municipality",
    "Município de Boa Vista (RR)" = "Boa Vista (RR) municipality",
    "Município de Belém (PA)" = "Belém (PA) municipality",
    "Município de Macapá (AP)" = "Macapá (AP) municipality",
    "Município de Palmas (TO)" = "Palmas (TO) municipality",
    "Município de São Luís (MA)" = "São Luís (MA) municipality",
    "Município de Teresina (PI)" = "Teresina (PI) municipality",
    "Município de Fortaleza (CE)" = "Fortaleza (CE) municipality",
    "Município de Natal (RN)" = "Natal (RN) municipality",
    "Município de João Pessoa (PB)" = "João Pessoa (PB) municipality",
    "Município de Recife (PE)" = "Recife (PE) municipality",
    "Município de Maceió (AL)" = "Maceió (AL) municipality",
    "Município de Aracaju (SE)" = "Aracaju (SE) municipality",
    "Município de Salvador (BA)" = "Salvador (BA) municipality",
    "Município de Belo Horizonte (MG)" = "Belo Horizonte (MG) municipality",
    "Município de Vitória (ES)" = "Vitória (ES) municipality",
    "Município de Rio de Janeiro (RJ)" = "Rio de Janeiro (RJ) municipality",
    "Município de São Paulo (SP)" = "São Paulo (SP) municipality",
    "Município de Curitiba (PR)" = "Curitiba (PR) municipality",
    "Município de Florianópolis (SC)" = "Florianópolis (SC) municipality",
    "Município de Porto Alegre (RS)" = "Porto Alegre (RS) municipality",
    "Município de Campo Grande (MS)" = "Campo Grande (MS) municipality",
    "Município de Cuiabá (MT)" = "Cuiabá (MT) municipality",
    "Município de Goiânia (GO)" = "Goiânia (GO) municipality",
    "Município de Brasília (DF)" = "Brasília (DF) municipality",
    "Região Metropolitana de Manaus (AM)" = "Manaus (AM) metropolitan region",
    "Região Metropolitana de Belém (PA)" = "Belém (PA) metropolitan region",
    "Região Metropolitana de Macapá (AP)" = "Macapá (AP) metropolitan region",
    "Região Metropolitana de Grande São\nLuís (MA)" = "São Luís (MA) metropolitan region",
    "Região Administrativa Integrada\nde Desenvolvimento da Grande Teresina (PI)" =
      "Administrative integrated development region of Teresina (PI)",
    "Região Metropolitana de Fortaleza (CE)" = "Fortaleza (CE) metropolitan region",
    "Região Metropolitana de Natal (RN)" = "Natal (RN) metropolitan region",
    "Região Metropolitana de João Pessoa (PB)" = "João Pessoa (PB) metropolitan region",
    "Região Metropolitana de Recife (PE)" = "Recife (PE) metropolitan region",
    "Região Metropolitana de Maceió (AL)" = "Maceió (AL) metropolitan region",
    "Região Metropolitana de Aracaju (SE)" = "Aracaju (SE) metropolitan region",
    "Região Metropolitana de Salvador (BA)" = "Salvador (BA) metropolitan region",
    "Região Metropolitana de Belo Horizonte (MG)" = "Belo Horizonte (MG) metropolitan region",
    "Região Metropolitana de Grande Vitória (ES)" = "Vitória (ES) metropolitan region",
    "Região Metropolitana de Rio de Janeiro (RJ)" = "Rio de Janeiro (RJ) metropolitan region",
    "Região Metropolitana de São Paulo (SP)" = "São Paulo (SP) metropolitan region",
    "Região Metropolitana de Curitiba (PR)" = "Curitiba (PR) metropolitan region",
    "Região Metropolitana de Florianópolis (SC)" = "Florianópolis (SC) metropolitan region",
    "Região Metropolitana de Porto Alegre (RS)" = "Porto Alegre (RS) metropolitan region",
    "Região Metropolitana de Vale do Rio Cuiabá (MT)" = "Vale do Rio Cuiabá (MT) met
    ropolitan region",
    "Região Metropolitana de Goiânia (GO)" = "Goiânia (GO) metropolitan region",
    "Número do domicílio" = "Household number",
    "Mês da pesquisa (janeiro a dezembro)" = "Month of the interview (January to December)",
    "Número da entrevista" = "Interview number",
    "Urbana" = "Urban",
    "Rural" = "Rural",
    "Capital" = "Capital",
    "Resto da RM (Região Metropolitana, excluindo a capital)" =
      "Rest of metropolitan region (RM), excluding capital",
    "Resto da RIDE (Região Integrada de Desenvolvimento Econômico, excluindo a capital)" =
      "Rest of integrated region of economic development (RIDE)",
    "Resto da UF  (Unidade da Federação, excluindo a região metropolitana e a RIDE)" =
      "Rest of state, excludindg RM and RIDE",
    "Projeção da população do mês (referência: dia 15 do mês de referência da coleta)" =
      "Population
    projection in the month (reference: day 15 of the reference month)",
    "Peso mensal com correção de não entrevista sem pós estratificação pela projeção de população" =
      "Monthly weight with correction for non-interview without post stratification by
    population projection",
    "Peso mensal com correção de não entrevista com pós estratificação pela projeção de população" =
      "Monthly weight with correction for non-interview with post stratification by
    population projection",
    "As 2 primeiras posições representam o código da Unidade da Federação, a terceira, o sexo do morador e a última,
  a faixa etária do morador. UF(2) + A003(1) + Faixa Etária com base na A002(1)" =
      "The first 2 positions represent the state code, the third, the gender, and the last,
    the person's age bracket. UF(2) + A003(1) + age bracket based on A002(1)",
    "Pessoa responsável pelo domicílio" = "Person responsible for the household",
    "Cônjuge ou companheiro(a) de sexo diferente" = "Spouse or partner of opposite sex",
    "Cônjuge ou companheiro(a) do mesmo sexo" = "Spouse or partner of the same sex",
    "Filho(a) do responsável e do cônjuge" = "Son/Daughter of the householder and spouse",
    "Filho(a) somente do responsável" = "Son/Daughter of the householder only",
    "Filho(a) somente do cônjuge" = "Son/Daughter of the spouse only",
    "Genro ou nora" = "Son or daugther in law",
    "Pai, mãe, padrasto ou madrasta" = "Father, mother, stepfather or stepmother",
    "Sogro(a)" = "Father or mother in law",
    "Neto(a)" = "Grandson or granddaughter",
    "Bisneto(a)" = "Great grandson or great granddaughter",
    "Irmão ou irmã" = "Brother or sister",
    "Avô ou avó" = "Grandfather or grandmother",
    "Outro parente" = "Other relative",
    "Agregado(a) - Não parente que não compartilha despesas" = "In law that does not share expenses",
    "Convivente - Não parente que compartilha despesas" = "In law that shares expenses",
    "Pensionista" = "Pensioner",
    "Empregado(a) doméstico(a)" = "Housekeeper",
    "Parente do(a) empregado(a) doméstico(a)" = "Housekeeper's relative",
    "Dia de nascimento" = "Day of birth",
    "Não informado" = "Not informed",
    "Mês" = "Month",
    "Ano" = "Year",
    "Idade (em anos)" = "Age (years)",
    "Homem" = "Man",
    "Mulher" = "Woman",
    "Branca" = "White",
    "Preta" = "Black",
    "Amarela" = "Yellow",
    "Parda" = "Brown",
    "Indígena" = "Indigenous",
    "Ignorado" = "Ignored",
    "Sem instrução" = "No schooling",
    "Fundamental incompleto" = "Incomplete elementary school",
    "Fundamental completo" = "Completed elementary school",
    "Médio incompleto" = "Incomplete high school",
    "Médio completo" = "Completed high school",
    "Superior incompleto" = "Incomplete college",
    "Superior completo" = "Complete college",
    "Pós-graduação, mestrado ou doutorado" = "Graduate, masters or doctorate",
    "Sim" = "Yes",
    "Não" = "No",
    "Não aplicável" = "Does not apply",
    "Privada" = "Private",
    "Pública" = "Public",
    "Sim, normalmente" = "Yes, usually",
    "Sim, mas apenas parcialmente" = "Yes, but only partially",
    "Não, e meu normalmente é presencial/semipresencial" = "No, and mine is presencial/semi-presential",
    "Não, meu curso é online" = "No, my course is online",
    "Sim, e realizou pelo menos parte delas" = "Yes, and done at least part of them",
    "Sim, mas não realizou (por qualquer motivo)" = "Yes, but have not done them (for any reason)",
    "Não, porque estava de férias" = "No, because was in vacations",
    "Não tinha computador / tablet / celular disponível" = "Did not have a computer, tablet, or mobile phone available",
    "Não tinha acesso à internet ou a qualidade dela era insuficiente" = "Did not have access to the internet or its qualitfy was insufficient",
    "Por problemas de saúde da própria pessoa" = "Because of health issues of the own person",
    "Tinha que cuidar dos afazeres domésticos, do(s) filhos ou de outro(s) parentes" = "Had to take care of house chores, of sons or other relatives",
    "Não conseguiu se concentrar" = "Could not concentrate",
    "Outro motivo. Especifique." = "Other reason. Specify",
    "1 dia" = "1 day",
    "2 dias" = "2 days",
    "3 dias" = "3 days",
    "4 dias" = "4 days",
    "5 dias" = "5 days",
    "6 ou 7 dias" = "6 or 7 days",
    "Menos de 1 hora" = "Less than an hour",
    "De 1 hora a menos de 2 horas" = "From 1 hour to less than 2 hours",
    "De 2 horas a menos de 5 horas" = "From 2 hours to less than 5 hours",
    "5 horas ou mais" = "5 hours or more",
    "Não sabe" = "Doesn't know",
    "Não foi atendido" = "Was not seen at the hospital",
    "Positivo" = "Positive",
    "Negativo" = "Negative",
    "Inconclusivo" = "Inconclusive",
    "Ainda não recebeu o resultado" = "Have received the result yet",
    "Não aplicavel" = "Not applicable",
    "Não fez restrição, levou vida normal como antes da pandemia" =
      "Made no restrictions, led a normal life as before the pandemic",
    "Reduziu o contato com as pessoas, mas continuou saindo de casa para trabalho
  ou atividades não essenciais e/ou recebendo visitas" =
      "Reduced contact with people, but kept leaving home for work or
  non-essential activities and / or receiving visitors",
    "Ficou em casa e só saiu em caso de necessidade básica" =
      "Stayed at home and only left in case of basic necessity",
    "Ficou rigorosamente em casa" = "Stayed rigorously at home",
    "Estava em quarentena, isolamento, distanciamento social ou férias coletivas" =
      "Was in quarantine, isolation, social distance or collective vacation",
    "Férias, folga ou jornada de trabalho variável" = "Vacation, time off or variable working hours",
    "Licença maternidade ou paternidade" = "Maternity or paternity leave",
    "Licença remunerada por motivo de saúde ou acidente da própria pessoa" =
      "Paid leave due to the person's own health or accident",
    "Outro tipo de licença remunerada (estudo, paternidade, casamento, licença prêmio, etc.)" =
      "Other type of paid leave (study, paternity, marriage, premium leave, etc.)",
    "Afastamento do próprio negócio/empresa por motivo de gestação, saúde, acidente, etc.,
  sem ser remunerado por instituto de previdência" =
      "Temporary absence of own business because of pregnancy, illness, accident, etc.,
    without payment by pension institution",
    "Fatores ocasionais (mau tempo, paralisação nos serviços de transportes, etc.)" =
      "Occasional factors (weather, stoppage in transportation services, etc.)",
    "Outro motivo" = "Other reason",
    "O trabalho já não era remunerado" = "The job was already unpaid",
    "Menos de 1 mês" = "Less than a month",
    "De 1 mês a menos de 1 ano" = "From one month to less than one year",
    "De 1 ano a menos de 2 anos" = "From one year to less than two years",
    "2 anos ou mais" = "Two years or more",
    "01 a 11 meses" = "1 to 11 months",
    "00 a 11 meses" = "0 to 11 months",
    "Trabalhador doméstico" = "Housekeeper",
    "Militar do exercito, marinha ou aeronáutica" = "Military from army, navy, air-force",
    "Plicial militar ou bombeiro mlilitar" = "Military police or  military firefighter",
    "Empregado do setor privado" = "Private sector employee",
    "Empregado do setor público (inclusive empresas de economia mista)" =
      "Public sector employees (including mixed companies)",
    "Empregador" = "Employer",
    "Conta própria" = "Self employed",
    "Trabalhador familiar não remunerado em ajuda a membro do domicílio ou parente" =
      "Unpaid family worker assisting a member of the household or a relative",
    "Estava fora do mercado de trabalho (fazia apenas afazeres domésticos, cuidados de pessoas ou produção para próprio consumo)" =
      "Was out of the labor market (only did household chores, cared for people or produced for own consumption)",
    "Federal" = "Federal",
    "Estadual" = "State",
    "Municipal" = "Municipal",
    "Sim, tem carteira de trabalho assinada" = "Yes, with a formal contract",
    "Sim, é servidor público estatutário" = "Yes, is a statutory public servant",
    "Empregado doméstico, diarista, cozinheiro (em domicílios particulares)," =
      "Domestic worker, daily cleaner, cook (in private households),",
    "Faxineiro, auxiliar de limpeza etc. (em empresa pública ou privada)," =
      "Janitor, cleaning assistant, etc. (in public or private company)",
    "Auxiliar de escritório, escriturário" = "Office clerk, clerk",
    "Secretária, recepcionista" = "Receptionist",
    "Operador de Telemarketing" = "Telemarketing operator",
    "Comerciante (dono do bar, da loja etc.)" = "Merchant (owner of the bar, shop etc.)",
    "Balconista, vendedor de loja" = "Sales clerk, store salesman",
    "Vendedor a domicílio, representante de vendas, vendedor de catálogo (Avon, Natura etc.)" =
      "Home salesman, sales representative, catalog seller",
    "Vendedor ambulante (feirante, camelô, comerciante de rua, quiosque)" = "Street vendor",
    "Cozinheiro e garçon (de restaurantes, empresas)" = "Cook and waiter (for restaurants, companies)",
    "Padeiro, açougueiro e doceiro" = "Baker, butcher, or confectioner",
    "Agricultor, criador de animais, pescador, silvicultor e jardineiro" =
      "Farmer, animal breeder, fisherman, forester, or gardener",
    "Auxiliar da agropecuária (colhedor de frutas, boia fria, etc.)" = "Agricultural assistant",
    "Motorista (de aplicativo, de taxi, de van, de mototáxi, de ônibus)" = "Driver",
    "Motorista de caminhão (caminhoneiro)," = "Truck driver",
    "Motoboy," = "Motoboy",
    "Entregador de mercadorias (de restaurante, de farmácia, de loja, Uber Eats, IFood, Rappy etc.)" =
      "Delivery man",
    "Pedreiro, servente de pedreiro, pintor, eletricista, marceneiro" = "Bricklayer, painter, electrician, joiner",
    "Mecânico de veículos, máquinas industriais etc." = "Mechanic of vehicles or industrial machines",
    "Artesão, costureiro e sapateiro" = "Craftsman, dressmaker and shoemaker",
    "Cabeleireiro, manicure e afins" = "Hairdresser, manicure and the like",
    "Operador de máquinas, montador na indústria;" = "Machine operator, assembler manufacturing;",
    "Auxiliar de produção, de carga e descarga;" = "Production assistant in loading and unloading;",
    "Professor da educação infantil, de ensino fundamental, médio ou superior," =
      "Teacher of early childhood education, elementary school, high school or higher education",
    "Pedagogo, professor de idiomas, música, arte e reforço escolar" =
      "Pedagogue, teacher of languages, music, art and tutoring",
    "Médico, enfermeiro, profissionais de saúde de nível superior" =
      "Doctor, nurse, health professionals at a higher level",
    "Técnico, profissional da saúde de nível médio" =
      "Technician, mid-level health professional",
    "Cuidador de crianças, doentes ou idosos" = "Caregiver for children, the sick or the elderly",
    "Segurança, vigilante, outro trabalhador dos serviços de proteção" = "Security, vigilant, other protection services worker",
    "Policial civil" = "Civil policeman",
    "Porteiro, zelador" = "Doorman, custodian",
    "Artista, religioso (padre, pastor etc.)" = "Artist, religious (priest, pastor, etc.)",
    "Diretor, gerente, cargo político ou comissionado" = "Director, manager, political or commissioned position",
    "Outra profissão de nível superior (advogado, engenheiro, contador, jornalista etc.)" =
      "Other higher-level profession (lawyer, engineer, accountant, journalist, etc.)",
    "Outro técnico ou profissional de nível médio" = "Other technician or mid-level professional",
    "Outros" = "Others",
    "não aplicável" = "Not applicable",
    "Agricultura, pecuária, produção florestal e pesca" = "Agriculture, livestock, forest production, and fisheries",
    "Extração de petróleo, carvão mineral, minerais metálicos, pedra, areia, sal etc." =
      "Extraction of petroleum, mineral coal, metallic minerals, stone, sand, salt, etc.",
    "Indústria da transformação (inclusive confecção e fabricação caseira)" =
      "Manufacturing industry",
    "Fornecimento de eletricidade e gás, água, esgoto e coleta de lixo" =
      "Supply of electricity and gas, water, sewage and garbage collection",
    "Construção" = "Construction",
    "Comércio no atacado e varejo;" = "Wholesale and retail trade",
    "Reparação de veículos automotores e motocicletas" = "Repair of motor vehicles and motorcycles",
    "Transporte de passageiros" = "Passenger transportation",
    "Transporte de mercadorias" = "Freight transport",
    "Armazenamento, correios e serviços de entregas" = "Storage, post and delivery services",
    "Hospedagem (hotéis, pousadas etc.)" = "Accommodation (hotels, inns, etc.)",
    "Serviço de alimentação (bares, restaurantes, ambulantes de alimentação)" =
      "Food service (bars, restaurants, food vendors)",
    "Informação e comunicação (jornais, rádio e televisão, telecomunicações e informática)" =
      "Information and communication (newspapers, radio and television, telecommunications and information technology)",
    "Bancos, atividades financeiras e de seguros" = "Banks, financial and insurance activities",
    "Atividades imobiliárias" = "Real estate activities",
    "Escritórios de advocacia, engenharia, publicidade e veterinária (Atividades profissionais, científicas e técnicas)" =
      "Law, engineering, advertising and veterinary offices (Professional, scientific and technical activities)",
    "Atividades de locação de mão de obra, segurança, limpeza, paisagismo e teleatendimento" =
      "Manpower rental, security, cleaning, landscaping and call center activities",
    "Administração pública (governo federal, estadual e municipal)" = "Public administration (federal, state and municipal government)",
    "Educação" = "Education",
    "Saúde humana e assistência social" = "Healthcare and social assistance",
    "Organizações religiosas, sindicatos e associações" = "Religious organizations, unions and associations",
    "Atividade artísticas, esportivas e de recreação" = "Artistic, sporting and recreational activities",
    "Cabeleireiros, tratamento de beleza e serviços pessoais" = "Hairdressers, beauty treatment and personal services",
    "Serviço doméstico remunerado (será imputado da posição na ocupação)" = "Paid domestic service",
    "Outro" = "Other",
    "1 a 5 empregados" = "1 to 5 employees",
    "6 a 10 empregados" = "6 to 10 employees",
    "11 ou mais empregados" = "11 or more employees",
    "06 a 10 empregados" = "6 to 10 employees",
    "Horas" = "Hours",
    "Indica se o quesito foi respondido" = "Indicates whether the question has been answered",
    "Em dinheiro" = "In cash",
    "R$" = "R$",
    "Em produtos ou mercadorias" = "In products or goods",
    "Em benefícios" = "In benefits",
    "Não remunerado" = "Unpaid",
    "Devido à pandemia (isolamento, quarentena ou distanciamento social)" =
      "Due to the pandemic (isolation, quarantine or social detachment)",
    "Por problemas de saúde ou gravidez" = "Due to health problems or pregnancy",
    "Estava estudando" = "Was studying",
    "Não quer trabalhar ou é aposentado" = "Doesn't want to work or is retired",
    "Não tinha experiência profissional ou qualificação" = "Had no professional experience or qualification",
    "Acha que não vai encontrar trabalho por ser muito jovem ou idoso" =
      "Thinks will not find work because you are too young or old",
    "Não havia trabalho na localidade" = "There was no work in the locality",
    "Tinha que cuidar dos afazeres domésticos e ou de parentes" =
      "Had to take care of household chores and or relatives",
    "Estava aguardando resposta de medida tomada para conseguir trabalho" =
      "Was  waiting for a response to the action taken to get a job",
    "Sim, e pelo menos um morador conseguiu" = "Yes, and at least one resident did it",
    "Sim, mas nenhum morador conseguiu" = "Yes, but no resident has succeeded",
    "Não solicitou" = "Didn't request",
    "Próprio - já pago" = "Own - already paid for",
    "Alugado" = "Rented",
    "Cedido por empregador" = "Provided by an employer",
    "Cedido por familiar" = "Provided by family member",
    "Cedido de outra forma" = "Provided otherwise",
    "Outra condição" = "Other condition",
    "Aluguel a vencer" = "Rent to expire",
    "Pessoa moradora" = "Resident person",
    "Pessoa não moradora" = "Non resident person",
    "Número de ordem do morador" = "Resident's order number",
    "Não aplicado" = "Not applied",
    "01 a 30" = "1 to 30",
    "01 a 31" = "1 to 31"
  )
######

label_description_en <- data.frame(
  value_label_en = label_description_en,
  value_label = names(label_description_en)
)

variable_type <-
  c(
    "V1030" = "double",
    "A002" = "double",
    "C008" = "double",
    "C009" = "double",
    "C01012" = "double",
    "C01022" = "double",
    "C011A12" = "double",
    "C011A22" = "double",
    "D0013" = "double",
    "D0023" = "double",
    "D0033" = "double",
    "D0043" = "double",
    "D0053" = "double",
    "D0063" = "double",
    "D0073" = "double",
    "F0021" = "double"
  )


variable_type <- data.frame(
  variable = names(variable_type),
  variable_type = variable_type
)


dictionary <- dictionary %>%
  dplyr::left_join(label_description_en)

dictionary <- dictionary %>%
  dplyr::left_join(variable_type) %>%
  dplyr::mutate(
    variable_type = ifelse(is.na(variable_type), "factor", variable_type)
  )


dictionary <- dplyr::distinct(dictionary)

dictionary_pnadcovid_pt.br <- dictionary %>%
  dplyr::select(variable, descricao, value, value_label, variable_type) %>%
  dplyr::rename(
    codigo_variavel = variable,
    valor = value,
    valor_descricao = value_label,
    classe_variavel = variable_type
  ) %>%
  dplyr::add_row(
    codigo_variavel = "Estrato",
    descricao = NA,
    valor = NA,
    valor_descricao = NA,
    classe_variavel = "factor",
    .before = TRUE
  ) %>%
  dplyr::add_row(
    codigo_variavel = "Ano",
    descricao = "Ano de referência",
    valor = NA,
    valor_descricao = NA,
    classe_variavel = "factor",
    .before = TRUE
  )



dictionary_pnadcovid_eng <- dictionary %>%
  dplyr::select(variable, label, value, value_label_en, variable_type) %>%
  dplyr::add_row(
    variable = "Stratum",
    label = NA,
    value = NA,
    value_label_en = NA,
    variable_type = "factor",
    .before = TRUE
  ) %>%
  dplyr::add_row(
    variable = "Year",
    label = "Reference year",
    value = NA,
    value_label_en = NA,
    variable_type = "factor",
    .before = TRUE
  ) %>%
  dplyr::rename(value_label = value_label_en)





save(dictionary_pnadcovid_eng, dictionary_pnadcovid_pt.br,
  file = "./data/dictionary_pnadcovid.RData"
)



data_labels_pnadcovid <- data.frame(
  variable_eng = dictionary_pnadcovid_eng$variable,
  variable_pt.br = dictionary_pnadcovid_pt.br$codigo_variavel,
  label_eng = dictionary_pnadcovid_eng$label,
  label_pt.br = dictionary_pnadcovid_pt.br$descricao,
  var_type = dictionary_pnadcovid_eng$variable_type
) %>%
  dplyr::distinct()
