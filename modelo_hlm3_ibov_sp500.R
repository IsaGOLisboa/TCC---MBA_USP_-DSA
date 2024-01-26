################################################################################
#               INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
################################################################################
#Pacotes utilizados:
pacotes <- c("plotly","tidyverse","reshape2","knitr","kableExtra",
             "nlme","lmtest","fastDummies","msm","lmeInfo","jtools","gganimate",
             "ggridges","viridis","hrbrthemes")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Algoritmo para determinação dos erros-padrão das variâncias no componente de
#efeitos aleatórios

#ATENÇÃO: A função abaixo é plenamente funcional para modelos do tipo HLM2
#e HLM3, desde que estimados pelo pacote nlme 

stderr_nlme <- function(model){
  if(base::class(model) != "lme"){
    base::message("Use a lme object model from nlme package")
    stop()}
  resume <- base::summary(model)
  if(base::length(base::names(model$groups))==1){
    m.type <- "HLM2"
  } else if(base::length(base::names(model$groups))==2){
    m.type <- "HLM3"
  }
  if(m.type == "HLM2"){
    vcov_matrix <- model$apVar
    logs_sd_re <- base::attr(vcov_matrix,"Pars")
    if(base::length(logs_sd_re)==2){
      stderr_tau00 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(`RE Components`=base::c("Var(v0j)","Var(e)"),
                                  `Variance Estimatives`= base::c(base::exp(logs_sd_re)[[1]]^2,
                                                                  base::exp(logs_sd_re[[2]])^2),
                                  `Std Err.`=base::c(stderr_tau00,
                                                     stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                            base::exp(logs_sd_re[[2]])^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                                                               base::exp(logs_sd_re[[2]])^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
    else{
      stderr_tau00 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau01 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x4)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(Components=base::c("Var(v0j)","Var(v1j)","Var(e)"),
                                  Estimatives= base::c(base::exp(logs_sd_re)[[1]]^2,
                                                       base::exp(logs_sd_re[[2]])^2,
                                                       base::exp(logs_sd_re[[4]])^2),
                                  Std_Err=base::c(stderr_tau00,
                                                  stderr_tau01,
                                                  stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                            base::exp(logs_sd_re[[2]])^2/stderr_tau01,
                                            base::exp(logs_sd_re[[4]])^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                                                               base::exp(logs_sd_re[[2]])^2/stderr_tau01,
                                                                               base::exp(logs_sd_re[[4]])^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
  }
  if(m.type == "HLM3"){
    vcov_matrix <- model$apVar
    logs_sd_re <-  base::attr(vcov_matrix,"Pars")
    if(base::length(logs_sd_re) == 3){
      stderr_tau_r000 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u000 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x3)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(Components=base::c("Var(t00k)","Var(v0jk)","Var(e)"),
                                  Estimatives=base::c(base::exp(logs_sd_re)[[2]]^2,
                                                      base::exp(logs_sd_re)[[1]]^2,
                                                      base::exp(logs_sd_re)[[3]]^2),
                                  Std_Err=base::c(stderr_tau_u000,
                                                  stderr_tau_r000,
                                                  stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[2]]^2/stderr_tau_u000,
                                            base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                            base::exp(logs_sd_re)[[3]]^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[2]]^2/stderr_tau_u000,
                                                                               base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                                                               base::exp(logs_sd_re)[[3]]^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    } 
    else{
      stderr_tau_r000 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau_r100 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u000 <- msm::deltamethod(~exp(x4)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u100 <- msm::deltamethod(~exp(x5)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x7)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(`RE_Components`=base::c("Var(t00k)","Var(t10k)",
                                                          "Var(v0jk)","Var(v1jk)",
                                                          "Var(e)"),
                                  `Variance Estimatives`=base::c(base::exp(logs_sd_re)[[4]]^2,
                                                                 base::exp(logs_sd_re)[[5]]^2,
                                                                 base::exp(logs_sd_re)[[1]]^2,
                                                                 base::exp(logs_sd_re)[[2]]^2,
                                                                 base::exp(logs_sd_re)[[7]]^2),
                                  `Std Err.`=base::c(stderr_tau_u000,
                                                     stderr_tau_u100,
                                                     stderr_tau_r000,
                                                     stderr_tau_r100,
                                                     stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[4]]^2/stderr_tau_u000,
                                            base::exp(logs_sd_re)[[5]]^2/stderr_tau_u100,
                                            base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                            base::exp(logs_sd_re)[[2]]^2/stderr_tau_r100,
                                            base::exp(logs_sd_re)[[7]]^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[4]]^2/stderr_tau_u000,
                                                                               base::exp(logs_sd_re)[[5]]^2/stderr_tau_u100,
                                                                               base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                                                               base::exp(logs_sd_re)[[2]]^2/stderr_tau_r100,
                                                                               base::exp(logs_sd_re)[[7]]^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
  }
}


################################################################################
#                      DESCRIÇÃO E EXPLORAÇÃO DO DATASET                       #
################################################################################
dados_edgar_concat<- read.csv('dados_edgar_conc.csv')
ibov<- read.csv('empresas_ibov.csv')
sp500<-read.csv('empresas_S&P500.csv')
sp500$CIK <- sprintf("%010d", sp500$CIK)

dados_edgar_concat_duplica<- distinct(dados_edgar_concat, .keep_all = TRUE)
dados_edgar<- dados_edgar_concat_duplica
dados_edgar<- dados_edgar_concat_duplica[!(dados_edgar_concat_duplica$ROA == 0 | is.infinite(dados_edgar_concat_duplica$ROA)), ]


# Criando um novo dataframe vazio para armazenar os resultados
dados_ibov_sp500 <- data.frame()

# Iterarando sobre cada CNPJ em ibov
for (i in 1:nrow(ibov)) {
  cnpj_ibov <- ibov[i, "CNPJ"]
  
  # Verificando se o CNPJ está em dados_edgar
  if (cnpj_ibov %in% dados_edgar$CNPJ_CIA) {
    # Filtrar a linha correspondente em dados_edgar
    linha_edgar <- dados_edgar[dados_edgar$CNPJ_CIA == cnpj_ibov, ]
    
    # Adicionando a linha ao novo dataframe
    dados_ibov_sp500 <- bind_rows(dados_ibov_sp500, linha_edgar)
  }
}


# Iterando sobre cada CIK em sp500
for (i in 1:nrow(sp500)) {
  cik_sp500 <- sp500[i, "CIK"]
  
  # Verificando se o CIK está em dados_edgar
  if (cik_sp500 %in% dados_edgar$CNPJ_CIA) {
    # Filtrar a linha correspondente em dados_edgar
    linha_edgar <- dados_edgar[dados_edgar$CNPJ_CIA == cik_sp500, ]
    
    # Adicionando a linha ao novo dataframe
    dados_ibov_sp500 <- bind_rows(dados_ibov_sp500, linha_edgar)
  }
}


# Criando uma tabela de contagem dos países para visualização
table(dados_ibov_sp500$Pais_k)

n_ibov <- dados_ibov_sp500 %>%
  filter(Pais_k == 1) %>%
  summarize(Contagem = n_distinct(CNPJ_CIA))
n_sp500 <- dados_ibov_sp500 %>%
  filter(Pais_k == 2) %>%
  summarize(Contagem = n_distinct(CNPJ_CIA))
tabela_final <- rbind(n_ibov, n_sp500)
print(tabela_final)

summary(dados_ibov_sp500$ROA)

#Visualizando ROA no ano de 2012 e 2022
ggplot(dados_ibov_sp500, aes(x = Ano_t, y = ROA, color = as.factor(Pais_k))) +
  geom_point() +
  labs(title = "Gráfico de Dispersão de ROA por Ano_t",
       x = "Ano_t",
       y = "ROA") +
  theme_minimal()


dados_pais_1_ano_1 <- dados_ibov_sp500[dados_ibov_sp500$Pais_k == 1 & dados_ibov_sp500$Ano_t == 1, ]

# Filtrando para o Pais_k 2 no ano 1
dados_pais_1_e_2_ano_1 <- dados_ibov_sp500[dados_ibov_sp500$Pais_k %in% c(1, 2) & dados_ibov_sp500$Ano_t == 1, ]


ggplot(dados_pais_1_e_2_ano_1, aes(x = Empresa, y = ROA, color = as.factor(Pais_k))) +
  geom_point() +
  labs(title = "Gráfico de ROA por Empresa - Ano_t 1",
       x = "Empresa",
       y = "ROA") +
  theme_minimal() +
  facet_wrap(~Pais_k)


ggplot(dados_pais_1_e_2_ano_1, aes(x = as.factor(Pais_k), y = ROA, fill = as.factor(Pais_k))) +
  geom_boxplot() +
  labs(title = "Boxplot de ROA por Pais_k - Ano_t 1",
       x = "Pais_k",
       y = "ROA") +
  theme_minimal()


ggplot(dados_ibov_sp500, aes(x = as.factor(Ano_t), y = ROA, fill = as.factor(Pais_k))) +
  geom_boxplot(position = "dodge") +
  labs(title = "Boxplot de ROA por Ano_t, Colorido por Pais_k",
       x = "Ano_t",
       y = "ROA") +
  theme_minimal()


# Filtrando para Ano_t igual a 1 e 10
dados_filtrados <- dados_ibov_sp500[dados_ibov_sp500$Ano_t %in% c(1, 10), ]

# Selecionar as colunas desejadas
dados_resumidos <- dados_filtrados[, c("Empresa", "ROA", "Pais_k", "Ano_t")]

dados_resumidos <- dados_resumidos %>%
  group_by(Empresa) %>%
  filter(all(c(1, 10) %in% Ano_t)) %>%
  ungroup()
dados_resumidos <- dados_resumidos %>%
  distinct(Empresa, Ano_t, .keep_all = TRUE)

dados_resumidos$ROA_1 <- ifelse(dados_resumidos$Ano_t == 1, dados_resumidos$ROA, NA)
dados_resumidos$ROA_2 <- ifelse(dados_resumidos$Ano_t == 10, dados_resumidos$ROA, NA)
dados_resumidos$Ano_t <- NULL
dados_resumidos <- dados_resumidos %>%
  group_by(Empresa) %>%
  summarize_all(~ ifelse(all(is.na(.)), NA, first(na.omit(.))))

# Remova as linhas duplicadas
dados_resumidos <- dados_resumidos[!duplicated(dados_resumidos$Empresa), ]

ggplot(dados_resumidos, aes(x = ROA_1, y = ROA_2, color = as.factor(Pais_k))) +
  geom_point() +
  labs(title = "Gráfico de Dispersão: ROA_1 vs. ROA_2",
       x = "ROA_1",
       y = "ROA_2") +
  theme_minimal()


empresas_financeiras <- c(
  "Cboe Global Markets, Inc.",
  "CBRE GROUP, INC.",
  "CME GROUP INC.",
  "COGNIZANT TECHNOLOGY SOLUTIONS CORP",
  "HUMANA INC",
  "Intercontinental Exchange, Inc.",
  "MOLSON COORS BEVERAGE CO",
  "NASDAQ, INC.",
  "Paychex, Inc.",
  "QUALCOMM INC/DE",
  "VISA INC.",
  "Willis Towers Watson PLC"
)
dados_ibov_sp500<- dados_ibov_sp500[!(dados_ibov_sp500$DENOM_CIA %in% empresas_financeiras), ]

modelo_nulo_hlm3 <- lme(fixed = ROA ~ 1,
                        random = list(Pais_k = ~1, Empresa = ~1),
                        data = dados_ibov_sp500,
                        method = "REML") # restricted estimation of maximum likelihood (Gelman) deixar computacionalmente menos demandante

#Parâmetros do modelo
summary(modelo_nulo_hlm3)

#Erros-padrão do modelo por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_nulo_hlm3)

# Boxplot do ROA
boxplot(dados_ibov_sp500$ROA, 
        main = "Boxplot do ROA",
        ylab = "ROA",
        col = "lightblue",
        border = "black")









empresas_financeiras_sp500 <- c(
  "VISA INC.", "MASTERCARD INC", "PEPSICO INC", 
  "INTERCONTINENTAL EXCHANGE, INC.", "CME GROUP INC.", 
  "MOODYS CORP", "CENTENE CORP", "CBRE GROUP, INC.", 
  "WILLIS TOWERS WATSON PLC", "HUNT J B TRANSPORT SERVICES INC", 
  "Cboe Global Markets, Inc."
)
dados_edgar_duplicata_sem_financeiras_sp500 <- dados_edgar_concat_duplica[!(dados_edgar_concat_duplica$DENOM_CIA %in% empresas_financeiras_sp500), ]
dados_edgar_duplicata_sem_financeiras_sp500<-dados_edgar_duplicata_sem_financeiras_sp500[!(dados_edgar_duplicata_sem_financeiras_sp500$ROA == 0 | is.infinite(dados_edgar_duplicata_sem_financeiras_sp500$ROA)), ]
dados_edgar_duplicata_sem_financeiras_sp500 <- dados_edgar_duplicata_sem_financeiras_sp500[dados_edgar_duplicata_sem_financeiras_sp500$DENOM_CIA != "Thunder Energies Corp", ]

'''#Remoção de valores extremos:
# 	PLASCAR PARTICIPACOES INDUSTRIAIS S.A.ativo total de 2016 a 2019 muito baixo (~23 000 reais)
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "PLASCAR PARTICIPACOES INDUSTRIAIS S.A.")
#TAUTACHROME INC. 2013 esta com prejuízo com 1 casa decimal a mais que os demais anos:
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "TAUTACHROME INC.")
#GLOBAL DIGITAL SOLUTIONS INC ativo total de 2022 com uma casa decimal a menos(1043)
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "GLOBAL DIGITAL SOLUTIONS INC")
#Appsoft Technologies, Inc.os valores de ativo cairam muito entre 2018 a 2022. O valore de 2022 foi extremamente baixo(6)
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "Appsoft Technologies, Inc.")
#AiXin Life International, Inc. valor de ativo de 2013 muito baixo(42)
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "AiXin Life International, Inc.")
#CANNAPHARMARX, INC. com valores de ativos de 2013 muito baixo (25)
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "CANNAPHARMARX, INC.")
#Veritas Farms, Inc. valores de ativos de 2016 muito baixo (15)
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "Veritas Farms, Inc.")
#Cosmos Health Inc. apresentou valores incoerentes das outras variáveis dentro do df
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "Cosmos Health Inc.")
#	RENOVARE ENVIRONMENTAL, INC. dados incoerentes no df
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "RENOVARE ENVIRONMENTAL, INC.")
#LFTD PARTNERS INC. com valor baixo de ativos em 2016 (605) e zerados em 2017 e 2018:
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "LFTD PARTNERS INC.")
#MMEX Resources Corp com valor baixo de ativos em 2016 (1416) :
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "MMEX Resources Corp")
#INTEGRATED VENTURES, INC. valores baixos de ativos 2012(25), 2013(609) e 2016(8111)
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "INTEGRATED VENTURES, INC.")
#SurgePays, Inc. com valores de ativos baixo em 2013(2794) e 2014(2598)
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "SurgePays, Inc.")
#1847 Holdings LLC por ser holdings
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "1847 Holdings LLC")
#PANACEA LIFE SCIENCES HOLDINGS, INC. valores incoerentes de CTPT (repetidos para 2020, mas com valores diferentes para CTPT):
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "PANACEA LIFE SCIENCES HOLDINGS")
#OMNIQ Corp. o ano de 2012 está com ativo baixo(1005)
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "OMNIQ Corp.")
#Biostax Corp. ativo está com muita oscilação para baixo nos anos de 2015, 2016, 2018, 2019 e 2020
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "Biostax Corp.") 
#Crown Equity Holdings, Inc. baixo ativo em 2014(2987) e 2015(2448)
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "Crown Equity Holdings, Inc.")
#Kun Peng International Ltd. com ativos muito baixo em 2012(53) e 2013(0):
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "Kun Peng International Ltd.")
#MJ Holdings, Inc. ativos incoerentes em 2013(478):
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "MJ Holdings, Inc.")
#PANACEA LIFE SCIENCES HOLDINGS, INC. valores de ativos com muita oscilação (holding?):
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "PANACEA LIFE SCIENCES HOLDINGS, INC.")
#Wendys Co valores incoerentes das outras variaveis
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "Wendy's Co")
#ACACIA RESEARCH CORP valores incoerentes das outras variaveis
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "ACACIA RESEARCH CORP")
#AKAMAI TECHNOLOGIES INC valores incoerentes das outras variaveis
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "AKAMAI TECHNOLOGIES INC")
#Alaunos Therapeutics, Inc.valores incoerentes das outras variaveis
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "Alaunos Therapeutics, Inc.")
#ANGIODYNAMICS INC valores incoerentes das outras variaveis
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "ANGIODYNAMICS INC")
#Apollo Medical Holdings, Inc. valores incoerentes das outras variaveis
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "Apollo Medical Holdings, Inc.")
#ARDELYX, INC.valores incoerentes das outras variaveis
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "ARDELYX, INC.")
#BARRETT BUSINESS SERVICES INC valores incoerentes das outras variaveis
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "BARRETT BUSINESS SERVICES INC")
#BIO KEY INTERNATIONAL INC valores incoerentes das outras variaveis
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "BIO KEY INTERNATIONAL INC")
#BIOCEPT INC valores incoerentes das outras variaveis
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "BIOCEPT INC")
#removendo as observações com os dados incoerentes
dados_edgar_duplicata_sem_financeiras_sp500 <- dados_edgar_duplicata_sem_financeiras_sp500 %>%
  distinct(ANO, DENOM_CIA, .keep_all = TRUE)
#Two Hands Corp valores incoerentes das outras variaveis
dados_edgar_duplicata_sem_financeiras_sp500 <- subset(dados_edgar_duplicata_sem_financeiras_sp500, DENOM_CIA != "Two Hands Corp")'''

