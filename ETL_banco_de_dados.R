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

################################################################################
#         Adição das variáveis macroeconomicas no banco de dados da CVM        #
################################################################################

#Carregando a base de dados
dados<- read.csv("dados_estruturados_tcc.csv")

names(dados) <- gsub("^X", "", names(dados))

dados[dados == -Inf | dados == Inf] <- NA

# Encontrar as observações onde todas as colunas de 3 a 13 são NA
observacoes_todas_na <- which(rowSums(!is.na(dados[, 4:9])) == 0)

# Exibir a lista de observações onde todas as colunas de 3 a 13 são NA
print(observacoes_todas_na)

# Extrair os CNPJ_CIA das observações onde todas as colunas de 3 a 13 são NA
cnpj_lista <- dados$CNPJ_CIA[observacoes_todas_na]
length(cnpj_lista)

# Exibir a lista de CNPJ_CIA
valores_unicos_cnpj<- (unique(cnpj_lista))

# Remover do dataframe 'dados' as observações com CNPJ_CIA presentes em valores_unicos_cnpj
dados <- subset(dados, !(CNPJ_CIA %in% valores_unicos_cnpj))

length(unique(dados$CNPJ_CIA))

# Criando um novo fator para representar as empresas com base no CNPJ_CIA
dados$Empresa <- as.factor(as.numeric(factor(dados$CNPJ_CIA, levels = unique(dados$CNPJ_CIA))))
dados$Empresa<- as.character(dados$Empresa)

#Visualização da base de dados
dados %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Estatísticas descritivas e tabelas de frequências
summary(dados)


#Estudo sobre o balanceamento dos dados em relação às empresas  
#por indice analisado
dados %>%
  group_by(ANO) %>% 
  summarise(`CNPJ_CIA` = n()) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

#por empresa
dados %>%
  group_by(Empresa) %>% 
  summarise(`CNPJ_CIA` = n()) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

#preparando o banco de dados para o formato necessário para o modelo Nulo:
# Verificando as linhas onde 'CNPJ_CIA' é igual a 'Brasil'
linhas_com_macro <- dados$CNPJ_CIA == 'Brasil'

#Criando um novo df com as variaveis macro
macro <- dados[linhas_com_macro, ]
macro<- macro[, -c(4:8, 10)]


#removendoas variaveis macro dde dados:
dados <-  subset(dados, Empresa != 169)
dados<- dados[, -9]
  
IPCA_valores <- macro$Macro[macro$DENOM_CIA == 'IPCA (%a.a)']
PIB_per_capta<- macro$Macro[macro$DENOM_CIA == 'PIB_per_capta (R$ 2022)']
cambio<- macro$Macro[macro$DENOM_CIA == 'cambio']
divida_liquida_pib<- macro$Macro[macro$DENOM_CIA == 'divida_liquida_pib']
SELIC<- macro$Macro[macro$DENOM_CIA == 'SELIC (%a.a)']
SELIC_over<- macro$Macro[macro$DENOM_CIA == 'SELIC_over (%a.a)']

#adicionando à dados:
# Criar colunas para cada variável
dados$IPCA <- IPCA_valores[match(dados$ANO, unique(macro$ANO))]
dados$PIB_per_capta <- PIB_per_capta[match(dados$ANO, unique(macro$ANO))]
dados$cambio <- cambio[match(dados$ANO, unique(macro$ANO))]
dados$divida_liquida_pib <- divida_liquida_pib[match(dados$ANO, unique(macro$ANO))]
dados$SELIC <- SELIC[match(dados$ANO, unique(macro$ANO))]
dados$SELIC_over <- SELIC_over[match(dados$ANO, unique(macro$ANO))]
dados$`Pais_k` <- 1
dados$`Ano_t` <- match(dados$ANO, unique(dados$ANO))
dados_tcc_conc<- dados

# Obter o diretório de trabalho atual
dir_trabalho <- getwd()

# Nome do arquivo dentro do diretório do projeto
nome_arquivo <- "dados_tcc_conc.csv"

# Caminho completo para o arquivo dentro do diretório do projeto
caminho_arquivo <- file.path(dir_trabalho, nome_arquivo)

# Salvando o DataFrame em um arquivo CSV
write.csv(dados, file = caminho_arquivo, row.names = FALSE)
