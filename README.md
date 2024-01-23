# TCC---MBA_USP_-DSA
Neste repositório estão contidos os códigos utilizados no desenvolvimento de meu TCC em Data Science e Analytics
# Desempenho das Empresas de Capital Aberto no Brasil: Uma Análise de Modelo Multinível de Coeficientes Aleatórios e Variáveis Macroeconômicas 

Os dados das empresas listadas na B3 foram obtidos pela confluência entre o arquivo que lista as empresas da B3 e a Denominação social da empresas nos relatórios financeiros da CVM. As empresas norte americanas foram obtidas com a utilização da API EDGAR.
CVM: http://dados.cvm.gov.br/dados/CIA_ABERTA/DOC/DFP/DADOS
EDGAR: https://www.sec.gov/files/company_tickers.json
download da lista empresas B3: não foi possivel obter a lista das por web scraping, então foi criado um documento com os elementos de tabela provenientes do web site da B3 (https://sistemaswebb3-listados.b3.com.br/shareCapitalPage/?language=pt-br, acessado em 17/10/2023)

## Instalação

Todas as bibliotecas estão disponíveis no código.
Para acesso ao Edgar, é neessário entrar com o header, que é o email pessoal.


## Uso

Para utilzação do projeto, deverá se utilizar o arquivo "Obtenção de Dados CVM/B3_TCC.ipynb" para a obtenção dos dados das empresas da CVM/B3 e ETL do mesmo. Depois deverá ser utilizado o arquivo "CIA_tcc_EDGAR.ipynb" para obtenção dos dados das empresas americanas, ETL, e unificação dos dois dfs. Para a estimação do modelo deverá ser utilizado o project em R "hlm3_tcc".
Alem do header da API Edgar, os demais campos que será necessário alteração serão os caminhos para salvar e obter os arquivos já salvos. 

## Contribuição

Contribuições são bem vindas!  

## Licença

Este projeto poderá ser utilizado pela comunidade acadêmica ou para projetos vinculados à educação.
Este projeto está licenciado sob a Licença Creative Commons Attribution-NoDerivs 4.0 International (CC BY-ND 4.0) - veja o arquivo [LICENSE](LICENSE) para detalhes.

## Contato

lisboaisabella9@gmail.com

