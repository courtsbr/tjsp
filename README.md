# tjsp
Scraper do TJSP (usa esaj e contém funções extras)

## Instalação

```
if (!require(devtools)) install.packages('devtools')
devtools::install_github('abjur/tjsp')
```

## Exemplo de utilização

Exemplo: download de processos das câmaras criminais de São Paulo.

```
library(tjsp)
library(tibble)
library(magrittr)

# Códigos das câmaras criminais.

sec <- list_secoes_2inst() %>%
  dplyr::filter(stringr::str_detect(secao, '[Cc]rim'),
                stringr::str_detect(pai, 'CRIM')) %>%
  with(cod)

# Abre uma sessão e adiciona parâmetros da pesquisa

session <- cjsg_session()
parms <- session %>%
  cjsg_parms(secoes = sec, data_inicial_julg = '2015-01-01', data_final_julg = '2015-12-31')

# número de paginas a serem baixadas
session %>% cjsg_npags(parms)

# Download das páginas HTML (cada página corresponde a 20 decisões)
d_result_cjpg <- session %>%
  cjsg(parms, path = 'data-raw/cjsg', max_pag = 5)

# Parse das páginas HTML em uma `tibble`
arqs_cjpg <- dir('data-raw/cjsg', full.names = TRUE)
d_jurisprudencia <- parse_cjsg(arqs)

d_jurisprudencia
```

Também é possível obter mais informações dos processos, da seguinte forma.

```
# Download dos processos obtidos em páginas HTML
d_result_cposg <- d_jurisprudencia %>%
  distinct(n_processo) %>%
  with(n_processo) %>%
  cposg(path = 'data-raw/cposg')

# Parse das páginas HTML obtidas pelo `cposg`
arqs_cposg <- dir('data-raw/cposg', full.names = TRUE)
d_cposg <- parse_cposg(arqs_cposg)

d_cposg
```
