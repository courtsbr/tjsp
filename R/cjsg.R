#' Sessão da CJSG
#'
#' Abre uma sessão da Consulta de Julgados de Segundo Grau.
#'
#' @return objeto de classe \code{session}.
#'
#' @export
cjsg_session <- function() {
  rvest::html_session('http://esaj.tjsp.jus.br/cjsg/resultadoCompleta.do')
}

#' Número de páginas
#'
#' Calcula o número de páginas retornadas por uma consulta de julgados de segundo grau.
#'
#' @param session sessão da CJSG.
#' @param parms se for \code{NULL}, admite que já está na página de resultados da CJSG.
#'
#' @return objeto de classe \code{session}.
#'
#' @examples
#' \dontrun{
#' library(tjsp)
#' s <- cjsg_session()
#' parms <- cjsg_parms(s, livre = 'acordam')
#' cjsg_npags(s, parms)
#' cjsg(s, parms, max_pag = 1, path = '.') # chamada internamente dentro de cjsg.
#' }
#'
#' @export
cjsg_npags <- function(session, parms = NULL) {
  if (!is.null(parms)) session <- session %>% rvest::submit_form(parms)
  num <- session$response %>%
    httr::content('text') %>%
    xml2::read_html() %>%
    rvest::html_node('#nomeAba-A') %>%
    rvest::html_text() %>%
    tidyr::extract_numeric()
  (num %/% 20) + 1
}

#' Parâmetros de pesquisa
#'
#' Inclui parâmetros de pesquisa da CJPG. Ainda falta adicionar comarcas, magistrados, classe e assunto na versão oficial do pacote.
#'
#' O intervalo de datas, caso seja incluído, precisa ser de no máximo um ano.
#'
#' @param session sessão da CJSG.
#' @param livre string com pesquisa livre por palavras-chave.
#' @param data_inicial data inicial da decisão em formado \%Y-\%m-\%d.
#' @param data_final data final da decisão em formado \%Y-\%m-\%d.
#' @param secoes vetor com as secoes que se deseja pesquisar. Obter o vetor de \code{\link{list_secoes_2inst}}.
#'
#' @return objeto de classe \code{form}.
#'
#' @examples
#' \dontrun{
#' library(tjsp)
#' s <- cjsg_session()
#' sec <- head(list_secoes_2inst(), 10)$cod
#' parms <- cjsg_parms(s, livre = 'acordam', data_inicial = '2015-01-01', data_final = '2015-05-01', secoes = sec)
#' parms
#' cjsg(s, parms, max_pag = 1, path = '.')
#' }
#' @export
cjsg_parms <- function(session, livre = '', data_inicial = NULL, data_final = NULL, secoes = '') {
  secoes <- paste(secoes, collapse = ',')
  dt_inicial <- ''
  if (!is.null(data_inicial)) {
    dt_inicial <- sprintf('%02d/%02d/%d', lubridate::day(data_inicial),
                          lubridate::month(data_inicial),
                          lubridate::year(data_inicial))
  }
  dt_final <- ''
  if (!is.null(data_final)) {
    dt_final <- sprintf('%02d/%02d/%d', lubridate::day(data_final),
                        lubridate::month(data_final),
                        lubridate::year(data_final))
  }
  suppressWarnings({
    session %>%
      rvest::html_form() %>%
      dplyr::first() %>%
      rvest::set_values('dados.buscaInteiroTeor' = livre,
                        'secoesTreeSelection.values' = secoes,
                        'dados.dtJulgamentoInicio' = dt_inicial,
                        'dados.dtJulgamentoFim' = dt_final)
  })
}

#' Baixa uma página.
#'
#' Baixa uma página a partir de um resultado da CJSG. Não deve ser usado diretamente.
#'
#' @param pag número da página a ser baixada.
#' @param path pasta em que o arquivo será salvo.
#' @param ow logical, sobrescrever arquivo?
#' @param s sessao da CJSG.
#'
cjsg_pag <- function(pag, path, ow, s) {
  Sys.sleep(1) # precisa esperar pois o servidor bloqueia IP.
  u <- 'http://esaj.tjsp.jus.br/cjsg/trocaDePagina.do?tipoDeDecisao=A&pagina=%d'
  u_pag <- sprintf(u, pag)
  arq <- sprintf('%s/%05d.html', path, pag)
  if (!file.exists(arq) || ow) {
    httr::GET(sprintf(u, pag), httr::write_disk(arq, overwrite = ow), handle = s$handle)
    tibble::data_frame(result = 'OK')
  } else {
    tibble::data_frame(result = 'já existe')
  }
}

#' Consulta de Julgados de Segundo Grau
#'
#' Baixa arquivos HTML correspondentes a uma busca na CJSG.
#'
#' @param session sessão da CJSG, retornado pela função \code{\link{cjsg_session}}.
#' @param parms form com parâmetros de busca, retornado pela função \code{\link{cjsg_parms}}.
#' @param path caminho onde os arquivos HTML serão baixados. Tentará criar a pasta automaticamente.
#' @param max_pag número máximo de páginas a serem baixadas. Se \code{NA} ou \code{Inf}, baixará todas as páginas resultantes da pesquisa.
#' @param overwrite sobrescrever arquivos salvos?
#' @param verbose logical: imprimir mensagens de acompanhamento do download?
#' @param p probabilidade de imprimir uma mensagem de acompanhamento se \code{verbose} for \code{TRUE}. Default 5\%.
cjsg <- function(session, parms, path = './cjsg',
                 max_pag = 10, overwrite = FALSE,
                 verbose = TRUE, p = .05) {
  suppressWarnings(dir.create(path, recursive = TRUE))
  if (!file.exists(path)) stop(sprintf('Pasta não "%s" pôde ser criada', path))
  r0 <- session %>% rvest::submit_form(parms)
  n_pags <- if (is.na(max_pag) || is.infinite(max_pag)) cjsg_npags(r0) else max_pag
  abjutils::dvec(cjsg_pag, 1:n_pags, path = path, ow = overwrite, s = session)
}
