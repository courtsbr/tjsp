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
    readr::parse_number()
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
#' @param data_inicial_julg data inicial da decisão em formado \%Y-\%m-\%d.
#' @param data_final_julg data final da decisão em formado \%Y-\%m-\%d.
#' @param data_inicial_reg data inicial de registro em formado \%Y-\%m-\%d.
#' @param data_final_reg data final de registro em formado \%Y-\%m-\%d.
#' @param classes códigos das classes.
#' @param assuntos códigos dos assuntos.
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
cjsg_parms <- function(session,
                       livre = '',
                       data_inicial_julg = NULL,
                       data_final_julg = NULL,
                       data_inicial_reg = NULL,
                       data_final_reg = NULL,
                       secoes = '',
                       classes = '',
                       assuntos = '') {
  secoes <- paste(secoes, collapse = ',')
  dt_inicial_julg <- ''
  if (!is.null(data_inicial_julg)) {
    data_inicial_julg <- lubridate::ymd(data_inicial_julg)
    dt_inicial_julg <- sprintf('%02d/%02d/%d',
                               lubridate::day(data_inicial_julg),
                               lubridate::month(data_inicial_julg),
                               lubridate::year(data_inicial_julg))
  }
  dt_final_julg <- ''
  if (!is.null(data_final_julg)) {
    data_final_julg <- lubridate::ymd(data_final_julg)
    dt_final_julg <- sprintf('%02d/%02d/%d',
                             lubridate::day(data_final_julg),
                             lubridate::month(data_final_julg),
                             lubridate::year(data_final_julg))
  }
  dt_inicial_reg <- ''
  if (!is.null(data_inicial_reg)) {
    data_inicial_reg <- lubridate::ymd(data_inicial_reg)
    dt_inicial_reg <- sprintf('%02d/%02d/%d',
                               lubridate::day(data_inicial_reg),
                               lubridate::month(data_inicial_reg),
                               lubridate::year(data_inicial_reg))
  }
  dt_final_reg <- ''
  if (!is.null(data_final_reg)) {
    data_final_reg <- lubridate::ymd(data_final_reg)
    dt_final_reg <- sprintf('%02d/%02d/%d',
                             lubridate::day(data_final_reg),
                             lubridate::month(data_final_reg),
                             lubridate::year(data_final_reg))
  }
  suppressWarnings({
    session %>%
      rvest::html_form() %>%
      dplyr::first() %>%
      rvest::set_values('dados.buscaInteiroTeor' = livre,
                        'secoesTreeSelection.values' = secoes,
                        'dados.dtRegistroInicio' = dt_inicial_reg,
                        'dados.dtRegistroFim' = dt_final_reg,
                        'dados.dtJulgamentoInicio' = dt_inicial_julg,
                        'dados.dtJulgamentoFim' = dt_final_julg,
                        'assuntosTreeSelection.values' = assuntos,
                        'classesTreeSelection.values' = classes)
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
#'
#' @export
cjsg <- function(session, parms, path = './cjsg',
                 max_pag = 10, overwrite = FALSE,
                 verbose = TRUE, p = .05) {
  suppressWarnings(dir.create(path, recursive = TRUE))
  if (!file.exists(path)) stop(sprintf('Pasta não "%s" pôde ser criada', path))
  r0 <- session %>% rvest::submit_form(parms)
  n_pags <- if (is.na(max_pag) || is.infinite(max_pag)) cjsg_npags(r0) else max_pag
  abjutils::dvec(cjsg_pag, 1:n_pags, path = path, ow = overwrite, s = session)
}
