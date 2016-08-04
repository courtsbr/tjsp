#' Seções de segunda instância.
#'
#' Baixa lista das seções de segunda instância.
#'
#' @return \code{tibble} com as colunas \code{pai}, \code{secao} e \code{cod}. Usualmente \code{cod} será usado na função \code{\link{cjsg_parms}}
#'
#' @export
list_secoes_2inst <- function() {
  u <- 'https://esaj.tjsp.jus.br/cjsg/secaoTreeSelect.do?campoId=secoes'
  xp_child <- './/li[@class="leafItem"]//span[contains(@id, "secoes_tree")]'
  xp_parent <- './/span[contains(@id, "secoes_tree")]'
  u %>%
    httr::GET(httr::config(ssl_verifypeer = FALSE)) %>%
    httr::content('text') %>%
    xml2::read_html() %>%
    rvest::html_nodes('li.open') %>%
    lapply(function(x) {
      pai <- x %>% rvest::html_node(xpath = xp_parent) %>% rvest::html_text()
      x %>%
        rvest::html_nodes(xpath = xp_child) %>%
        purrr::map(~tibble::tibble(
          cod = rvest::html_attr(.x, 'value'),
          secao = rvest::html_text(.x))
        ) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(pai = pai)
    }) %>%
    dplyr::bind_rows() %>%
    dplyr::select(pai, secao, cod)
}

