parse_cjsg_um <- function(i, nodes) {
  node <- nodes[[i]]
  trim <- stringr::str_trim
  id <- node %>%
    rvest::html_node('.ementaClass') %>%
    rvest::html_text() %>%
    trim() %>%
    stringr::str_replace_all('[^0-9]', '')
  infos <- node %>%
    rvest::html_node('.downloadEmenta') %>% {
      tibble::tibble(n_processo = trim(rvest::html_text(.)),
                     cd_acordao = rvest::html_attr(., 'cdacordao'))
    }
  ca <- node %>%
    rvest::html_node('.assuntoClasse') %>%
    rvest::html_text() %>%
    trim()
  tsf <- node %>%
    rvest::html_node('textarea') %>%
    rvest::html_text()
  tab_infos <- node %>%
    rvest::html_nodes('.ementaClass2') %>%
    rvest::html_text() %>%
    stringr::str_split_fixed(':', 2) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    magrittr::set_names(c('key', 'val')) %>%
    dplyr::mutate_all(dplyr::funs(trim(.))) %>%
    dplyr::mutate(key = tolower(abjutils::rm_accent(key)),
                  key = stringr::str_replace_all(key, ' +', '_'),
                  key = stringr::str_replace_all(key, '[^a-z_]', ''),
                  key = stringr::str_replace_all(key, '_d[eo]_', '_')) %>%
    tidyr::spread(key, val) %>%
    dplyr::bind_cols(infos) %>%
    dplyr::mutate(id = id, classe_assunto = ca, txt_ementa = tsf) %>%
    dplyr::select(id, cd_acordao, n_processo, dplyr::everything(), txt_ementa)
  tab_infos
}

parse_cjsg_arq <- function(arq) {
  itens <- xml2::read_html(arq, encoding = 'UTF-8') %>%
    rvest::html_nodes('.fundocinza1')
  abjutils::dvec(parse_cjsg_um, 1:length(itens), nodes = itens, verbose = FALSE) %>%
    dplyr::select(-item)
}

#' @export
parse_cjsg <- function(arqs) {
  abjutils::dvec(parse_cjsg_arq, arqs) %>%
    dplyr::rename(arq = item)
}
