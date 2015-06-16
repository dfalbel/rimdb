#' Função que cria a query de pesquisa
#'
#'@param q palavras chave pesquisadas
#'@param t tipo da pesquisa ("e" pesquisa por titulo extao, "ne" pode fazer aproximações)
#'
imdb_criar_query <- function(q, t = "e"){
  base_url <- "http://www.imdb.com.br/find?q="
  q <- str_replace_all(q, " ", "+")
  query <- paste0(base_url, q, "&s=tt")
  if (t == "e"){
    query <- paste0(query, "&exact=true&ref_=fn_tt_ex")
  } else if (t == "ne"){
  } else {
    stop("Não é um tipo")
  }

  return(query)
}


#' Função que retorna o resultado da pesquisa (formato html)
#'
#'@param q palavras chave pesquisadas
#'@param t tipo da pesquisa ("e" pesquisa por titulo extao, "ne" pode fazer aproximações)
#'
imdb_aux_pesquisa <- function(q, t = "e"){
  query <- imdb_criar_query(q, t)
  pesquisa <- html(query)
  return(pesquisa)
}

#' Retorna lista com resultado das palavras pesquisadas
#'
#'@param p pesquisa retornada pela função imdb_pesquisa
#'
#'
imdb_parse_pesquisa <- function(p){

  titulo <- p %>%
    html_nodes(".result_text") %>%
    html_text

  tts <- p %>%
    html_nodes(".result_text") %>%
    html_node("a") %>%
    html_attr("href")

  return(
    data.frame(
      titulo = titulo,
      tt = tts,
      stringsAsFactors = F
    )
  )
}

#' Função que retorna o resultado da pesquisa (formato html)
#'
#'@param q palavras chave pesquisadas
#'@param t tipo da pesquisa ("e" pesquisa por titulo extao, "ne" pode fazer aproximações)
#'
#'@export
imdb_pesquisa <- function(q, t = "e"){
  imdb_aux_pesquisa(q, t) %>% imdb_parse_pesquisa
}



#' Retorna as infos de um filme (usando o tt)
#'
#'@param tt codigo de um filme no imdb
#'
imdb_infos_tt <- function(tt){
  base_url <- "http://www.imdb.com.br"
  url <- paste0(base_url, tt) %>% html


  titulo <- url %>%
    html_node(".itemprop") %>%
    html_text

  duracao <- url %>%
    html_node("time") %>%
    html_text %>%
    str_trim

  categorias <- url %>%
    html_node(".infobar") %>%
    html_nodes(".itemprop") %>%
    html_text


  # criar codigo para pegar
  # ano, diretor, e atores principais

  return(
    data.frame(tt = tt,
               titulo = titulo,
               duracao = duracao,
               categorias = list(categorias),
               stringsAsFactors = F
               )
    )
}

