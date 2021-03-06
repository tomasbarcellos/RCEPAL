#' Uma API para os dados abertos da CEPAL
#'
#' Esta função faz download das séries da CEPAL
#' @param id_serie ID da série que será baixada. Para conhecer as séries disponíveis e seus IDs, use a função series_CEPAL().
#' @param exportar Os dados devem ser exportados? Neste caso o nome de um arquivo deve ser fornecido e o retorno será NULL. Padrão: FALSE
#' @param arquivo O nome do arquivo que será criado caso exportar = TRUE. Padrão: ""
#' @param silencioso O progresso deve ser informado? Padrão: FALSE
#' @keywords CEPAL dados
#' @export
#' @examples
#' termos_troca <- CEPAL_stat(883)

CEPAL_stat <- function(id_serie, exportar = FALSE,
                       arquivo = "", silencioso = FALSE) {

  if(!require(rvest) | !require(tidyr) | !require(dplyr)) {
    stop("Precisa dos pacotes 'rvest', 'tidyr' e 'dplyr'.")
  }
  if (exportar == TRUE & arquivo == "") {
    stop("Por favor, informe o nome do arquivo para que os dados sejam exportados")
  }
  if (silencioso == FALSE) {
    cat("Realizando download dos dados: ")
  }

  serie <- paste0("https://estadisticas.cepal.org/sisgen/ws/cepalstat/getDataWithoutMeta.asp?IdIndicator=",
                  id_serie, "&language=spanish")
  pagina <- read_xml(serie)

  lista <- pagina %>% xml_find_all("//dato") %>% xml_attrs()
  dados_m <- do.call(rbind, lista)
  dados <- as.data.frame(dados_m, stringsAsFactors = FALSE)

  dims <- grep("dim_", names(dados))

  n_dim <- pagina %>% xml_find_all("//info") %>% xml_attr('numeroDimensiones') %>% as.numeric()

  n_dados <- pagina %>% xml_find_all("//info") %>% xml_attr('numeroDatos') %>% as.numeric()

  if(nrow(dados) != n_dados | length(dims) != n_dim) {
    stop("Houve erro na compilação dos dados")
  }

  if (silencioso == FALSE) {
    cat("Pronto!\n")
  }

  # dicionario

  if (silencioso == FALSE) {
    cat("Realizando download do dicionário: ")
  }

  url <- paste0("https://estadisticas.cepal.org/sisgen/ws/cepalstat/getDimensions.asp?idIndicator=",
                id_serie,"&language=spanish")

  pagina_dic <- read_xml(url)

  # Dimensoes
  dic_lista <- pagina_dic %>% xml_find_all("//des") %>% xml_attrs()
  dic_m <- do.call(rbind, dic_lista)
  dicionario <- as.data.frame(dic_m, stringsAsFactors = FALSE)
  dicionario <- dicionario[, c('name', 'id')]

  nomes_dimensoes <- pagina_dic %>% xml_find_all("//dim") %>% xml_attr('name')

  id_dimensoes <- pagina_dic %>% xml_find_all("//dim") %>% xml_attr('id')

  # numero de filhos
  netos <- sapply(seq_len(n_dim),
                  function(x) pagina_dic %>% xml_child(x) %>% xml_length())

  id_dimensoes_lista <- vector("list", length(n_dim))

  for (i in seq_len(n_dim)) {
    id_dimensoes_lista[[i]] <- rep(id_dimensoes[i], times = netos[i])
  }; rm(i)

  if (silencioso == FALSE) {
    cat("Pronto!\n")
  }

  dimencao <- do.call(c, id_dimensoes_lista)

  dicionario_ok <- cbind(dimencao, dicionario,
                         stringsAsFactors = FALSE)

  dicionarios_lista <- vector('list', n_dim)
  resultado <- dados
  names(resultado)[seq_len(n_dim)] <- nomes_dimensoes

  if (silencioso == FALSE) {
    cat("Unindo dados ao dicionário: ")
  }

  for (i in seq_len(n_dim)) {
    dicionarios_lista[[i]] <- dicionario_ok %>%
      filter(dimencao == unique(dimencao)[i]) %>%
      select(id, name)

    names(dicionarios_lista[[i]])[1] <- names(resultado)[i]

    resultado <- left_join(resultado, dicionarios_lista[[i]],
                           by = names(resultado)[i])
    names(resultado)[length(resultado)] <- paste0(names(resultado)[i], "_desc")
  }

  resultado$valor <- as.numeric(resultado$valor)

  if ("Años_desc" %in% names(resultado)) {
    resultado$Años_desc <- as.numeric(resultado$Años_desc)
  }

  if (silencioso == FALSE) {
    cat("Pronto!\n")
  }

  if (exportar == TRUE) {
    write.csv2(x = resultado, file = arquivo,
               row.names = FALSE, fileEncoding = "UTF-8")

    return(NULL)
  }

  return(resultado)

}
