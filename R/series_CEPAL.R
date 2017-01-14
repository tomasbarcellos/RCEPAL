#' Uma API para os dados abertos da CEPAL
#'
#' Esta função retorna uma tabela com o nome e id de todas as series disponíveis nos dados abertos da CEPAL
#' @keywords CEPAL dados
#' @export
#' @examples
#' series <- series_CEPAL()

series_CEPAL <- function() {
  if(!require(rvest)) stop("Precisa do pacote 'rvest' para que esta função funcione.")
  url <- "http://interwp.cepal.org/sisgen/ws/cepalstat/getThematicTree.asp?language=spanish"
  pagina <- read_xml(url)

  lista <- pagina %>% xml_find_all("//item") %>% xml_attrs()
  dados_m <- do.call(rbind, lista)
  dados <- as.data.frame(dados_m, stringsAsFactors = FALSE)

  indicadores <- pagina %>% xml_find_all("//item") %>%
    xml_attr("idIndicator") %>% is.na() %>% `!`()

  if (length(indicadores) != nrow(dados))
    cat("Houve um erro! Número de linha é diferente do esperado")

  dados <- dados[indicadores, ]
  names(dados) <- c("nome_serie", "id_serie")
  dados$id_serie <- as.numeric(dados$id_serie)

  return(dados)

}
