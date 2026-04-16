#' Separar Registros Ignorados
#'
#' Esta função separa registros ignorados (campos em branco ou inválidos) de uma base de dados do SIM.
#' Os registros ignorados incluem casos onde o município (`cdmun`), sexo ou idade estão ausentes ou marcados como valores inválidos.
#'
#' @param x Data frame contendo os dados do SIM, incluindo colunas como `cdmun`, `idade`, `sexo` e outras relevantes.
#' @return Uma lista com duas bases de dados:
#' \itemize{
#'   \item `ignorados`: Data frame com registros que possuem campos ignorados (município inválido, sexo ou idade ausente).
#'   \item `completos`: Data frame com registros válidos, sem campos ignorados.
#' }
#' @examples
#' \dontrun{
#' # Exemplo de uso com dados fictícios
#' dados <- data.frame(
#'   cdmun = c("110001", "120001", "999999"),
#'   idade = c(30, 999, "IGN"),
#'   sexo = c("Masculino", "Feminino", "IGN"),
#'   obitos = c(5, 10, 2)
#' )
#'
#' resultado <- separa_reg_ing(dados)
#' head(resultado$ignorados)
#' head(resultado$completos)
#' }
#' @export

separa_reg_ing = function(x){
  if (!require("pacman")) install.packages("pacman") #garantir que o pacman está instalado
  pacman::p_load(tidyverse) # pacotes necessári
  `%notin%` <- Negate(`%in%`)

  #Códigos para municípios ignorados
  notwantedlevels <- c(110000,120000,130000,140000, 150000, 160000, 170000, 210000, 220000,
                       230000,240000,250000,260000,270000,280000,290000, 310000,320000,330000,
                       350000, 410000,420000,430000,500000,510000,520000,000000)

  ##### Dados SIM IGNORADOS

  ign <- x%>%
    ungroup() %>%
    mutate(idade=ifelse(idade ==999,'IGN',idade))%>%
    filter(cdmun %in% notwantedlevels | sexo=='IGN' | idade =='IGN')
  colnames(ign)[8] <- 'ign'

  print(paste0("total de registros com campos em branco: ",sum(ign$ign))) # total de registros com campos ignorados

  ##### Dados SIM completos
  dados_completos <- x %>%
    filter(cdmun %notin% notwantedlevels & sexo!='IGN' & idade!=999 & idade!='IGN')

  if ((sum(nrow(dados_completos),nrow(ign)))==nrow(x)) print("Todos os registros estão separados nas bases de ignorados ou dados completos")
  total_obitos_x   <- sum(x$obitos, na.rm = TRUE)
  total_obitos_out <- sum(dados_completos$obitos, na.rm = TRUE) + sum(ign$ign, na.rm = TRUE)
  if (total_obitos_out == total_obitos_x) {
    print(paste0("Conservação de óbitos OK: ", total_obitos_x, " óbitos"))
  } else {
    warning(paste0("DIVERGÊNCIA de óbitos em separa_reg_ing! Entrada: ", total_obitos_x,
                   " | Saída: ", total_obitos_out,
                   " | Diferença: ", total_obitos_out - total_obitos_x))
  }

  out.file <- list(ignorados = ign, completos = dados_completos)

  return(out.file)
}
