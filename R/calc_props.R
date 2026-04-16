calc_props <- function(base, causa, prefix, obito_in, obito_out,
                       sexo_filtro = NULL, idades_filtro = NULL) {

  # Identify target rows (those that will receive redistribution)
  is_target <- base$GBD %in% causa
  if (!is.null(sexo_filtro))   is_target <- is_target & (base$sexo %in% sexo_filtro)
  if (!is.null(idades_filtro)) is_target <- is_target & (base$idade %in% idades_filtro)

  # Non-target rows pass through with obito_out = obito_in (unchanged)
  base_other <- base[!is_target, ] %>% mutate(!!obito_out := .data[[obito_in]])

  base_filt <- base[is_target, ] %>%
    mutate(reg = str_sub(cdmun, 1, 1))
  
  # MUNICÍPIO
  muni <- base_filt %>%
    group_by(cdmun, micro, meso, GBD, idade, ano, sexo, uf) %>%
    summarise(ob = sum(.data[[obito_in]], na.rm = TRUE), .groups = "drop") %>%
    group_by(cdmun, micro, meso, idade, ano, sexo, uf) %>%
    mutate(
      pr.mu = ob / sum(ob, na.rm = TRUE),
      ob.mu = sum(ob, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # MICRORREGIÃO
  micro_df <- base_filt %>%
    group_by(micro, meso, GBD, idade, ano, sexo, uf) %>%
    summarise(ob = sum(.data[[obito_in]], na.rm = TRUE), .groups = "drop") %>%
    group_by(micro, meso, idade, ano, sexo, uf) %>%
    mutate(
      pr.mi = ob / sum(ob, na.rm = TRUE),
      ob.mi = sum(ob, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # MESORREGIÃO
  meso_df <- base_filt %>%
    group_by(meso, GBD, idade, ano, sexo, uf) %>%
    summarise(ob = sum(.data[[obito_in]], na.rm = TRUE), .groups = "drop") %>%
    group_by(meso, idade, ano, sexo, uf) %>%
    mutate(
      pr.me = ob / sum(ob, na.rm = TRUE),
      ob.me = sum(ob, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # UF
  uf_df <- base_filt %>%
    group_by(GBD, idade, ano, sexo, uf) %>%
    summarise(ob = sum(.data[[obito_in]], na.rm = TRUE), .groups = "drop") %>%
    group_by(idade, ano, sexo, uf) %>%
    mutate(
      pr.uf = ob / sum(ob, na.rm = TRUE),
      ob.uf = sum(ob, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # REGIÃO
  reg_df <- base_filt %>%
    group_by(GBD, idade, ano, sexo, reg) %>%
    summarise(ob = sum(.data[[obito_in]], na.rm = TRUE), .groups = "drop") %>%
    group_by(idade, ano, sexo, reg) %>%
    mutate(
      pr.rg = ob / sum(ob, na.rm = TRUE),
      ob.rg = sum(ob, na.rm = TRUE)
    ) %>%
    ungroup()

  # BRASIL (fallback nacional — evita perda de óbitos sem alvo geográfico)
  nat_df <- base_filt %>%
    group_by(GBD, idade, ano, sexo) %>%
    summarise(ob = sum(.data[[obito_in]], na.rm = TRUE), .groups = "drop") %>%
    group_by(idade, ano, sexo) %>%
    mutate(
      pr.nat = ob / sum(ob, na.rm = TRUE),
      ob.nat = sum(ob, na.rm = TRUE)
    ) %>%
    ungroup()

  # JUNTAR PROPORÇÕES
  base_alt <- base_filt %>%
    left_join(muni,     by = c("cdmun","micro","meso","GBD","idade","ano","sexo","uf")) %>%
    left_join(micro_df, by = c("micro","meso","GBD","idade","ano","sexo","uf")) %>%
    left_join(meso_df,  by = c("meso","GBD","idade","ano","sexo","uf")) %>%
    left_join(uf_df,    by = c("GBD","idade","ano","sexo","uf")) %>%
    left_join(reg_df,   by = c("GBD","idade","ano","sexo","reg")) %>%
    left_join(nat_df,   by = c("GBD","idade","ano","sexo"))

  # REDISTRIBUIÇÃO
  base_alt <- base_alt %>%
    mutate(
      "{prefix}.1" := redis * pr.mu,
      redis.2      = ifelse(is.na(.data[[paste0(prefix, ".1")]]) & ob.mu == 0, redis, NA),

      "{prefix}.2" := redis.2 * pr.mi,
      redis.3      = ifelse(is.na(.data[[paste0(prefix, ".2")]]) & ob.mi == 0, redis.2, NA),

      "{prefix}.3" := redis.3 * pr.me,
      redis.4      = ifelse(is.na(.data[[paste0(prefix, ".3")]]) & ob.me == 0, redis.3, NA),

      "{prefix}.4" := redis.4 * pr.uf,
      redis.5      = ifelse(is.na(.data[[paste0(prefix, ".4")]]) & ob.uf == 0, redis.4, NA),

      "{prefix}.5" := redis.5 * pr.rg,
      redis.6      = ifelse(is.na(.data[[paste0(prefix, ".5")]]) & ob.rg == 0, redis.5, NA),

      "{prefix}.6" := redis.6 * pr.nat,

      "{obito_out}" :=
        ifelse(!is.na(.data[[paste0(prefix, ".1")]]), .data[[obito_in]] + .data[[paste0(prefix, ".1")]],
               ifelse(!is.na(.data[[paste0(prefix, ".2")]]), .data[[obito_in]] + .data[[paste0(prefix, ".2")]],
                      ifelse(!is.na(.data[[paste0(prefix, ".3")]]), .data[[obito_in]] + .data[[paste0(prefix, ".3")]],
                             ifelse(!is.na(.data[[paste0(prefix, ".4")]]), .data[[obito_in]] + .data[[paste0(prefix, ".4")]],
                                    ifelse(!is.na(.data[[paste0(prefix, ".5")]]), .data[[obito_in]] + .data[[paste0(prefix, ".5")]],
                                           ifelse(!is.na(.data[[paste0(prefix, ".6")]]), .data[[obito_in]] + .data[[paste0(prefix, ".6")]],
                                                  .data[[obito_in]])))))))

  # Opção 3: redis que falhou em todos os 6 níveis geográficos é acumulado
  # em RedGCSIM_stranded_bin (global) e adicionado ao output SOMENTE ao final
  # da redistribuicao_causas_ivestigacao — sem participar de nenhuma etapa
  # de redistribuição intermediária (evita dupla contagem).
  p_names <- paste0(prefix, c(".1",".2",".3",".4",".5",".6"))
  p_exist  <- p_names[p_names %in% names(base_alt)]

  stranded <- base_alt[!is.na(base_alt$redis) & base_alt$redis > 0, ]
  for (p in p_exist) stranded <- stranded[is.na(stranded[[p]]), ]

  if (nrow(stranded) > 0) {
    gc_rows <- distinct(stranded, cdmun, micro, meso, idade, ano, sexo, uf, c.red, redis) %>%
      mutate(GBD = c.red, !!sym(obito_in) := 0)
    if (exists("RedGCSIM_stranded_bin", envir = .GlobalEnv)) {
      assign("RedGCSIM_stranded_bin",
             bind_rows(get("RedGCSIM_stranded_bin", envir = .GlobalEnv), gc_rows),
             envir = .GlobalEnv)
    } else {
      assign("RedGCSIM_stranded_bin", gc_rows, envir = .GlobalEnv)
    }
  }

  bind_rows(base_alt, base_other)
}
