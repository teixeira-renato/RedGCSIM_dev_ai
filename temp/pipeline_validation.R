options(scipen=999)  # desativa notação científica

devtools::load_all('.')
library(tidyverse)

load('temp/sim_2023.rda')
sim_2023 <- out.file
TOTAL_ESP <- nrow(sim_2023)
cat(sprintf('=== Pipeline de Validação ===\nTotal registros brutos: %s\n\n', format(TOTAL_ESP, big.mark=',')))

chk <- function(label, val, exp=NA, digits=0) {
  diff <- if (!is.na(exp)) val - exp else NA
  flag <- if (!is.na(diff)) ifelse(abs(diff) < 0.5, '[OK]', sprintf('[DIVERGÊNCIA: %+.0f]', diff)) else ''
  cat(sprintf('  %-45s obt=%s%s\n',
              label,
              format(round(val, digits), big.mark=',', nsmall=digits),
              if (!is.na(exp)) sprintf('  esp=%s  %s', format(round(exp, digits), big.mark=',', nsmall=digits), flag) else ''))
}

# =========================================
# STEP 1: padroniza_idade
# =========================================
cat('=== Etapas iniciais ===\n')
out.file1 <- padroniza_idade(sim_2023)
chk('1 padroniza_idade: nrow', nrow(out.file1), TOTAL_ESP)

# =========================================
# STEP 2: padroniza_local
# =========================================
out.file2 <- padroniza_local(out.file1)
chk('2 padroniza_local: nrow', nrow(out.file2), TOTAL_ESP)

# =========================================
# STEP 3: tabela_final_1
# =========================================
out.file3 <- tabela_final_1(out.file2)
chk('3 tabela_final_1: sum(obitos)', sum(out.file3$obitos), TOTAL_ESP)

# =========================================
# STEP 4: separa_reg_ing
# =========================================
out.file4 <- separa_reg_ing(out.file3)
completos_ing <- sum(out.file4$completos$obitos)
ign_ing       <- sum(out.file4$ignorados$ign)
chk('4 separa_reg_ing: completos+ignorados', completos_ing + ign_ing, TOTAL_ESP)
cat(sprintf('     completos: %s | ignorados: %s\n',
            format(round(completos_ing), big.mark=','),
            format(round(ign_ing), big.mark=',')))

save(out.file4, file='temp/checkpoint4.rda')
cat('  [checkpoint4 saved]\n')

# =========================================
# STEP 5: prepara_base_generalizada
# =========================================
cat('\n=== Etapa 5: prepara_base_generalizada ===\n')
out.file5 <- prepara_base_generalizada(out.file4[['completos']])

excluidos_5 <- if (exists("RedGCSIM_excluidos_base1")) sum(RedGCSIM_excluidos_base1$obitos, na.rm=TRUE) else 0
completos_efetivos <- completos_ing - excluidos_5
chk('5 prepara_base_gen: sum(obitos)', sum(out.file5$obitos, na.rm=TRUE), completos_efetivos)
if (excluidos_5 > 0) {
  cat(sprintf('     excluídos por inconsistência: %s óbito(s)\n', format(excluidos_5, big.mark=',')))
  print(RedGCSIM_excluidos_base1[, intersect(c("GBD","idade","sexo","obitos","motivo_exclusao"), names(RedGCSIM_excluidos_base1))])
}

save(out.file5, file='temp/checkpoint5.rda')
cat('  [checkpoint5 saved]\n')

# =========================================
# STEP 6: prop_causas
# =========================================
cat('\n=== Etapa 6: prop_causas ===\n')
out.file6 <- prop_causas(out.file5)
chk('6 prop_causas: sum(obitos)', sum(out.file6$obitos, na.rm=TRUE), completos_efetivos)

save(out.file6, file='temp/checkpoint6.rda')
cat('  [checkpoint6 saved]\n')

# =========================================
# STEP 7: redistribuicao_dados_faltantes
# =========================================
cat('\n=== Etapa 7: redistribuicao_dados_faltantes ===\n')
out.file7 <- redistribuicao_dados_faltantes(base_prop=out.file6, dados_ign=out.file4[['ignorados']])
chk('7 redist_faltantes: sum(obitos.2)', sum(out.file7$obitos.2, na.rm=TRUE), TOTAL_ESP - excluidos_5)

save(out.file7, file='temp/checkpoint7.rda')
cat('  [checkpoint7 saved]\n')

# =========================================
# STEP 8: separa_reg_GC
# =========================================
cat('\n=== Etapa 8: separa_reg_GC ===\n')
out.file8 <- separa_reg_GC(out.file7)

ob2_completos <- sum(out.file8$completos$obitos.2, na.rm=TRUE)
ob2_redis     <- sum(out.file8$redistribuir$redis, na.rm=TRUE)
ob2_covid     <- sum(base_covid$obitos.2, na.rm=TRUE)
# _pneumo is in both completos AND redis (by design), so completos+redis > total
ob2_total_naive <- ob2_completos + ob2_redis + ob2_covid
cat(sprintf('  completos(obitos.2): %s | redis(GC): %s | covid: %s\n',
            format(round(ob2_completos), big.mark=','),
            format(round(ob2_redis), big.mark=','),
            format(round(ob2_covid), big.mark=',')))
ob2_check <- if (abs(ob2_total_naive - TOTAL_ESP + 1) < 1) '[OK — conservação esperada]' else sprintf('[DIVERGÊNCIA: %+.0f]', ob2_total_naive - (TOTAL_ESP - 1))
cat(sprintf('  completos+redis+covid=%s  esp=%s  %s\n',
            format(round(ob2_total_naive), big.mark=','),
            format(TOTAL_ESP - 1, big.mark=','),
            ob2_check))

save(out.file8, file='temp/checkpoint8.rda')
cat('  [checkpoint8 saved]\n')

# =========================================
# STEP 9: redistribuicao_causas_externas (obitos.2 -> obitos.7)
# =========================================
cat('\n=== Etapa 9: redistribuicao_causas_externas ===\n')
out.file9 <- redistribuicao_causas_externas(dados_completos=out.file8[['completos']],
                                             dados_redis=out.file8[['redistribuir']])
for (col in paste0('obitos.', c(3,4,5,6,7))) {
  if (col %in% names(out.file9)) {
    chk(sprintf('  9 externas: sum(%s)', col), sum(out.file9[[col]], na.rm=TRUE))
  }
}
chk('  9 externas: sum(obitos.7)', sum(out.file9$obitos.7, na.rm=TRUE))

save(out.file9, file='temp/checkpoint9.rda')
cat('  [checkpoint9 saved]\n')

# =========================================
# STEP 10: redistribuicao_causas_mat_inf (obitos.7 -> obitos.9)
# =========================================
cat('\n=== Etapa 10: redistribuicao_causas_mat_inf ===\n')
out.file10 <- redistribuicao_causas_mat_inf(dados_completos=out.file9,
                                              dados_redis=out.file8[['redistribuir']])
chk('  10 mat_inf: sum(obitos.8)', sum(out.file10$obitos.8, na.rm=TRUE))
chk('  10 mat_inf: sum(obitos.9)', sum(out.file10$obitos.9, na.rm=TRUE))

save(out.file10, file='temp/checkpoint10.rda')
cat('  [checkpoint10 saved]\n')

# =========================================
# STEP 11: redistribuicao_causas_ivestigacao (obitos.9 -> obitos.13)
# =========================================
cat('\n=== Etapa 11: redistribuicao_causas_ivestigacao ===\n')
out.file11 <- redistribuicao_causas_ivestigacao(dados_completos=out.file10,
                                                  dados_redis=out.file8[['redistribuir']],
                                                  total_esp=TOTAL_ESP)
for (col in paste0('obitos.', 10:13)) {
  if (col %in% names(out.file11)) {
    chk(sprintf('  11 investig: sum(%s)', col), sum(out.file11[[col]], na.rm=TRUE))
  }
}

save(out.file11, file='temp/checkpoint11.rda')
cat('  [checkpoint11 saved]\n')

# =========================================
# FINAL CONSERVATION CHECK
# =========================================
cat('\n=== VALIDAÇÃO FINAL ===\n')
final_sum <- sum(out.file11$obitos.13, na.rm=TRUE)

chk('FINAL sum(obitos.13)', final_sum, TOTAL_ESP)

if (excluidos_5 > 0) {
  cat(sprintf('  (inclui %s óbito(s) restaurados de RedGCSIM_excluidos_base1)\n',
              format(excluidos_5, big.mark=',')))
}

cat('\nDone.\n')









# 1. Ver o que o DEBUG do step 11 reportou (já foi impresso durante a execução)
#    Procure por linhas [DEBUG] no output acima

# 2. Checar se há linhas GC em out.file11 com obitos.13 > 0
gc_codes <- unique(RedGCSIM::ICD$CLASS_GPEAS_PRODUCAO)[
  grepl("^_", unique(RedGCSIM::ICD$CLASS_GPEAS_PRODUCAO))
]
out.file11 %>%
  filter(GBD %in% gc_codes, obitos.13 > 0) %>%
  select(GBD, idade, sexo, ano, obitos.13) %>%
  arrange(desc(obitos.13))

# 3. Verificar se stranded_bin ainda existe (não deveria)
exists("RedGCSIM_stranded_bin")

# 4. Contar quantas linhas gc_restore foram adicionadas ao final
# (checar pelo DEBUG: "[DEBUG] gc_restore nrow=...")
gc_restore_count <- sum(grepl("\\[DEBUG\\] gc_restore nrow=", capture.output(out.file11)))




# Total de óbitos em linhas GC no output final
out.file11 %>%
  filter(GBD %in% gc_codes) %>%
  summarise(n_linhas = n(), sum_obitos13 = sum(obitos.13, na.rm=TRUE))

# Separar _pneumo (esperado) dos demais GC codes
out.file11 %>%
  filter(GBD %in% gc_codes, GBD != "_pneumo", obitos.13 > 0) %>%
  select(GBD, idade, sexo, ano, cdmun, obitos.13)

# Verificar se há linhas com obitos.13 = NA em GC codes
out.file11 %>%
  filter(GBD %in% gc_codes, is.na(obitos.13)) %>%
  summarise(n = n(), sum_redis = sum(redis, na.rm=TRUE))



df_cod <- out.file11 %>%
  group_by(GBD) %>%
  summarise(obitos_f=sum(obitos.13,na.rm=T)) %>%
  arrange((GBD))

df_cod

sum(df_cod$obitos_f)



# Verificar redis total que entrou no pipeline vs. o que foi distribuído
redis_total <- sum(out.file8$redistribuir$redis, na.rm=TRUE)
cat("Redis total entrada:", redis_total)
cat("obitos.13 - obitos.2 (redistribuído):",
    sum(out.file11$obitos.13, na.rm=TRUE) - sum(out.file7$obitos.2, na.rm=TRUE))
