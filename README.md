# CreditDesmat

Pipeline em R para cruzar credito rural (SICOR) com desmatamento (PRODES) por
imovel (CAR), reproduzindo a regra de borda interna de 60m e gerando outputs
descritivos, agregados por municipio e um dashboard Shiny.

## Estrutura

```
master.R                                 -- orquestra o pipeline
r2c/
  1_prepare_sicor_main.R                 -- consolida SICOR anual
  2_merge_comp_basic.R                   -- + municipio + IPCA + ano_safra
  3_cross_prodes_car.R                   -- PRODES x CAR (regra da borda)
                                            + AUDITORIA do desmat por CAR
  4_municipal_long_output.R              -- output agregado long (item 4)
built/
  sicor_per_car.R                        -- SICOR x CAR + status_car/faixa_mf
desciptive.R / descriptive_2.R           -- tabelas + graficos finais
sicor_montly.R                           -- analise mensal auxiliar
INPEs_exersice_altamira.R                -- exploracao espacial p/ Altamira
shiny/
  app.R                                  -- dashboard Shiny (item 5)
```

## Como rodar

1. Configure os caminhos: o codigo assume a pasta de dados em
   `C:/Users/<USER>/Documents/baseMCR/dados/` com a estrutura
   `raw/`, `clean/`, `built/`, `output/`.
2. Execute `master.R` na ordem (passo 1 ate 6).
3. (Opcional) suba o dashboard com `shiny::runApp("shiny")`.

## Outputs principais

| Output | Descricao |
| ------ | --------- |
| `output/INPEs_exercise/audit_desmat_per_car.csv` | desmat por CAR: original (`soma_desmat`) vs recalculado, com diff_ha e diff_pct |
| `output/INPEs_exercise/audit_desmat_per_car_resumo.csv` | resumo agregado da auditoria |
| `output/INPEs_exercise/changedImpact.rds` | base CAR + desmat + classificacao "Apresente ASV / Salvo pela borda" |
| `built/credit_asv.Rds` | 1 linha por contrato SICOR + `status_car` + `faixa_mf` |
| `output/long/credit_long_municipal.csv` | output agregado por municipio x bioma x ano_safra x status_car x faixa_mf x tipo_pessoa x fonte/programa/subprograma/modalidade |
| `output/analysis_asv_credit/*` | tabelas Excel + graficos PNG das 9 secoes da analise descritiva |

## Pendente

- **Item 6**: desmatamento sem sobreposicao (`desmat_upper` / `desmat_lower`).
  Discussao em aberto -- o tratamento espacial vai ser feito a partir do
  cruzamento PRODES x CAR ja existente, por par de linhas que se interseccionam.
