
tab_credit_flag_brasil <- asvCar %>%
  group_by(tookCredit) %>%
  summarise(
    n_properties = n_distinct(cod_imovel),
    area_mha = sum(area_total_ha, na.rm = TRUE) / 1e6,
    desmat_km2 = sum(soma_desmat, na.rm = TRUE) / 100,
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

  cores_credito <- c(
    "Tomou credito" = "#1B3A4B",
    "Nao tomou credito" = "#9B2226"
  )
  
    ### Grafico ####
    
    g_credit_flag <- ggplot(
      tab_credit_flag_brasil,
      aes(x = tookCredit, y = desmat_km2, color = tookCredit, group = tookCredit)
    ) +
      geom_point(size = 3) +
      
      geom_text(
        aes(label = paste0(round(desmat_km2), " km²")),
        vjust = -1,
        size = 3,
        show.legend = FALSE
      ) +
      
      scale_color_manual(values = cores_credito) +
      
      labs(
        x = "",
        y = "Desmatamento (km2)",
        color = NULL,
        title = "Desmatamento por acesso ao credito"
      ) +
      
      coord_cartesian(
        ylim = c(0, max(tab_credit_flag_brasil$desmat_km2) * 1.15)
      ) +
      
      theme_minimal() +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold",
          size = 16
        ),
        axis.title = element_text(size = 13),
        axis.text  = element_text(size = 11),
        legend.position = "none",
        plot.margin = margin(20, 20, 20, 20)
      )
  