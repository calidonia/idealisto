#' Scrap idealista website.
#' 
#' This function scraps idealista (a spanish real estate website) and downloads all the rent ads in the given province, city, district or neighborhood.
#' @param url An idealista website url that links to the area you want to scrap, e.g. 'https://www.idealista.com/alquiler-viviendas/madrid/arganzuela/'.
#' @param area The type of area you want to scrap. It can take these values: 'Provincia', 'Ciudad', 'Distrito' or 'Barrio'.
#' @param ruta A valid path in your computer where you want to create the csv file.
#' @return It returns a csv in the specified path
#' @export
idealisto <- function(url, area, ruta = "~/idealisto.csv") {
  
  start <- Sys.time()
  
  list.of.packages <- c("stringr", "rvest", "httr")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)>0) {install.packages(new.packages)}
  
  library(stringr)
  library(rvest)
  library(httr)
  
  desktop_agents <-  c('Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
                       'Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
                       'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
                       'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/602.2.14 (KHTML, like Gecko) Version/10.0.1 Safari/602.2.14',
                       'Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36',
                       'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36',
                       'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36',
                       'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36',
                       'Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
                       'Mozilla/5.0 (Windows NT 10.0; WOW64; rv:50.0) Gecko/20100101 Firefox/50.0')
  
  if (area == "Provincia" | area == "provincia") {
    x <- GET(url, add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))
    url_distris <- x %>% read_html() %>% html_nodes(".breadcrumb-subitems li li a") %>% html_attr(name = "href")
    subarea <- "zona"
    url_distris_tot <- c()
    
  } else if(area == "Ciudad" | area == "ciudad") {
    x <- GET(url, add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))
    url_distris <- x %>% read_html() %>% html_nodes(".breadcrumb-subitems li a") %>% html_attr(name = "href")
    subarea <- "distrito"
    url_distris_tot <- c()
    
  } else if (area == "Distrito" | area == "distrito") {
    x <- GET(url, add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))
    url_distris <- x %>% read_html() %>% html_nodes(".breadcrumb-subitems li li a") %>% html_attr(name = "href")
    subarea <- "barrio"
    url_distris_tot <- c()
    
  } else if (area == "Barrio" | area == "barrio") {
    url_distris <- c()
    url_distris_tot <- c()
    
  } else {
    url_distris <- c()
    url_distris_tot <- c()
    
  }
  
  d <- length(url_distris)
  
  repeat {
    links <- paste0("https://www.idealista.com", url_distris[d])
    url_distris_tot <- c(url_distris_tot, links)
    d <- d - 1
    if (d <= 0) {
      break
    }
  }
  
  if (area == "Distrito" | area == "distrito") {
    url_distris_tot <- url
    url_distris_tot_ <- url
  } else {
    print(paste("Estas son as ligazons as paxinas principais de cada", subarea, ":"))
    print(url_distris_tot)
    url_distris_tot_ <- paste0("https://www.idealista.com", url_distris)
  }
  
  repeat {
    
    p <- length(url_distris_tot)
    
    sig_pag_tot <- c()
    
    repeat {
      x <- GET(url_distris_tot[p], add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))
      sig_pag <- x %>% read_html() %>% html_nodes(".icon-arrow-right-after") %>% html_attr(name = "href", default = NA)
      
      if (length(sig_pag) == 0) {
        sig_pag <- NA
      } else {
        sig_pag_tot <- c(sig_pag,sig_pag_tot)
      }
      
      p <- p - 1
      
      if (p == 0) {
        break
      }
      
    }
    
    url_distris_tot <- paste0("https://www.idealista.com", sig_pag_tot)
    
    if (url_distris_tot == "https://www.idealista.com") {
      break
    }
    
    url_distris_tot_ <- c(url_distris_tot_, url_distris_tot)
    
    if (area == "Distrito" | area == "distrito") {
      
    } else {
      print(url_distris_tot_)
      print(paste("Capturando as ligazons de todalas paxinas de cada", subarea, "..."))
    }
    
  }
  
  urls_paginas <- unique(url_distris_tot_[url_distris_tot_ != "https://www.idealista.com"])
  
  p <- length(urls_paginas)
  
  links_anuncios_tot <- c()
  
  repeat {
    x <- GET(urls_paginas[p], add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))
    links <- x %>% read_html() %>% html_nodes(".item-link") %>% html_attr(name = "href", default = NA)
    links_anuncios <- paste0("https://www.idealista.com", links)
    links_anuncios_tot <- c(links_anuncios_tot, links_anuncios)
    print(links_anuncios_tot)
    print("Capturando as ligazons a todolos anuncios...")
    p <- p - 1
    if (p == 0) {
      break
    }
  }
  
  links_anuncios_tot <- unique(links_anuncios_tot)
  
  links_anuncios_tot <<- links_anuncios_tot
  
  line <- data_frame("Titulo",
                     "Distrito", "Barrio", "Enderezo",
                     "Superficie", "Cuartos", "Andar", "Exterior",
                     "Prezo", "Prezo_m2",
                     "Descricion",
                     "Detalles",
                     "Casa", "Superf_cons", "Superf_util", "Coef_aprov", "Banhos", "Garaxe", "Balcon", "Terraza", "Obra_nova", "Empotrados", "Trasteiro",
                     "Norte", "Sur", "Leste", "Oeste", "Construido_o", "Cocinha", "Amoblado", "Cert_enerx", "kWh_m2_ano", "Cat_efic_enerx",
                     "PMR", "Ascensor", "Aire_acondo",
                     "Actualizado_o",
                     # "Estatisticas", "Visitas", "Envios", "Contactos", "Favoritos",
                     "Profesional", "Axencia",
                     "Url", "Data")
  
  write.table(line, file = ruta, sep = ";", quote = FALSE, col.names = FALSE, row.names = FALSE, na = "")
  
  p <- length(links_anuncios_tot)
  
  print(paste("Idealisto recompilou as urls de", p, "anuncios."))
  print(paste("En breve comezara a extraccion dos datos deses", p, "anuncios, pero antes farase unha pausa de 30 segundos."))
  
  Sys.sleep(30)
  
  start_2 <- Sys.time()
  
  repeat {
    x <- GET(links_anuncios_tot[p], add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))
    
    titulo <- x %>% read_html() %>% html_nodes(".main-info__title-main") %>% html_text()
    prezo <- x %>% read_html() %>% html_nodes(".h1-simulated") %>% html_text()
    loc <- x %>% read_html() %>% html_nodes("#headerMap li") %>% html_text()
    info <- x %>% read_html() %>% html_nodes(".info-features") %>% html_text()
    desc <- x %>% read_html() %>% html_nodes(".expandable") %>% html_text()
    detalles <- x %>% read_html() %>% html_nodes(".details-property") %>% html_text()
    actualiza <- x %>% read_html() %>% html_nodes("#stats p") %>% html_text()
    # estats <- x %>% read_html() %>% html_nodes("#stats-ondemand li") %>% html_text()
    anunciante <- x %>% read_html() %>% html_nodes(".name") %>% html_text()
    axencia <- x %>% read_html() %>% html_nodes(".about-advertiser-name") %>% html_text()
    
    if (length(titulo) == 0) {titulo <- NA}
    
    prezo <- as.integer(str_replace_all(pattern = " eur/mes|\\.",
                                        replacement = "",
                                        string = prezo))
    if (is.na(prezo)) {prezo <- NA}
    
    if (length(loc) == 0) {loc <- NA}
    enderezo <- loc[1]
        
    distrito <- as.character(loc[str_detect(loc, pattern = "Distrito ") == TRUE])
    distrito <- str_replace_all(pattern = "Distrito ",
                                replacement = "",
                                string = distrito)
    if (length(distrito) == 0) {
      # distrito <- str_replace(string = links_anuncios_tot[p], pattern = "https://www.idealista.com/alquiler-viviendas/", replacement = "")
      distrito <- loc[2]
    }
    
    barrio <- as.character(loc[str_detect(loc, pattern = "Barrio ") == TRUE])
    barrio <- str_replace_all(pattern = "Barrio ",
                              replacement = "",
                              string = barrio)
    if (length(barrio) == 0) {barrio <- NA}
    
    desc <- desc[1]
    
    desc <- str_replace_all(pattern = '\"', replacement = "", string = desc)
    if (length(desc) == 0) {desc <- NA}
    
    info <- info[1]
    
    superf <- as.numeric(str_replace_all(pattern = " m\u00B2",
                                         replacement = "",
                                         string = str_extract(pattern = "[[:digit:]]+ m\u00B2",
                                                              string = info)))
    if (is.na(superf)) {superf <- NA}
    
    prezo_m2 <- prezo/superf
    
    cuartos <- as.integer(str_replace_all(pattern = " hab\\.",
                                          replacement = "",
                                          string = str_extract(pattern = "[[:digit:]]+ hab\\.",
                                                               string = info)))
    if (is.na(cuartos)) {cuartos <- 1}
    
    andar <- as.integer(str_replace_all(pattern = "\u00AA planta",
                                        replacement = "",
                                        string = str_extract(pattern = "[[:digit:]]+\u00AA planta",
                                                             string = info)))
    if (is.na(andar)) {andar <- NA}

    exterior <- 0
    if (!is.na(str_extract(pattern = " exterior", string = info))) {exterior = 1}
    if (!is.na(str_extract(pattern = " interior", string = info))) {exterior = -1}
    
    # extraccion detalles
    
    detalles <- str_replace_all(pattern = '\"', replacement = "", string = detalles)
    if (length(detalles) == 0) {detalles <- NA}
    
    casa <- 0
    if (!is.na(str_extract(pattern = "Casa", string = detalles))) {casa = 1}
    if (!is.na(str_extract(pattern = "Chalet", string = detalles))) {casa = 1}
        
    if (casa == 1) {exterior = 1}
    
    superf_cons <- as.integer(str_replace_all(pattern = " m\u00B2 construidos",
                                              replacement = "",
                                              string = str_extract(pattern = "[[:digit:]]+ m\u00B2 construidos",
                                                                   string = detalles)))
    
    if (is.na(superf_cons)) {superf_cons <- NA}
    
    superf_util <- as.integer(str_replace_all(pattern = " m\u00B2 \u00FAtiles",
                                              replacement = "",
                                              string = str_extract(pattern = "[[:digit:]]+ m\u00B2 \u00FAtiles",
                                                                   string = detalles)))
    
    if (is.na(superf_util)) {superf_util <- NA}
    
    if (!is.na(superf_cons) & !is.na(superf_util)) {
      if (superf == superf_util) {superf <- superf_cons}
      }
    
    coef_util <- superf_util/superf
    
    if (is.na(coef_util)) {coef_util <- NA}
    
    banhos <- as.integer(str_replace_all(pattern = " ba\u00F1os?",
                                         replacement = "",
                                         string = str_extract(pattern = "[[:digit:]]+ ba\u00F1os?",
                                                              string = detalles)))
    
    if (is.na(banhos)) {banhos <- NA}
    
    if (!is.na(str_extract(pattern = "garaje", string = detalles))) {garaxe = 1} else {garaxe = 0}
    
    if (!is.na(str_extract(pattern = "Balc\u00F3n", string = detalles))) {balcon = 1} else {balcon = 0}
    
    if (!is.na(str_extract(pattern = "Terraza", string = detalles))) {terraza = 1} else {terraza = 0}
    
    if (!is.na(str_extract(pattern = "Promoci\u00F3n de obra nueva", string = detalles))) {obra_nova = 1} else {obra_nova = 0}
    
    if (!is.na(str_extract(pattern = "Armarios empotrados", string = detalles))) {empotrados = 1} else {empotrados = 0}
    
    if (!is.na(str_extract(pattern = "Trastero", string = detalles))) {trasteiro = 1} else {trasteiro = 0}
    
    orientacion <- str_replace_all(pattern = "Orientaci\u00F3n",
                                   replacement = "",
                                   string = str_extract(pattern = "Orientaci\u00F3n [a-z]+, [a-z]+, [a-z]+|Orientaci\u00F3n [a-z]+, [a-z]+|Orientaci\u00F3n [a-z]+",
                                                        string = detalles))

    if (!is.na(str_extract(pattern = " norte", string = orientacion))) {norte = 1} else {norte = 0}
    if (!is.na(str_extract(pattern = " sur", string = orientacion))) {sur = 1} else {sur = 0}
    if (!is.na(str_extract(pattern = " este", string = orientacion))) {leste = 1} else {leste = 0}
    if (!is.na(str_extract(pattern = " oeste", string = orientacion))) {oeste = 1} else {oeste = 0}
    
    ano_cons <- as.integer(str_replace_all(pattern = "Construido en ",
                                           replacement = "",
                                           string = str_extract(pattern = "Construido en [[:digit:]]+",
                                                                string = detalles)))
    
    if (is.na(ano_cons)) {ano_cons <- NA}
    
    cocinha <- NA
    amoblado <- NA
    if (!is.na(str_extract(pattern = "Totalmente amueblado y equipado", string = detalles))) {
      cocinha = 1
      amoblado = 1
    }
    if (!is.na(str_extract(pattern = "Cocina equipada y casa sin amueblar", string = detalles))) {
      cocinha = 1
      amoblado = 0
    }
    if (!is.na(str_extract(pattern = "Cocina sin equipar y casa sin amueblar", string = detalles))) {
      cocinha = 0
      amoblado = 0
    }
    
    # certificacion energetica del edificio terminado (se asimila como idéntico al de la vivienda)
    cert_enerx <-0
    kwh_m2_ano <- NA
    if (!is.na(str_extract(pattern = "kWh/m\u00B2 a\u00F1o", string = detalles))) {
      cert_enerx <- 1
      kwh_m2_ano <- as.numeric(str_replace_all(pattern = "\\,",
                                               replacement = "\\.",
                                               string = str_replace_all(pattern = " kWh/m\u00B2 a\u00F1o",
                                                                        replacement = "",
                                                                        string = str_extract(pattern = "[[:digit:]]+\\,*[[:digit:]]* kWh/m\u00B2 a\u00F1o",
                                                                                             string = detalles))))
      }
    if (!is.na(str_extract(pattern = "IPE no indicado", string = detalles))) {
      cert_enerx <- 1
      kwh_m2_ano <- NA
      }
    
    if (!is.na(kwh_m2_ano)) {
      if (kwh_m2_ano > 172.35) {cat_efic_enerx <- "E"}
      if (kwh_m2_ano <= 172.35) {cat_efic_enerx <- "D"}
      if (kwh_m2_ano <= 112.15) {cat_efic_enerx <- "C"}
      if (kwh_m2_ano <= 72.35) {cat_efic_enerx <- "B"}
      if (kwh_m2_ano <= 44.65) {cat_efic_enerx <- "A"}
      } else {cat_efic_enerx <- NA}
    
    if (!is.na(str_extract(pattern = "Acceso adaptado a personas con movilidad reducida", string = detalles))) {pmr = 1} else {pmr = 0}

    ascensor <- 0
    if (!is.na(str_extract(pattern = "Con ascensor", string = detalles))) {ascensor = 1}
    if (!is.na(str_extract(pattern = "Sin ascensor", string = detalles))) {ascensor = -1}
    
    aire_acond <- 0
    if (!is.na(str_extract(pattern = "Aire acondicionado", string = detalles))) {aire_acond = 1}
    
    ###

    actualiza <- str_replace_all(pattern = "Anuncio actualizado el ", 
                                 replacement = "",
                                 string = actualiza)

    if (length(actualiza) == 0) {actualiza <- NA}
    
    # if (is.na(estats)) {estats <- NA}
    # estats <- str_replace_all(pattern = '\"',
    #                           replacement = "",
    #                           string = estats)

    # extraccion estatisticas

    # visitas <- as.integer(str_replace_all(pattern = " visitas",
    #                                       replacement = "",
    #                                       string = str_extract(pattern = "[[:digit:]]+ visitas",
    #                                                            string = estats)))
    # if (length(visitas) == 0) {visitas <- 0}

    # envios <- as.integer(str_replace_all(pattern = " env\u00EDos a amigos",
    #                                      replacement = "",
    #                                      string = str_extract(pattern = "[[:digit:]]+ env\u00EDos a amigos",
    #                                                           string = estats)))
    # if (length(envios) == 0) {envios <- 0}

    # contactos <- as.integer(str_replace_all(pattern = " contactos por email",
    #                                         replacement = "",
    #                                         string = str_extract(pattern = "[[:digit:]]+ contactos por email",
    #                                                              string = estats)))
    # if (length(contactos) == 0) {contactos <- 0}

    # favoritos <- as.integer(str_replace_all(pattern = " veces guardado como favorito",
    #                                         replacement = "",
    #                                         string = str_extract(pattern = "[[:digit:]]+ veces guardado como favorito",
    #                                                              string = estats)))
    # if (length(favoritos) == 0) {favoritos <- 0}

    ###

    if (length(anunciante) == 0 | isTRUE(anunciante == " "| anunciante == " Particular ")) {profesional <- 0}
    if (anunciante == " Profesional ") {profesional <- 1}
    
    if (length(axencia) == 0) {axencia <- NA}
    
    data <- Sys.Date()

    line <- data_frame(titulo,
                       distrito, barrio, enderezo,
                       superf, cuartos, andar, exterior,
                       prezo, prezo_m2,
                       desc,
                       detalles,
                       casa, superf_cons, superf_util, coef_util, banhos, garaxe, balcon, terraza, obra_nova, empotrados, trasteiro,
                       norte, sur, leste, oeste, ano_cons, cocinha, amoblado, cert_enerx, kwh_m2_ano, cat_efic_enerx,
                       pmr, ascensor, aire_acond,
                       actualiza,
                       # estats, visitas, envios, contactos, favoritos,
                       profesional, axencia,
                       links_anuncios_tot[p], data)
    
    process <- 100 - ((p/length(links_anuncios_tot))*100)
    
    cat(paste0(links_anuncios_tot[p], " :\n"))
    print(line)
    cat(paste0("\nIdealisto leva descargados o ", round(process, digits = 1),"% dos anuncios.\n"))
    cat("-------------------------------------------------\n")

    write.table(line, file = ruta, sep = ";", append = TRUE, quote = TRUE, col.names = FALSE, row.names = FALSE, na = "")

    p <- p - 1
    if (p <= 0) {
      break
      }

    if (Sys.time() > start_2 + 420) {
      stop_t <- sample(x = 100:120, size = 1)
      print(paste("Para evitar que Idealista bloquee Idealisto farase unha pausa durante", stop_t, "segundos."))
      Sys.sleep(time = stop_t)
      start_2 <- Sys.time()
      }

    Sys.sleep(sample(x = 1:3, size = 1))
  }
  
  end <- Sys.time()
  diff <- end - start
  print(paste("Idealisto acabou de descargar ", length(links_anuncios_tot), " anuncios"))
  print(round(diff, digits = 1))
}
