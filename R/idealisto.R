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
                     "Superficie", "Cuartos", "Andar",
                     "Prezo", "Prezo_m2",
                     "Descricion", "Detalles",
                     "Actualizado_o", "Estatisticas",
                     "Anunciante", "Axencia",
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
    actualiza <- x %>% read_html() %>% html_nodes("#stats-ondemand p") %>% html_text()
    estats <- x %>% read_html() %>% html_nodes("#stats-ondemand li") %>% html_text()
    anunciante <- x %>% read_html() %>% html_nodes(".name") %>% html_text()
    axencia <- x %>% read_html() %>% html_nodes(".about-advertiser-name") %>% html_text()
        
    if (length(titulo) == 0) {titulo <- NA}
    
    if (length(prezo) == 0) {prezo <- NA}
    prezo <- as.integer(str_replace_all(pattern = " eur/mes|\\.",
                                        replacement = "",
                                        string = prezo))
    
    if (length(loc) == 0) {loc <- NA}
    enderezo <- loc[1]
    
    distrito <- as.character(loc[str_detect(loc, pattern = "Distrito ") == TRUE])
    distrito <- str_replace_all(pattern = "Distrito ",
                                replacement = "",
                                string = distrito)
    if (length(distrito) == 0) {
      distrito <- str_replace(string = links_anuncios_tot[p], pattern = "https://www.idealista.com/alquiler-viviendas/", replacement = "")
    }
    
    barrio <- as.character(loc[str_detect(loc, pattern = "Barrio ") == TRUE])
    barrio <- str_replace_all(pattern = "Barrio ",
                              replacement = "",
                              string = barrio)
    if (length(barrio) == 0) {barrio <- NA}
    
    if (length(desc) == 0) {desc <- NA}
    desc <- str_replace_all(pattern = '\"', replacement = "", string = desc)
    
    superf <- as.numeric(str_replace_all(pattern = " m\u00B2| |\\.",
                                         replacement = "",
                                         string = str_extract(pattern = "..m\u00B2|...m\u00B2|....m\u00B2|.....m\u00B2|......m\u00B2|.......m\u00B2|........m\u00B2|.........m\u00B2|..........m\u00B2",
                                                              string = info)))
    if (length(superf) == 0) {superf <- NA}
    
    prezo_m2 <- prezo/superf
        
    cuartos <- as.integer(str_replace_all(pattern = " hab.",
                                          replacement = "",
                                          string = str_extract(pattern = ".hab.|..hab.",
                                                               string = info)))
    if (length(cuartos) == 0) {cuartos <- 1}
    
    andar <- as.integer(str_replace_all(pattern = "Entreplanta| planta| planta exterior| planta interior",
                                        replacement = "",
                                        string = str_extract(pattern = "Entreplanta|.\u00AA planta|..\u00AA planta||.\u00AA planta exterior|..\u00AA planta exterior|.\u00AA planta interior|..\u00AA planta interior",
                                                             string = info)))
    if (length(andar) == 0) {andar <- NA}
    
    if (length(detalles) == 0) {detalles <- NA}
    detalles <- str_replace_all(pattern = '\"', replacement = "", string = detalles)
    
    if (length(actualiza) == 0) {actualiza <- NA}
    actualiza <- str_replace_all(pattern = "Anuncio actualizado el ", 
                                 replacement = "",
                                 string = actualiza)
    
    if (length(estats) == 0) {estats <- NA}
    estats <- str_replace_all(pattern = '\"', replacement = "", string = estats)
        
    if (length(axencia) == 0) {axencia <- NA}
    
    if (length(anunciante) == 0 | isTRUE(anunciante == " ")) {anunciante <- "Particular"}
        
    data <- Sys.Date()
    
    line <- data_frame(titulo,
                       distrito, barrio, enderezo,
                       superf, cuartos, andar,
                       prezo, prezo_m2,
                       desc, detalles,
                       actualiza, estats,
                       anunciante, axencia,
                       links_anuncios_tot[p], data)
    print(line)

    process <- 100 - ((p/length(links_anuncios_tot))*100)
    print(paste0("Idealisto leva descargados o ", round(process, digits = 1),"% dos anuncios."))
        
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
  print(paste("Idealisto leva descargado o", length(links_anuncios_tot), "dos anuncios"))
  print(round(diff, digits = 1))
}
