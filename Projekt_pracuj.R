# Biblioteki
library(tidyverse)
library(stringi)
library(httr)
library(rvest)

# Wczytanie ca³ej strony g³ownej
strona <- GET('https://www.pracuj.pl/praca/wroclaw;wp?rd=20')
strona


#Linki
linki_www <- content(strona) %>%
    html_elements(".offer-details__title-link") %>%
    html_attr("href")
linki_www


ostatnia_strona <- content(strona) %>%
    html_element('.pagination') %>% html_element('.pagination_label--max')  %>% html_text2()
ostatnia_strona <- stri_extract(ostatnia_strona, regex = '\\d+')
urls <- paste0("https://www.pracuj.pl/praca/wroclaw;wp?rd=20", sep = "&pn=", (1:ostatnia_strona))


offerUrls <- list()
for(idx in seq_along(urls)) {
    str_1 <- GET(urls[idx])
    offerUrls[[idx]] <- tryCatch(content(str_1) %>%
                                     html_elements(".offer-details__title-link") %>%
                                     html_attr("href"), error = function(e){
                                         print(e)
                                         'NA'
                                     })
    
    Sys.sleep(1)
    time <- format(Sys.time(), '%X')
    cat('[#] Iteracja: ', idx, ' | Czas: ', time, '\n')
}
offerUrls <- unlist(offerUrls)
offerUrls

lokacja <- ''
forma <- ''
etat <- ''
kontrakt <- ''
kategoria <- ''
offer_name <- ''
offer_info <- ''
offer_requir <- ''
offer_benefits <-'' 
for(idx in seq_along(offerUrls)){
    str_1 <- GET(offerUrls[idx])
    offer_name[idx] <- tryCatch(content(str_1) %>%
                                        html_element(".offer-viewkHIhn3") %>% html_text2(), error = function(e){
                                            print(e)
                                            'NA'
                                        })
    
    lokacja[idx] <- tryCatch(content(str_1) %>%
                                    html_element(".offer-viewqtkGPu") %>% html_text2(), error = function(e){
                                        print(e)
                                        'NA'
                                    })
    
    forma[idx] <- tryCatch(content(str_1) %>%
                                    html_element(xpath = "//div[@data-test = 'sections-benefit-work-modes-text']") %>% html_text2(), error = function(e){
                                        print(e)
                                        'NA'
                                    })
    
    forma[idx] <- if(grepl("hybr", forma[idx]))
                     "praca hybrydowa"
else
    forma[idx]
    
    forma[idx] <- if(grepl("home", forma[idx]))
        "praca zdalna"
    else
        forma[idx]
    
    
    etat[idx] <- tryCatch(content(str_1) %>%
                                    html_element(xpath = "//div[@data-test = 'sections-benefit-work-schedule-text']") %>% html_text2(), error = function(e){
                                        print(e)
                                        'NA'
                                    })
    
    kontrakt[idx] <- tryCatch(content(str_1) %>%
                                    html_element(".offer-viewXo2dpV") %>% html_text2(), error = function(e){
                                        print(e)
                                        'NA'
                                    })
    
    kategoria[idx] <- tryCatch(content(str_1) %>%
                                   html_elements(".offer-viewoea5v2", ".offer-viewoea5v2:nth-of-type(2)") %>% html_text2(), error = function(e){
                                    print(e)
                                    'NA'
                                })
    
    # kategoria[idx] <- tryCatch(content(str_1) %>%
    #                                        html_elements(xpath = "//*[@id=\"kansas-offerview\"]/div/div[1]/div[1]/ul/li[3]/a[@title]") %>% html_text2(), error = function(e){
    #                                         print(e)
    #                                                    'NA'
    #                                            })
    #  
    offer_info[idx] <- tryCatch(content(str_1) %>%
                                   html_element(".offer-vieweLojfZ") %>% html_text2(), error = function(e){
                                       print(e)
                                       'NA'
                                   })
    
    offer_requir[idx] <- tryCatch(content(str_1) %>%
                                   html_element(xpath = "//div[@data-test = 'section-requirements']") %>% html_text2(), error = function(e){
                                       print(e)
                                       'NA'
                                   })
    offer_benefits[idx] <- tryCatch(content(str_1) %>%
                                     html_element(xpath =  "//div[@data-test = 'section-offered']") %>% html_text2(), error = function(e){
                                         print(e)
                                         'NA'
                                     })
    Sys.sleep(0.1)
    time <- format(Sys.time(), '%X')
    cat('[#] Iteracja: ', idx, ' | Czas: ', time, '\n')
}

df_ogloszenia <- data.frame(Nazwa = offer_name, Lokalizacja = lokacja, Kategoria = kategoria, Tryb = forma, Wymiar = etat, Umowa = kontrakt, Info = offer_info, Wymagania = offer_requir, Benefity = offer_benefits)
write_csv2(df_ogloszenia, "pracuj_160622.csv")
dfpracuj <- read_csv2("pracuj_160622.csv")
print(dfpracuj)
plot(dfpracuj)
df_ogloszenia2 <- data.frame(Nazwa = offer_name, Lokalizacja = lokacja, Kategoria = kategoria, Tryb = forma, Wymiar = etat, Umowa = kontrakt)
#Lokalizacje og³oszeñ
a <- str_extract_all(dfpracuj$Info, pattern = "[a-zA-Z¹æê³ñóœŸ¿¥ÆÊ£ÑÓŒ¯]+")
head(a)

stri_split(dfpracuj$Info, fixed='\n')
wek <- stri_split(dfpracuj$Info, fixed='\n')

plot(kontrakt)

#Analiza danych
#wymiar
W_kolo_wymiar <- table(df_ogloszenia$Wymiar) / length(df_ogloszenia$Wymiar)
pie(W_kolo_wymiar)

#Tryb
w_kolo_tryb <- table(df_ogloszenia$Tryb) / length(df_ogloszenia$Tryb)
pie(w_kolo_tryb)

#Lokalizacje
stri_split(df_ogloszenia$Lokalizacja, fixed='\n')
count_L <- table(df_ogloszenia$Lokalizacja)
colors <- rainbow(8)
barplot(count_L, 
        main = "Oferty pracy w danej lokalizacji", 
        ylim = c(0, 50), 
        xlab = "Lokalizacja", 
        ylab = "Iloœæ", 
        col = colors)


#Umowa
count_U <- table(df_ogloszenia$Umowa)
barplot(count_U, 
        main = "Liczebnoœæ gatunków", 
        ylim = c(0, 200), 
        xlab = "Rodzaj umowy", 
        ylab = "Iloœæ", 
        col = colors)
library(plotrix)
pie3D(w_kolo_tryb, radius = 3, labels = df_ogloszenia$Tryb)

kable(df_ogloszenia[1:5, ])
