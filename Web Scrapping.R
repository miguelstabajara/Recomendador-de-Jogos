#Bibliotecas
library(stringr)
library(purrr)
library(rvest)
library(dplyr)
library(XML)
library(xml2)
library(webdriver)

#Rselenium
library(RSelenium)
library(wdman)
library(netstat)
#-----------------------------------------------------------
#Links dos gêneros que contêm os jogos
url<-"https://store.steampowered.com"

html<-read_html(url)
genderlinks<-(html |>
        html_elements("body") |>
        html_elements("div.responsive_page_frame") |>
        html_elements("div.responsive_page_content") |>
        html_elements("div.popup_body") |>
        html_elements("div.popup_menu_subheader") |>
        html_elements("a.popup_menu_item") |>
        html_attr("href"))
genderlinks<- gsub("?snr=1_4_600__12","",genderlinks)
genderlinks<-str_replace(genderlinks,'[?]',"")
genderlinks


#----------------------------------------------------------
#Links dos subgeneros para adquirir os links dos jogos
#Rselenium para raspagem de dados
#Configuração do ambiente: https://www.youtube.com/watch?v=GnpJujF9dBw
selenium()
selenium_object<-selenium(retcommand=TRUE,check=FALSE)
binman::list_versions("chromedriver")
rD <- rsDriver(browser="chrome", chromever = '113.0.5672.63',verbose = F, port = 1235L)

remDr<-rD$client
remDr$open()

p<-1
link_subgenero<-c()
for(j in 1:length(genderlinks)){
    remDr$navigate(genderlinks[j])
    Sys.sleep(5)
    busca<-remDr$findElement(using="class",value="_1UD2hUjCkCaT0iG7AlOB7w")
    final<-busca$findChildElements(using="class",value="_3jWYuVhw4djavxCLhKnHqX")
    
    id<-c()
    for(i in 1:length(final)){
        id[i]<-final[[i]]$getElementAttribute(attrName="id")
        link_subgenero[p]<-paste(genderlinks[j],id[i],sep="")
        p<-p+1
    }
}
link_subgenero

botao<-remDr$findElement(using="class",value="_3d9cKhzXJMPBYzFkB_IaRp")
botao$clickElement()

#stop selenium
rD$server$stop()

#-----------------------------------------------------------
#Raspagem das informações do jogo
url<-"https://store.steampowered.com/app/730/CounterStrike_2/"
html<-read_html(url)

name<-(html |>
        html_elements("div.apphub_AppName")|>
        html_text2())[-1]
summary<-(html|>
             html_elements("div._1UD2hUjCkCaT0iG7AlOB7w")|>
             html_text())
release<-(html|>
            html_elements("div.date")|>
            html_text())
gameReviewSummary<-(html|>
                        html_elements("div.summary")|>
                        html_text())
marcadores<-(html|>
                 html_elements("div.glance_tags")|>
                 html_elements("a") |>
                 html_text())
generos<-(html|>
              html_elements("div.details_block")|>
              html_elements("span") |>
              html_elements("a") |>
              html_text())
preco<-(html|>
            html_element("div.game_purchase_price")|>
            html_text())
recomendacao<-
requisitos<-(html|>
                 html_elements("ul.bb_ul")|>
                 html_elements("li")|>
                 html_text())[-(c(1:7))]

#---------------------------------------------------------------------------------

