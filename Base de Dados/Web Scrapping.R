#Bibliotecas
library(stringr)
library(purrr)
library(rvest)
library(dplyr)
library(XML)
library(xml2)
library(webdriver)
library(dplyr)
library(xlsx)
library(haven)
library(readxl)

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
    busca<-remDr$findElement(using="class",value="_3gb3JeV_1IMaIeODzBSrP3")
    final<-busca$findChildElements(using="class",value="gc-rqvG6ITYgP26WGwJZz")
    
    id<-c()
    for(i in 1:length(final)){
        id[i]<-gsub("Tab_","",final[[i]]$getElementAttribute(attrName="id"))
        id[i]<-paste("?tab=",id[i],sep="")
        link_subgenero[p]<-paste(genderlinks[j],id[i],sep="")
        p<-p+1
    }
}
link_subgenero

link_jogos<-c()
p<-1
for(i in 1:length(link_subgenero)){
    remDr$navigate(link_subgenero[i])
    Sys.sleep(4)
    remDr$executeScript("const elementsToHide = document.getElementsByClassName('i2PTzRNXOK1OXvXb9-wzd');
   for (const element of elementsToHide) {
       element.style.display = 'none';
   }
    const hiddenElement = document.getElementById('SaleSection_13268');
    hiddenElement.style.display = 'block';")
    remDr$executeScript("window.scrollTo(0, 0)")
    print(i)
    Sys.sleep(3)
    remDr$executeScript("window.scrollBy(0, 5000)")
    Sys.sleep(3)
    for(j in 1:30){
        botao<-remDr$findElement(using="class",value="_2tkiJ4VfEdI9kq1agjZyNz")
        botao$clickElement()
        Sys.sleep(3)
        remDr$executeScript("window.scrollBy(0, 5000)")
        Sys.sleep(3)
    }
    busca<-remDr$findElements(using="class",value="_111nfdz8Xyg7lDjTWv_OmK")
    for(j in 1:length(busca)){
        elemento<-busca[[j]]$findChildElement(value="a")
        link_jogos[p]<-elemento$getElementAttribute(attrName="href")
        p<-p+1
    }
}
link_jogos

links_games<-c()
for(i in 1:length(link_jogos)){
    links_games[i]<-link_jogos[[i]]   
}

write.table(links_games,file="data.txt",sep=",")
teste<-read.table("data.txt",sep=",")

#stop selenium
rD$server$stop()

#-----------------------------------------------------------
#Raspagem das informações do jogo

games<-read.table("data.txt",sep=",")
games<-games$x

mat<-matrix(ncol=10,nrow=nrow(games))
df<-data.frame(mat)
df<-rename(df,name=X1,summary=X2,release=X3,gameReviewSummary=X4,markers=X5,genders=X6,price=X7,systemRequiriments=X8,languages=X9,url=X10)

p<-1
aux<-c()
rep<-0
for(k in 1:length(games)){
    start<-Sys.time()
    html<-read_html(games[k])
    teste<-(html |>
               html_elements("div.apphub_AppName")|>
               html_text2())[1]
    print(teste)
    if(is.na(teste)){
        aux[p]<-games[k]
        p<-p+1
    }else{
        if(!(teste%in%df$name)){
            #Nome
            df[k,1]<-teste
            #Descrição do jogo
            tratamento<-(html|>
                          html_elements("div.game_description_snippet")|>
                          html_text())
            if(length(tratamento)==0){
                df[k,2]<-"Exception"
            }else{
                df[k,2]<-tratamento
            }
            #Data de lançamento
            tratamento<-(html|>
                             html_elements("div.date")|>
                             html_text())
            if(length(tratamento)==0){
                df[k,3]<-"Exception"
            }else{
                df[k,3]<-tratamento
            }
        
            #Avaliações
            tratamento<-(html|>
                                    html_elements("div.summary")|>
                                    html_elements("span")|>
                                    html_text2())
            
            tratamento<-gsub('\r',"",tratamento)
            tratamento<-tratamento[-c(7:length(tratamento))]
            for(i in 2:length(tratamento)){
                tratamento[1]<-paste(tratamento[1],tratamento[2],sep=" ")
                tratamento<-tratamento[-2]
            }
            df[k,4]<-tratamento
            #Tags do jogo
            tratamento<-(html|>
                             html_elements("div.glance_tags")|>
                             html_elements("a") |>
                             html_text2())
            tratamento<-gsub('\r',"",tratamento)
            for(i in 2:length(tratamento)){
                tratamento[1]<-paste(tratamento[1],tratamento[2],sep=",")
                tratamento<-tratamento[-2]
            }
            df[k,5]<-tratamento
            
            #Generos
            tratamento<-(html|>
                          html_elements("div.details_block")|>
                          html_elements("span") |>
                          html_elements("a") |>
                          html_text())
            for(i in 2:length(tratamento)){
                tratamento[1]<-paste(tratamento[1],tratamento[2],sep=",")
                tratamento<-tratamento[-2]
            }
            df[k,6]<-tratamento
            
            #Preço
            tratamento<-(html|>
                        html_element("div.game_purchase_price")|>
                        html_text2())
            if(is.na(tratamento)){
                tratamento<-(html|>
                                 html_element("div.discount_original_price")|>
                                 html_text2())
                tratamento<-substr(tratamento, 4, nchar(tratamento))
                tratamento<-gsub(',',".",tratamento)
            }
            tratamento<-gsub('\r ',"",tratamento)
            if(!is.na(tratamento)){
                if(substr(tratamento,1,1)=="R"){
                    tratamento<-substr(tratamento, 4, nchar(tratamento))
                    tratamento<-gsub(',',".",tratamento)
                }
                if(tratamento=="Free to Play"||tratamento=="Free To Play"){
                    tratamento=0
                }else{
                    tratamento = as.numeric(tratamento)
                }
            }else{
                tratamento="Excpetion"
            }
            df[k,7]<-tratamento
            #Requisitos do sistema
            tratamento<-(html|>
                                html_element("div.game_area_sys_req_leftCol")|>
                                 html_elements("ul.bb_ul")|>
                                 html_elements("li")|>
                                 html_text())
            if(length(tratamento)==0){
                tratamento<-(html|>
                                 html_element("div.game_area_sys_req_full")|>
                                 html_elements("ul.bb_ul")|>
                                 html_elements("li")|>
                                 html_text())
            }
            for(i in 2:length(tratamento)){
                tratamento[1]<-paste(tratamento[1],tratamento[2],sep=" , ")
                tratamento<-tratamento[-2]
            }
            df[k,8]<-tratamento
            
            #Linguas Dísponiveis
            tratamento<-(html|>
                             html_elements("td.ellipsis")|>
                             html_text2())
            tratamento<-gsub('\r ',"",tratamento)
            for(i in 2:length(tratamento)){
                tratamento[1]<-paste(tratamento[1],tratamento[2],sep=",")
                tratamento<-tratamento[-2]
            }
            df[k,9]<-tratamento
            
            #Url
            df[k,10]<-games[k]
            
        }else{
            games<-games[-k]
            k<-k-1
            rep<-rep+1
        }
    }
    end<-Sys.time()
    print(end-start)
}
#Função para retirar as linhas nulas do data frame
x<-1
k<-1
while(x){
    if(is.na(df[k,1])){
        df<-df[-k,]
    }else{
        k<-k+1
    }
    if(k==nrow(df)){
        x<-0
    }
}
#Este Arquivo contem os links de jogos que devem ser raspados de forma dinâmica
write.table(aux,file="DynamicLinks.txt",sep=",")

#Persistência dos dados raspados com rvest
write.xlsx(df,"SteamDataGames.xlsx",sheetName="Sheet1",col.names=TRUE,row.names=FALSE,showNA=TRUE)

dataBase<-read.xlsx("SteamDataGames.xlsx",sheetName="Sheet1")

#Raspagem dinâmica dos jogos
selenium()
selenium_object<-selenium(retcommand=TRUE,check=FALSE)
binman::list_versions("chromedriver")
rD <- rsDriver(browser="chrome", chromever = '113.0.5672.63',verbose = F, port = 1235L)

links<-read.table("DynamicLinks.txt",sep=",")
links<-links$x
mat<-matrix(ncol=10,nrow=length(links))
df<-data.frame(mat)
df<-rename(df,name=X1,summary=X2,release=X3,gameReviewSummary=X4,markers=X5,genders=X6,price=X7,systemRequiriments=X8,languages=X9,url=X10)

remDr<-rD$client
remDr$open()

remDr$navigate(links[1])
Sys.sleep(3)
day<-remDr$findElement(using="id",value="ageDay")
day$sendKeysToElement(list("2"))
month<-remDr$findElement(using="id",value="ageMonth")
month$sendKeysToElement(list("fevereiro"))
year<-remDr$findElement(using="id",value="ageYear")
year$sendKeysToElement(list("2001"))
btn<-remDr$findElement(using="id",value="view_product_page_btn")
btn$clickElement()
Sys.sleep(3)
#Mudar para inglês
remDr$setWindowSize(width=1382,height=736)
btn<-remDr$findElement(using="id",value="language_pulldown")
btn$clickElement()
btn<-remDr$findElement(using="xpath",value='//*[@id="language_dropdown"]/div/a[10]')
btn$clickElement()
Sys.sleep(3)

for(k in 1:length(links)){
    remDr$navigate(links[k])
    Sys.sleep(3)
    remDr$setWindowSize(width=1382,height=736)
    #Nome
    name<-remDr$findElement(using="id",value="appHubAppName")$getElementText()[[1]]
    
    if(!(name%in%df$name)){
        df[k,1]<-name
        #Descrição
        aux<-remDr$findElement(using="class",value="glance_ctn")
        child<-aux$findChildElements(value="div")
        summary<-child[[2]]$getElementText()[[1]]
        df[k,2]<-summary
        #Lançamento
        release<-remDr$findElement(using="class",value="date")$getElementText()[[1]]
        df[k,3]<-release
        #Avaliações
        aux<-remDr$findElements(using="id",value="userReviews")
        
        temp<-aux[[1]]$findChildElements(using="class",value="summary")
        remDr$setWindowSize(width=1382,height=736)
        review<-temp[[1]]$findChildElements(value="span")
        final<-paste(review[[1]]$getElementText(),review[[2]]$getElementText(),sep=" ")
        remDr$setWindowSize(width=805,height=700)
        final<-paste(final,review[[3]]$getElementText(),sep=" ")
        if(length(temp)>1){
            remDr$setWindowSize(width=1382,height=736)
            review<-temp[[2]]$findChildElements(value="span")
            final2<-paste(review[[1]]$getElementText(),review[[2]]$getElementText(),sep=" ")
            remDr$setWindowSize(width=805,height=700)
            final2<-paste(final2,review[[3]]$getElementText(),sep=" ")
            
            gameReview<-paste(final,final2,sep=" ")
        }else{
            gameReview<-final
        }
        df[k,4]<-gameReview
        #Tags do Jogo
        remDr$setWindowSize(width=805,height=700)
        remDr$executeScript("window.scrollTo(0, 0)")
        btn<-remDr$findElement(using="xpath",value='//*[@id="glanceCtnResponsiveRight"]/div[2]/div[2]/div')
        btn$clickElement()
        Sys.sleep(1)
        aux<-remDr$findElement(using='class',value="app_tags")
        child<-aux$findChildElements(using="class",value="app_tag_control")
        tags<-child[[1]]$getElementText()
        for(i in 2:length(child)){
            tags<-paste(tags,child[[i]]$getElementText(),sep=",")
        }
        btn<-remDr$findElement(using="class",value="newmodal_close")
        btn$clickElement()
        
        df[k,5]<-tags
        Sys.sleep(1)
        
        #Generos
        
        aux<-remDr$findElement(using="id",value="genresAndManufacturer")
        span<-aux$findChildElement(value="span")
        a<-span$findChildElements(value="a")
        gender<-a[[1]]$getElementText()[[1]]
        if(length(a)>1){
            for(i in 2:length(a)){
                gender<-paste(gender,a[[i]]$getElementText(),sep=",")
            }
        }
        df[k,6]<-gender
        Sys.sleep(1)
        
        #Preço
        aux<-remDr$findElement(using="class",value="game_purchase_action_bg")
        child<-aux$findChildElements(value="div")
        if(child[[1]]$getElementAttribute(attrName="class")=="game_purchase_price price"){
            price<-child[[1]]$getElementText()[[1]]
            df[k,7]<-price
        }else{
            child<-remDr$findElement(using="class",value="discount_original_price")
            price<-child$getElementText()[[1]]
        }
        df[k,7]<-price
        Sys.sleep(1)
        
        #Linguas disponiveis
        
        aux<-remDr$findElement(using="class",value="game_language_options")
        child<-aux$findChildElements(using="class",value="ellipsis")
        if(length(child)>5){
            btn<-remDr$findElement(using="class",value="all_languages")
            btn$clickElement()
            Sys.sleep(1)
        }
        languages<-child[[1]]$getElementText()[[1]]
        for(i in 2:length(child)){
            languages<-paste(languages,child[[i]]$getElementText(),sep=",")
        }
        df[k,9]<-languages
        Sys.sleep(1)
        
        #Requisitos de sistema
        temp<-remDr$findElement(using="class",value="sysreq_contents")
        aux<-temp$findChildElement(using="class",value="bb_ul")
        child<-aux$findChildElements(value="li")
        requiriments<-child[[1]]$getElementText()[[1]]
        for(i in 2:length(child)){
            requiriments<-paste(requiriments,child[[i]]$getElementText(),sep=",")
        }
        df[k,8]<-requiriments
        Sys.sleep(1)
        
        #Url
        df[k,10]<-links[k]
        Sys.sleep(1)
    }
}

rD$server$stop()

#---------------------------------------------------------------------------------

