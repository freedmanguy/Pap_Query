library(DT)
library(shiny)
library(dplyr)
library(tidyr)
library(openxlsx)
library(writexl)
library(ggplot2)

rm(list=ls())

  function(input, output) {
    
    myo <- eventReactive(input$Query, { 
      load("PapCodebook.RData")
      terms <- input$search
      terms <- gsub("^ +","",terms)
      terms <- gsub(" +$","",terms)
      terms <- gsub(" *, *",",",terms)
      terms <- gsub(" *\\| *","\\|",terms)
      Oterms <- terms
      output$search <- renderText(paste("Search terms:",Oterms)) #input$search
      if(grepl(",",terms)){
        terms <- strsplit(terms, ",") %>% 
          unlist()
      }
      
      Eterms <- gsub("^ +","",input$exclude)
      Eterms <- gsub(" +$","",Eterms)
      Eterms <- gsub(" *, *",",",Eterms)
      eterms <- Eterms
      output$exclude <- renderText(paste("Excluded terms:",Eterms))
      if(grepl(",",Eterms)){
        eterms <- strsplit(Eterms, ",") %>% 
          unlist()
      }
      if(nchar(Eterms)>0){
        Fterms <- paste(terms,"AND NOT",Eterms)
      } else {
        Fterms <- Oterms
      }
      
      if(input$source=="State of the Union Speeches"){
        output$url <- renderUI(a("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Executive-State_of_the_Union_Speeches-19.5.csv",
                                 href = "https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Executive-State_of_the_Union_Speeches-19.5.csv"))
        output$source <- renderText(paste("Source:",input$source))
        mydo <- read.csv("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Executive-State_of_the_Union_Speeches-19.5.csv")
        output$raw <- renderDT({
          mydf <- select(mydo, id, year, pap_subtopic, description) %>% 
            filter(., year %in% c(input$styear:input$endyear))
          
          i <- 1
          j <- length(terms)+1
          while(i<j){
            mydf <- filter(mydf, grepl(terms[i],description,ignore.case = T))
            i <- i+1
          }
          rm(i,j)
          if(nchar(eterms)>0){
            i <- 1
            j <- length(eterms)+1
            while(i<j){
              mydf <- filter(mydf, !grepl(eterms[i],description,ignore.case = T))
              i <- i+1
            }
            rm(i,j)
          }
          datatable(mydf, options = list(dom = 't', pageLength = nrow(mydf)))
        })
        
        output$years <- renderText({
          mydf <- select(mydo, id, year, pap_subtopic, description) %>%
            filter(., year %in% c(input$styear:input$endyear))
          styear <- min(mydf$year, na.rm = T)
          endyear <- max(mydf$year, na.rm = T)

          paste("Years: ",styear,"-",endyear,sep="")
        })


        
        myd <- select(mydo, id, year, pap_subtopic, description) %>% 
          filter(., year %in% c(input$styear:input$endyear))
        styear <- min(myd$year, na.rm = T)
        endyear <- max(myd$year, na.rm = T)
        i <- 1
        j <- length(terms)+1
        while(i<j){
          myd <- filter(myd, grepl(terms[i],description,ignore.case = T))
          i <- i+1
        }
        rm(i,j)
        if(nchar(eterms)>0){
          i <- 1
          j <- length(eterms)+1
          while(i<j){
            myd <- filter(myd, !grepl(eterms[i],description,ignore.case = T))
            i <- i+1
          }
          rm(i,j)
        }
        raw <- myd
        mydy <- myd %>% 
          group_by(pap_subtopic) %>% 
          group_by(year, add = T) %>% 
          count() %>% 
          rename(., "Subtopic"="pap_subtopic") %>% 
          arrange(., year, Subtopic, n)
        myy <- myd %>% 
          group_by(year) %>% 
          count() %>% 
          arrange(., year)
        
          myd <- myd %>% 
          group_by(pap_subtopic) %>% 
          count() %>% 
          rename(., "Subtopic"="pap_subtopic") %>% 
          arrange(., -n) %>%
          left_join(., papcb[,c("Subtopic","Subtopic Title")]) %>% 
          select(., Subtopic, `Subtopic Title`,n)
        rm(papcb)
        myq <- data.frame(x <- c("Query Details:","Source:","Original Data:","Search Terms","Excluded Terms","Years:","Date Queried:","Web App:",NA,"coding provided by the Policy Agendas Project"),
                          y <- c(NA,input$source,"https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Executive-State_of_the_Union_Speeches-19.4.csv",Oterms,Eterms,paste(styear,"-",endyear,sep=""),as.character(Sys.Date()),"https://freedmanguy.shinyapps.io/querypap/",NA,NA))
        
        myp <- myd %>% 
          arrange(., n)
        myp$Subtopic <- factor(myp$Subtopic, levels = unique(myp$Subtopic))
        output$sumplot <- renderPlot({
          ggplot(myp, aes(y = n, x = Subtopic)) + 
            stat_summary(geom = "bar") +
            coord_flip() +
            theme(plot.caption = element_text(hjust = 0)) +
            labs(title = "State of the Union Speeches",
                 caption = paste("Number of observations in each Policy Agendas subtopic that includes the terms:",Fterms))
        })
        
        myp2 <- mydy %>% 
          ungroup() %>% 
          mutate(Major = floor(Subtopic/100)) %>% 
          group_by(Major) %>% 
          group_by(year, add = T) %>% 
          summarise(n = sum(n)) %>% 
          ungroup %>% 
          arrange(., Major, year)
        myp2$year <- as.numeric(as.character(myp2$year))  
        temp <- expand.grid(c(min(myp2$year):max(myp2$year)),unique(myp2$Major))
        colnames(temp) <- c("year","Major")
        myp2 <- left_join(temp, myp2)
        myp2$n[is.na(myp2$n)] <- 0
        myp2$Major <- factor(myp2$Major,
                             levels = c(23,21:12,10:1), labels=c("23: Arts","21: Public Lands","20: Government Operations","19: International Affairs","18: Foreign Trade","17: Technology","16: Defense","15: Domestic Commerce","14: Housing","13: Social Welfare","12: Law and Crime","10: Transportation","9: Immigration","8: Energy","7: Environment","6: Education","5: Labor","4: Agriculture","3: Health","2: Civil Rights","1: Macroeconomics")) 
        output$annualplot <- renderPlot({
          ggplot(myp2, aes(x = year, y = n, fill = Major)) +
            stat_summary(geom = "bar", position = "stack") +
            theme(plot.caption = element_text(hjust = 0)) +
            ylab("Number of Observations") +
              xlab("Year") +
            labs(fill = "Major Topic",
                 title = "State of the Union Speeches",
                 caption = paste("Number of observations in each Policy Agendas major topic that includes the terms:",Fterms))
        })
        
        output$dl <- downloadHandler(
          filename = function(){
            paste0("PapQueryPlots",Sys.Date(),".zip")
            
          },
          content = function(file){
            ggplot(myp, aes(y = n, x = Subtopic)) + 
              stat_summary(geom = "bar") +
              coord_flip() +
              theme(plot.caption = element_text(hjust = 0)) +
              labs(title = "State of the Union Speeches",
                   caption = paste("Number of observations in each Policy Agendas subtopic that includes the terms:",Fterms))
            ggsave(filename = paste0(getwd(),"/","Summary.png"), device = "png", width = 9, height = 6, unit = "in")
            
            ggplot(myp2, aes(x = year, y = n, fill = Major)) +
              stat_summary(geom = "bar", position = "stack") +
              theme(plot.caption = element_text(hjust = 0)) +
              ylab("Number of Observations") +
              xlab("Year") +
              labs(fill = "Major Topic",
                   title = "State of the Union Speeches",
                   caption = paste("Number of observations in each Policy Agendas major topic that includes the terms:",Fterms))
            ggsave(filename = paste0(getwd(),"/","Annual.png"), device = "png", width = 9, height = 6, unit = "in")
            
            xlsx::write.xlsx(as.data.frame(myq), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Query Details", row.names = F, showNA = F, col.names = F)
            xlsx::write.xlsx(as.data.frame(myd), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Summary", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(mydy), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Annual", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(myy), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Annual Sum", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(raw), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Raw Data", row.names = F, append = T, showNA = F)
            myfiles <- c(paste0(getwd(),"/",c("Summary.png","Annual.png")),paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"))
            zip(file,files = myfiles, flags = "-j")
          }
        )
        
        
        myd
      }
      
      if(input$source=="Presidential Veto Rhetoric"){
        output$url <- renderUI(a("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-executive_vetothreats_19.3.csv",
                                 href = "https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-executive_vetothreats_19.3.csv"))
        output$source <- renderText(paste("Source:",input$source))
        mydo <- read.csv("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-executive_vetothreats_19.3.csv")
        output$raw <- renderDT({
          mydf <- select(mydo, id, year, pap_subtopic, description) %>% 
            filter(., year %in% c(input$styear:input$endyear))
          
          i <- 1
          j <- length(terms)+1
          while(i<j){
            mydf <- filter(mydf, grepl(terms[i],description,ignore.case = T))
            i <- i+1
          }
          rm(i,j)
          if(nchar(eterms)>0){
            i <- 1
            j <- length(eterms)+1
            while(i<j){
              mydf <- filter(mydf, !grepl(eterms[i],description,ignore.case = T))
              i <- i+1
            }
            rm(i,j)
          }
          datatable(mydf, options = list(dom = 't', pageLength = nrow(mydf)))
        })
        
        output$years <- renderText({
          mydf <- select(mydo, id, year, pap_subtopic, description) %>%
            filter(., year %in% c(input$styear:input$endyear))
          styear <- min(mydf$year, na.rm = T)
          endyear <- max(mydf$year, na.rm = T)
          
          paste("Years: ",styear,"-",endyear,sep="")
        })
        
        
        
        myd <- select(mydo, id, year, pap_subtopic, description) %>% 
          filter(., year %in% c(input$styear:input$endyear))
        styear <- min(myd$year, na.rm = T)
        endyear <- max(myd$year, na.rm = T)
        i <- 1
        j <- length(terms)+1
        while(i<j){
          myd <- filter(myd, grepl(terms[i],description,ignore.case = T))
          i <- i+1
        }
        rm(i,j)
        if(nchar(eterms)>0){
          i <- 1
          j <- length(eterms)+1
          while(i<j){
            myd <- filter(myd, !grepl(eterms[i],description,ignore.case = T))
            i <- i+1
          }
          rm(i,j)
        }
        raw <- myd
        mydy <- myd %>% 
          group_by(pap_subtopic) %>% 
          group_by(year, add = T) %>% 
          count() %>% 
          rename(., "Subtopic"="pap_subtopic") %>% 
          arrange(., year, Subtopic, n)
        myy <- myd %>% 
          group_by(year) %>% 
          count() %>% 
          arrange(., year)
        myd <- myd %>% 
          group_by(pap_subtopic) %>% 
          count() %>% 
          rename(., "Subtopic"="pap_subtopic") %>% 
          arrange(., -n) %>%
          left_join(., papcb[,c("Subtopic","Subtopic Title")]) %>% 
          select(., Subtopic, `Subtopic Title`,n)
        rm(papcb)
        myq <- data.frame(x <- c("Query Details:","Source:","Original Data:","Search Terms","Excluded Terms","Years:","Date Queried:","Web App:",NA,"coding provided by the Policy Agendas Project"),
                          y <- c(NA,input$source,"https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-executive_vetothreats_19.3.csv",Oterms,Eterms,paste(styear,"-",endyear,sep=""),as.character(Sys.Date()),"https://freedmanguy.shinyapps.io/querypap/",NA,NA))
        
        myp <- myd %>% 
          arrange(., n)
        myp$Subtopic <- factor(myp$Subtopic, levels = unique(myp$Subtopic))
        output$sumplot <- renderPlot({
          ggplot(myp, aes(y = n, x = Subtopic)) + 
            stat_summary(geom = "bar") +
            coord_flip() +
            theme(plot.caption = element_text(hjust = 0)) +
            labs(title = "Presidential Veto Rhetoric",
                 caption = paste("Number of observations in each Policy Agendas subtopic that includes the terms:",Fterms))
        })
        
        myp2 <- mydy %>% 
          ungroup() %>% 
          mutate(Major = floor(Subtopic/100)) %>% 
          group_by(Major) %>% 
          group_by(year, add = T) %>% 
          summarise(n = sum(n)) %>% 
          ungroup %>% 
          arrange(., Major, year)
        myp2$year <- as.numeric(as.character(myp2$year))  
        temp <- expand.grid(c(min(myp2$year):max(myp2$year)),unique(myp2$Major))
        colnames(temp) <- c("year","Major")
        myp2 <- left_join(temp, myp2)
        myp2$n[is.na(myp2$n)] <- 0
        myp2$Major <- factor(myp2$Major,
                             levels = c(23,21:12,10:1), labels=c("23: Arts","21: Public Lands","20: Government Operations","19: International Affairs","18: Foreign Trade","17: Technology","16: Defense","15: Domestic Commerce","14: Housing","13: Social Welfare","12: Law and Crime","10: Transportation","9: Immigration","8: Energy","7: Environment","6: Education","5: Labor","4: Agriculture","3: Health","2: Civil Rights","1: Macroeconomics")) 
        output$annualplot <- renderPlot({
          ggplot(myp2, aes(x = year, y = n, fill = Major)) +
            stat_summary(geom = "bar", position = "stack") +
            theme(plot.caption = element_text(hjust = 0)) +
            ylab("Number of Observations") +
            xlab("Year") +
            labs(fill = "Major Topic",
                 title = "Presidential Veto Rhetoric",
                 caption = paste("Number of observations in each Policy Agendas major topic that includes the terms:",Fterms))
        })
        output$dl <- downloadHandler(
          filename = function(){
            paste0("PapQueryPlots",Sys.Date(),".zip")
            
          },
          content = function(file){
            ggplot(myp, aes(y = n, x = Subtopic)) + 
              stat_summary(geom = "bar") +
              coord_flip() +
              theme(plot.caption = element_text(hjust = 0)) +
              labs(title = "Presidential Veto Rhetoric",
                   caption = paste("Number of observations in each Policy Agendas subtopic that includes the terms:",Fterms))
            ggsave(filename = paste0(getwd(),"/","Summary.png"), device = "png", width = 9, height = 6, unit = "in")
            
            ggplot(myp2, aes(x = year, y = n, fill = Major)) +
              stat_summary(geom = "bar", position = "stack") +
              theme(plot.caption = element_text(hjust = 0)) +
              ylab("Number of Observations") +
              xlab("Year") +
              labs(fill = "Major Topic",
                   title = "Presidential Veto Rhetoric",
                   caption = paste("Number of observations in each Policy Agendas major topic that includes the terms:",Fterms))
            ggsave(filename = paste0(getwd(),"/","Annual.png"), device = "png", width = 9, height = 6, unit = "in")
            
            xlsx::write.xlsx(as.data.frame(myq), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Query Details", row.names = F, showNA = F, col.names = F)
            xlsx::write.xlsx(as.data.frame(myd), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Summary", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(mydy), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Annual", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(myy), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Annual Sum", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(raw), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Raw Data", row.names = F, append = T, showNA = F)
            myfiles <- c(paste0(getwd(),"/",c("Summary.png","Annual.png")),paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"))
            zip(file,files = myfiles, flags = "-j")
          }
        )
        myd
      }
      
      if(input$source=="Executive Orders"){
        output$url <- renderUI(a("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Executive-executive_orders_19.3.csv",
                                 href = "https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Executive-executive_orders_19.3.csv"))
        output$source <- renderText(paste("Source:",input$source))
        mydo <- read.csv("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Executive-executive_orders_19.3.csv")
        output$raw <- renderDT({
          mydf <- select(mydo, id, year, pap_subtopic, description) %>% 
            filter(., year %in% c(input$styear:input$endyear))
          
          i <- 1
          j <- length(terms)+1
          while(i<j){
            mydf <- filter(mydf, grepl(terms[i],description,ignore.case = T))
            i <- i+1
          }
          rm(i,j)
          if(nchar(eterms)>0){
            i <- 1
            j <- length(eterms)+1
            while(i<j){
              mydf <- filter(mydf, !grepl(eterms[i],description,ignore.case = T))
              i <- i+1
            }
            rm(i,j)
          }
          datatable(mydf, options = list(dom = 't', pageLength = nrow(mydf)))
        })
        
        output$years <- renderText({
          mydf <- select(mydo, id, year, pap_subtopic, description) %>%
            filter(., year %in% c(input$styear:input$endyear))
          styear <- min(mydf$year, na.rm = T)
          endyear <- max(mydf$year, na.rm = T)
          
          paste("Years: ",styear,"-",endyear,sep="")
        })
        
        
        
        myd <- select(mydo, id, year, pap_subtopic, description) %>% 
          filter(., year %in% c(input$styear:input$endyear))
        styear <- min(myd$year, na.rm = T)
        endyear <- max(myd$year, na.rm = T)
        i <- 1
        j <- length(terms)+1
        while(i<j){
          myd <- filter(myd, grepl(terms[i],description,ignore.case = T))
          i <- i+1
        }
        rm(i,j)
        if(nchar(eterms)>0){
          i <- 1
          j <- length(eterms)+1
          while(i<j){
            myd <- filter(myd, !grepl(eterms[i],description,ignore.case = T))
            i <- i+1
          }
          rm(i,j)
        }
        raw <- myd
        mydy <- myd %>% 
          group_by(pap_subtopic) %>% 
          group_by(year, add = T) %>% 
          count() %>% 
          rename(., "Subtopic"="pap_subtopic") %>% 
          arrange(., year, Subtopic, n)
        myy <- myd %>% 
          group_by(year) %>% 
          count() %>% 
          arrange(., year)
        myd <- myd %>% 
          group_by(pap_subtopic) %>% 
          count() %>% 
          rename(., "Subtopic"="pap_subtopic") %>% 
          arrange(., -n) %>%
          left_join(., papcb[,c("Subtopic","Subtopic Title")]) %>% 
          select(., Subtopic, `Subtopic Title`,n)
        rm(papcb)
        myq <- data.frame(x <- c("Query Details:","Source:","Original Data:","Search Terms","Excluded Terms","Years:","Date Queried:","Web App:",NA,"coding provided by the Policy Agendas Project"),
                          y <- c(NA,input$source,"https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Executive-executive_orders_19.3.csv",Oterms,Eterms,paste(styear,"-",endyear,sep=""),as.character(Sys.Date()),"https://freedmanguy.shinyapps.io/querypap/",NA,NA))
        
        myp <- myd %>% 
          arrange(., n)
        myp$Subtopic <- factor(myp$Subtopic, levels = unique(myp$Subtopic))
        output$sumplot <- renderPlot({
          ggplot(myp, aes(y = n, x = Subtopic)) + 
            stat_summary(geom = "bar") +
            coord_flip() +
            theme(plot.caption = element_text(hjust = 0)) +
            labs(title = "Executive Orders",
                 caption = paste("Number of observations in each Policy Agendas subtopic that includes the terms:",Fterms))
        })
        
        myp2 <- mydy %>% 
          ungroup() %>% 
          mutate(Major = floor(Subtopic/100)) %>% 
          group_by(Major) %>% 
          group_by(year, add = T) %>% 
          summarise(n = sum(n)) %>% 
          ungroup %>% 
          arrange(., Major, year)
        myp2$year <- as.numeric(as.character(myp2$year))  
        temp <- expand.grid(c(min(myp2$year):max(myp2$year)),unique(myp2$Major))
        colnames(temp) <- c("year","Major")
        myp2 <- left_join(temp, myp2)
        myp2$n[is.na(myp2$n)] <- 0
        myp2$Major <- factor(myp2$Major,
                             levels = c(23,21:12,10:1), labels=c("23: Arts","21: Public Lands","20: Government Operations","19: International Affairs","18: Foreign Trade","17: Technology","16: Defense","15: Domestic Commerce","14: Housing","13: Social Welfare","12: Law and Crime","10: Transportation","9: Immigration","8: Energy","7: Environment","6: Education","5: Labor","4: Agriculture","3: Health","2: Civil Rights","1: Macroeconomics")) 
        output$annualplot <- renderPlot({
          ggplot(myp2, aes(x = year, y = n, fill = Major)) +
            stat_summary(geom = "bar", position = "stack") +
            theme(plot.caption = element_text(hjust = 0)) +
            ylab("Number of Observations") +
            xlab("Year") +
            labs(fill = "Major Topic",
                 title = "Executive Orders",
                 caption = paste("Number of observations in each Policy Agendas major topic that includes the terms:",Fterms))
        })
        output$dl <- downloadHandler(
          filename = function(){
            paste0("PapQueryPlots",Sys.Date(),".zip")
            
          },
          content = function(file){
            ggplot(myp, aes(y = n, x = Subtopic)) + 
              stat_summary(geom = "bar") +
              coord_flip() +
              theme(plot.caption = element_text(hjust = 0)) +
              labs(title = "Executive Orders",
                   caption = paste("Number of observations in each Policy Agendas subtopic that includes the terms:",Fterms))
            ggsave(filename = paste0(getwd(),"/","Summary.png"), device = "png", width = 9, height = 6, unit = "in")
            
            ggplot(myp2, aes(x = year, y = n, fill = Major)) +
              stat_summary(geom = "bar", position = "stack") +
              theme(plot.caption = element_text(hjust = 0)) +
              ylab("Number of Observations") +
              xlab("Year") +
              labs(fill = "Major Topic",
                   title = "Executive Orders",
                   caption = paste("Number of observations in each Policy Agendas major topic that includes the terms:",Fterms))
            ggsave(filename = paste0(getwd(),"/","Annual.png"), device = "png", width = 9, height = 6, unit = "in")
            
            xlsx::write.xlsx(as.data.frame(myq), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Query Details", row.names = F, showNA = F, col.names = F)
            xlsx::write.xlsx(as.data.frame(myd), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Summary", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(mydy), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Annual", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(myy), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Annual Sum", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(raw), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Raw Data", row.names = F, append = T, showNA = F)
            myfiles <- c(paste0(getwd(),"/",c("Summary.png","Annual.png")),paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"))
            zip(file,files = myfiles, flags = "-j")
          }
        )
        myd
      }
      
      if(input$source=="Democratic Party Platform"){
        output$url <- renderUI(a("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Party-Dem_Platform_19.2.csv",
                                 href = "https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Party-Dem_Platform_19.2.csv"))
        output$source <- renderText(paste("Source:",input$source))
        mydo <- read.csv("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Party-Dem_Platform_19.2.csv")
        output$raw <- renderDT({
          mydf <- select(mydo, id, year, pap_subtopic, description) %>% 
            filter(., year %in% c(input$styear:input$endyear))
          
          i <- 1
          j <- length(terms)+1
          while(i<j){
            mydf <- filter(mydf, grepl(terms[i],description,ignore.case = T))
            i <- i+1
          }
          rm(i,j)
          if(nchar(eterms)>0){
            i <- 1
            j <- length(eterms)+1
            while(i<j){
              mydf <- filter(mydf, !grepl(eterms[i],description,ignore.case = T))
              i <- i+1
            }
            rm(i,j)
          }
          datatable(mydf, options = list(dom = 't', pageLength = nrow(mydf)))
        })
        
        output$years <- renderText({
          mydf <- select(mydo, id, year, pap_subtopic, description) %>%
            filter(., year %in% c(input$styear:input$endyear))
          styear <- min(mydf$year, na.rm = T)
          endyear <- max(mydf$year, na.rm = T)
          
          paste("Years: ",styear,"-",endyear,sep="")
        })
        
        
        
        myd <- select(mydo, id, year, pap_subtopic, description) %>% 
          filter(., year %in% c(input$styear:input$endyear))
        styear <- min(myd$year, na.rm = T)
        endyear <- max(myd$year, na.rm = T)
        i <- 1
        j <- length(terms)+1
        while(i<j){
          myd <- filter(myd, grepl(terms[i],description,ignore.case = T))
          i <- i+1
        }
        rm(i,j)
        if(nchar(eterms)>0){
          i <- 1
          j <- length(eterms)+1
          while(i<j){
            myd <- filter(myd, !grepl(eterms[i],description,ignore.case = T))
            i <- i+1
          }
          rm(i,j)
        }
        raw <- myd
        mydy <- myd %>% 
          group_by(pap_subtopic) %>% 
          group_by(year, add = T) %>% 
          count() %>% 
          rename(., "Subtopic"="pap_subtopic") %>% 
          arrange(., year, Subtopic, n)
        myy <- myd %>% 
          group_by(year) %>% 
          count() %>% 
          arrange(., year)
        myd <- myd %>% 
          group_by(pap_subtopic) %>% 
          count() %>% 
          rename(., "Subtopic"="pap_subtopic") %>% 
          arrange(., -n) %>%
          left_join(., papcb[,c("Subtopic","Subtopic Title")]) %>% 
          select(., Subtopic, `Subtopic Title`,n)
        rm(papcb)
        myq <- data.frame(x <- c("Query Details:","Source:","Original Data:","Search Terms","Excluded Terms","Years:","Date Queried:","Web App:",NA,"coding provided by the Policy Agendas Project"),
                          y <- c(NA,input$source,"https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Party-Dem_Platform_19.2.csv",Oterms,Eterms,paste(styear,"-",endyear,sep=""),as.character(Sys.Date()),"https://freedmanguy.shinyapps.io/querypap/",NA,NA))
        
        myp <- myd %>% 
          arrange(., n)
        myp$Subtopic <- factor(myp$Subtopic, levels = unique(myp$Subtopic))
        output$sumplot <- renderPlot({
          ggplot(myp, aes(y = n, x = Subtopic)) + 
            stat_summary(geom = "bar") +
            coord_flip() +
            theme(plot.caption = element_text(hjust = 0)) +
            labs(title = "Democratic Party Platform",
                 caption = paste("Number of observations in each Policy Agendas subtopic that includes the terms:",Fterms))
        })
        
        myp2 <- mydy %>% 
          ungroup() %>% 
          mutate(Major = floor(Subtopic/100)) %>% 
          group_by(Major) %>% 
          group_by(year, add = T) %>% 
          summarise(n = sum(n)) %>% 
          ungroup %>% 
          arrange(., Major, year)
        myp2$year <- as.numeric(as.character(myp2$year))  
        temp <- expand.grid(c(min(myp2$year):max(myp2$year)),unique(myp2$Major))
        colnames(temp) <- c("year","Major")
        myp2 <- left_join(temp, myp2)
        myp2$n[is.na(myp2$n)] <- 0
        myp2$Major <- factor(myp2$Major,
                             levels = c(23,21:12,10:1), labels=c("23: Arts","21: Public Lands","20: Government Operations","19: International Affairs","18: Foreign Trade","17: Technology","16: Defense","15: Domestic Commerce","14: Housing","13: Social Welfare","12: Law and Crime","10: Transportation","9: Immigration","8: Energy","7: Environment","6: Education","5: Labor","4: Agriculture","3: Health","2: Civil Rights","1: Macroeconomics")) 
        output$annualplot <- renderPlot({
          ggplot(myp2, aes(x = year, y = n, fill = Major)) +
            stat_summary(geom = "bar", position = "stack") +
            theme(plot.caption = element_text(hjust = 0)) +
            ylab("Number of Observations") +
            xlab("Year") +
            labs(fill = "Major Topic",
                 title = "Democratic Party Platform",
                 caption = paste("Number of observations in each Policy Agendas major topic that includes the terms:",Fterms))
        })
        output$dl <- downloadHandler(
          filename = function(){
            paste0("PapQueryPlots",Sys.Date(),".zip")
            
          },
          content = function(file){
            ggplot(myp, aes(y = n, x = Subtopic)) + 
              stat_summary(geom = "bar") +
              coord_flip() +
              theme(plot.caption = element_text(hjust = 0)) +
              labs(title = "Democratic Party Platform",
                   caption = paste("Number of observations in each Policy Agendas subtopic that includes the terms:",Fterms))
            ggsave(filename = paste0(getwd(),"/","Summary.png"), device = "png", width = 9, height = 6, unit = "in")
            
            ggplot(myp2, aes(x = year, y = n, fill = Major)) +
              stat_summary(geom = "bar", position = "stack") +
              theme(plot.caption = element_text(hjust = 0)) +
              ylab("Number of Observations") +
              xlab("Year") +
              labs(fill = "Major Topic",
                   title = "Democratic Party Platform",
                   caption = paste("Number of observations in each Policy Agendas major topic that includes the terms:",Fterms))
            ggsave(filename = paste0(getwd(),"/","Annual.png"), device = "png", width = 9, height = 6, unit = "in")
            
            xlsx::write.xlsx(as.data.frame(myq), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Query Details", row.names = F, showNA = F, col.names = F)
            xlsx::write.xlsx(as.data.frame(myd), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Summary", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(mydy), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Annual", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(myy), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Annual Sum", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(raw), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Raw Data", row.names = F, append = T, showNA = F)
            myfiles <- c(paste0(getwd(),"/",c("Summary.png","Annual.png")),paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"))
            zip(file,files = myfiles, flags = "-j")
          }
        )
        myd
      }
      
      if(input$source=="Republican Party Platform"){
        output$url <- renderUI(a("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Party-Rep_Platform_19.2..csv",
                                 href = "https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Party-Rep_Platform_19.2..csv"))
        output$source <- renderText(paste("Source:",input$source))
        mydo <- read.csv("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Party-Rep_Platform_19.2..csv")
        output$raw <- renderDT({
          mydf <- select(mydo, id, year, pap_subtopic, description) %>% 
            filter(., year %in% c(input$styear:input$endyear))
          
          i <- 1
          j <- length(terms)+1
          while(i<j){
            mydf <- filter(mydf, grepl(terms[i],description,ignore.case = T))
            i <- i+1
          }
          rm(i,j)
          if(nchar(eterms)>0){
            i <- 1
            j <- length(eterms)+1
            while(i<j){
              mydf <- filter(mydf, !grepl(eterms[i],description,ignore.case = T))
              i <- i+1
            }
            rm(i,j)
          }
          datatable(mydf, options = list(dom = 't', pageLength = nrow(mydf)))
        })
        
        output$years <- renderText({
          mydf <- select(mydo, id, year, pap_subtopic, description) %>%
            filter(., year %in% c(input$styear:input$endyear))
          styear <- min(mydf$year, na.rm = T)
          endyear <- max(mydf$year, na.rm = T)
          
          paste("Years: ",styear,"-",endyear,sep="")
        })
        
        
        
        myd <- select(mydo, id, year, pap_subtopic, description) %>% 
          filter(., year %in% c(input$styear:input$endyear))
        styear <- min(myd$year, na.rm = T)
        endyear <- max(myd$year, na.rm = T)
        i <- 1
        j <- length(terms)+1
        while(i<j){
          myd <- filter(myd, grepl(terms[i],description,ignore.case = T))
          i <- i+1
        }
        rm(i,j)
        if(nchar(eterms)>0){
          i <- 1
          j <- length(eterms)+1
          while(i<j){
            myd <- filter(myd, !grepl(eterms[i],description,ignore.case = T))
            i <- i+1
          }
          rm(i,j)
        }
        raw <- myd
        mydy <- myd %>% 
          group_by(pap_subtopic) %>% 
          group_by(year, add = T) %>% 
          count() %>% 
          rename(., "Subtopic"="pap_subtopic") %>% 
          arrange(., year, Subtopic, n)
        myy <- myd %>% 
          group_by(year) %>% 
          count() %>% 
          arrange(., year)
        myd <- myd %>% 
          group_by(pap_subtopic) %>% 
          count() %>% 
          rename(., "Subtopic"="pap_subtopic") %>% 
          arrange(., -n) %>%
          left_join(., papcb[,c("Subtopic","Subtopic Title")]) %>% 
          select(., Subtopic, `Subtopic Title`,n)
        rm(papcb)
        myq <- data.frame(x <- c("Query Details:","Source:","Original Data:","Search Terms","Excluded Terms","Years:","Date Queried:","Web App:",NA,"coding provided by the Policy Agendas Project"),
                          y <- c(NA,input$source,"https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Party-Rep_Platform_19.2..csv",Oterms,Eterms,paste(styear,"-",endyear,sep=""),as.character(Sys.Date()),"https://freedmanguy.shinyapps.io/querypap/",NA,NA))
        
        myp <- myd %>% 
          arrange(., n)
        myp$Subtopic <- factor(myp$Subtopic, levels = unique(myp$Subtopic))
        output$sumplot <- renderPlot({
          ggplot(myp, aes(y = n, x = Subtopic)) + 
            stat_summary(geom = "bar") +
            coord_flip() +
            theme(plot.caption = element_text(hjust = 0)) +
            labs(title = "Republican Party Platform",
                 caption = paste("Number of observations in each Policy Agendas subtopic that includes the terms:",Fterms))
        })
        myp2 <- mydy %>% 
          ungroup() %>% 
          mutate(Major = floor(Subtopic/100)) %>% 
          group_by(Major) %>% 
          group_by(year, add = T) %>% 
          summarise(n = sum(n)) %>% 
          ungroup %>% 
          arrange(., Major, year)
        myp2$year <- as.numeric(as.character(myp2$year))  
        temp <- expand.grid(c(min(myp2$year):max(myp2$year)),unique(myp2$Major))
        colnames(temp) <- c("year","Major")
        myp2 <- left_join(temp, myp2)
        myp2$n[is.na(myp2$n)] <- 0
        myp2$Major <- factor(myp2$Major,
                             levels = c(23,21:12,10:1), labels=c("23: Arts","21: Public Lands","20: Government Operations","19: International Affairs","18: Foreign Trade","17: Technology","16: Defense","15: Domestic Commerce","14: Housing","13: Social Welfare","12: Law and Crime","10: Transportation","9: Immigration","8: Energy","7: Environment","6: Education","5: Labor","4: Agriculture","3: Health","2: Civil Rights","1: Macroeconomics")) 
        output$annualplot <- renderPlot({
          ggplot(myp2, aes(x = year, y = n, fill = Major)) +
            stat_summary(geom = "bar", position = "stack") +
            theme(plot.caption = element_text(hjust = 0)) +
            ylab("Number of Observations") +
            xlab("Year") +
            labs(fill = "Major Topic",
                 title = "Republican Party Platform",
                 caption = paste("Number of observations in each Policy Agendas major topic that includes the terms:",Fterms))
        })
        output$dl <- downloadHandler(
          filename = function(){
            paste0("PapQueryPlots",Sys.Date(),".zip")
            
          },
          content = function(file){
            ggplot(myp, aes(y = n, x = Subtopic)) + 
              stat_summary(geom = "bar") +
              coord_flip() +
              theme(plot.caption = element_text(hjust = 0)) +
              labs(title = "Republican Party Platform",
                   caption = paste("Number of observations in each Policy Agendas subtopic that includes the terms:",Fterms))
            ggsave(filename = paste0(getwd(),"/","Summary.png"), device = "png", width = 9, height = 6, unit = "in")
            
            ggplot(myp2, aes(x = year, y = n, fill = Major)) +
              stat_summary(geom = "bar", position = "stack") +
              theme(plot.caption = element_text(hjust = 0)) +
              ylab("Number of Observations") +
              xlab("Year") +
              labs(fill = "Major Topic",
                   title = "Republican Party Platform",
                   caption = paste("Number of observations in each Policy Agendas major topic that includes the terms:",Fterms))
            ggsave(filename = paste0(getwd(),"/","Annual.png"), device = "png", width = 9, height = 6, unit = "in")
            
            xlsx::write.xlsx(as.data.frame(myq), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Query Details", row.names = F, showNA = F, col.names = F)
            xlsx::write.xlsx(as.data.frame(myd), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Summary", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(mydy), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Annual", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(myy), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Annual Sum", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(raw), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Raw Data", row.names = F, append = T, showNA = F)
            myfiles <- c(paste0(getwd(),"/",c("Summary.png","Annual.png")),paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"))
            zip(file,files = myfiles, flags = "-j")
          }
        )
        myd
      }
      
      if(input$source=="Congressional Bills"){
        output$url <- renderUI(a("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Legislative-congressional_bills_19.3_3_2.csv",
                                 href = "https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Legislative-congressional_bills_19.3_3_2.csv"))
        output$source <- renderText(paste("Source:",input$source))
        mydo <- read.csv("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Legislative-congressional_bills_19.3_3_2.csv")
        output$raw <- renderDT({
          mydf <- select(mydo, id, year, pap_subtopic, description) %>% 
            filter(., year %in% c(input$styear:input$endyear))
          
          i <- 1
          j <- length(terms)+1
          while(i<j){
            mydf <- filter(mydf, grepl(terms[i],description,ignore.case = T))
            i <- i+1
          }
          rm(i,j)
          if(nchar(eterms)>0){
            i <- 1
            j <- length(eterms)+1
            while(i<j){
              mydf <- filter(mydf, !grepl(eterms[i],description,ignore.case = T))
              i <- i+1
            }
            rm(i,j)
          }
          datatable(mydf, options = list(dom = 't', pageLength = nrow(mydf)))
        })
        
        output$years <- renderText({
          mydf <- select(mydo, id, year, pap_subtopic, description) %>%
            filter(., year %in% c(input$styear:input$endyear))
          styear <- min(mydf$year, na.rm = T)
          endyear <- max(mydf$year, na.rm = T)
          
          paste("Years: ",styear,"-",endyear,sep="")
        })
        
        
        
        myd <- select(mydo, id, year, pap_subtopic, description) %>% 
          filter(., year %in% c(input$styear:input$endyear))
        styear <- min(myd$year, na.rm = T)
        endyear <- max(myd$year, na.rm = T)
        i <- 1
        j <- length(terms)+1
        while(i<j){
          myd <- filter(myd, grepl(terms[i],description,ignore.case = T))
          i <- i+1
        }
        rm(i,j)
        if(nchar(eterms)>0){
          i <- 1
          j <- length(eterms)+1
          while(i<j){
            myd <- filter(myd, !grepl(eterms[i],description,ignore.case = T))
            i <- i+1
          }
          rm(i,j)
        }
        raw <- myd
        mydy <- myd %>% 
          group_by(pap_subtopic) %>% 
          group_by(year, add = T) %>% 
          count() %>% 
          rename(., "Subtopic"="pap_subtopic") %>% 
          arrange(., year, Subtopic, n)
        myy <- myd %>% 
          group_by(year) %>% 
          count() %>% 
          arrange(., year)
        myd <- myd %>% 
          group_by(pap_subtopic) %>% 
          count() %>% 
          rename(., "Subtopic"="pap_subtopic") %>% 
          arrange(., -n) %>%
          left_join(., papcb[,c("Subtopic","Subtopic Title")]) %>% 
          select(., Subtopic, `Subtopic Title`,n)
        rm(papcb)
        myq <- data.frame(x <- c("Query Details:","Source:","Original Data:","Search Terms","Excluded Terms","Years:","Date Queried:","Web App:",NA,"coding provided by the Policy Agendas Project"),
                          y <- c(NA,input$source,"https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Legislative-congressional_bills_19.3_3_2.csv",Oterms,Eterms,paste(styear,"-",endyear,sep=""),as.character(Sys.Date()),"https://freedmanguy.shinyapps.io/querypap/",NA,NA))
        
        myp <- myd %>% 
          arrange(., n)
        myp$Subtopic <- factor(myp$Subtopic, levels = unique(myp$Subtopic))
        output$sumplot <- renderPlot({
          ggplot(myp, aes(y = n, x = Subtopic)) + 
            stat_summary(geom = "bar") +
            coord_flip() +
            theme(plot.caption = element_text(hjust = 0)) +
            labs(title = "Congressional Bills",
                 caption = paste("Number of observations in each Policy Agendas subtopic that includes the terms:",Fterms))
        })
        myp2 <- mydy %>% 
          ungroup() %>% 
          mutate(Major = floor(Subtopic/100)) %>% 
          group_by(Major) %>% 
          group_by(year, add = T) %>% 
          summarise(n = sum(n)) %>% 
          ungroup %>% 
          arrange(., Major, year)
        myp2$year <- as.numeric(as.character(myp2$year))  
        temp <- expand.grid(c(min(myp2$year):max(myp2$year)),unique(myp2$Major))
        colnames(temp) <- c("year","Major")
        myp2 <- left_join(temp, myp2)
        myp2$n[is.na(myp2$n)] <- 0
        myp2$Major <- factor(myp2$Major,
                             levels = c(23,21:12,10:1), labels=c("23: Arts","21: Public Lands","20: Government Operations","19: International Affairs","18: Foreign Trade","17: Technology","16: Defense","15: Domestic Commerce","14: Housing","13: Social Welfare","12: Law and Crime","10: Transportation","9: Immigration","8: Energy","7: Environment","6: Education","5: Labor","4: Agriculture","3: Health","2: Civil Rights","1: Macroeconomics")) 
        output$annualplot <- renderPlot({
          ggplot(myp2, aes(x = year, y = n, fill = Major)) +
            stat_summary(geom = "bar", position = "stack") +
            theme(plot.caption = element_text(hjust = 0)) +
            ylab("Number of Observations") +
            xlab("Year") +
            labs(fill = "Major Topic",
                 title = "Congressional Bills",
                 caption = paste("Number of observations in each Policy Agendas major topic that includes the terms:",Fterms))
        })
        output$dl <- downloadHandler(
          filename = function(){
            paste0("PapQueryPlots",Sys.Date(),".zip")
            
          },
          content = function(file){
            ggplot(myp, aes(y = n, x = Subtopic)) + 
              stat_summary(geom = "bar") +
              coord_flip() +
              theme(plot.caption = element_text(hjust = 0)) +
              labs(title = "Congressional Bills",
                   caption = paste("Number of observations in each Policy Agendas subtopic that includes the terms:",Fterms))
            ggsave(filename = paste0(getwd(),"/","Summary.png"), device = "png", width = 9, height = 6, unit = "in")
            
            ggplot(myp2, aes(x = year, y = n, fill = Major)) +
              stat_summary(geom = "bar", position = "stack") +
              theme(plot.caption = element_text(hjust = 0)) +
              ylab("Number of Observations") +
              xlab("Year") +
              labs(fill = "Major Topic",
                   title = "Congressional Bills",
                   caption = paste("Number of observations in each Policy Agendas major topic that includes the terms:",Fterms))
            ggsave(filename = paste0(getwd(),"/","Annual.png"), device = "png", width = 9, height = 6, unit = "in")
            
            xlsx::write.xlsx(as.data.frame(myq), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Query Details", row.names = F, showNA = F, col.names = F)
            xlsx::write.xlsx(as.data.frame(myd), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Summary", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(mydy), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Annual", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(myy), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Annual Sum", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(raw), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Raw Data", row.names = F, append = T, showNA = F)
            myfiles <- c(paste0(getwd(),"/",c("Summary.png","Annual.png")),paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"))
            zip(file,files = myfiles, flags = "-j")
          }
        )
        myd
      }
      
      if(input$source=="Congressional Hearings"){
        output$url <- renderUI(a("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Legislative-congressional_hearings-19.4.csv",
                                 href = "https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Legislative-congressional_hearings-19.4.csv"))
        output$source <- renderText(paste("Source:",input$source))
        mydo <- read.csv("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Legislative-congressional_hearings-19.4.csv")
        output$raw <- renderDT({
          mydf <- select(mydo, id, year, pap_subtopic, description) %>% 
            filter(., year %in% c(input$styear:input$endyear))
          
          i <- 1
          j <- length(terms)+1
          while(i<j){
            mydf <- filter(mydf, grepl(terms[i],description,ignore.case = T))
            i <- i+1
          }
          rm(i,j)
          if(nchar(eterms)>0){
            i <- 1
            j <- length(eterms)+1
            while(i<j){
              mydf <- filter(mydf, !grepl(eterms[i],description,ignore.case = T))
              i <- i+1
            }
            rm(i,j)
          }
          datatable(mydf, options = list(dom = 't', pageLength = nrow(mydf)))
        })
        
        output$years <- renderText({
          mydf <- select(mydo, id, year, pap_subtopic, description) %>%
            filter(., year %in% c(input$styear:input$endyear))
          styear <- min(mydf$year, na.rm = T)
          endyear <- max(mydf$year, na.rm = T)
          
          paste("Years: ",styear,"-",endyear,sep="")
        })
        
        
        
        myd <- select(mydo, id, year, pap_subtopic, description) %>% 
          filter(., year %in% c(input$styear:input$endyear))
        styear <- min(myd$year, na.rm = T)
        endyear <- max(myd$year, na.rm = T)
        i <- 1
        j <- length(terms)+1
        while(i<j){
          myd <- filter(myd, grepl(terms[i],description,ignore.case = T))
          i <- i+1
        }
        rm(i,j)
        if(nchar(eterms)>0){
          i <- 1
          j <- length(eterms)+1
          while(i<j){
            myd <- filter(myd, !grepl(eterms[i],description,ignore.case = T))
            i <- i+1
          }
          rm(i,j)
        }
        raw <- myd
        mydy <- myd %>% 
          group_by(pap_subtopic) %>% 
          group_by(year, add = T) %>% 
          count() %>% 
          rename(., "Subtopic"="pap_subtopic") %>% 
          arrange(., year, Subtopic, n)
        myy <- myd %>% 
          group_by(year) %>% 
          count() %>% 
          arrange(., year)
        myd <- myd %>% 
          group_by(pap_subtopic) %>% 
          count() %>% 
          rename(., "Subtopic"="pap_subtopic") %>% 
          arrange(., -n) %>%
          left_join(., papcb[,c("Subtopic","Subtopic Title")]) %>% 
          select(., Subtopic, `Subtopic Title`,n)
        rm(papcb)
        myq <- data.frame(x <- c("Query Details:","Source:","Original Data:","Search Terms","Excluded Terms","Years:","Date Queried:","Web App:",NA,"coding provided by the Policy Agendas Project"),
                          y <- c(NA,input$source,"https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Legislative-congressional_hearings-19.4.csv",Oterms,Eterms,paste(styear,"-",endyear,sep=""),as.character(Sys.Date()),"https://freedmanguy.shinyapps.io/querypap/",NA,NA))
        
        myp <- myd %>% 
          arrange(., n)
        myp$Subtopic <- factor(myp$Subtopic, levels = unique(myp$Subtopic))
        output$sumplot <- renderPlot({
          ggplot(myp, aes(y = n, x = Subtopic)) + 
            stat_summary(geom = "bar") +
            coord_flip() +
            theme(plot.caption = element_text(hjust = 0)) +
            labs(title = "Congressional Hearings",
                 caption = paste("Number of observations in each Policy Agendas subtopic that includes the terms:",Fterms))
        })
        myp2 <- mydy %>% 
          ungroup() %>% 
          mutate(Major = floor(Subtopic/100)) %>% 
          group_by(Major) %>% 
          group_by(year, add = T) %>% 
          summarise(n = sum(n)) %>% 
          ungroup %>% 
          arrange(., Major, year)
        myp2$year <- as.numeric(as.character(myp2$year))  
        temp <- expand.grid(c(min(myp2$year):max(myp2$year)),unique(myp2$Major))
        colnames(temp) <- c("year","Major")
        myp2 <- left_join(temp, myp2)
        myp2$n[is.na(myp2$n)] <- 0
        myp2$Major <- factor(myp2$Major,
                             levels = c(23,21:12,10:1), labels=c("23: Arts","21: Public Lands","20: Government Operations","19: International Affairs","18: Foreign Trade","17: Technology","16: Defense","15: Domestic Commerce","14: Housing","13: Social Welfare","12: Law and Crime","10: Transportation","9: Immigration","8: Energy","7: Environment","6: Education","5: Labor","4: Agriculture","3: Health","2: Civil Rights","1: Macroeconomics")) 
        output$annualplot <- renderPlot({
          ggplot(myp2, aes(x = year, y = n, fill = Major)) +
            stat_summary(geom = "bar", position = "stack") +
            theme(plot.caption = element_text(hjust = 0)) +
            ylab("Number of Observations") +
            xlab("Year") +
            labs(fill = "Major Topic",
                 title = "Congressional Hearings",
                 caption = paste("Number of observations in each Policy Agendas major topic that includes the terms:",Fterms))
        })
        output$dl <- downloadHandler(
          filename = function(){
            paste0("PapQueryPlots",Sys.Date(),".zip")
            
          },
          content = function(file){
            ggplot(myp, aes(y = n, x = Subtopic)) + 
              stat_summary(geom = "bar") +
              coord_flip() +
              theme(plot.caption = element_text(hjust = 0)) +
              labs(title = "Congressional Hearings",
                   caption = paste("Number of observations in each Policy Agendas subtopic that includes the terms:",Fterms))
            ggsave(filename = paste0(getwd(),"/","Summary.png"), device = "png", width = 9, height = 6, unit = "in")
            
            ggplot(myp2, aes(x = year, y = n, fill = Major)) +
              stat_summary(geom = "bar", position = "stack") +
              theme(plot.caption = element_text(hjust = 0)) +
              ylab("Number of Observations") +
              xlab("Year") +
              labs(fill = "Major Topic",
                   title = "Congressional Hearings",
                   caption = paste("Number of observations in each Policy Agendas major topic that includes the terms:",Fterms))
            ggsave(filename = paste0(getwd(),"/","Annual.png"), device = "png", width = 9, height = 6, unit = "in")
            
            xlsx::write.xlsx(as.data.frame(myq), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Query Details", row.names = F, showNA = F, col.names = F)
            xlsx::write.xlsx(as.data.frame(myd), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Summary", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(mydy), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Annual", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(myy), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Annual Sum", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(raw), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Raw Data", row.names = F, append = T, showNA = F)
            myfiles <- c(paste0(getwd(),"/",c("Summary.png","Annual.png")),paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"))
            zip(file,files = myfiles, flags = "-j")
          }
        )
        myd
      }
      
      if(input$source=="Congressional Quarterly Almanac"){
        output$url <- renderUI(a("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Media-congressional_quarterly_almanac_19.3.csv",
                                 href = "https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Media-congressional_quarterly_almanac_19.3.csv"))
        output$source <- renderText(paste("Source:",input$source))
        mydo <- read.csv("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Media-congressional_quarterly_almanac_19.3.csv")
        output$raw <- renderDT({
          mydf <- select(mydo, id, year, subtopic, description) %>% 
            filter(., year %in% c(input$styear:input$endyear))
          
          i <- 1
          j <- length(terms)+1
          while(i<j){
            mydf <- filter(mydf, grepl(terms[i],description,ignore.case = T))
            i <- i+1
          }
          rm(i,j)
          if(nchar(eterms)>0){
            i <- 1
            j <- length(eterms)+1
            while(i<j){
              mydf <- filter(mydf, !grepl(eterms[i],description,ignore.case = T))
              i <- i+1
            }
            rm(i,j)
          }
          datatable(mydf, options = list(dom = 't', pageLength = nrow(mydf)))
        })
        
        output$years <- renderText({
          mydf <- select(mydo, id, year, subtopic, description) %>%
            filter(., year %in% c(input$styear:input$endyear))
          styear <- min(mydf$year, na.rm = T)
          endyear <- max(mydf$year, na.rm = T)
          
          paste("Years: ",styear,"-",endyear,sep="")
        })
        
        
        
        myd <- select(mydo, id, year, subtopic, description) %>% 
          filter(., year %in% c(input$styear:input$endyear))
        styear <- min(myd$year, na.rm = T)
        endyear <- max(myd$year, na.rm = T)
        i <- 1
        j <- length(terms)+1
        while(i<j){
          myd <- filter(myd, grepl(terms[i],description,ignore.case = T))
          i <- i+1
        }
        rm(i,j)
        if(nchar(eterms)>0){
          i <- 1
          j <- length(eterms)+1
          while(i<j){
            myd <- filter(myd, !grepl(eterms[i],description,ignore.case = T))
            i <- i+1
          }
          rm(i,j)
        }
        raw <- myd
        mydy <- myd %>% 
          group_by(subtopic) %>% 
          group_by(year, add = T) %>% 
          count() %>% 
          rename(., "Subtopic"="subtopic") %>% 
          arrange(., year, Subtopic, n)
        myy <- myd %>% 
          group_by(year) %>% 
          count() %>% 
          arrange(., year)
        myd <- myd %>% 
          group_by(subtopic) %>% 
          count() %>% 
          rename(., "Subtopic"="subtopic") %>% 
          arrange(., -n) %>%
          left_join(., papcb[,c("Subtopic","Subtopic Title")]) %>% 
          select(., Subtopic, `Subtopic Title`,n)
        rm(papcb)
        myq <- data.frame(x <- c("Query Details:","Source:","Original Data:","Search Terms","Excluded Terms","Years:","Date Queried:","Web App:",NA,"coding provided by the Policy Agendas Project"),
                          y <- c(NA,input$source,"https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Media-congressional_quarterly_almanac_19.3.csv",Oterms,Eterms,paste(styear,"-",endyear,sep=""),as.character(Sys.Date()),"https://freedmanguy.shinyapps.io/querypap/",NA,NA))
        
        myp <- myd %>% 
          arrange(., n)
        myp$Subtopic <- factor(myp$Subtopic, levels = unique(myp$Subtopic))
        output$sumplot <- renderPlot({
          ggplot(myp, aes(y = n, x = Subtopic)) + 
            stat_summary(geom = "bar") +
            coord_flip() +
            theme(plot.caption = element_text(hjust = 0)) +
            labs(title = "Congressional Quarterly Almanac",
                 caption = paste("Number of observations in each Policy Agendas subtopic that includes the terms:",Fterms))
        })
        myp2 <- mydy %>% 
          ungroup() %>% 
          mutate(Major = floor(Subtopic/100)) %>% 
          group_by(Major) %>% 
          group_by(year, add = T) %>% 
          summarise(n = sum(n)) %>% 
          ungroup %>% 
          arrange(., Major, year)
        myp2$year <- as.numeric(as.character(myp2$year))  
        temp <- expand.grid(c(min(myp2$year):max(myp2$year)),unique(myp2$Major))
        colnames(temp) <- c("year","Major")
        myp2 <- left_join(temp, myp2)
        myp2$n[is.na(myp2$n)] <- 0
        myp2$Major <- factor(myp2$Major,
                             levels = c(23,21:12,10:1), labels=c("23: Arts","21: Public Lands","20: Government Operations","19: International Affairs","18: Foreign Trade","17: Technology","16: Defense","15: Domestic Commerce","14: Housing","13: Social Welfare","12: Law and Crime","10: Transportation","9: Immigration","8: Energy","7: Environment","6: Education","5: Labor","4: Agriculture","3: Health","2: Civil Rights","1: Macroeconomics")) 
        output$annualplot <- renderPlot({
          ggplot(myp2, aes(x = year, y = n, fill = Major)) +
            stat_summary(geom = "bar", position = "stack") +
            theme(plot.caption = element_text(hjust = 0)) +
            ylab("Number of Observations") +
            xlab("Year") +
            labs(fill = "Major Topic",
                 title = "Congressional Quarterly Almanac",
                 caption = paste("Number of observations in each Policy Agendas major topic that includes the terms:",Fterms))
        })
        output$dl <- downloadHandler(
          filename = function(){
            paste0("PapQueryPlots",Sys.Date(),".zip")
            
          },
          content = function(file){
            ggplot(myp, aes(y = n, x = Subtopic)) + 
              stat_summary(geom = "bar") +
              coord_flip() +
              theme(plot.caption = element_text(hjust = 0)) +
              labs(title = "Congressional Quarterly Almanac",
                   caption = paste("Number of observations in each Policy Agendas subtopic that includes the terms:",Fterms))
            ggsave(filename = paste0(getwd(),"/","Summary.png"), device = "png", width = 9, height = 6, unit = "in")
            
            ggplot(myp2, aes(x = year, y = n, fill = Major)) +
              stat_summary(geom = "bar", position = "stack") +
              theme(plot.caption = element_text(hjust = 0)) +
              ylab("Number of Observations") +
              xlab("Year") +
              labs(fill = "Major Topic",
                   title = "Congressional Quarterly Almanac",
                   caption = paste("Number of observations in each Policy Agendas major topic that includes the terms:",Fterms))
            ggsave(filename = paste0(getwd(),"/","Annual.png"), device = "png", width = 9, height = 6, unit = "in")
            
            xlsx::write.xlsx(as.data.frame(myq), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Query Details", row.names = F, showNA = F, col.names = F)
            xlsx::write.xlsx(as.data.frame(myd), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Summary", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(mydy), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Annual", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(myy), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Annual Sum", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(raw), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Raw Data", row.names = F, append = T, showNA = F)
            myfiles <- c(paste0(getwd(),"/",c("Summary.png","Annual.png")),paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"))
            zip(file,files = myfiles, flags = "-j")
          }
        )
        myd
      }
      
      if(input$source=="Congressional Research Service Reports"){
        output$url <- renderUI(a("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Legislative-crs_reports_19.3.csv",
                                 href = "https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Legislative-crs_reports_19.3.csv"))
        output$source <- renderText(paste("Source:",input$source))
        mydo <- readr::read_csv("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Legislative-crs_reports_19.3.csv")
        output$raw <- renderDT({
          mydf <- select(mydo, id, year, pap_subtopic, description) %>% 
            filter(., year %in% c(input$styear:input$endyear))
          
          i <- 1
          j <- length(terms)+1
          while(i<j){
            mydf <- filter(mydf, grepl(terms[i],description,ignore.case = T))
            i <- i+1
          }
          rm(i,j)
          if(nchar(eterms)>0){
            i <- 1
            j <- length(eterms)+1
            while(i<j){
              mydf <- filter(mydf, !grepl(eterms[i],description,ignore.case = T))
              i <- i+1
            }
            rm(i,j)
          }
          datatable(mydf, options = list(dom = 't', pageLength = nrow(mydf)))
        })
        
        output$years <- renderText({
          mydf <- select(mydo, id, year, pap_subtopic, description) %>%
            filter(., year %in% c(input$styear:input$endyear))
          styear <- min(mydf$year, na.rm = T)
          endyear <- max(mydf$year, na.rm = T)
          
          paste("Years: ",styear,"-",endyear,sep="")
        })
        
        
        
        myd <- select(mydo, id, year, pap_subtopic, description) %>% 
          filter(., year %in% c(input$styear:input$endyear))
        styear <- min(myd$year, na.rm = T)
        endyear <- max(myd$year, na.rm = T)
        i <- 1
        j <- length(terms)+1
        while(i<j){
          myd <- filter(myd, grepl(terms[i],description,ignore.case = T))
          i <- i+1
        }
        rm(i,j)
        if(nchar(eterms)>0){
          i <- 1
          j <- length(eterms)+1
          while(i<j){
            myd <- filter(myd, !grepl(eterms[i],description,ignore.case = T))
            i <- i+1
          }
          rm(i,j)
        }
        raw <- myd
        mydy <- myd %>% 
          group_by(pap_subtopic) %>% 
          group_by(year, add = T) %>% 
          count() %>% 
          rename(., "Subtopic"="pap_subtopic") %>% 
          arrange(., year, Subtopic, n)
        myy <- myd %>% 
          group_by(year) %>% 
          count() %>% 
          arrange(., year)
        myd <- myd %>% 
          group_by(pap_subtopic) %>% 
          count() %>% 
          rename(., "Subtopic"="pap_subtopic") %>% 
          arrange(., -n) %>%
          left_join(., papcb[,c("Subtopic","Subtopic Title")]) %>% 
          select(., Subtopic, `Subtopic Title`,n)
        rm(papcb)
        myq <- data.frame(x <- c("Query Details:","Source:","Original Data:","Search Terms","Excluded Terms","Years:","Date Queried:","Web App:",NA,"coding provided by the Policy Agendas Project"),
                          y <- c(NA,input$source,"https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Legislative-crs_reports_19.3.csv",Oterms,Eterms,paste(styear,"-",endyear,sep=""),as.character(Sys.Date()),"https://freedmanguy.shinyapps.io/querypap/",NA,NA))
        
        myp <- myd %>% 
          arrange(., n)
        myp$Subtopic <- factor(myp$Subtopic, levels = unique(myp$Subtopic))
        output$sumplot <- renderPlot({
          ggplot(myp, aes(y = n, x = Subtopic)) + 
            stat_summary(geom = "bar") +
            coord_flip() +
            theme(plot.caption = element_text(hjust = 0)) +
            labs(title = "Congressional Research Service Reports",
                 caption = paste("Number of observations in each Policy Agendas subtopic that includes the terms:",Fterms))
        })
        myp2 <- mydy %>% 
          ungroup() %>% 
          mutate(Major = floor(Subtopic/100)) %>% 
          group_by(Major) %>% 
          group_by(year, add = T) %>% 
          summarise(n = sum(n)) %>% 
          ungroup %>% 
          arrange(., Major, year)
        myp2$year <- as.numeric(as.character(myp2$year))  
        temp <- expand.grid(c(min(myp2$year):max(myp2$year)),unique(myp2$Major))
        colnames(temp) <- c("year","Major")
        myp2 <- left_join(temp, myp2)
        myp2$n[is.na(myp2$n)] <- 0
        myp2$Major <- factor(myp2$Major,
                             levels = c(23,21:12,10:1), labels=c("23: Arts","21: Public Lands","20: Government Operations","19: International Affairs","18: Foreign Trade","17: Technology","16: Defense","15: Domestic Commerce","14: Housing","13: Social Welfare","12: Law and Crime","10: Transportation","9: Immigration","8: Energy","7: Environment","6: Education","5: Labor","4: Agriculture","3: Health","2: Civil Rights","1: Macroeconomics")) 
        output$annualplot <- renderPlot({
          ggplot(myp2, aes(x = year, y = n, fill = Major)) +
            stat_summary(geom = "bar", position = "stack") +
            theme(plot.caption = element_text(hjust = 0)) +
            ylab("Number of Observations") +
            xlab("Year") +
            labs(fill = "Major Topic",
                 title = "Congressional Research Service Reports",
                 caption = paste("Number of observations in each Policy Agendas major topic that includes the terms:",Fterms))
        })
        output$dl <- downloadHandler(
          filename = function(){
            paste0("PapQueryPlots",Sys.Date(),".zip")
            
          },
          content = function(file){
            ggplot(myp, aes(y = n, x = Subtopic)) + 
              stat_summary(geom = "bar") +
              coord_flip() +
              theme(plot.caption = element_text(hjust = 0)) +
              labs(title = "Congressional Research Service Reports",
                   caption = paste("Number of observations in each Policy Agendas subtopic that includes the terms:",Fterms))
            ggsave(filename = paste0(getwd(),"/","Summary.png"), device = "png", width = 9, height = 6, unit = "in")
            
            ggplot(myp2, aes(x = year, y = n, fill = Major)) +
              stat_summary(geom = "bar", position = "stack") +
              theme(plot.caption = element_text(hjust = 0)) +
              ylab("Number of Observations") +
              xlab("Year") +
              labs(fill = "Major Topic",
                   title = "Congressional Research Service Reports",
                   caption = paste("Number of observations in each Policy Agendas major topic that includes the terms:",Fterms))
            ggsave(filename = paste0(getwd(),"/","Annual.png"), device = "png", width = 9, height = 6, unit = "in")
            
            xlsx::write.xlsx(as.data.frame(myq), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Query Details", row.names = F, showNA = F, col.names = F)
            xlsx::write.xlsx(as.data.frame(myd), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Summary", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(mydy), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Annual", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(myy), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Annual Sum", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(raw), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Raw Data", row.names = F, append = T, showNA = F)
            myfiles <- c(paste0(getwd(),"/",c("Summary.png","Annual.png")),paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"))
            zip(file,files = myfiles, flags = "-j")
          }
        )
        myd
      }
      
      if(input$source=="Public Law Titles"){
        output$url <- renderUI(a("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Legislative_Public_Laws_Titles_19.3.csv",
                                 href = "https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Legislative_Public_Laws_Titles_19.3.csv"))
        output$source <- renderText(paste("Source:",input$source))
        mydo <- read.csv("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Legislative_Public_Laws_Titles_19.3.csv")
        output$raw <- renderDT({
          mydf <- select(mydo, id, year, law_pap_subtopic, law_description) %>% 
            filter(., year %in% c(input$styear:input$endyear))
          
          i <- 1
          j <- length(terms)+1
          while(i<j){
            mydf <- filter(mydf, grepl(terms[i],law_description,ignore.case = T))
            i <- i+1
          }
          rm(i,j)
          if(nchar(eterms)>0){
            i <- 1
            j <- length(eterms)+1
            while(i<j){
              mydf <- filter(mydf, !grepl(eterms[i],description,ignore.case = T))
              i <- i+1
            }
            rm(i,j)
          }
          datatable(mydf, options = list(dom = 't', pageLength = nrow(mydf)))
        })
        
        output$years <- renderText({
          mydf <- select(mydo, id, year, law_pap_subtopic, law_description) %>%
            filter(., year %in% c(input$styear:input$endyear))
          styear <- min(mydf$year, na.rm = T)
          endyear <- max(mydf$year, na.rm = T)
          
          paste("Years: ",styear,"-",endyear,sep="")
        })
        
        
        
        myd <- select(mydo, id, year, law_pap_subtopic, law_description) %>% 
          filter(., year %in% c(input$styear:input$endyear))
        styear <- min(myd$year, na.rm = T)
        endyear <- max(myd$year, na.rm = T)
        i <- 1
        j <- length(terms)+1
        while(i<j){
          myd <- filter(myd, grepl(terms[i],law_description,ignore.case = T))
          i <- i+1
        }
        rm(i,j)
        if(nchar(eterms)>0){
          i <- 1
          j <- length(eterms)+1
          while(i<j){
            myd <- filter(myd, !grepl(eterms[i],description,ignore.case = T))
            i <- i+1
          }
          rm(i,j)
        }
        raw <- myd
        mydy <- myd %>% 
          group_by(law_pap_subtopic) %>% 
          group_by(year, add = T) %>% 
          count() %>% 
          rename(., "Subtopic"="law_pap_subtopic") %>% 
          arrange(., year, Subtopic, n)
        myy <- myd %>% 
          group_by(year) %>% 
          count() %>% 
          arrange(., year)
        myd <- myd %>% 
          group_by(law_pap_subtopic) %>% 
          count() %>% 
          rename(., "Subtopic"="law_pap_subtopic") %>% 
          arrange(., -n) %>%
          left_join(., papcb[,c("Subtopic","Subtopic Title")]) %>% 
          select(., Subtopic, `Subtopic Title`,n)
        rm(papcb)
        myq <- data.frame(x <- c("Query Details:","Source:","Original Data:","Search Terms","Excluded Terms","Years:","Date Queried:","Web App:",NA,"coding provided by the Policy Agendas Project"),
                          y <- c(NA,input$source,"https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Legislative_Public_Laws_Titles_19.3.csv",Oterms,Eterms,paste(styear,"-",endyear,sep=""),as.character(Sys.Date()),"https://freedmanguy.shinyapps.io/querypap/",NA,NA))
        
        myp <- myd %>% 
          arrange(., n)
        myp$Subtopic <- factor(myp$Subtopic, levels = unique(myp$Subtopic))
        output$sumplot <- renderPlot({
          ggplot(myp, aes(y = n, x = Subtopic)) + 
            stat_summary(geom = "bar") +
            coord_flip() +
            theme(plot.caption = element_text(hjust = 0)) +
            labs(title = "Public Law Titles",
                 caption = paste("Number of observations in each Policy Agendas subtopic that includes the terms:",Fterms))
        })
        myp2 <- mydy %>% 
          ungroup() %>% 
          mutate(Major = floor(Subtopic/100)) %>% 
          group_by(Major) %>% 
          group_by(year, add = T) %>% 
          summarise(n = sum(n)) %>% 
          ungroup %>% 
          arrange(., Major, year)
        myp2$year <- as.numeric(as.character(myp2$year))  
        temp <- expand.grid(c(min(myp2$year):max(myp2$year)),unique(myp2$Major))
        colnames(temp) <- c("year","Major")
        myp2 <- left_join(temp, myp2)
        myp2$n[is.na(myp2$n)] <- 0
        myp2$Major <- factor(myp2$Major,
                             levels = c(23,21:12,10:1), labels=c("23: Arts","21: Public Lands","20: Government Operations","19: International Affairs","18: Foreign Trade","17: Technology","16: Defense","15: Domestic Commerce","14: Housing","13: Social Welfare","12: Law and Crime","10: Transportation","9: Immigration","8: Energy","7: Environment","6: Education","5: Labor","4: Agriculture","3: Health","2: Civil Rights","1: Macroeconomics")) 
        output$annualplot <- renderPlot({
          ggplot(myp2, aes(x = year, y = n, fill = Major)) +
            stat_summary(geom = "bar", position = "stack") +
            theme(plot.caption = element_text(hjust = 0)) +
            ylab("Number of Observations") +
            xlab("Year") +
            labs(fill = "Major Topic",
                 title = "Public Law Titles",
                 caption = paste("Number of observations in each Policy Agendas major topic that includes the terms:",Fterms))
        })
        output$dl <- downloadHandler(
          filename = function(){
            paste0("PapQueryPlots",Sys.Date(),".zip")
            
          },
          content = function(file){
            ggplot(myp, aes(y = n, x = Subtopic)) + 
              stat_summary(geom = "bar") +
              coord_flip() +
              theme(plot.caption = element_text(hjust = 0)) +
              labs(title = "Public Law Titles",
                   caption = paste("Number of observations in each Policy Agendas subtopic that includes the terms:",Fterms))
            ggsave(filename = paste0(getwd(),"/","Summary.png"), device = "png", width = 9, height = 6, unit = "in")
            
            ggplot(myp2, aes(x = year, y = n, fill = Major)) +
              stat_summary(geom = "bar", position = "stack") +
              theme(plot.caption = element_text(hjust = 0)) +
              ylab("Number of Observations") +
              xlab("Year") +
              labs(fill = "Major Topic",
                   title = "Public Law Titles",
                   caption = paste("Number of observations in each Policy Agendas major topic that includes the terms:",Fterms))
            ggsave(filename = paste0(getwd(),"/","Annual.png"), device = "png", width = 9, height = 6, unit = "in")
            
            xlsx::write.xlsx(as.data.frame(myq), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Query Details", row.names = F, showNA = F, col.names = F)
            xlsx::write.xlsx(as.data.frame(myd), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Summary", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(mydy), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Annual", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(myy), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Annual Sum", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(raw), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Raw Data", row.names = F, append = T, showNA = F)
            myfiles <- c(paste0(getwd(),"/",c("Summary.png","Annual.png")),paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"))
            zip(file,files = myfiles, flags = "-j")
          }
        )
        myd
      }
      
      if(input$source=="Public Laws"){
        output$url <- renderUI(a("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Legislative-public_laws_19.3.csv",
                                 href = "https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Legislative-public_laws_19.3.csv"))
        output$source <- renderText(paste("Source:",input$source))
        mydo <- read.csv("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Legislative-public_laws_19.3.csv")
        output$raw <- renderDT({
          mydf <- select(mydo, id, year, pap_subtopic, description) %>% 
            filter(., year %in% c(input$styear:input$endyear))
          
          i <- 1
          j <- length(terms)+1
          while(i<j){
            mydf <- filter(mydf, grepl(terms[i],description,ignore.case = T))
            i <- i+1
          }
          rm(i,j)
          if(nchar(eterms)>0){
            i <- 1
            j <- length(eterms)+1
            while(i<j){
              mydf <- filter(mydf, !grepl(eterms[i],description,ignore.case = T))
              i <- i+1
            }
            rm(i,j)
          }
          datatable(mydf, options = list(dom = 't', pageLength = nrow(mydf)))
        })
        
        output$years <- renderText({
          mydf <- select(mydo, id, year, pap_subtopic, description) %>%
            filter(., year %in% c(input$styear:input$endyear))
          styear <- min(mydf$year, na.rm = T)
          endyear <- max(mydf$year, na.rm = T)
          
          paste("Years: ",styear,"-",endyear,sep="")
        })
        
        
        
        myd <- select(mydo, id, year, pap_subtopic, description) %>% 
          filter(., year %in% c(input$styear:input$endyear))
        styear <- min(myd$year, na.rm = T)
        endyear <- max(myd$year, na.rm = T)
        i <- 1
        j <- length(terms)+1
        while(i<j){
          myd <- filter(myd, grepl(terms[i],description,ignore.case = T))
          i <- i+1
        }
        rm(i,j)
        if(nchar(eterms)>0){
          i <- 1
          j <- length(eterms)+1
          while(i<j){
            myd <- filter(myd, !grepl(eterms[i],description,ignore.case = T))
            i <- i+1
          }
          rm(i,j)
        }
        raw <- myd
        mydy <- myd %>% 
          group_by(pap_subtopic) %>% 
          group_by(year, add = T) %>% 
          count() %>% 
          rename(., "Subtopic"="pap_subtopic") %>% 
          arrange(., year, Subtopic, n)
        myy <- myd %>% 
          group_by(year) %>% 
          count() %>% 
          arrange(., year)
        myd <- myd %>% 
          group_by(pap_subtopic) %>% 
          count() %>% 
          rename(., "Subtopic"="pap_subtopic") %>% 
          arrange(., -n) %>%
          left_join(., papcb[,c("Subtopic","Subtopic Title")]) %>% 
          select(., Subtopic, `Subtopic Title`,n)
        rm(papcb)
        myq <- data.frame(x <- c("Query Details:","Source:","Original Data:","Search Terms","Excluded Terms","Years:","Date Queried:","Web App:",NA,"coding provided by the Policy Agendas Project"),
                          y <- c(NA,input$source,"https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Legislative-public_laws_19.3.csv",Oterms,Eterms,paste(styear,"-",endyear,sep=""),as.character(Sys.Date()),"https://freedmanguy.shinyapps.io/querypap/",NA,NA))
        
        myp <- myd %>% 
          arrange(., n)
        myp$Subtopic <- factor(myp$Subtopic, levels = unique(myp$Subtopic))
        output$sumplot <- renderPlot({
          ggplot(myp, aes(y = n, x = Subtopic)) + 
            stat_summary(geom = "bar") +
            coord_flip() +
            theme(plot.caption = element_text(hjust = 0)) +
            labs(title = "Public Laws",
                 caption = paste("Number of observations in each Policy Agendas subtopic that includes the terms:",Fterms))
        })
        myp2 <- mydy %>% 
          ungroup() %>% 
          mutate(Major = floor(Subtopic/100)) %>% 
          group_by(Major) %>% 
          group_by(year, add = T) %>% 
          summarise(n = sum(n)) %>% 
          ungroup %>% 
          arrange(., Major, year)
        myp2$year <- as.numeric(as.character(myp2$year))  
        temp <- expand.grid(c(min(myp2$year):max(myp2$year)),unique(myp2$Major))
        colnames(temp) <- c("year","Major")
        myp2 <- left_join(temp, myp2)
        myp2$n[is.na(myp2$n)] <- 0
        myp2$Major <- factor(myp2$Major,
                             levels = c(23,21:12,10:1), labels=c("23: Arts","21: Public Lands","20: Government Operations","19: International Affairs","18: Foreign Trade","17: Technology","16: Defense","15: Domestic Commerce","14: Housing","13: Social Welfare","12: Law and Crime","10: Transportation","9: Immigration","8: Energy","7: Environment","6: Education","5: Labor","4: Agriculture","3: Health","2: Civil Rights","1: Macroeconomics")) 
        output$annualplot <- renderPlot({
          ggplot(myp2, aes(x = year, y = n, fill = Major)) +
            stat_summary(geom = "bar", position = "stack") +
            theme(plot.caption = element_text(hjust = 0)) +
            ylab("Number of Observations") +
            xlab("Year") +
            labs(fill = "Major Topic",
                 title = "Public Laws",
                 caption = paste("Number of observations in each Policy Agendas major topic that includes the terms:",Fterms))
        })
        output$dl <- downloadHandler(
          filename = function(){
            paste0("PapQueryPlots",Sys.Date(),".zip")
            
          },
          content = function(file){
            ggplot(myp, aes(y = n, x = Subtopic)) + 
              stat_summary(geom = "bar") +
              coord_flip() +
              theme(plot.caption = element_text(hjust = 0)) +
              labs(title = "Public Laws",
                   caption = paste("Number of observations in each Policy Agendas subtopic that includes the terms:",Fterms))
            ggsave(filename = paste0(getwd(),"/","Summary.png"), device = "png", width = 9, height = 6, unit = "in")
            
            ggplot(myp2, aes(x = year, y = n, fill = Major)) +
              stat_summary(geom = "bar", position = "stack") +
              theme(plot.caption = element_text(hjust = 0)) +
              ylab("Number of Observations") +
              xlab("Year") +
              labs(fill = "Major Topic",
                   title = "Public Laws",
                   caption = paste("Number of observations in each Policy Agendas major topic that includes the terms:",Fterms))
            ggsave(filename = paste0(getwd(),"/","Annual.png"), device = "png", width = 9, height = 6, unit = "in")
            
            xlsx::write.xlsx(as.data.frame(myq), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Query Details", row.names = F, showNA = F, col.names = F)
            xlsx::write.xlsx(as.data.frame(myd), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Summary", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(mydy), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Annual", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(myy), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Annual Sum", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(raw), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Raw Data", row.names = F, append = T, showNA = F)
            myfiles <- c(paste0(getwd(),"/",c("Summary.png","Annual.png")),paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"))
            zip(file,files = myfiles, flags = "-j")
          }
        )
        myd
      }
      
      if(input$source=="Roll Call Votes"){
        output$url <- renderUI(a("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Legislative_roll_call_votes_19.4.csv",
                                 href = "https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Legislative_roll_call_votes_19.4.csv"))
        output$source <- renderText(paste("Source:",input$source))
        mydo <- read.csv("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Legislative_roll_call_votes_19.4.csv")
        output$raw <- renderDT({
          mydf <- select(mydo, id, year, pap_subtopic, description) %>% 
            filter(., year %in% c(input$styear:input$endyear))
          
          i <- 1
          j <- length(terms)+1
          while(i<j){
            mydf <- filter(mydf, grepl(terms[i],description,ignore.case = T))
            i <- i+1
          }
          rm(i,j)
          if(nchar(eterms)>0){
            i <- 1
            j <- length(eterms)+1
            while(i<j){
              mydf <- filter(mydf, !grepl(eterms[i],description,ignore.case = T))
              i <- i+1
            }
            rm(i,j)
          }
          datatable(mydf, options = list(dom = 't', pageLength = nrow(mydf)))
        })
        
        output$years <- renderText({
          mydf <- select(mydo, id, year, pap_subtopic, description) %>%
            filter(., year %in% c(input$styear:input$endyear))
          styear <- min(mydf$year, na.rm = T)
          endyear <- max(mydf$year, na.rm = T)
          
          paste("Years: ",styear,"-",endyear,sep="")
        })
        
        
        
        myd <- select(mydo, id, year, pap_subtopic, description) %>% 
          filter(., year %in% c(input$styear:input$endyear))
        styear <- min(myd$year, na.rm = T)
        endyear <- max(myd$year, na.rm = T)
        i <- 1
        j <- length(terms)+1
        while(i<j){
          myd <- filter(myd, grepl(terms[i],description,ignore.case = T))
          i <- i+1
        }
        rm(i,j)
        if(nchar(eterms)>0){
          i <- 1
          j <- length(eterms)+1
          while(i<j){
            myd <- filter(myd, !grepl(eterms[i],description,ignore.case = T))
            i <- i+1
          }
          rm(i,j)
        }
        raw <- myd
        mydy <- myd %>% 
          group_by(pap_subtopic) %>% 
          group_by(year, add = T) %>% 
          count() %>% 
          rename(., "Subtopic"="pap_subtopic") %>% 
          arrange(., year, Subtopic, n)
        myy <- myd %>% 
          group_by(year) %>% 
          count() %>% 
          arrange(., year)
        myd <- myd %>% 
          group_by(pap_subtopic) %>% 
          count() %>% 
          rename(., "Subtopic"="pap_subtopic") %>% 
          arrange(., -n) %>%
          left_join(., papcb[,c("Subtopic","Subtopic Title")]) %>% 
          select(., Subtopic, `Subtopic Title`,n)
        rm(papcb)
        myq <- data.frame(x <- c("Query Details:","Source:","Original Data:","Search Terms","Excluded Terms","Years:","Date Queried:","Web App:",NA,"coding provided by the Policy Agendas Project"),
                          y <- c(NA,input$source,"https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Legislative_roll_call_votes_19.4.csv",Oterms,Eterms,paste(styear,"-",endyear,sep=""),as.character(Sys.Date()),"https://freedmanguy.shinyapps.io/querypap/",NA,NA))
        
        myp <- myd %>% 
          arrange(., n)
        myp$Subtopic <- factor(myp$Subtopic, levels = unique(myp$Subtopic))
        output$sumplot <- renderPlot({
          ggplot(myp, aes(y = n, x = Subtopic)) + 
            stat_summary(geom = "bar") +
            coord_flip() +
            theme(plot.caption = element_text(hjust = 0)) +
            labs(title = "Roll Call Votes",
                 caption = paste("Number of observations in each Policy Agendas subtopic that includes the terms:",Fterms))
        })
        
        myp2 <- mydy %>% 
          ungroup() %>% 
          mutate(Major = floor(Subtopic/100)) %>% 
          group_by(Major) %>% 
          group_by(year, add = T) %>% 
          summarise(n = sum(n)) %>% 
          ungroup %>% 
          arrange(., Major, year)
        myp2$year <- as.numeric(as.character(myp2$year))  
        temp <- expand.grid(c(min(myp2$year):max(myp2$year)),unique(myp2$Major))
        colnames(temp) <- c("year","Major")
        myp2 <- left_join(temp, myp2)
        myp2$n[is.na(myp2$n)] <- 0
        myp2$Major <- factor(myp2$Major,
                             levels = c(23,21:12,10:1), labels=c("23: Arts","21: Public Lands","20: Government Operations","19: International Affairs","18: Foreign Trade","17: Technology","16: Defense","15: Domestic Commerce","14: Housing","13: Social Welfare","12: Law and Crime","10: Transportation","9: Immigration","8: Energy","7: Environment","6: Education","5: Labor","4: Agriculture","3: Health","2: Civil Rights","1: Macroeconomics")) 
        output$annualplot <- renderPlot({
          ggplot(myp2, aes(x = year, y = n, fill = Major)) +
            stat_summary(geom = "bar", position = "stack") +
            theme(plot.caption = element_text(hjust = 0)) +
            ylab("Number of Observations") +
            xlab("Year") +
            labs(fill = "Major Topic",
                 title = "Roll Call Votes",
                 caption = paste("Number of observations in each Policy Agendas major topic that includes the terms:",Fterms))
        })
        output$dl <- downloadHandler(
          filename = function(){
            paste0("PapQueryPlots",Sys.Date(),".zip")
            
          },
          content = function(file){
            ggplot(myp, aes(y = n, x = Subtopic)) + 
              stat_summary(geom = "bar") +
              coord_flip() +
              theme(plot.caption = element_text(hjust = 0)) +
              labs(title = "Roll Call Votes",
                   caption = paste("Number of observations in each Policy Agendas subtopic that includes the terms:",Fterms))
            ggsave(filename = paste0(getwd(),"/","Summary.png"), device = "png", width = 9, height = 6, unit = "in")
            
            ggplot(myp2, aes(x = year, y = n, fill = Major)) +
              stat_summary(geom = "bar", position = "stack") +
              theme(plot.caption = element_text(hjust = 0)) +
              ylab("Number of Observations") +
              xlab("Year") +
              labs(fill = "Major Topic",
                   title = "Roll Call Votes",
                   caption = paste("Number of observations in each Policy Agendas major topic that includes the terms:",Fterms))
            ggsave(filename = paste0(getwd(),"/","Annual.png"), device = "png", width = 9, height = 6, unit = "in")
            
            xlsx::write.xlsx(as.data.frame(myq), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Query Details", row.names = F, showNA = F, col.names = F)
            xlsx::write.xlsx(as.data.frame(myd), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Summary", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(mydy), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Annual", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(myy), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Annual Sum", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(raw), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Raw Data", row.names = F, append = T, showNA = F)
            myfiles <- c(paste0(getwd(),"/",c("Summary.png","Annual.png")),paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"))
            zip(file,files = myfiles, flags = "-j")
          }
        )
        myd
      }
      
      if(input$source=="Supreme Court Cases"){
        output$url <- renderUI(a("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Judicial-supreme_court_cases_19.2.csv",
                                 href = "https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Judicial-supreme_court_cases_19.2.csv"))
        output$source <- renderText(paste("Source:",input$source))
        mydo <- read.csv("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Legislative_roll_call_votes_19.4.csv")
        output$raw <- renderDT({
          mydf <- select(mydo, id, year, pap_subtopic, description) %>% 
            filter(., year %in% c(input$styear:input$endyear))
          
          i <- 1
          j <- length(terms)+1
          while(i<j){
            mydf <- filter(mydf, grepl(terms[i],description,ignore.case = T))
            i <- i+1
          }
          rm(i,j)
          if(nchar(eterms)>0){
            i <- 1
            j <- length(eterms)+1
            while(i<j){
              mydf <- filter(mydf, !grepl(eterms[i],description,ignore.case = T))
              i <- i+1
            }
            rm(i,j)
          }
          datatable(mydf, options = list(dom = 't', pageLength = nrow(mydf)))
        })
        
        output$years <- renderText({
          mydf <- select(mydo, id, year, pap_subtopic, description) %>%
            filter(., year %in% c(input$styear:input$endyear))
          styear <- min(mydf$year, na.rm = T)
          endyear <- max(mydf$year, na.rm = T)
          
          paste("Years: ",styear,"-",endyear,sep="")
        })
        
        
        
        myd <- select(mydo, id, year, pap_subtopic, description) %>% 
          filter(., year %in% c(input$styear:input$endyear))
        styear <- min(myd$year, na.rm = T)
        endyear <- max(myd$year, na.rm = T)
        i <- 1
        j <- length(terms)+1
        while(i<j){
          myd <- filter(myd, grepl(terms[i],description,ignore.case = T))
          i <- i+1
        }
        rm(i,j)
        if(nchar(eterms)>0){
          i <- 1
          j <- length(eterms)+1
          while(i<j){
            myd <- filter(myd, !grepl(eterms[i],description,ignore.case = T))
            i <- i+1
          }
          rm(i,j)
        }
        raw <- myd
        mydy <- myd %>% 
          group_by(pap_subtopic) %>% 
          group_by(year, add = T) %>% 
          count() %>% 
          rename(., "Subtopic"="pap_subtopic") %>% 
          arrange(., year, Subtopic, n)
        myy <- myd %>% 
          group_by(year) %>% 
          count() %>% 
          arrange(., year)
        myd <- myd %>% 
          group_by(pap_subtopic) %>% 
          count() %>% 
          rename(., "Subtopic"="pap_subtopic") %>% 
          arrange(., -n) %>%
          left_join(., papcb[,c("Subtopic","Subtopic Title")]) %>% 
          select(., Subtopic, `Subtopic Title`,n)
        rm(papcb)
        myq <- data.frame(x <- c("Query Details:","Source:","Original Data:","Search Terms","Excluded Terms","Years:","Date Queried:","Web App:",NA,"coding provided by the Policy Agendas Project"),
                          y <- c(NA,input$source,"https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Judicial-supreme_court_cases_19.2.csv",Oterms,Eterms,paste(styear,"-",endyear,sep=""),as.character(Sys.Date()),"https://freedmanguy.shinyapps.io/querypap/",NA,NA))
        
        myp <- myd %>% 
          arrange(., n)
        myp$Subtopic <- factor(myp$Subtopic, levels = unique(myp$Subtopic))
        output$sumplot <- renderPlot({
          ggplot(myp, aes(y = n, x = Subtopic)) + 
            stat_summary(geom = "bar") +
            coord_flip() +
            theme(plot.caption = element_text(hjust = 0)) +
            labs(title = "Supreme Court Cases",
                 caption = paste("Number of observations in each Policy Agendas subtopic that includes the terms:",Fterms))
        })
        
        myp2 <- mydy %>% 
          ungroup() %>% 
          mutate(Major = floor(Subtopic/100)) %>% 
          group_by(Major) %>% 
          group_by(year, add = T) %>% 
          summarise(n = sum(n)) %>% 
          ungroup %>% 
          arrange(., Major, year)
        myp2$year <- as.numeric(as.character(myp2$year))  
        temp <- expand.grid(c(min(myp2$year):max(myp2$year)),unique(myp2$Major))
        colnames(temp) <- c("year","Major")
        myp2 <- left_join(temp, myp2)
        myp2$n[is.na(myp2$n)] <- 0
        myp2$Major <- factor(myp2$Major,
                             levels = c(23,21:12,10:1), labels=c("23: Arts","21: Public Lands","20: Government Operations","19: International Affairs","18: Foreign Trade","17: Technology","16: Defense","15: Domestic Commerce","14: Housing","13: Social Welfare","12: Law and Crime","10: Transportation","9: Immigration","8: Energy","7: Environment","6: Education","5: Labor","4: Agriculture","3: Health","2: Civil Rights","1: Macroeconomics")) 
        output$annualplot <- renderPlot({
          ggplot(myp2, aes(x = year, y = n, fill = Major)) +
            stat_summary(geom = "bar", position = "stack") +
            theme(plot.caption = element_text(hjust = 0)) +
            ylab("Number of Observations") +
            xlab("Year") +
            labs(fill = "Major Topic",
                 title = "Supreme Court Cases",
                 caption = paste("Number of observations in each Policy Agendas major topic that includes the terms:",Fterms))
        })
        output$dl <- downloadHandler(
          filename = function(){
            paste0("PapQueryPlots",Sys.Date(),".zip")
            
          },
          content = function(file){
            ggplot(myp, aes(y = n, x = Subtopic)) + 
              stat_summary(geom = "bar") +
              coord_flip() +
              theme(plot.caption = element_text(hjust = 0)) +
              labs(title = "Supreme Court Cases",
                   caption = paste("Number of observations in each Policy Agendas subtopic that includes the terms:",Fterms))
            ggsave(filename = paste0(getwd(),"/","Summary.png"), device = "png", width = 9, height = 6, unit = "in")
            
            ggplot(myp2, aes(x = year, y = n, fill = Major)) +
              stat_summary(geom = "bar", position = "stack") +
              theme(plot.caption = element_text(hjust = 0)) +
              ylab("Number of Observations") +
              xlab("Year") +
              labs(fill = "Major Topic",
                   title = "Supreme Court Cases",
                   caption = paste("Number of observations in each Policy Agendas major topic that includes the terms:",Fterms))
            ggsave(filename = paste0(getwd(),"/","Annual.png"), device = "png", width = 9, height = 6, unit = "in")
            
            xlsx::write.xlsx(as.data.frame(myq), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Query Details", row.names = F, showNA = F, col.names = F)
            xlsx::write.xlsx(as.data.frame(myd), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Summary", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(mydy), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Annual", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(myy), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Annual Sum", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(raw), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Raw Data", row.names = F, append = T, showNA = F)
            myfiles <- c(paste0(getwd(),"/",c("Summary.png","Annual.png")),paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"))
            zip(file,files = myfiles, flags = "-j")
          }
        )
        myd
      }
      
      if(input$source=="New York Times Front Page"){
        output$url <- renderUI(a("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Media-NYTimes_front_page_19.3.csv",
                                 href = "https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Media-NYTimes_front_page_19.3.csv"))
        output$source <- renderText(paste("Source:",input$source))
        mydo <- read.csv("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Media-NYTimes_front_page_19.3.csv")
        output$raw <- renderDT({
          mydf <- select(mydo, id, year, subtopic, title) %>% 
            filter(., year %in% c(input$styear:input$endyear))
          
          i <- 1
          j <- length(terms)+1
          while(i<j){
            mydf <- filter(mydf, grepl(terms[i],title,ignore.case = T))
            i <- i+1
          }
          rm(i,j)
          if(nchar(eterms)>0){
            i <- 1
            j <- length(eterms)+1
            while(i<j){
              mydf <- filter(mydf, !grepl(eterms[i],description,ignore.case = T))
              i <- i+1
            }
            rm(i,j)
          }
          datatable(mydf, options = list(dom = 't', pageLength = nrow(mydf)))
        })
        
        output$years <- renderText({
          mydf <- select(mydo, id, year, subtopic, title) %>%
            filter(., year %in% c(input$styear:input$endyear))
          styear <- min(mydf$year, na.rm = T)
          endyear <- max(mydf$year, na.rm = T)
          
          paste("Years: ",styear,"-",endyear,sep="")
        })
        
        
        
        myd <- select(mydo, id, year, subtopic, title) %>% 
          filter(., year %in% c(input$styear:input$endyear))
        styear <- min(myd$year, na.rm = T)
        endyear <- max(myd$year, na.rm = T)
        i <- 1
        j <- length(terms)+1
        while(i<j){
          myd <- filter(myd, grepl(terms[i],title,ignore.case = T))
          i <- i+1
        }
        rm(i,j)
        if(nchar(eterms)>0){
          i <- 1
          j <- length(eterms)+1
          while(i<j){
            myd <- filter(myd, !grepl(eterms[i],description,ignore.case = T))
            i <- i+1
          }
          rm(i,j)
        }
        raw <- myd
        mydy <- myd %>% 
          group_by(subtopic) %>% 
          group_by(year, add = T) %>% 
          count() %>% 
          rename(., "Subtopic"="subtopic") %>% 
          arrange(., year, Subtopic, n)
        myy <- myd %>% 
          group_by(year) %>% 
          count() %>% 
          arrange(., year)
        myd <- myd %>% 
          group_by(subtopic) %>% 
          count() %>% 
          rename(., "Subtopic"="subtopic") %>% 
          arrange(., -n) %>%
          left_join(., papcb[,c("Subtopic","Subtopic Title")]) %>% 
          select(., Subtopic, `Subtopic Title`,n)
        rm(papcb)
        myq <- data.frame(x <- c("Query Details:","Source:","Original Data:","Search Terms","Excluded Terms","Years:","Date Queried:","Web App:",NA,"coding provided by the Policy Agendas Project"),
                          y <- c(NA,input$source,"https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Media-NYTimes_front_page_19.3.csv",Oterms,Eterms,paste(styear,"-",endyear,sep=""),as.character(Sys.Date()),"https://freedmanguy.shinyapps.io/querypap/",NA,NA))
        
        myp <- myd %>% 
          arrange(., n)
        myp$Subtopic <- factor(myp$Subtopic, levels = unique(myp$Subtopic))
        output$sumplot <- renderPlot({
          ggplot(myp, aes(y = n, x = Subtopic)) + 
            stat_summary(geom = "bar") +
            coord_flip() +
            theme(plot.caption = element_text(hjust = 0)) +
            labs(title = "New York Times Front Page",
                 caption = paste("Number of observations in each Policy Agendas subtopic that includes the terms:",Fterms))
        })
        myp2 <- mydy %>% 
          ungroup() %>% 
          mutate(Major = floor(Subtopic/100)) %>% 
          group_by(Major) %>% 
          group_by(year, add = T) %>% 
          summarise(n = sum(n)) %>% 
          ungroup %>% 
          arrange(., Major, year)
        myp2$year <- as.numeric(as.character(myp2$year))  
        temp <- expand.grid(c(min(myp2$year):max(myp2$year)),unique(myp2$Major))
        colnames(temp) <- c("year","Major")
        myp2 <- left_join(temp, myp2)
        myp2$n[is.na(myp2$n)] <- 0
        myp2$Major <- factor(myp2$Major,
                             levels = c(23,21:12,10:1), labels=c("23: Arts","21: Public Lands","20: Government Operations","19: International Affairs","18: Foreign Trade","17: Technology","16: Defense","15: Domestic Commerce","14: Housing","13: Social Welfare","12: Law and Crime","10: Transportation","9: Immigration","8: Energy","7: Environment","6: Education","5: Labor","4: Agriculture","3: Health","2: Civil Rights","1: Macroeconomics")) 
        output$annualplot <- renderPlot({
          ggplot(myp2, aes(x = year, y = n, fill = Major)) +
            stat_summary(geom = "bar", position = "stack") +
            theme(plot.caption = element_text(hjust = 0)) +
            ylab("Number of Observations") +
            xlab("Year") +
            labs(fill = "Major Topic",
                 title = "New York Times Front Page",
                 caption = paste("Number of observations in each Policy Agendas major topic that includes the terms:",Fterms))
        })
        output$dl <- downloadHandler(
          filename = function(){
            paste0("PapQueryPlots",Sys.Date(),".zip")
            
          },
          content = function(file){
            ggplot(myp, aes(y = n, x = Subtopic)) + 
              stat_summary(geom = "bar") +
              coord_flip() +
              theme(plot.caption = element_text(hjust = 0)) +
              labs(title = "New York Times Front Page",
                   caption = paste("Number of observations in each Policy Agendas subtopic that includes the terms:",Fterms))
            ggsave(filename = paste0(getwd(),"/","Summary.png"), device = "png", width = 9, height = 6, unit = "in")
            
            ggplot(myp2, aes(x = year, y = n, fill = Major)) +
              stat_summary(geom = "bar", position = "stack") +
              theme(plot.caption = element_text(hjust = 0)) +
              ylab("Number of Observations") +
              xlab("Year") +
              labs(fill = "Major Topic",
                   title = "New York Times Front Page",
                   caption = paste("Number of observations in each Policy Agendas major topic that includes the terms:",Fterms))
            ggsave(filename = paste0(getwd(),"/","Annual.png"), device = "png", width = 9, height = 6, unit = "in")
            
            xlsx::write.xlsx(as.data.frame(myq), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Query Details", row.names = F, showNA = F, col.names = F)
            xlsx::write.xlsx(as.data.frame(myd), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Summary", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(mydy), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Annual", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(myy), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Annual Sum", row.names = F, showNA = F, append = T)
            xlsx::write.xlsx(as.data.frame(raw), file = paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"), sheetName = "Raw Data", row.names = F, append = T, showNA = F)
            myfiles <- c(paste0(getwd(),"/",c("Summary.png","Annual.png")),paste0(getwd(),"/","PapQueryData",Sys.Date(),".xlsx"))
            zip(file,files = myfiles, flags = "-j")
          }
        )
        myd
      }
      myd
    })
    
    output$results <- renderTable(myo(), digits = 0, width = 500)
    output$details <- renderText("Query Details:")
    output$summary <- renderText("Summary:")
    #output$search <- renderText(paste("Search terms:",terms)) #input$search
    output$years <- renderText(paste("Years: ",input$styear,"-",input$endyear,sep=""))
    output$eq <- renderText(paste(rep("=",80),sep="",collapse = ""))
    output$rawres <- renderText("Raw Data:")
    output$pap <- renderUI({
      tagList("Full list of PAP datasets: ",a("https://www.comparativeagendas.net/us",href = "https://www.comparativeagendas.net/us"))
    })
    
  }


