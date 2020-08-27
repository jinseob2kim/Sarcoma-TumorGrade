source("global.R")
library(jsmodule)
library(dplyr)
library(shiny)
library(DT)
library(ggplot2)
library(stringr)

out %>% names

ui<-navbarPage(
    "Tumor Grade",
    tabPanel("Table1",
             sidebarLayout(
                 sidebarPanel(
                     selectInput(
                         "table1_vars","Variables",
                         names(out),
                         c("Age","Sex","Height","Weight",
                         "underlying_DM","underlying_HTN","underlying_CAD","underlying_CRD",
                         "Histology_primary","FNCLCCgrade_primary","Necrosis_primary","Mitosis_primary",
                         "Histology_firstLR","FNCLCCgrade_firstLR",
                         "RTx","Chemo","Day_FU","Day_LR"),
                         TRUE
                     )
                 ),
                 mainPanel(
                     h2("Table1"),
                     DTOutput("table1")
                 )
             )
    ),
    tabPanel("KM plots",
             
    ),
    tabPanel("Worsening group analysis",
             
    ),
    tabPanel("2nd recur analysis"
             
    ),
    fluid=TRUE
)
server<-function(input,output,session){
    
    output$table1<-renderDT({
        
        table1_factor_vars<-input$table1_vars[lapply(out[input$table1_vars],class)=="factor"]
        table1_conti_vars<-input$table1_vars[lapply(out[input$table1_vars],class)=="numeric"]
        
        vars.fisher<-vars.fisher <- sapply(table1_factor_vars,
                                           function(x){is(tryCatch(chisq.test(table(out[["Group"]], out[[x]])),error = function(e) e, warning=function(w) w), "warning")})
        vars.fisher <- table1_factor_vars[vars.fisher]
        
        tbl1<- lapply(input$table1_vars[!grepl("primary|firstLR|underlying|complication",input$table1_vars)], function(va){
            if (va %in% table1_conti_vars){

                forms <- as.formula(paste0(va, "~ Group"))
                mean_sd <- aggregate(forms, data = out, FUN = function(x){c(mean = mean(x), sd = sd(x))})

                p<-oneway.test(forms, data = out)$p.value

                out.final <- c(va, "",
                               paste0(round(mean_sd[[va]][, "mean"], 2), " (", round(mean_sd[[va]][, "sd"], 2), ")"),
                               ifelse(p < 0.001, "< 0.001", round(p, 3)))

                return(out.final)

            } else if(va %in% table1_factor_vars) {

                tb <- table(out[[va]], out[["Group"]])
                tb.prop <- round(100 * prop.table(tb, 2), 2)      ## prop.table : 1- byrow 2 - bycol
                tb.out <- matrix(paste0(tb, " (", tb.prop, ")"), ncol =3)

                p <- ifelse(va %in% vars.fisher, fisher.test(tb)$p.value, chisq.test(tb)$p.value)

                
                if(out[[va]] %>% levels %>% grepl("^0$",.)%>% sum(.,na.rm=TRUE)==1){
                    if(nlevels(out[[va]])==2){
                        out.final <- c(paste0(va, " (%)"),"Yes",tb.out[2,],ifelse(p < 0.001, "< 0.001", round(p, 3)))
                    }else{
                        out.final <- c(paste0(va, " (%)"),"No",tb.out[1,],ifelse(p < 0.001, "< 0.001", round(p, 3)))
                    }
                }else{
                    out.final <- cbind(c(paste0(va, " (%)"), rep("", nrow(tb.out) - 1)),
                                       rownames(tb),
                                       tb.out,
                                       c(ifelse(p < 0.001, "< 0.001", round(p, 3)), rep("", nrow(tb.out) - 1)))
                }
                return(out.final)
            }
        }) %>% Reduce(rbind, .)
        
        if(input$table1_vars[grepl("primary",input$table1_vars)] %>% length !=0){
            tbl1<-rbind(tbl1,c("Primary Tumor"," ","","","",""))
            
            tbl1<- rbind(tbl1,lapply(input$table1_vars[grepl("primary",input$table1_vars)], function(va){
                vva<-paste0("- ",gsub("_primary","",va), " (%)")
                
                tb <- table(out[[va]], out[["Group"]])
                tb.prop <- round(100 * prop.table(tb, 2), 2)      ## prop.table : 1- byrow 2 - bycol
                tb.out <- matrix(paste0(tb, " (", tb.prop, ")"), ncol =3)
                
                p <- ifelse(va %in% vars.fisher, fisher.test(tb)$p.value, chisq.test(tb)$p.value)
                
                if(out[[va]] %>% levels %>% grepl("^0$",.)%>% sum(.,na.rm=TRUE)==1){
                    if(nlevels(out[[va]])==2){
                        out.final <- c(vva,"Yes",tb.out[2,],ifelse(p < 0.001, "< 0.001", round(p, 3)))
                    }else{
                        out.final <- c(vva,"No",tb.out[1,],ifelse(p < 0.001, "< 0.001", round(p, 3)))
                    }
                }else{
                    out.final <- cbind(c(vva, rep("", nrow(tb.out) - 1)),
                                       rownames(tb),
                                       tb.out,
                                       c(ifelse(p < 0.001, "< 0.001", round(p, 3)), rep("", nrow(tb.out) - 1)))
                }
                return(out.final)
                
            }) %>% Reduce(rbind, .))
        }
        
        if(input$table1_vars[grepl("firstLR",input$table1_vars)] %>% length !=0){
            tbl1<-rbind(tbl1,c("Recurrent Tumor"," ","","","",""))
            
            tbl1<- rbind(tbl1,lapply(input$table1_vars[grepl("firstLR",input$table1_vars)], function(va){
                vva<-paste0("- ",gsub("_firstLR","",va), " (%)")
                
                tb <- table(out[[va]], out[["Group"]])
                tb.prop <- round(100 * prop.table(tb, 2), 2)      ## prop.table : 1- byrow 2 - bycol
                tb.out <- matrix(paste0(tb, " (", tb.prop, ")"), ncol =3)
                
                p <- ifelse(va %in% vars.fisher, fisher.test(tb)$p.value, chisq.test(tb)$p.value)
                
                if(out[[va]] %>% levels %>% grepl("^0$",.)%>% sum(.,na.rm=TRUE)==1){
                    if(nlevels(out[[va]])==2){
                        out.final <- c(vva,"Yes",tb.out[2,],ifelse(p < 0.001, "< 0.001", round(p, 3)))
                    }else{
                        out.final <- c(vva,"No",tb.out[1,],ifelse(p < 0.001, "< 0.001", round(p, 3)))
                    }
                }else{
                    out.final <- cbind(c(vva, rep("", nrow(tb.out) - 1)),
                                       rownames(tb),
                                       tb.out,
                                       c(ifelse(p < 0.001, "< 0.001", round(p, 3)), rep("", nrow(tb.out) - 1)))
                }
                
                return(out.final)
                
            }) %>% Reduce(rbind, .))
        }
        
        if(input$table1_vars[grepl("underlying",input$table1_vars)] %>% length !=0){
            tbl1<-rbind(tbl1,c("Underlying diseases"," ","","","",""))
            
            tbl1<- rbind(tbl1,lapply(input$table1_vars[grepl("underlying",input$table1_vars)], function(va){
                vva<-paste0("- ",gsub("underlying_","",va), " (%)")
                
                tb <- table(out[[va]], out[["Group"]])
                tb.prop <- round(100 * prop.table(tb, 2), 2)      ## prop.table : 1- byrow 2 - bycol
                tb.out <- matrix(paste0(tb, " (", tb.prop, ")"), ncol =3)
                
                p <- ifelse(va %in% vars.fisher, fisher.test(tb)$p.value, chisq.test(tb)$p.value)
                
                if(out[[va]] %>% levels %>% grepl("^0$",.)%>% sum(.,na.rm=TRUE)==1){
                    if(nlevels(out[[va]])==2){
                        out.final <- c(vva,"Yes",tb.out[2,],ifelse(p < 0.001, "< 0.001", round(p, 3)))
                    }else{
                        out.final <- c(vva,"No",tb.out[1,],ifelse(p < 0.001, "< 0.001", round(p, 3)))
                    }
                }else{
                    out.final <- cbind(c(vva, rep("", nrow(tb.out) - 1)),
                                       rownames(tb),
                                       tb.out,
                                       c(ifelse(p < 0.001, "< 0.001", round(p, 3)), rep("", nrow(tb.out) - 1)))
                }
                
                return(out.final)
                
            }) %>% Reduce(rbind, .))
        }
        
        if(input$table1_vars[grepl("complication",input$table1_vars)] %>% length !=0){
            tbl1<-rbind(tbl1,c("Complications"," ","","","",""))
            
            tbl1<- rbind(tbl1,lapply(input$table1_vars[grepl("complication",input$table1_vars)], function(va){
                vva<-paste0("- ",gsub("complication_","",va), " (%)")
                
                tb <- table(out[[va]], out[["Group"]])
                tb.prop <- round(100 * prop.table(tb, 2), 2)      ## prop.table : 1- byrow 2 - bycol
                tb.out <- matrix(paste0(tb, " (", tb.prop, ")"), ncol =3)
                
                p <- ifelse(va %in% vars.fisher, fisher.test(tb)$p.value, chisq.test(tb)$p.value)
                
                if(out[[va]] %>% levels %>% grepl("^0$",.)%>% sum(.,na.rm=TRUE)==1){
                    if(nlevels(out[[va]])==2){
                        out.final <- c(vva,"Yes",tb.out[2,],ifelse(p < 0.001, "< 0.001", round(p, 3)))
                    }else{
                        out.final <- c(vva,"No",tb.out[1,],ifelse(p < 0.001, "< 0.001", round(p, 3)))
                    }
                }else{
                    out.final <- cbind(c(vva, rep("", nrow(tb.out) - 1)),
                                       rownames(tb),
                                       tb.out,
                                       c(ifelse(p < 0.001, "< 0.001", round(p, 3)), rep("", nrow(tb.out) - 1)))
                }
                
                return(out.final)
                
            }) %>% Reduce(rbind, .))
        }

        tbl1<-rbind(c("Total Patients"," ",as.integer(summary(out$Group))[1:3],""),tbl1)
        colnames(tbl1) <- c("Variable", "Subgroup","Improving","Stable","Worsening","p-value")
        rownames(tbl1)<-NULL
        

        datatable(tbl1,options = list(pageLength = 50)) %>%
            formatStyle(columns=c("Subgroup"),
                     target="row",
                     backgroundColor = styleEqual(" ","yellow"))
    })
}

shinyApp(ui,server)