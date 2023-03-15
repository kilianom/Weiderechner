library(data.table)
library(ggplot2)
library(shiny)
library(shinyvalidate)
library(bslib)
theme_a<-bs_theme(
  version = 3,
  bootswatch = "sandstone")

gfm<-fread("FM.csv")
#set thresholds for fibre
t_NDF<-300
t_ADF<-200
t_NFC<-400

GRUBER_DMI<-function(n_lac,br,dim,lwt,ecm,cm,fq_f){
  br<-ifelse(br=="milchbetont",-1.667,-2.631)
  n_lac<-ifelse(n_lac <= 1,-0.728,ifelse(n_lac >1 & n_lac <4,0.218,0))
  DMI<-3.878+br+n_lac-4.287+4.153*(1-exp(-0.01486*dim))+((0.0148-0.0000474*(dim)+0.0000000904*(dim)^2))*lwt+(0.0825+0.0008098*dim-0.0000000966*dim^2)*ecm+(0.6962-0.0023289*dim+0.0000040634*dim)*cm+0.858*fq_f
  DMI<-DMI*0.93+0.47
  DMI
}
list.files("helper/")
######################ui#################################################################################
ui <-fluidPage(theme = theme_a,
  titlePanel("Weiderechner"),
  fluidRow(
           column(width =11,
                  actionButton("toggle","Eingabebereich",icon = icon("database"))),
           column(width=1 ,
                  actionButton("help1","Information", onclick ="window.open('helper/Manual_Weiderechner.html', '_blank')",icon = icon("question")))
                  ),

  sidebarLayout(
                div(id="sidebar",
                           
                sidebarPanel(
                     shinyjs::useShinyjs(),
                    # shinybrowser::detect(),
                     
                     icon("cow","fa-2x"),
                     shinyBS::bsCollapse(
                       shinyBS::bsCollapsePanel(style = "success",title = "Eingabe Herde",
                      h3("Herde"),
                              numericInput("ncow","Kuhanzahl",min = 1,max=2000,value = 90),
                              selectInput("br","Rasse",choices = c("milchbetont"="milchbetont","zweinutzung"="zweinutzung")),
                              sliderInput("lwt","Kuhgewicht kg", min = 450,max =800,value = 650),
                              sliderInput("m_y","Milchleistung kg",min=5,max=45,value=24),
                              sliderInput("m_p","Protein %",min = 2.8,max = 6,value = 3.4,step = 0.1),
                              sliderInput("m_f","Fett %",min = 2.8,max=6,value = 4,step = 0.1),
                              sliderInput("dim","Laktationstag",min=5,max =330,value = 150),
                              sliderInput("n_lac","Laktationszahl",min = 1,max=10,value = 2)
                     ) ),
 
                     icon("seedling","fa-2x"),
                     shinyBS::bsCollapse(
                       shinyBS::bsCollapsePanel(style = "success",title = "Eingabe Fütterung",
                      h3("Fütterung "),
                              h4("Ration Stall"),
                              p("Eingaben in kg Trockensubstanz pro Kuh und Tag nach Abzug Fütterungsrest"),
                              actionButton("FMplus","Futtermittel eingeben",width = '50%'),
                              DT::dataTableOutput('feed_table'),
                              actionButton("FMminus","ausgewähltes Futtermittel entfernen",width = '50%'),
                              br(),
                              hr(style = "border-top: 1px solid #000000;"),
                              br(), 
                              h4("Inhaltsstoffe Weidefutter"),
                              sliderInput("pqual","Energiegehalt MJ NEL /kg TS",min = 4.5,max=7.5,value=6.5,step = 0.1),
                              sliderInput("pXP","XP g/kg TS",min = 100,max=300,value=200,step = 1),
                              sliderInput("pNDF","NDF g/kg TS",min = 200,max=600,value=387,step = 1),
                              sliderInput("pADF","ADF g/kg TS",min = 100,max=500,value=261,step = 1),
                              sliderInput("pNFC","NFC g/kg TS",min = 150,max=500,value=253,step = 1)
                      )),
                    
                     h4("Benennung Szenario"),
                     textInput("sz","",value = "Standardszenario"),
                     actionButton("save","Szenario hinzufügen"),
                     conditionalPanel( condition = "output.nrows",
                                       h4("Szenarien entfernen")),
                     splitLayout(
                     conditionalPanel( condition = "output.nrows",
                                       actionButton("rmsz","Auswahl entfernen")),
                     conditionalPanel( condition = "output.nrows",
                                       actionButton("reset","Alle entfernen"))
                     ),
                     conditionalPanel( condition = "output.nrows",
                                       checkboxGroupInput("resetsz","",""))
              
                                 )
  ),
  mainPanel(
   
    fluidRow(
      tabsetPanel(
        tabPanel(title=h4("Weidefläche und Versorgung"),
                 shinyjs::hidden(
                   div(id = "hiddenbox1",
          hr(),
      h2("Berechnungen"),
      br(),
      div(DT::dataTableOutput('table'),style="font-size:90%"),
      hr(style = "border-top: 1px solid #000000;"),
      br(),
     box(
       plotOutput("block"),
       width = 6),
       h2("Eingaben"),
      tableOutput("inputs")
       )
      )
     ),

   tabPanel(title = h4("Einteilung Parzellen und Portionen"),
     fluidRow(
       shinyjs::hidden(
         div(id = "hiddenbox2",
      box(
      plotOutput("breaks"),
      width = 8),
      box(
       h2("Weideparameter (advanced)"),
                 numericInput("preg","Weidereife Aufwuchshöhe in cm (komprimiert) ",min = 6,max=15,value = 10),
                 numericInput("postg","Weiderest Aufwuchshöhe in cm (komprimiert) ",min = 3,max = 6,value = 4.5),
                p("Die Angaben entsprechend der komprimierten Aufwuchshöhe gemessen mit einem Rising Plate Meter.
                  Umrechnungsmethoden von anderen Methoden der Aufwuchshöhenmessung sind bei ",
                  a("Raumberg Gumpenstein",
                   href = "https://raumberg-gumpenstein.at/jdownloads/Tagungen/Viehwirtschaftstagung/Viehwirtschaftstagung%202015/1v_2015_steinwidder_haeusler.pdf",target="_blank"),
                  " zu finden."),
          h4("Umrechnung der Aufwuchshöhe (cm) in verfügbare Trockenmasse (kg TM/ha)"),
        splitLayout(
          numericInput("mult","Multiplikator",value = 240,min=100,max=400),
          numericInput("konst","Konstante",value = -110,min = -1000,max=1000)
          ),
          p("Der Multiplikator wird mit der komprimierten Aufwuchshöhe (cm) multipliziert und die Konstante wird nach Vorzeichen addiert.
              Die eingestellten Werte sind aus dem MuD Projekt entstanden und sollten nur verändert werden,
              wenn eine betriebsspezifische Umrechnungsformel existiert.")
        ,width=4)
                  )
                )
              )
            )
          )
        )
      )
    )
  )

###################### Define server logic ################
server <- function(input, output,session) {
  
  ####helpers####
  shinyhelper::observe_helpers(help_dir = "helper")
  ####reactive values####
  rv<-reactiveValues(dt_ra=NULL,dt_input=NULL,dt_calc=NULL,dt_feed=NULL)
  
  observeEvent(input$save, {
    shinyjs::show(id = "hiddenbox1")
    })
  observeEvent(input$save, {
    shinyjs::show(id = "hiddenbox2")
  })
  
  observeEvent(input$toggle, {
    shinyjs::toggle(id = "sidebar")
  })
 ###validate inputs####
  iv<-InputValidator$new()#iniate validator
  iv$add_rule("ncow",sv_between(1,2000,message_fmt = "Eingabe muss zwischen {left} und {right} liegen."))
  iv$add_rule("milk_y",sv_between(5,45,message_fmt = "Eingabe muss zwischen {left} und {right} liegen."))
  iv$add_rule("n_lac",sv_between(1,10,message_fmt = "Eingabe muss zwischen {left} und {right} liegen."))
  iv$add_rule("m_p",sv_between(2.8,6,message_fmt = "Eingabe muss zwischen {left} und {right} liegen."))
  iv$add_rule("n_lac",sv_between(1,10,message_fmt = "Eingabe muss zwischen {left} und {right} liegen."))
  iv$add_rule("dim",sv_between(5,330,message_fmt = "Eingabe muss zwischen {left} und {right} liegen."))
  iv$add_rule("lwt",sv_between(400,800,message_fmt = "Eingabe muss zwischen {left} und {right} liegen."))
  iv$add_rule("pqual",sv_between(5,7.5,message_fmt = "Eingabe muss zwischen {left} und {right} liegen."))
  iv$add_rule("preg",sv_between(5,15,message_fmt = "Eingabe muss zwischen {left} und {right} liegen."))
  iv$add_rule("postg",sv_between(3,8,message_fmt = "Eingabe muss zwischen {left} und {right} liegen."))
  iv$add_rule("mult",sv_between(120,300,message_fmt = "Eingabe muss zwischen {left} und {right} liegen."))
  iv$add_rule("konst",sv_between(-1000,1000,message_fmt = "Eingabe muss zwischen {left} und {right} liegen."))
  iv$add_rule("NEL",sv_between(4,9,message_fmt = "Eingabe muss zwischen {left} und {right} liegen.",allow_na = T))
  iv$add_rule("XP",sv_between(50,600,message_fmt = "Eingabe muss zwischen {left} und {right} liegen.",allow_na = T))
  iv$add_rule("NDF",sv_between(50,800,message_fmt = "Eingabe muss zwischen {left} und {right} liegen.",allow_na = T))
  iv$add_rule("ADF",sv_between(20,500,message_fmt = "Eingabe muss zwischen {left} und {right} liegen.",allow_na = T))
  iv$add_rule("NFC",sv_between(0,800,message_fmt = "Eingabe muss zwischen {left} und {right} liegen.",allow_na = T))
  iv$add_rule("TS",sv_between(0,20,message_fmt = "Eingabe muss zwischen {left} und {right} liegen."))
  
  iv$enable()  
  #attributes

  ####own feed input#####
observe({
  if(is.null(rv$dt_feed)){
  rv$dt_feed<-data.table(FMuser=character(),
                         art=character(),
                         TS=numeric(),
                         NEL=numeric(),
                         XP=numeric(),
                         NDF=numeric(),
                         ADF=numeric(),
                         NFC=numeric(),
                         FMcount=numeric())
  fi<-rv$dt_feed[,.(FMuser,TS,NEL,XP,NDF,ADF,NFC)]
  colnames(fi)<-c("Futtermittel","Menge","MJ NEL","XP","NDF","ADF","NFC")
  output$feed_table<-DT::renderDataTable(DT::datatable(fi,filter = "none",rownames = F,
                                                     options = list(dom='t',language = list(zeroRecords = "Keine Zufütterung"))))
  }
  })


  
  observeEvent(input$FMplus,{
    
    showModal(modalDialog(
      title = "Eingabe Futtermittel",
      selectInput("FM","Futtermittel",choices = c("eigenes Futtermittel",gfm[,label])),
      numericInput("TS","Menge kg TS",min=0,max=20,value = 0,step = 0.5),
      conditionalPanel(condition="input.FM=='eigenes Futtermittel'",
      textInput("FMuser"," Bezeichnung Futtermittel"),
      selectInput("FMart","Futtermittel Art",choices = c("Grundfutter","Kraftfutter")),
      numericInput("NEL","MJ NEL/kg TS",min=4,max=9,value = 6.5,step = 0.5),
      numericInput("XP","XP g/kg TS",min = 50,max = 600,value = NA,step = 1),
      numericInput("NDF","NDF g/kg TS",min = 50,max = 800,value = NA,step = 1),
      numericInput("ADF","ADF g/kg TS",min = 20,max = 500,value = NA,step = 1),
      numericInput("NFC","NFC g/kg TS",min = 20,max = 800,value = NA,step = 1)),
      conditionalPanel(condition="input.FM!='eigenes Futtermittel'",
                       renderTable({gfm_s<-gfm[label==input$FM,.(NEL,XP,NDF,ADF,NFC)]
                                    data.frame(colnames(gfm_s),t(gfm_s[1,]))
                                  },colnames = F)
                       ),
      footer = tagList(modalButton("Abbrechen"),actionButton("FMown_save","Speichern"))
    ))
  })
 
feedinput<-reactive({
  if(input$FM=="eigenes Futtermittel"){
    feedinput<- data.table(
                            FMuser=substr(input$FMuser,1,18),
                            TS=input$TS,
                            NEL=input$TS,
                            XP=input$XP,
                            NDF=input$NDF,
                            ADF=input$ADF,
                            NFC=input$NFC,
                            art=input$FMart,
                            FMcount=NULL)
  } else {
    feedinput<-data.table(
                            FMuser=input$FM,
                            TS=input$TS,
                            NEL=gfm[label==input$FM,NEL],
                            XP=gfm[label==input$FM,XP],
                            NDF=gfm[label==input$FM,NDF],
                            ADF=gfm[label==input$FM,ADF],
                            NFC=gfm[label==input$FM,NFC],
                            art=gfm[label==input$FM,art],
                            FMcount=NULL)
  }
 
})

  observeEvent(input$FMown_save,{
    if(nrow(rv$dt_feed)==0){
      rv$dt_feed<-feedinput()
      rv$dt_feed[,FMcount:=1]
    }  else{
      fi<-feedinput()
      fi[,FMcount:=max(rv$dt_feed$FMcount)+1]
      rv$dt_feed<-rbind(rv$dt_feed,fi)
    }
  fi<-rv$dt_feed[,.(FMuser,TS,NEL,XP,NDF,ADF,NFC)]
  colnames(fi)<-c("Futtermittel","Menge","MJ NEL","XP","NDF","ADF","NFC")
  output$feed_table<-DT::renderDataTable(DT::datatable(fi,filter = "none",rownames = F,
                                                       options = list(dom='t',language = list(zeroRecords = "Keine Zufütterung"))))
  removeModal()
  })
  
  observeEvent(input$FMminus,{
    if(!is.null(input$feed_table_rows_selected) ){
      rv$dt_feed<-rv$dt_feed[-as.numeric(input$feed_table_rows_selected),]
      fi<-rv$dt_feed[,.(FMuser,TS,NEL,XP,NDF,ADF,NFC)]
      colnames(fi)<-c("Futtermittel","Menge","MJ NEL","XP","NDF","ADF","NFC")
      output$feed_table<-DT::renderDataTable(DT::datatable(fi,filter = "none",rownames = F,
                                                           options = list(dom='t',language = list(zeroRecords = "Keine Zufütterung"))))
    }
  })
  
 
 #####szenario data frame###### 
  dt<-reactive({
    
   dt_feed<- rv$dt_feed
   if (is.null(dt_feed)){
     dt_feed<-data.table(FMuser=NA,TS=0,art=NA,NEL=0,XP=0,NDF=0,ADF=0,NFC=0,FMcount=NA)
   }
   dt<-data.table(fq_f=ifelse(dt_feed[,sum(TS)]<20,
                               ((20-dt_feed[,sum(TS)])*input$pqual+dt_feed[art=="Grundfutter",sum(TS*NEL)])/(20-dt_feed[art=="Kraftfutter",sum(TS)]),
                               dt_feed[art=="Grundfutter",sum(TS)*(sum(NEL)/sum(TS))]),
                   ecm=input$m_y*(0.38*input$m_f+0.21*input$m_p+1.05)/3.28,
                   cm=dt_feed[art=="Kraftfutter",sum(TS)],
                   fm=dt_feed[art=="Grundfutter",sum(TS)]
                  
                  )
   print(dt_feed)
   
    dt[,dmi:=GRUBER_DMI(n_lac=input$n_lac,br=input$br,dim=input$dim,lwt=input$lwt,ecm=ecm,cm=cm,fq_f=fq_f)]
    dt[,dmi_p:=dmi-(cm+fm)]
    dt_feed_t<-rbind(dt_feed,
                     data.table(FMuser="WF",art="Grundfutter",TS=dt$dmi_p,NEL=input$pqual,XP=input$pXP,NDF=input$pNDF,ADF=input$pADF,NFC=input$pNFC,FMcount=0))



   dt[,':='(NDF=dt_feed_t[,sum(TS*NDF)],
            ADF=dt_feed_t[,sum(TS*ADF)],
            NFC=dt_feed_t[,sum(TS*NFC)],
            NDF_GF=dt_feed_t[art=="Grundfutter",sum(TS*NDF)])]
    dt[,c("ADF","NDF","NFC","NDF_GF"):=lapply(.SD,function(x)x/dmi),.SDcols=c("ADF","NDF","NFC","NDF_GF")]
    dt[,fibre_pr:=ifelse(NDF>=t_NDF & ADF>=t_ADF & NFC<=t_NFC,"ausreichend","nicht ausreichend")]# mehrstufig evaluieren, GF =pasturebased+high conc---?
    dt[,e_req:=ecm*3.3+input$lwt^0.75*0.293]
    dt[,e_prov:=dt_feed_t[,sum(TS*NEL)]]
    dt[,fd_p_herd:=dmi_p*input$ncow]
    dt[,a_TS:=input$mult*(input$preg-input$postg)+input$konst]
    dt[,':='(sz=input$sz,n_sz=0,n_cow=input$ncow,m_y=input$m_y,m_f=input$m_f,m_p=input$m_p,br=input$br,dim=input$dim,n_lac=input$n_lac,lwt=input$lwt)]
    dt
  })
  
  breaks_d<-c(0.5,1,2,3)
  growth_rates<-seq(40,100,length.out=8)
  bsdt<-reactive({
    ha_d<-dt()[,fd_p_herd/growth_rates]
    data.table(fd_p_herd=dt()[,fd_p_herd],
               g_block= ha_d,
               gr=growth_rates,
               breaks_d=breaks_d,
               sz=input$sz,
               n_sz=0)
    
  })
  
  observeEvent(input$save,{
    
    if(unique(bsdt()$sz %in% unique(rv$dt_ra$sz))){
      shinyalert::shinyalert("Eingabefehler","Bitte einen eindeutigen Szenarionamen eingeben",type = "warning")
    } else{
      print(colnames(dt()))
      input_params<-colnames(dt())
      names(input_params)<-c("Energie Grundfutter MJ NEL/kg TS","ECM","Kraftfuttermenge","Grundfuttermenge","Futteraufnahme","benötigte Futteraufnahme Weide",
                             "NDF g/kg TS","ADF g/kg TS","NFC g/kg TS","NDF GF g/kg TS","Faserversorgung","Energiebedarf","Energieangebot",
                             "Herdenbedarf Weide kg TS","verfügbares Weidefutter kg/TS ha","Szenario","Nr Szenario","Kuhzahl",
                             "Milchleistung","Fett %", "Eiweiß %","Rasse","Laktationstag","Laktationszahl","Lebendgewicht" )
      if(is.null(rv$dt_ra)){
        rv$dt_ra<-bsdt()
        rv$dt_ra[,n_sz:=1]
      }  else{
        rv$dt_ra<-rbind(rv$dt_ra,bsdt())
        rv$dt_ra[n_sz==0,n_sz:=max(rv$dt_ra$n_sz)+1]
        setorder(rv$dt_ra,n_sz)
      }
      if(is.null(rv$dt_calc)){
        rv$dt_calc<-dt()
        rv$dt_calc[,n_sz:=1]
      }  else{
        rv$dt_calc<-rbind(rv$dt_calc,dt())
        rv$dt_calc[n_sz==0,n_sz:=max(rv$dt_calc$n_sz)+1]
        setorder(rv$dt_calc,n_sz)}
      if(nrow(rv$dt_calc)>3){
        shinyjs::disable("save")
        shinyalert::shinyalert("Szenarienanzahl","Maximalanzahl Szenarien erreicht. Für weitere Szenarien bitte die kostenpflichtige Version nutzen",type = "warning")
      } 
      output$table<-DT::renderDataTable({
        if (is.null(rv$dt_calc)){
            return(NULL)} else{
              print(colnames(rv$dt_calc))
                calc<-rv$dt_calc[, !c("fm","cm","m_y","m_f","m_p","br","dim","n_lac","lwt","n_cow","a_TS")]
                calc[,':='(e_bal=e_prov-e_req)]
                calc[,c("fq_f","n_sz")]<-NULL
                calc[,colnames(calc)[unlist(lapply(calc,is.numeric))]:=lapply(.SD,round,1),.SDcols=colnames(calc)[unlist(lapply(calc,is.numeric))]]
                colnames(calc)<-c(names(input_params[input_params %in% colnames(calc)]),"Energiebilanz")
                setcolorder(calc,c("Szenario","ECM","Futteraufnahme","benötigte Futteraufnahme Weide","Herdenbedarf Weide kg TS",
                                   "Energiebedarf","Energieangebot","Energiebilanz"))
                calc<-DT::datatable(calc,filter = "none",rownames = F,
                              options = list(dom='t',language = list(zeroRecords = "Keine Szenarien vorhanden")))
                calc<-DT::formatStyle(calc,columns=c("Energiebilanz"),color = DT::styleInterval(cuts=0,c("red","black")),fontWeight = "bold")
                calc<-DT::formatStyle(calc,columns=c("Faserversorgung"),color = DT::styleEqual("nicht ausreichend","red"),fontWeight = "bold")
               
               calc
          }
      })
      
      if(rv$dt_calc[max(n_sz),e_prov-e_req]<0  ){
        shinyalert::shinyalert("Energieversorgung!!",paste0("Negative Energiebilanz in: \n ",paste(rv$dt_calc[max(n_sz),sz]), " \n Fütterung sollte angepasst werden!"))
      }
      output$block<-renderPlot({
        if (is.null(rv$dt_ra)){
          return(NULL)}else{
            block<- ggplot(rv$dt_ra,aes(x=gr,y=g_block,color=as.factor(n_sz)))+
              geom_line(linewidth=1.4)+
              labs(x="Wachstumsrate kg TS pro Tag und Hektar", y="benötigte Gesamtweidefläche ha",color="Szenario",title = "Benötigte Gesamtweidefläche\nbei unterschiedlichem Wachstum")+
              theme_bw()+
              theme(text=element_text(size = 14))+
              scale_color_manual(labels=unique(rv$dt_ra$sz),values=paletteer::paletteer_d("awtools::a_palette"))
            block
          }
      })
  

      if(is.null(rv$dt_input)){
        rv$dt_input<-data.table(vars=input_params,Eingabe=names(input_params),as.matrix(t(dt())))
        colnames(rv$dt_input)<-c("vars","Eingabe",input$sz)
      } else{
        old_cols<-colnames(rv$dt_input)
        rv$dt_input<-data.table(cbind(rv$dt_input,as.matrix(t(dt()))))
        colnames(rv$dt_input)<-c(old_cols,input$sz)
      }
    }
    
    output$inputs<-renderTable({
      if (is.null(rv$dt_input)){
        return(NULL)}else{
          in_t<-rv$dt_input[vars %in% c("fq","fm","cm","m_y","m_f","m_p","br","dim","n_lac","lwt","n_cow"),]
          in_t[,vars:=NULL]
        }
    })
    
    ##reset feed inputs
    rv$dt_feed<-NULL
    
  })
  
  output$breaks<-renderPlot({
    if (is.null(rv$dt_ra)){
      return(NULL)}
    else{
        if(input$preg<=input$postg){
          shinyalert::shinyalert("Aufwuchshöhe","Die Aufwuchshöhe zu Beweidungshöhe ist geringer als der Weiderest. Bitte anpassen!",type = "warning")
          } else{
        ats<-input$mult*(input$preg-input$postg)+input$konst
        breaks<- ggplot(rv$dt_ra,aes(x=as.factor(breaks_d),y=breaks_d*(fd_p_herd/ats),color=as.factor(n_sz)))+
            geom_point(size=2,position = position_dodge(width = 1/length(unique(rv$dt_ra$n_sz))))+
            geom_linerange(aes(ymin=0,x=as.factor(breaks_d),ymax=breaks_d*(fd_p_herd/ats),color=as.factor(n_sz)),position = position_dodge(width = 1/length(unique(rv$dt_ra$n_sz))))+
            labs(x="Tage pro Portion", y="Portionsfläche ha",color="Szenario",title = "Benötigte Weidefläche\nfür unterschiedliche Besatzzeiten")+
            theme_bw()+
            theme(text=element_text(size = 14))+
            scale_color_manual(labels=unique(rv$dt_ra$sz),values=paletteer::paletteer_d("awtools::a_palette"))
          breaks
        }
    }
  })
  
  observeEvent(input$reset,{
    rv$dt_ra=NULL
    rv$dt_input=NULL
    rv$dt_calc=NULL
  })
  
  output$nrows <- reactive({
    nrow(rv$dt_ra)
  })
  outputOptions(output, "nrows", suspendWhenHidden = FALSE)  
  
  observe({
    updateCheckboxGroupInput(session, "resetsz",
                             choices = unique( rv$dt_ra$sz)
                             
    )})
  observeEvent(input$rmsz,{
    
    rv$dt_input[,paste0(input$resetsz)]<-NULL
    rv$dt_ra<-rv$dt_ra[!sz %in% paste0(input$resetsz),]
    rv$dt_calc<-rv$dt_calc[!sz %in% paste0(input$resetsz),]
    if(nrow(rv$dt_calc)<=3){
      shinyjs::enable("save")
    }
  })
  
}

############################################### Run the application ############################
shinyApp(ui = ui, server = server)
