library(data.table)
library(ggplot2)
library(shiny)
library(shinyvalidate)
library(shinydashboard)
library(bslib)
theme_a<-bs_theme(
  version = 3,
  bootswatch = "readable")
#xml2::write_html(rvest::html_node(xml2::read_html("www/help_Weiderechner.html"), "body"), file = "www/help_body.html")
gfm<-fread("FM.csv")
#set thresholds for fibre
t_NDF<-350
t_ADF<-200
t_NFC<-400
t_GFNDF<-200

GRUBER_DMI<-function(n_lac,br,dim,lwt,ecm,cm,fq_f){
  br<-ifelse(br=="milchbetont",-1.667,-2.631)
  n_lac<-ifelse(n_lac <= 1,-0.728,ifelse(n_lac >1 & n_lac <4,0.218,0))
  DMI<-3.878+br+n_lac-4.287+4.153*(1-exp(-0.01486*dim))+((0.0148-0.0000474*(dim)+0.0000000904*(dim)^2))*lwt+(0.0825+0.0008098*dim-0.0000000966*dim^2)*ecm+(0.6962-0.0023289*dim+0.0000040634*dim)*cm+0.858*fq_f
  DMI<-DMI*0.93+0.47
  DMI
}


######################ui#################################################################################
ui <-fluidPage(theme = theme_a,tags$head(tags$style('
   body {
      font-family: Arial}')),

             fluidRow(
               column(width = 1,
                      tags$style(".topimg {
                            margin-left:0px;
                            margin-top:5%;
                          }"),
                      div(class="topimg",img(src='Wbutton40.png', align = "left",width="50%"))
               ),
    column(width = 1,div(h2("Weiderechner")),style="margin-top: 1%; align: left; margin-left: -3%; "),

    column(width= 1,offset = 9,div(actionButton("help1","Information", onclick ="window.open(' helper/Manual_Weiderechner.html', '_blank')",icon = icon("question")),
                        ),style=" align: right;margin-top: 2%;margin-right:-10%"),

    ),

    # column(width=12,offset = 11,
    #        tags$a(href="https://www.mud-tierschutz.de/mud-tierschutz/wissen-dialog-praxis/milchkuehe/weidehaltung-von-milchkuehen",target="blank",
    #        tags$style(".topimg {
    #                         margin-left:-100px;
    #                         margin-right:0px;
    #                         margin-top:-50px;
    #                       }"),
    #        div(class="topimg",img(src='BLElogo.png', align = "top",width="10%")),
    #        ))
    
  titlePanel("", windowTitle = "Weiderechner" ),
            
 fluidRow(
           column(width =3,
                  actionButton("toggle","Eingabebereich",icon = icon("bars"))),
           column(width =1,
                  div(     style="float:right",
                   actionButton("help2","",icon = icon("question"))))
 
),

  sidebarLayout(
                div(id="sidebar",
                           
                sidebarPanel(
                     shinyjs::useShinyjs(),
                    # shinybrowser::detect(),
                    fluidRow( 
                     icon("cow","fa-2x"),
                    
                    actionButton("toggle_herd","Eingabe Herde",icon=icon("chevron-down"),
                                 style="background-color: #69b62d;border-color: #69b62d;font-weight: bold;width: 100%;padding-right:90% ;margin: auto"),
                              
                    shinyjs::hidden(
                      div(id="herd_sidebar",
                      h3("Herde"),
                              numericInput("ncow","Kuhanzahl",min = 1,max=2000,value = 90),
                              selectInput("br","Rasse",choices = c("milchbetont"="milchbetont","zweinutzung"="zweinutzung")),
                              sliderInput("lwt","Kuhgewicht kg", min = 450,max =800,value = 650),
                              sliderInput("m_y","Milchleistung kg",min=5,max=45,value=22),
                              sliderInput("m_p","Protein %",min = 2.8,max = 6,value = 3.4,step = 0.1),
                              sliderInput("m_f","Fett %",min = 2.8,max=6,value = 4,step = 0.1),
                              sliderInput("dim","Laktationstag",min=5,max =330,value = 150),
                              sliderInput("n_lac","Laktationszahl",min = 1,max=10,value = 2)
                     ) )),
                    br(),
                    fluidRow(
                    icon("seedling","fa-2x"),
                   
                    actionButton("toggle_feed",label="Fütterung",icon=icon("chevron-down"),
                                 style="background-color: #69b62d;border-color: #69b62d;font-weight: bold;width:100%; padding-right: 90%;margin: auto"),
                  
                  shinyjs::hidden(
                    div(id="feed_sidebar",
                      h3("Fütterung "),
                              h4("Ration Stall"),
                              p("Eingaben in kg Trockensubstanz pro Kuh und Tag nach Abzug Fütterungsrest"),
                              actionButton("FMplus","Futtermittel eingeben",width = '50%'),
                              DT::dataTableOutput('feed_table'),
                              actionButton("FMminus","ausgewähltes Futtermittel entfernen",width = '50%'),
                              br(),
                              hr(style = "border-top: 1px solid #000000;"),
                              br(), 
                              h4("Weidedauer"),
                              sliderInput("dgrazh","Weidedauer h/Tag",min = 1,max=24,value=16,step = 0.5),
                              h4("Inhaltsstoffe Weidefutter"),
                              sliderInput("pqual","Energiegehalt MJ NEL /kg TS",min = 4.5,max=7.5,value=6.6,step = 0.1),
                              sliderInput("pXP","XP g/kg TS",min = 100,max=300,value=200,step = 1),
                              sliderInput("pNDF","NDF g/kg TS",min = 200,max=600,value=387,step = 1),
                              sliderInput("pADF","ADF g/kg TS",min = 100,max=500,value=261,step = 1),
                              sliderInput("pNFC","NFC g/kg TS",min = 150,max=500,value=253,step = 1)
                      ))
                ),
               
                     h4("Benennung Szenario"),
                     textInput("sz","",value = "Standardszenario"),
                     actionButton("save","Szenario hinzufügen",style="background-color: #69b62d;border-color: #69b62d;font-weight: bold"),
                     conditionalPanel( condition = "output.nrows",
                                       h4("Szenarien entfernen")),
                     conditionalPanel( condition = "output.nrows",
                                  checkboxGroupInput("resetsz","","")),
                    fluidRow(
                     
                     column(width = 5,
                     conditionalPanel( condition = "output.nrows",
                                       actionButton("rmsz","Auswahl entfernen"))),
                     column(width=5,offset = 1,
                     conditionalPanel( condition = "output.nrows",
                                       actionButton("reset","Alle entfernen")))
                     
                     ),
              
                                 )
  ),
  mainPanel(id="mainpanel",width = 8,
   
    fluidRow(
      tabsetPanel(
        tabPanel(title=h4("Weidefläche und Versorgung"),
                 shinyjs::hidden(
                   div(id = "hiddenbox1",
          hr(),
          box(width = 12,
      h3("Berechnungen"),
      br(),
      fluidRow(
      div(DT::dataTableOutput('table'),style="font-size:90%")),
       fluidRow(
       hr(style = "border-top: 1px solid #000000;")),
      br()
      ),
      box(width = 12,
      fluidRow(column(width = 8,offset = 2,
       plotOutput("block"))
      )
      ),
     fluidRow(column(width = 9,offset = 1,
     h2("Eingaben"),
     p("Angaben zur Fütterung in kg TM /Kuh und Tag"),
     br(),

     DT::dataTableOutput('inputdata')
     
     
                   ))
    )

      )
     ),

   tabPanel(title = h4("Parzellengröße"),
     box(width = 12,
       shinyjs::hidden(
         div(id = "hiddenbox2",
             box(width = 10,
               h3("Weideparameter"),
               br(),
               wellPanel(h4("Aufwuchshöhe in cm (komprimiert)"),
               splitLayout(
               numericInput("preg",label ="Weidereife",min = 6,max=15,value = 10),
               numericInput("postg","Weiderest",min = 3,max = 6,value = 5),
               ),
               p("Die Angaben entsprechen der komprimierten Aufwuchshöhe gemessen mit einem Rising Plate Meter.
                  Umrechnungen von anderen Methoden der Aufwuchshöhenmessung sind", a(href="https://www.gruenlandzentrum.org/Weideleitfaden/#52_Aufwuchshoehenmessung", target="_blank", "hier"), "zu finden.")
               )
               ),

             fluidRow(column(width = 8,offset = 2,
      plotOutput("breaks")
      )),
      box(actionButton("CSHTS","Formel Rising Plate Meter",icon=icon("chevron-down")),
      shinyjs::hidden(
        div(id = "hiddenbox3",
            h5("Umrechnung der komprimierten Aufwuchshöhe (cm) in Futterangebot (kg TM /ha) "),
            splitLayout(
              numericInput("mult","Multiplikator",value = 240,min=100,max=400),
              numericInput("konst","Konstante",value = -110,min = -1000,max=1000)
            ),
            p("Der Multiplikator wird mit der komprimierten Aufwuchshöhe (cm) multipliziert und die Konstante wird nach Vorzeichen addiert.
              Die eingestellten Werte sind aus dem MuD Projekt entstanden und sollten nur verändert werden,
              wenn eine betriebsspezifische Umrechnungsformel existiert.")
              )
            )
            ,width=9),

                  )
                )
              )
            )
          )
        )
      )
    ),
 hr(),
 fluidRow(column(width = 1,offset = 11 ,tags$a("Datenschutzerklärung", href="datenschutz.html",target="_blank",style = "font-size: 80%;color: #000000 ;margin-top=0px;"))),        
 
  )

###################### Define server logic ################
server <- function(input, output,session) {
  observe({
  showModal(modalDialog(size = 'l',
    title = "Anleitung",
          includeHTML("www/help_body.html"),
    footer = tagList(modalButton("Verstanden"))))
  })

  observeEvent(input$help2,{
    showModal(modalDialog(size = 'l',
                          title = "Anleitung",
                            includeHTML("www/help_body.html"),
                          footer = tagList(modalButton("Verstanden"))))
  })
  
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
    if(input$toggle[1]%%2==1){
      shinyjs::removeCssClass("mainpanel", "col-sm-8")
      shinyjs::addCssClass("mainpanel", "col-sm-12")
    } else {
      shinyjs::removeCssClass("mainpanel", "col-sm-12")
      shinyjs::addCssClass("mainpanel", "col-sm-8")
     }
  })
  
  observeEvent(input$toggle_feed, {
    shinyjs::hide(id = "herd_sidebar")
    shinyjs::toggle(id = "feed_sidebar")
    
  })
  
  observeEvent(input$toggle_herd, {
    shinyjs::hide(id = "feed_sidebar")
    shinyjs::toggle(id = "herd_sidebar")
    
  })
  
  observeEvent(input$CSHTS, {
    shinyjs::toggle(id = "hiddenbox3")
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
      numericInput("TS","Menge kg TM",min=0,max=20,value = 0,step = 0.5),
      conditionalPanel(condition="input.FM=='eigenes Futtermittel'",
      textInput("FMuser"," Bezeichnung Futtermittel"),
      selectInput("FMart","Futtermittel Art",choices = c("Grundfutter","Kraftfutter")),
      numericInput("NEL","MJ NEL/kg TM",min=4,max=9,value = 6.5,step = 0.5),
      numericInput("XP","XP g/kg TM",min = 50,max = 600,value = NA,step = 1),
      numericInput("NDF","NDF g/kg TM",min = 50,max = 800,value = NA,step = 1),
      numericInput("ADF","ADF g/kg TM",min = 20,max = 500,value = NA,step = 1),
      numericInput("NFC","NFC g/kg TM",min = 20,max = 800,value = NA,step = 1)),
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
                               dt_feed[art=="Grundfutter",sum(TS*NEL)/sum(TS)]),
                   ecm=input$m_y*(0.38*input$m_f+0.21*input$m_p+1.05)/3.28,
                   cm=dt_feed[art=="Kraftfutter",sum(TS)],
                   fm=dt_feed[art=="Grundfutter",sum(TS)]
                  
                  )

    dt[,dmi:=GRUBER_DMI(n_lac=input$n_lac,br=input$br,dim=input$dim,lwt=input$lwt,ecm=ecm,cm=cm,fq_f=fq_f)]
    dt[,dmi_p:=dmi-(cm+fm)]
    print(dt)
    dt_feed_t<-rbind(dt_feed,
                     data.table(FMuser="WF",art="Grundfutter",TS=dt$dmi_p,NEL=input$pqual,XP=input$pXP,NDF=input$pNDF,ADF=input$pADF,NFC=input$pNFC,FMcount=0))



   dt[,':='(NDF=dt_feed_t[,sum(TS*NDF)],
            ADF=dt_feed_t[,sum(TS*ADF)],
            NFC=dt_feed_t[,sum(TS*NFC)],
            NDF_GF=dt_feed_t[art=="Grundfutter",sum(TS*NDF)])]
    dt[,c("ADF","NDF","NFC","NDF_GF"):=lapply(.SD,function(x)x/dmi),.SDcols=c("ADF","NDF","NFC","NDF_GF")]
    dt[,fibre_pr:=ifelse(NDF>=t_NDF & ADF>=t_ADF & NFC<=t_NFC & NDF_GF>=t_GFNDF,"ausreichend","nicht ausreichend")]# mehrstufig evaluieren, GF =pasturebased+high conc---?
    dt[,e_req:=ecm*3.3+input$lwt^0.75*0.293+0.15*(dmi_p/dmi)*input$lwt^0.75*0.293]
    dt[,e_prov:=dt_feed_t[,sum(TS*NEL)]]
    dt[,fd_p_herd:=dmi_p*input$ncow]
    dt[,p_dmi_h:=dmi_p/input$dgrazh]
    dt[,p_class:=ifelse(dmi_p/dmi>=0.85,"Vollweide",
                        ifelse(dmi_p/dmi<0.3,"Auslaufweide","Teilweide"))]
    dt[,a_TS:=input$mult*(input$preg-input$postg)+input$konst]
    dt[,':='(sz=input$sz,n_sz=0,n_cow=input$ncow,m_y=input$m_y,m_f=input$m_f,m_p=input$m_p,br=input$br,dim=input$dim,n_lac=input$n_lac,lwt=input$lwt)]
    dt
  })
  
  breaks_d<-c(0.5,1,2,3)
  growth_rates<-seq(25,100,length.out=8)
  bsdt<-reactive({
    ha_d<-dt()[,fd_p_herd/growth_rates]
    if(dt()$fd_p_herd>0){
    data.table(fd_p_herd=dt()[,fd_p_herd],
               g_block= ha_d,
               gr=growth_rates,
               breaks_d=breaks_d,
               sz=input$sz,
               n_sz=0)
    } else {
      data.table(fd_p_herd=0,
                 g_block= 0,
                 gr=growth_rates,
                 breaks_d=breaks_d,
                 sz=input$sz,
                 n_sz=0)
      }
  })
  
  observeEvent(input$save,{

    if(unique(bsdt()$sz %in% unique(rv$dt_ra$sz))){
      shinyalert::shinyalert("Eingabefehler","Bitte einen eindeutigen Szenarionamen eingeben",type = "warning")
    } else{
      input_params<-colnames(dt())
      names(input_params)<-c("Energie Grundfutter MJ NEL/kg TM","ECM","Kraftfuttermenge","Grundfuttermenge","Futteraufnahme","benötigte Futteraufnahme Weide",
                             "NDF g/kg TM","ADF g/kg TM","NFC g/kg TM","NDF GF g/kg TM","Faserversorgung","Energiebedarf","Energieangebot",
                             "Herdenbedarf Weide kg TM","stündliche Futteraufnahme kg TM/ha","Weideart","verfügbares Weidefutter kg TM/ ha","Szenario","Nr Szenario","Kuhzahl",
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
       
      output$table<-DT::renderDataTable({
        if (is.null(rv$dt_calc)){
            return(NULL)} else{
                calc<-rv$dt_calc[, !c("fm","cm","m_y","m_f","m_p","br","dim","n_lac","lwt","n_cow","a_TS")]
                calc[,':='(e_bal=e_prov-e_req)]
                calc[,c("fq_f","n_sz")]<-NULL
                calc[,colnames(calc)[unlist(lapply(calc,is.numeric))]:=lapply(.SD,round,1),.SDcols=colnames(calc)[unlist(lapply(calc,is.numeric))]]
                colnames(calc)<-c(names(input_params[input_params %in% colnames(calc)]),"Energiebilanz")
                print(calc)
                setcolorder(calc,c("Szenario","Weideart","ECM","Futteraufnahme","stündliche Futteraufnahme kg TM/ha","benötigte Futteraufnahme Weide","Herdenbedarf Weide kg TM",
                                   "Energiebedarf","Energieangebot","Energiebilanz"))
                calc<-DT::datatable(calc,filter = "none",rownames = F,escape = F,
                                    colnames = c("Szenario","Weideart","Milch<br/>kg ECM/Kuh","Futter-<br/>aufnahme kg TM/Kuh","stündliche<br/>Futteraufnahme kg TM/Kuh","Bedarf<br/>Weide <br/>kg TM/Kuh","Herdenbedarf<br/>Weide<br/>kg TM",
                                                  "Energie-<br/>bedarf<br/>Kuh","Energie-<br/>angebot<br/>Kuh","Energie-<br/>bilanz<br/>Kuh",
                                                 "NDF g/kg TM","ADF g/kg TM","NFC g/kg TM","NDF g/kg TM Grund-<br/>futter","Faser-<br/>versorgung"),
                              options = list(dom='t',scrollX=T,scrollCollapse=T,language = list(zeroRecords = "Keine Szenarien vorhanden")))
                calc<-DT::formatStyle(calc,columns=c("Energiebilanz"),color = DT::styleInterval(cuts=0,c("red","black")),fontWeight = "bold")
                calc<-DT::formatStyle(calc,columns=c("Faserversorgung"),color = DT::styleEqual("nicht ausreichend","red"),fontWeight = "bold")
               print(calc)
               calc
          }
      })
      
      if(rv$dt_calc[max(n_sz),e_prov-e_req]<0  ){
        shinyalert::shinyalert("Energieversorgung!",paste0("Negative Energiebilanz in: \n ",paste(rv$dt_calc[max(n_sz),sz]), " \n Fütterung sollte angepasst werden!"))
      }
      if(rv$dt_calc[max(n_sz),fibre_pr]=="nicht ausreichend"  ){
        shinyalert::shinyalert("Faserversorgung!",paste0("Faserversorgung mangelhaft in: \n ",paste(rv$dt_calc[max(n_sz),sz]), " \n Fütterung sollte angepasst werden!"))
      }
      if(nrow(rv$dt_calc)>3){
        shinyjs::disable("save")
        shinyalert::shinyalert("Szenarienanzahl","Maximale Anzahl Szenarien erreicht. Bitte einzelne oder mehrere Szenarien entfernen.",type = "warning")
      }
      output$block<-renderPlot({
        if (is.null(rv$dt_ra)){
          return(NULL)}
        else{ block<- ggplot(rv$dt_ra,aes(x=gr,y=g_block,color=as.factor(n_sz)))+
                        geom_line(linewidth=1.4)+
                        labs(x="Wachstumsrate [kg TM/(Tag x ha)]", y="benötigte Gesamtweidefläche [ha]",color="Szenario",title = "Benötigte Gesamtweidefläche für die gesamte Herde in Abhängigkeit \n von Wachstumsraten des Bestandes")+
                        theme_bw()+
                        theme(text=element_text(size = 18,family = "Arial"),
                              axis.text = element_text(size = 18),
                              legend.text = element_text(size = 18))+
                        guides(color = guide_legend(override.aes = list(size = 10)))+
                        scale_color_manual(labels=unique(rv$dt_ra$sz),values=paletteer::paletteer_d("awtools::a_palette"))+
                        ylim(c(0,max(rv$dt_ra$g_block)+2))
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
    
    
      
    output$inputdata<-DT::renderDataTable({
      if (is.null(rv$dt_input)){
        return(NULL)} else{
          in_t<-rv$dt_input[vars %in% c("fq","fm","cm","m_y","m_f","m_p","br","dim","n_lac","lwt","n_cow"),]
          in_t[,vars:=NULL]
          in_t<-DT::datatable(in_t,filter = "none",rownames = F,escape = F,options = list(dom='t',scrollX=T,scrollCollapse=T,language = list(zeroRecords = "Keine Szenarien vorhanden")))
        
        }
     
    })
    }
  
    
  })
  
  output$breaks<-renderPlot({
    if (is.null(rv$dt_ra)){
      return(NULL)}
    else{
        if(input$preg<=input$postg){
          shinyalert::shinyalert("Aufwuchshöhe","Die Aufwuchshöhe zu Beweidungsbeginn ist geringer als der Weiderest. Bitte anpassen!",type = "warning")
          } else{
        ats<-input$mult*(input$preg-input$postg)+input$konst
        breaks<- ggplot(rv$dt_ra,aes(x=as.factor(breaks_d),y=breaks_d*(fd_p_herd/ats),color=as.factor(n_sz)))+
                        geom_point(size=3,position = position_dodge(width = 1/length(unique(rv$dt_ra$n_sz))))+
                        geom_linerange(aes(ymin=0,x=as.factor(breaks_d),ymax=breaks_d*(fd_p_herd/ats),color=as.factor(n_sz)),position = position_dodge(width = 1/length(unique(rv$dt_ra$n_sz))))+
                        labs(x="Tage pro Portion", y="Portionsfläche [ha]",color="Szenario",title = "Benötigte Weidefläche für die gesamte Herde für unterschiedliche Besatzzeiten")+
                        theme_bw()+
                        theme(text=element_text(size = 18,family = "Arial"),
                              axis.text = element_text(size = 18),
                              legend.text = element_text(size = 18))+
                        guides(color = guide_legend(override.aes = list(size = 4)))+
                        scale_color_manual(labels=unique(rv$dt_ra$sz),values=paletteer::paletteer_d("awtools::a_palette"))+
                        ylim(c(0,max(rv$dt_ra[,breaks_d*(fd_p_herd/ats)])+2))
          breaks
        }
    }
  })
  
  observeEvent(input$reset,{
    rv$dt_ra=NULL
    rv$dt_input=NULL
    rv$dt_calc=NULL
    
    shinyjs::enable("save")
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
