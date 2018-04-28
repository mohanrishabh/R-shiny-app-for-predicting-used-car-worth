# myApp2
library(shinythemes)
library(shiny)
library(png)
library(shinydashboard)
library(dplyr)

########################### READING DATA ###########################

myFile <-  read.csv("https://drive.google.com/uc?export=download&id=0Bwl8NvYh4msVR29MZkJ2V0pqWTA", na.strings = "NA")
# Data cleaning Part 
autos<-subset(myFile, yearOfRegistration>=1990 & price>=400 & price<=25000)

set.seed(100)
sample <- sample.int(n = nrow(autos), size = floor(.05*nrow(autos)), replace = F)
train <- autos[sample, ]
test  <- autos[-sample, ]
lmfit<-lm(price ~ . , data = train)
# summary(lmfit)
# pred <- predict(lmfit,test)
# sample<-cbind(pred,test)

########################### RSHINY APP #############################

# Define UI for application that draws a histogram
ui <- fluidPage(tabsetPanel(
  
  tabPanel("Descriptive",   titlePanel("Descriptive Stats"),
           
           tags$head(
             tags$style(HTML("body{
                             background-image: url(https://www.wallpapersbrowse.com/images/gb/gb75ogt.jpg);
                             }")))),
 
  
  
  tabPanel("Predictions",
           # Application title
           titlePanel("Predicting Used Car Prices"),
           
           tags$head(
             tags$style(HTML("body{
                             background-image: url(https://www.wallpapersbrowse.com/images/gb/gb75ogt.jpg);
                             }"))),
 
           
           #Panel 1
           fluidRow(column(5, wellPanel(
             
             selectizeInput("vehicleType",
                            h2("What type of Vehicle do you need?"),
                            
                            c("convertible",  "combination vehicle  (passenger and cargo)",    "limousine ",               "SUV",   "other",               "compact car",  "bus",    "sedan "),
                            c("SUV"),
                            multiple = FALSE),
             
             
             selectizeInput("brand",
                            h2("What Brand?"),
                            
                            c("bmw",              "mercedes_benz",            "ford",   "chevrolet",        "renault",               "honda",              "volkswagen",    "nissan",              "hyundai",           "trabant",               "peugeot",          "saab",  "fiat",    "suzuki",              "opel",  "mitsubishi",       "daewoo",               "mini",   "mazda",             "volvo", "audi",   "alfa_romeo",    "porsche",           "chrysler",               "seat",   "citroen",            "kia",     "toyota",             "lancia",               "subaru",               "daihatsu",          "smart",               "jaguar",              "rover", "skoda",               "land_rover",               "jeep",   "dacia", "lada"),
                            c("ford"),
                            multiple = FALSE),
             
             selectizeInput("model",
                            h2("It's Model?"),
                            
                            c("3er",  "andere",             "clio",    "cr_reihe",          "e_klasse",          "fiesta",               "golf",   "laguna",             "micra",               "transporter",     "tucson",             "601",               "a_klasse",          "astra", "corsa", "focus", "lancer",              "lanos", "lupo",  "omega",               "one",   "passat",              "rx_reihe",          "v50",    "80",      "90",      "100",    "147",    "156",               "159",    "500",    "850",    "900",    "911",    "9000", "1_reihe",            "1er",    "2_reihe",               "3_reihe",            "300c",  "3er",    "4_reihe",            "5_reihe",            "5er",    "6_reihe",               "6er",    "7er",    "a1",      "a3",      "a4",      "a5",      "a6",      "a8",      "accord",               "agila",  "alhambra",        "almera",             "altea", "antara",             "arosa",               "auris",  "avensis",            "aveo", "aygo", "b_klasse",          "b_max",             "beetle",               "berlingo",          "bora",  "boxster",            "bravo",               "c_klasse",          "c_max",               "c_reihe",            "c1",      "c2",      "c3",      "c4",      "c5",      "caddy",               "calibra",               "captiva",            "carisma",           "carnival",           "cayenne",          "cc",      "ceed",               "charade",           "cherokee",        "citigo",               "civic",  "cl",       "clk",     "clubman",               "colt",   "cooper",            "cordoba",          "corolla",             "crossfire",          "cuore",               "cx_reihe",          "delta", "discovery",        "eos",    "escort",              "espace",               "exeo", "fabia", "forester",           "forfour",            "fortwo",             "freelander",               "fusion",              "g_klasse",          "galant",              "galaxy",              "getz",   "gl",       "glk",               "grand",               "i_reihe",             "i3",       "ibiza",  "impreza",           "insignia",            "jazz",               "jetta",  "jimny",                "juke",   "justy",  "ka",      "kadett",              "kaefer",               "kalos", "kangoo",            "kappa",              "kuga",  "legacy",              "leon",  "lybra",               "m_klasse",         "m_reihe",          "materia",           "matiz",               "megane",               "meriva",             "mii",     "modus",             "mondeo",          "move",               "musa",               "mustang",          "mx_reihe",        "navara",             "note",  "nubira",              "octavia",               "outlander",        "pajero",              "panda",              "phaeton",          "picanto",               "polo",  "primera",           "ptcruiser",         "punto",               "q3",      "q5",      "q7",               "qashqai",           "r19",    "range_rover",   "range_rover_sport",      "rav",     "rio",               "roadster",          "roomster",        "s_klasse",          "s_max",              "s_type",             "s60",               "sandero",           "santa", "scenic",              "scirocco",          "seicento",          "sharan",               "signum",            "sirion",               "sl",        "slk",      "sorento",           "spider",               "sportage",         "stilo",   "superb",             "swift", "terios",               "tigra",  "tiguan",               "toledo",             "touareg",           "touran",             "transit",              "tt",       "twingo",               "up",      "v_klasse",          "v40",    "v60",    "v70",    "vectra",              "verso", "viano", "vito",               "vivaro",              "voyager",           "wrangler",         "x_reihe",            "x_trail",               "x_type",             "xc_reihe",          "yaris",  "yeti",    "ypsilon",             "z_reihe",               "zafira",               "80",      "80",      "90",      "145",    "200",    "a2",      "combo",               "croma",              "defender",         "doblo",               "ducato",             "duster",               "elefantino",       "fox",    "kalina",               "lodgy", "logan",               "niva",   "rangerover",               "samara",            "spark", "sprinter",           "amarok",           "range_rover_evoque"),
                            c("fiesta"),
                            multiple = FALSE),
             sliderInput("kilometer",
                         h2("Mileage Travelled"),
                         
                         value=208,min=5000,max=150000,step=5000)
             
             
           )),
           
           #Panel 2
           column(5, wellPanel(                        
             
             selectizeInput("yearOfRegistration",
                            h2("Registration Year"),
                            c(1990,  1991,     1992,     1993,     1994,     1995,     1996,     1997,     1998,     1999,               2000,     2001,     2002,     2003,     2004,     2005,     2006,     2007,     2008,     2009,     2010,               2011,     2012,     2013,     2014,     2015,     2016,     2017,     2018),
                            c(2016),
                            multiple = FALSE),               
             
             
             selectizeInput("fuelType",
                            h2("Fuel Type"),
                            
                            c("other",             "petrol",              "natural gas",     "diesel",               "electric ",               "hybrid",              "liquefied petroleum gas"),
                            c("petrol"),
                            multiple = FALSE),
             
             selectizeInput("gearbox",
                            h2("Transmission"),
                            
                            c("automatic",    "manual"),
                            c("automatic"),
                            multiple = FALSE)
             
             
             
             ,                actionButton("go", "Get The Car Price",
                                           style="display:inline-block;width:100%;text-align: center; font-size: 30px;background-color:gold",
                                           icon = icon("exclamation-sign",lib="glyphicon")),
             h2("Your car should cost around:", style="color:black"),
             verbatimTextOutput("value")
           )                                                                              
           )
           ))
  
             ))

server<-function(input,output){ 
  output$value <- renderText({
    if (input$go > 0){
      pred <- predict(lmfit,  newdata = data.frame(vehicleType=input$vehicleType,
                                                   yearOfRegistration=as.numeric(input$yearOfRegistration),
                                                   gearbox=input$gearbox,
                                                   model=input$model,
                                                   kilometer=input$kilometer,
                                                   fuelType=input$fuelType,
                                                   brand=input$brand,
                                                   interval="predict"))
      a<-pred
      paste(a[1])
    }}
  )}



shinyApp(ui=ui,server=server)
####################### THE END #############################

