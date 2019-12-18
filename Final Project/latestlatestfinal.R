#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinydashboard)
library(leaflet)
library(tidyverse)
library(ggplot2)
library(ggthemes)

#LOAD DATA
food = read.csv("Food_Inspections.csv")
#DATA CLEANUP
food$Facility.Type = fct_collapse(food$Facility.Type,
                                  "Bakery" = c("Bakery","BAKERY/ RESTAURANT","BAKERY/DELI","BAKERY/GROCERY","bakery/restaurant", "WHOLESALE BAKERY"),
                                  "Public Space" = c("Shelter","SOUP KITCHEN","FOOD PANTRY","Food Pantry","CHURCH/SPECIAL EVENTS","CHURCH/SPECIAL EVENT","CHURCH KITCHEN","Church Kitchen","CHURCH (SPECIAL EVENTS)","CHURCH","Church","CHARITY AID KITCHEN","Animal Shelter Cafe Permit", "PANTRY",  "youth housing", "FOOD PANTRY/CHURCH","employee kitchen","MAIN KITCHEN","Shared Kitchen", "Shared Kitchen User (Long Term)", "SHARED KITCHEN USER (LONG TREM)", "Shared Kitchen User (Short Term)","EVENT VENU","EVENT SPACE","DINING HALL","LOUNGE/BANQUET HALL","Lounge","Banquet","BANQUET","Banquet Dining","BANQUET FACILITY","banquet hall","Banquet Hall","BANQUET HALL","BANQUET HALL/CATERING","BANQUET ROOM","Banquet rooms","Banquet/kitchen","banquets","banquets/room service"),
                                  #"Candy/Ice cream Store" = c("GIFT/CARD SHOP WITH CANDY","MEXICAN CANDY STORE","Candy","CANDY MAKER","CANDY SHOP","CANDY STORE","candy/gelato","CANDY/GELATO", "ICE CREAM PARLOR", "donut shop", "watermelon house", "ICE CREAM", "JUICE BAR", "GELATO SHOP"),
                                  "Caterer" = c("CATERED EVENTS","Catering","Catering and Wholesale","CATERING/CAFE", "JUICE AND SALAD BAR", "PALETERIA /ICECREAM SHOP", "smoothie bar", "PALETERIA", "ICE CREAM SHOP"),
                                  #"Coffee Shop" = c("coffee", "COFFEE  SHOP", "COFFEE CART", "COFFEE KIOSK", "COFFEE ROASTER", "coffee shop", "Coffee shop", "COFFEE SHOP", "COFFEE/TEA"),
                                  "Day Care" = c("CHURCH/AFTER SCHOOL PROGRAM","CHURCH/DAY CARE","AFTER SCHOOL PROGRAM","after school program","AFTER SCHOOL CARE","Adult Family Care Center","ADULT DAYCARE","1584-DAY CARE ABOVE 2 YEARS","15 monts to 5 years old","1023 CHILDREN'S SERVICES FACILITY","1023 CHILDERN'S SERVICES FACILITY","1023 CHILDERN'S SERVICE S FACILITY","1023 CHILDERN'S SERVICE FACILITY","1023-CHILDREN'S SERVICES FACILITY","1023","CHILDERN'S SERVICE FACILITY","CHILDERN'S SERVICES  FACILITY","CHILDERN ACTIVITY FACILITY","Children's Services Facility","CHILDRENS SERVICES FACILITY","DAY CARE","DAY CARE 1023","DAY CARE 2-14","Day Care Combo (1586)",
                                                 "DAYCARE","Daycare (2 - 6 Years)","Daycare (2 Years)","Daycare (Under 2 Years)","DAYCARE 1586","DAYCARE 2-6, UNDER 6","DAYCARE 2 YRS TO 12 YRS","DAYCARE 6 WKS-5YRS","Daycare Above and Under 2 Years","DAYCARE COMBO","Daycare Combo 1586","Daycare Night"),
                                  #"Gas Station" = c("CAR WASH","SERVICE GAS STATION","(gas station)","GAS","gas station","Gas station","GAS STATION","GAS STATION /GROCERY","GAS STATION /SUBWAY MINI MART.","GAS STATION STORE","GAS STATION/ GROCERY STORE","GAS STATION/FOOD","GAS STATION/MINI MART","GAS STATION/RESTAURANT","GAS STATION/STORE","GAS STATION/STORE GROCERY","GROCERY(GAS STATION)", "RETAIL FOOD/GAS STATION"),
                                  #"Golden Diner" = c(),
                                  "Grocery Store" = c("GENERAL STORE","FRENCH MARKET SPACE","fish market","FARMER'S MARKET","DOLLAR TREE","DOLLAR STORE SELLING GROCERY","DOLLAR STORE","Dollar store","dollar store","DOLLAR & GROCERY STORE","Deli","DELI/GROCERY STORE","grocery","GROCERY","grocery & restaurant","Grocery & Restaurant","GROCERY & RESTAURANT","GROCERY AND BUTCHER","Grocery Store","GROCERY STORE / GAS STATION","GROCERY STORE/ RESTAURANT","GROCERY STORE/BAKERY","GROCERY STORE/COOKING SCHOOL","GROCERY STORE/DELI","GROCERY STORE/GAS STATION","GROCERY STORE/TAQUERIA",
                                                      "GROCERY& RESTAURANT","Grocery(Sushi prep)","GROCERY/ RESTAURANT","GROCERY/BAKERY","grocery/butcher","GROCERY/CAFE","grocery/dollar store","GROCERY/DRUG STORE","GROCERY/GAS STATION","GROCERY/LIQUOR","GROCERY/LIQUOR STORE","GROCERY/RESTAURANT","GROCERY/SERVICE GAS STATION","GROCERY/TAQUERIA","GROCERY/TAVERN", "SLAUGHTER HOUSE/ GROCERY", "Meat Market", "PRODUCE STAND", "PRODUCE VENDOR", "WINE STORE", "REST/GROCERY", "butcher shop", "BUTCHER SHOP"),
                                  #"Hospital" = c("HEALTH CARE STORE","HEALTH CENTER"),
                                  "Nursing Home" = c("LONG TERM CARE FACILITY","Long Term Care Facility","Long Term Care","Long-Term Care Facility","Long-Term Care","ASSISTED LIVING FACILITY","Assisted Living","Assisted Living Senior Care","ASSISTED LIVING","ASSISSTED LIVING","1005 NURSING HOME","NURSING HOME","SUPPORTIVE LIVING","SUPPORTIVE LIVING FACILITY", "SENIOR DAY CARE"),
                                  #"Liquor Store" = c("CAT/LIQUOR","1475 LIQUOR","LIQOUR BREWERY TASTING","Liquor","LIQUOR CONSUMPTION ON PREMISES.","LIQUOR STORE","LIQUOR/COFFEE KIOSK","LIQUORE STORE/BAR", "RETAIL WINE/WINE BAR"),
                                  "Restaurant" = c("TENT RSTAURANT", "Golden Diner","Restaurant","RESTAURANT AND LIQUOR","Restaurant(protein shake bar)","RESTAURANT.BANQUET HALLS","RESTAURANT/BAKERY","RESTAURANT/BAR","RESTAURANT/BAR/THEATER","RESTAURANT/GAS STATION","RESTAURANT/GROCERY","RESTAURANT/GROCERY STORE","RESTAURANT/HOSPITAL","RESTAURANT/LIQUOR","RESTUARANT AND BAR", "Theater & Restaurant", "ROOFTOP/RESTAURANT"),
                                  #"Paleteria" = c(),
                                  "School" = c("COLLEGE","CHICAGO PARK DISTRICT","A-Not-For-Profit Chef Training Program","CULINARY SCHOOL","CULINARY CLASS ROOMS","CULINARY ARTS SCHOOL","COOKING SCHOOL","cooking school","ALTERNATIVE SCHOOL","School","SCHOOL","school cafeteria","School Cafeteria","TEACHING SCHOOL","PUBLIC SHCOOL","Private School","PRIVATE SCHOOL","PASTRY school","HIGH SCHOOL KITCHEN","HEALTH CENTER/NUTRITION CLASSES","CITY OF CHICAGO COLLEGE",
                                               "CHARTER SCHOOL","CHARTER SCHOOL CAFETERIA","CHARTER SCHOOL/CAFETERIA","BEFORE AND AFTER SCHOOL PROGRAM", "UNIVERSITY CAFETERIA", "PREP INSIDE SCHOOL", "RETAIL STORE OFFERS COOKING CLASSES"),
                                  #"Shelter" = c("SOUP KITCHEN","FOOD PANTRY","Food Pantry","CHURCH/SPECIAL EVENTS","CHURCH/SPECIAL EVENT","CHURCH KITCHEN","Church Kitchen","CHURCH (SPECIAL EVENTS)","CHURCH","Church","CHARITY AID KITCHEN","Animal Shelter Cafe Permit", "PANTRY",  "youth housing", "FOOD PANTRY/CHURCH"),
                                  "Tavern" = c("CAT/LIQUOR","1475 LIQUOR","LIQOUR BREWERY TASTING","Liquor","LIQUOR CONSUMPTION ON PREMISES.","LIQUOR STORE","LIQUOR/COFFEE KIOSK","LIQUORE STORE/BAR", "RETAIL WINE/WINE BAR","SMOKEHOUSE", "hooka lounge","HOOKA BAR","BREWPUB","BAR/GRILL","BAR","bar","TAP room/tavern/liquor store","tavern",
                                                "Tavern","TAVERN","TAVERN-LIQUOR","TAVERN GRILL","TAVERN/1006","Tavern/Bar","TAVERN/LIQUOR","tavern/restaurant","TAVERN/RESTAURANT", "WINE TASTING BAR", "THEATER/BAR", "SERVICE BAR/THEATRE", "RETAIL WINE/WINE BAR"),
                                  #"Social Club" = c("MOVIE THEATER", "MOVIE THEATRE", "Museum/Gallery", "MUSIC VENUE", "SPA", "THEATER", "THEATRE", "STADIUM", "NIGHT CLUB", "night club", "theater","GOLF COURSE CONNCESSION STAND","GOLF COURSE","BOYS AND GIRLS CLUB","BOWLING LANES/BANQUETS","BOOK STORE","blockbuster video","ART GALLERY","ART GALLERY W/WINE AND BEER", "SOCIAL CLUB"),
                                  "Wholesaler" = c("DISTRIBUTOR","DISTRIBUTION CENTER","COLD/FROZEN FOOD STORAGE","BREWERY","BEVERAGE/SILVERWARE WAREHOUSE", "Wholesale", "WHOLESALE & RETAIL", "meat packing", "PREPACKAGE MEAL DISTRIBUTOR (1006 Retail)", "PRE PACKAGED", "Poultry Slaughter",
                                                   "PACKAGED FOOD DISTRIBUTION", "PACKAGED HEALTH FOODS", "PACKAGED LIQUOR", "warehouse", "WAREHOUSE", "URBAN FARM", "PREPACAKAGED FOODS"),
                                  #EXTRA CATEGORIES
                                  #"Retail" = c("FURNITURE STORE","CLOTHING STORE", "RETAIL STORE", "video store", "STORE", "RETAIL", "Gift Shop", "CELL PHONE STORE"),
                                  "Drug Store" = c("HERBALIFE/ZUMBA","HERBALIFE STORE","Herbalife Nutrition","HERBALIFE","HERBALCAL","HERBAL STORE","HERBAL REMEDY","HERBAL MEDICINE","HERBAL LIFE SHOP","HERBAL LIFE","HERBAL DRINKS","HERBAL","Herabalife","DRUG/GROCERY STORE","DRUG/FOOD STORE","drug treatment facility",
                                                   "DRUG STORE/W/ FOOD","DRUG STORE/GROCERY","DRUG STORE", "PHARMACY", "pharmacy/grocery", "CHINESE HERBS"),
                                  "Cafe" = c("Internet Cafe","coffee", "COFFEE  SHOP", "COFFEE CART", "COFFEE KIOSK", "COFFEE ROASTER", "coffee shop", "Coffee shop", "COFFEE SHOP", "COFFEE/TEA","CAFE","CAFE/STORE","cafeteria","Cafeteria","CAFETERIA", "RIVERWALK CAFE", "Kids Cafe'", "KIDS CAFE"),
                                  "Convenience" = c("Convenience Store","CAR WASH","SERVICE GAS STATION","(gas station)","GAS","gas station","Gas station","GAS STATION","GAS STATION /GROCERY","GAS STATION /SUBWAY MINI MART.","GAS STATION STORE","GAS STATION/ GROCERY STORE","GAS STATION/FOOD","GAS STATION/MINI MART","GAS STATION/RESTAURANT","GAS STATION/STORE","GAS STATION/STORE GROCERY","GROCERY(GAS STATION)", "RETAIL FOOD/GAS STATION","CONVENIENCE STORE","CONVNIENCE STORE","CONVENIENT STORE","CONVENIENCE/GAS STATION","convenience/drug store","convenience store","(convenience store)","convenience","CONVENIENCE"),
                                  "Mobile Food Vendors" = c("GIFT/CARD SHOP WITH CANDY","MEXICAN CANDY STORE","Candy","CANDY MAKER","CANDY SHOP","CANDY STORE","candy/gelato","CANDY/GELATO", "ICE CREAM PARLOR", "donut shop", "watermelon house", "ICE CREAM", "JUICE BAR", "GELATO SHOP","COFFEE VENDING MACHINE","Food Vending Machines","VENDING COMMISSARY","VENDING MACHINE","MOBIL FOOD 1315","MOBILE DESSERT CART","MOBILE DESSERT VENDOR","MOBILE DESSERTS VENDOR","MOBILE FOOD","MOBILE FOOD DESSERTS VENDOR","Mobile Food Dispenser","Mobile Food Preparer","MOBILE FOOD TRUCK","Mobile Frozen Dessert Disp/Non-motorized","Mobile Frozen Dessert Dispenser_non  Motorized.",
                                                            "Mobile frozen dessert vendor","MOBILE FROZEN DESSERT VENDOR","MOBILE FROZEN DESSERTS DISPENSER-NON- MOTORIZED","MOBILE FROZEN DESSERTS DISPENSER-NON-MOTOR","MOBILE FROZEN DESSERTS DISPENSER-NON-MOTORIZED","Mobile Frozen Desserts Vendor","MOBILE FROZEN DESSERTS VENDOR","Mobile Prepared Food Vendor","MOBILPREPARED FOOD VENDOR","MOBIL FOOD 1315", "MOBILE DESSERT VENDOR", "MOBILE DESSERTS VENDOR", "MOBILE FOOD", "MOBILE FOOD DESSERTS VENDOR", "Mobile Food Dispenser", "Mobile Food Preparer", "MOBILE FOOD TRUCK", "Mobile Frozen Dessert Disp/Non-motorized","MOBILE FROZEN DESSERT VENDOR", "MOBILE FROZEN DESSERTS DISPENSER-NON- MOTORIZED", "MOBILE FROZEN DESSERTS DISPENSER-NON-MOTOR",
                                                            "MOBILE FROZEN DESSERTS DISPENSER-NON-MOTORIZED", "Mobile Frozen Desserts Vendor", "MOBILE FROZEN DESSERTS VENDOR", "Mobile Prepared Food Vendor",
                                                            "MOBILPREPARED FOOD VENDOR", "Illegal Vendor", "Ice cream", "HOT DOG STATION", "HOT DOG CART", "FROZEN DESSERTS DISPENSER -NON MOTORIZED", "FROZEN DESSERTS DISPENSER-NON-MOTORIZED", "FROZEN DESSERT PUSHCARTS", "COMMIASARY", "COMMISARY RESTAURANT", "Commissary", "COMMISSARY", "COMMISSARY FOR SOFT SERVE ICE CREAM TRUCKS", "PUSH CARTS", "PUSHCART", "TEMPORARY KIOSK", "TRUCK", "MFD TRUCK"),
                                  #"Health" = c("weight loss program", "WEIGHT LOSS PROGRAM", "PROTEIN SHAKE BAR", "NUTRITION/HERBALIFE", "Rest/GYM", "HEALTH/ JUICE BAR", "Nutrition Store"),
                                  "Other" = c("","Social Club","Hospital","MOVIE THEATER", "MOVIE THEATRE", "Museum/Gallery", "MUSIC VENUE", "SPA", "THEATER", "THEATRE", "STADIUM", "NIGHT CLUB", "night club", "theater","GOLF COURSE CONNCESSION STAND","GOLF COURSE","BOYS AND GIRLS CLUB","BOWLING LANES/BANQUETS","BOOK STORE","blockbuster video","ART GALLERY","ART GALLERY W/WINE AND BEER", "SOCIAL CLUB","HEALTH CARE STORE","HEALTH CENTER","FITNESS CENTER","LAUNDROMAT", "Laundromat","FURNITURE STORE","CLOTHING STORE", "RETAIL STORE", "video store", "STORE", "RETAIL", "Gift Shop", "CELL PHONE STORE","weight loss program", "WEIGHT LOSS PROGRAM", "PROTEIN SHAKE BAR", "NUTRITION/HERBALIFE", "Rest/GYM", "HEALTH/ JUICE BAR", "Nutrition Store", "FITNESS STUDIO", "GYM STORE", "incubator", "Live Poultry", "newsstand", "NON-FOR PROFIT BASEMENT KIT", "HELICOPTER TERMINAL", "NOT-FOR-PROFIT CLUB", "O'Hare Kiosk", "Pool", "POPCORN CORN", 
                                              "REST/ROOFTOP", "REHAB CENTER", "ROOF TOP", "ROOFTOP PATIO", "ROOM SERVICE", "Shuffleboard Club with Bar", "SUMMER FEEDING", "TEA STORE", "UNLICENSED FACILITY", "Wrigley Roof Top", "WRIGLEY ROOFTOP", "Airport Lounge", 
                                              "CUSTOM POULTRY SLAUGHTER", "HOSTEL", "KIOSK", "LINITED BUSINESS", "LIVE POULTRY", "NAIL SHOP", "NEWSSTAND", "NORTHERLY ISLAND", "NP-KIOSK", "Other", "Pop-Up Establishment Host-Tier II", "POPCORN SHOP", 
                                              "religious", "ROOF TOPS", "ROOFTOP/RESTAURANT", "SHAKES/TEAS", "snack shop", "SUMMER FEEDING PREP AREA", "TOBACCO STORE", "UNUSED STORAGE", "WRIGLEY ROOFTOP", "HOTEL", "KITCHEN DEMO", "live butcher", "MASSAGE BAR", "Navy Pier Kiosk", "NON -PROFIT", "NOT FOR PROFIT", 
                                              "PEDDLER", "Pop-Up Food Establishment User-Tier II", "REGULATED BUSINESS", "REPACKAGING PLANT", "RIVERWALK", "ROOFTOP", "ROOFTOPS", "SHSHI COUNTER", "Special Event", "TEA BREWING", "unlicensed facility", "VFW HALL", "fitness center", "day spa")
                                  )

food$Results = fct_collapse(food$Results,"Pass" = c("Pass","Pass w/ Conditions"))
food = droplevels(filter(food, Results %in% c("Pass","Fail")))

food$Inspection.Date = as.Date(food$Inspection.Date, format = "%m/%d/%Y")
food$Year = as.numeric(format(food$Inspection.Date,"%Y"))
#food$Zip = as.factor(food$Zip)
#food = food %>% drop_na(Zip) %>% droplevels()
minYr = min(food$Year)
maxYr = max(food$Year)

#REMOVE DUPLICATES
food = food[!(duplicated(food$Address) & duplicated(food$Inspection.Date)),]

#MAKE LIST OF VIOLATIONS
s = strsplit(as.character(food$Violations),"\\| ")
s = unlist(s)
raw = rep("",length(s))
for (i in 1:length(s)) {
    raw[i] <- unlist(strsplit(s[i], "-"))[1]
}
raw = unique(raw)
violist = rep("", 70)
for (i in 1:length(violist)) {
    for (j in 1:length(raw)) {
        n = as.numeric(substr(raw[j],1,str_locate(raw[j],"\\.")[1]-1))
        if (n == i) {
            violist[i] = substr(raw[j],str_locate(raw[j],"\\.")[1] + 2, nchar(raw[j]))
            break
        }
    }
}

ui <- dashboardPage(
    skin = "black",
    dashboardHeader(title = "Chicago Food", titleWidth = 200),
    dashboardSidebar(
        width = 200,
        #column(12,align = "center",offset = 0,
               actionButton(inputId = "button",
                            label = " Haejin  |  Ijka  |  Joseph ",
                            icon("at"),
                            style="color: #fff; background-color: #00ccff; border-color: #00e6b8"
               ),
        #),
        
        sidebarMenu(
            menuItem(text = "Interactive Map", icon = icon("map-marked-alt"), tabName = "maps"),
            menuItem(text = "Graphs", icon = icon("chart-bar"), tabName = "graphs")
        ),
        tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #00ccff}.irs-from, .irs-to, .irs-single { background: #00ccff }")),
        #DATE RANGE
        sliderInput(inputId = "date", label = "Date Range", min = minYr, max = maxYr, value = c(minYr,maxYr), sep = ""),
        #TYPES OF FACILITIES
        selectInput(inputId = "types", label = "Facility Type", choices = append(sort(levels(food$Facility.Type)[-which(levels(food$Facility.Type) == "Other")]), "Other"), selected = 1)
        #ZIP CODES
        #selectInput(inputId = "zips", label = "Zip Code", choices = c("All",levels(food$Zip)), selected = 1)
    ),
    dashboardBody(
        tabItems(
            #INTERACTIVE MAP TAB
            tabItem(
                tabName = "maps",
                tags$style(type = "text/css", "#map {height: calc(100vh - 40px) !important;}"),
                leafletOutput("map")
            ),
            #GRAPHS TAB
            tabItem(
                tabName = "graphs",
                    tabsetPanel(
                        tabPanel(
                            "Plots",
                            fluidRow(
                                #STATISTICS
                                infoBoxOutput("num"),
                                infoBoxOutput("idk", width = 8)
                            ),
                            fluidRow(
                                #INSPECTION COORDINATE PLOT
                                box(
                                    title = "Inspection Results",
                                    status = "info",
                                    plotOutput(outputId = "inspect", height = 400)
                                    #solidHeader = TRUE
                                ),
                                #RISK COORDINATE PLOT
                                box(
                                    title = "Risk Level",
                                    status = "info",
                                    plotOutput(outputId = "risk", height = 400)
                                    #solidHeader = TRUE
                                )
                            ),
                            fluidRow(
                                #TABLE
                                dataTableOutput(outputId = "reptab")
                            )
                        ),
                        tabPanel(
                            "Trends",
                            br(),
                            br(),
                            fluidRow(
                                plotOutput(outputId = "rate", height = 700)
                            ),
                            br(),
                            br(),
                            fluidRow(
                                infoBoxOutput("avgfail", width = 6),
                                infoBoxOutput("avgrisk", width = 6)
                            )
                        ),
                        tabPanel(
                            "Histograms",
                            fluidRow(
                                plotOutput(outputId = "hist", height = 600)
                            ),
                            br(),
                            fluidRow(
                                #box(
                                    #title = "Most Frequently Violated",
                                    #status = "info",
                                    dataTableOutput(outputId = "violtab")
                                #)
                            )
                        )
                    )
            )
        )
    )
)

server <- function(input,output) {
    #INTERACTIVE MAP
    output$map <- renderLeaflet({
        filtered = food %>% select(AKA.Name,DBA.Name,Longitude,Latitude,Year,Risk, Address,Facility.Type,Results) %>%
            filter(Year > input$date[1], Year < input$date[2]) %>%
            filter(Facility.Type == input$types) %>%
            na.omit()
        
        pal <- colorFactor(
          palette = c("Red", "Gray"),
          domain = filtered$Results)
        leaflet(filtered) %>% addTiles() %>% setView(lng = -87.6298, lat = 41.8781, zoom = 12) %>% 
            addProviderTiles(providers$CartoDB.Positron) %>%
            addCircleMarkers(~Longitude,~Latitude, 
                             popup = ~paste(sep = "<br/>",paste(as.character(AKA.Name),"<br/>"), Address, paste("Risk Level: ",Risk), paste("Inspection: ",Results)),
                             label = ~as.character(DBA.Name),
                             color = ~pal(Results)
                             )
    })
    
    
    #INSPECTION COORDINATE MAP
    output$inspect <- renderPlot({
        filtered = food %>% select(Year,Longitude,Latitude,Results, Facility.Type) %>%
            filter(Year > input$date[1], Year < input$date[2]) %>%
            filter(Facility.Type == input$types) %>%
            na.omit()

        ggplot(data = filtered, aes(Longitude,Latitude, color = Results)) + 
          geom_point(shape = 18, size = 3)+
          theme_minimal() +
          theme(
            plot.title = element_text(color = "midnightblue", size = 20 ,face = "bold", hjust = 0.5),
            plot.subtitle = element_text(color = "royalblue3", size = 15, hjust = 0.5),
            plot.caption = element_text(color = "gray33", size = 15, face = "italic"),
            axis.title.x = element_text(color="midnightblue",size=20,face="bold"),
            axis.title.y = element_text(color="midnightblue",size=20,face="bold"),
            axis.text.x = element_text(size=15, vjust=0.5,face="bold"),
            axis.text.y = element_text(size=15, vjust=0.5,face="bold"),
            legend.title = element_text(size=15,face="bold"),
            legend.text = element_text(size=10, face="bold"),
            panel.background = element_rect(fill = "white",size=0.5, colour="white",linetype="solid"),
            panel.grid.major = element_line(size=1.5, linetype = "solid",colour = "white"),
            panel.grid.minor = element_line(size=1, linetype = "solid",colour = "white"),
            legend.position = "right",
            legend.background = element_rect(
              fill="lightsteelblue1",linetype="solid"),
            legend.title.align =0.5,
            legend.margin = margin(t=0.5,r=0.2,b=0.5,l=0.2,unit = "cm")
          ) + 
          ylab("\nLatitude\n") + 
          xlab("\nLongitude\n") + 
          theme_stata() 
    })
  
    #RISK COORDINATE MAP
    output$risk <- renderPlot({
        filtered = food %>% select(Year,Longitude,Latitude,Risk, Facility.Type) %>%
            filter(Year > input$date[1], Year < input$date[2]) %>%
            filter(Facility.Type == input$types) %>%
            na.omit()
        ggplot(data = filtered, aes(Longitude,Latitude, color = Risk)) + geom_point()+ 
          theme_stata()
       
    })
    
    #REPEATED OFFENSES LIST
    output$reptab <- renderDataTable({
        filtered = food %>% select(DBA.Name, Address, Year,Longitude,Latitude,Results,Facility.Type) %>%
            filter(Year > input$date[1], Year < input$date[2]) %>%
            filter(Facility.Type == input$types) %>%
            na.omit()
        grouped = filtered %>% group_by(DBA.Name,Address) %>% summarize(repOff = sum(Results == "Fail"))
        colnames(grouped) = c("Name", "Address", "Number of Offenses")
        grouped
    })
    
    #RATE GRAPH
    output$rate <- renderPlot({
        filtered = food %>% select(Year,Risk, Facility.Type, Results) %>%
            filter(Year > input$date[1], Year < input$date[2]) %>%
            filter(Facility.Type == input$types) %>%
            na.omit()
        toPlot = filtered %>% group_by(Year) %>% summarize(failRate = sum(Results == "Pass")/n(), 
                                                           riskRate = sum(Risk == "Risk 1 (High)")/n())
  
        ggplot(toPlot, aes(x = Year)) +
          geom_line(aes(y = failRate, group = 1, color = "a"), size = 3) + 
          geom_line(aes(y = riskRate, group = 1, color = "b"), size = 3) +
          geom_point(aes(y = failRate), shape = 21, size = 3, 
                     colour = "deeppink4", fill = "white", stroke = 3) +
          geom_point(aes(y = riskRate), shape = 22, size = 3, 
                     colour = "dodgerblue3", fill = "white", stroke = 3) +
          labs(title = "\n\nGraph of High Risk and Failed Inspection rates by year\n",
            subtitle ="High Risk = Risk 1 \n", 
            caption = "\n\nData source: Chicago Food Inspection\n") +
          theme(
            plot.title = element_text(color = "midnightblue", size = 28 ,face = "bold", hjust = 0.5),
            plot.subtitle = element_text(color = "royalblue3", size = 15, hjust = 0.5),
            plot.caption = element_text(color = "gray33", size = 15, face = "italic"),
            axis.title.x = element_text(color="midnightblue",size=25,face="bold"),
            axis.title.y = element_text(color="midnightblue",size=25,face="bold"),
            axis.text.x = element_text(angle=50, size=20, vjust=0.5,face="bold"),
            axis.text.y = element_text(size=20, vjust=0.5,face="bold"),
            axis.title.y.right = element_text(size=20),
            legend.title = element_text(size=20,face="bold"),
            legend.text = element_text(size=15, face="bold"),
            panel.background = element_rect(fill = "white",size=0.5, colour="white",linetype="solid"),
            panel.grid.major = element_line(size=2, linetype = "solid",colour = "gray84"),
            panel.grid.minor = element_line(size=1, linetype = "solid",colour = "gray83")
          ) +
          theme(panel.spacing = unit(5,"lines"))+ 
          ylab("\n\n\nRate\n") + 
          xlab("\n\nYear") + 
          scale_colour_manual(values=c("dodgerblue3","deeppink3")) +
          scale_color_discrete(name = "\n\nData",labels = c("High Risk","Failed Inspection")) +
          theme(legend.position = "right",
                legend.background = element_rect(
                  fill="seashell1",size=0.5, linetype="solid"),
                legend.key.size = unit(2,"cm"),
                legend.title.align =0.5,
                legend.spacing.y = unit(1.0,"cm"),
                legend.margin = margin(t=0,r=0.5,b=1,l=0.5,unit = "cm")
                ) 
    })
    #HISTOGRAM
    output$hist <- renderPlot({
        filtered = food %>% select( Year, Facility.Type, Violations) %>%
            filter(Year > input$date[1], Year < input$date[2]) %>%
            filter(Facility.Type == input$types) %>%
            na.omit()

        s = strsplit(as.character(filtered$Violations),"\\| ")
        viols = rep(0, 70)
        for (i in 1:length(s)) {
            for (j in 1:length(s[[i]])) {
                n = as.numeric(substr(s[[i]][j],1,str_locate(s[[i]][j],"\\.")[1]-1))
                if (!is.null(n) && length(n) != 0 && !is.na(n)){
                    viols[n] = viols[n] + 1
                }
            }
        }
        viols = data.frame(1:70,viols)
        colnames(viols) = c("x","y")
        ggplot(viols, aes(x,y)) + geom_bar(stat = "identity") + xlab("Violation Number") + ylab("Count") + theme_minimal() +
        labs(title = "\n\nFrequency of violations",
             subtitle ="\n70 types of violation types \n", 
             caption = "\n\nData source: Chicago Food Inspection\n") + 
          theme(
            plot.title = element_text(color = "midnightblue", size = 30 ,face = "bold", hjust = 0.5),
            plot.subtitle = element_text(color = "royalblue3", size = 15, hjust = 0.5),
            plot.caption = element_text(color = "gray33", size = 15, face = "italic"),
            axis.title.x = element_text(color="midnightblue",size=20,face="bold"),
            axis.title.y = element_text(color="midnightblue",size=20,face="bold"),
            axis.text.x = element_text( size=15, vjust=0.5,face="bold"),
            axis.text.y = element_text(size=15, vjust=0.5,face="bold"),
            axis.title.y.right = element_text(size=20)) + 
          ylab("\n\n\nCount\n") + 
          xlab("\n\nViolation Number") 
    })
    
    #VIOLATION LIST
    output$violtab <- renderDataTable({
        filtered = food %>% select(Year, Facility.Type, Violations) %>%
            filter(Year > input$date[1], Year < input$date[2]) %>%
            filter(Facility.Type == input$types) %>%
            na.omit()

        s = strsplit(as.character(filtered$Violations),"\\| ")
        viols = rep(0, 70)
        for (i in 1:length(s)) {
            for (j in 1:length(s[[i]])) {
                n = as.numeric(substr(s[[i]][j],1,str_locate(s[[i]][j],"\\.")[1]-1))
                if (!is.null(n) && length(n) != 0 && !is.na(n)){
                    viols[n] = viols[n] + 1
                }
            }
        }
        viols = data.frame(viols,1:70, violist)
        colnames(viols) = c("Number of Violations","Violation Number", "Description")
        viols
    })
    
    #NUMBER OF INSPECTIONS BOX
    output$num <- renderInfoBox({
        filtered = food %>% select(Year,Facility.Type) %>%
            filter(Year > input$date[1], Year < input$date[2]) %>%
            filter(Facility.Type == input$types) %>%
            na.omit()
        infoBox(
            #formatC(nrow(filtered), format="d", big.mark=',')
            'Number of Inspections'
            ,nrow(filtered)
            ,icon = icon("stats",lib='glyphicon')
            ,color = "red")  
    })
    
    #RECOMMENDED FACILITIES
    output$idk <- renderInfoBox({
        filtered = food %>% select(Year,Facility.Type, Results, Risk) %>%
            filter(Year > input$date[1], Year < input$date[2]) %>%
            filter(Facility.Type == input$types, Results == "Pass", Risk == "Risk 3 (Low)") %>%
            na.omit()
        infoBox(
            'Number of Recommended Facilities'
            ,nrow(filtered)
            ,'(Low Risk Level and Passed Inspection)'
            ,icon = icon("edit",lib='glyphicon')
            ,color = "yellow")  
    })
    
    #AVERAGE FAIL RATE BOX
    output$avgfail <- renderInfoBox({
        filtered = food %>% select(Year,Facility.Type, Results) %>%
            filter(Year > input$date[1], Year < input$date[2]) %>%
            filter(Facility.Type == input$types) %>%
            na.omit()
        infoBox(
            #formatC(sum(filtered$Results == "Fail")/nrow(filtered), format="g", big.mark=',')
            'Average Fail Rate'
            ,sum(filtered$Results == "Fail")/nrow(filtered)
            ,icon = icon("remove-sign",lib='glyphicon')
            ,color = "red")  
    })
    #AVERAGE RISK RATE BOX
    output$avgrisk <- renderInfoBox({
        filtered = food %>% select(Year,Risk, Facility.Type) %>%
            filter(Year > input$date[1], Year < input$date[2]) %>%
            filter(Facility.Type == input$types) %>%
            na.omit()
        infoBox(
            #formatC(sum(filtered$Risk == "Risk 1 (High)")/nrow(filtered), format="g", big.mark=',')
            'Average Risk Rate'
            ,sum(filtered$Risk == "Risk 1 (High)")/nrow(filtered)
            ,icon = icon("warning-sign",lib='glyphicon')
            ,color = "yellow")  
    })
}
shinyApp(ui, server)