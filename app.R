library(shiny)
library(data.table)

library(highcharter)
hcoptslang <- getOption("highcharter.lang")
hcoptslang$contextButtonTitle <- "Helyi menü"
hcoptslang$exitFullscreen <- "Kilépés a teljes képernyős módból"
hcoptslang$hideData <- "Adatok elrejtése"
hcoptslang$loading <- "Betöltés..."
hcoptslang$mainBreadcrumb <- "Fő ábra"
hcoptslang$noData <- "Nincs megjeleníthető adat"
hcoptslang$printChart <- "Ábra nyomtatása"
hcoptslang$viewData <- "Adatok megtekintése"
hcoptslang$viewFullscreen <- "Teljes képernyős nézet"
hcoptslang$months <- c(
  "január", "február", "március", "április", "május","június",
  "július", "augusztus", "szeptember", "október", "november",
  "december")
hcoptslang$shortMonths <- c(
  "jan", "febr", "márc", "ápr", "máj", "jún", "júl", "aug",
  "szept", "okt", "nov", "dec")
hcoptslang$weekdays <- c("vasárnap", "hétfő", "kedd", "szerda",
                         "csütörtök", "péntek", "szombat")
hcoptslang$shortWeekdays <- c("Vas", "Hét", "Ked", "Sze", "Csü",
                              "Pén", "Szo", "Vas")
hcoptslang$exportButtonTitle <- "Exportál"
hcoptslang$printButtonTitle <- "Importál"
hcoptslang$rangeSelectorFrom <- "ettől"
hcoptslang$rangeSelectorTo <- "eddig"
hcoptslang$rangeSelectorZoom <- "mutat:"
hcoptslang$downloadPNG <- "Letöltés PNG képként"
hcoptslang$downloadJPEG <- "Letöltés JPEG képként"
hcoptslang$downloadPDF <- "Letöltés PDF dokumentumként"
hcoptslang$downloadSVG <- "Letöltés SVG formátumban"
hcoptslang$downloadCSV <- "Letöltés CSV formátumú táblázatként"
hcoptslang$downloadXLS <- "Letöltés XLS formátumú táblázatként"
hcoptslang$resetZoom <- "Nagyítás alaphelyzetbe állítása"
hcoptslang$resetZoomTitle <- "Nagyítás alaphelyzetbe állítása"
hcoptslang$thousandsSep <- " "
hcoptslang$decimalPoint <- ","
hcoptslang$numericSymbols <- NA
options(highcharter.lang = hcoptslang)
options(highcharter.download_map_data = FALSE)
# source("hw_grid.R")

ProcData <- readRDS("Proc2Data.rds")
allomaskoord <- readRDS("allomaskoord.rds")
# mapdata <- readRDS("mapdata.rds")
mapdata <- jsonlite::read_json("hu-all.topo.json")
# mapdata <- download_map_data("countries/hu/hu-all")
colstops <- readRDS("colstops.rds")
choices <- readRDS("choices.rds")

desctext <- paste0(
  "A magyar vonatok késési adatait bemutató, vizualizáló, ",
  "elemezhetővé tevő oldal. Írta: Ferenci Tamás.")
urlpre <- "http://www.vonat-keses.hu/"
figcap <- "Ferenci Tamás, www.medstat.hu"

kesesExplanation <- paste0(
  "<b>Teljes késés</b>: Szokásos késés, a vonat adott állomásra érkezésekor fennálló késése.<br>",
  "<b>Indulási késés</b>: A vonat mennyivel később indult az indulási állomásáról a menetrendihez képest.<br>",
  "<b>Állomási késés</b>: A vonat mennyivel tartózkodott többet az állomáson (kivéve az indulásit), ",
  "mint menetrend szerint kellett volna.<br>",
  "<b>Nyíltvonali késés</b>: A vonat mennyivel lassabban tette meg az adott szakaszt a menetrendihez képest.")

dt18nurl <- "https://cdn.datatables.net/plug-ins/2.3.2/i18n/hu.json"

kesesstat <- function(x, metric) {
  # if(sum(!is.na(x)) < 3) return(NULL)
  # if(sum(!is.na(x)) == 0) return(NULL)
  if (all(is.na(x)) || length(x) == 0) return(NULL) # gyorsabb
  
  x <- x[!is.na(x)]

  stats_list <- list()
  value1_list <- list()
  value2_list <- list()
  
  if ("N" %in% metric) {
    stats_list[["N"]] <- "Megállások száma"
    value1_list[["N"]] <- length(x)
    value2_list[["N"]] <- NA
  }
  
  if ("Megoszlás" %in% metric) {
    tab <- table(cut(x, c(-Inf, 0, 5, 10, 15, 20, 30, 45, 60, Inf)))
    stats_list[["Megoszlás"]] <- c("-0", "0-5", "5-10", "10-15", "15-20", "20-30", "30-45", "45-60", "60-")
    value1_list[["Megoszlás"]] <- as.numeric(prop.table(tab))
    value2_list[["Megoszlás"]] <- as.numeric(tab)
  }
  
  if (any(c("Átlag", "Medián", "75. percentilis",
            "90. percentilis", "99. percentilis") %in% metric)) {
    x_pmax0 <- pmax(0, x)
  }
  
  if (">5" %in% metric) {
    x_gt_5 <- x > 5
    stats_list[[">5"]] <- ">5"
    value1_list[[">5"]] <- mean(x_gt_5)
    value2_list[[">5"]] <- sum(x_gt_5)
  }
  
  if (">20" %in% metric) {
    x_gt_20 <- x > 20
    stats_list[[">20"]] <- ">20"
    value1_list[[">20"]] <- mean(x_gt_20)
    value2_list[[">20"]] <- sum(x_gt_20)
  }
  
  if ("Átlag" %in% metric) {
    stats_list[["Átlag"]] <- "Átlag"
    value1_list[["Átlag"]] <- mean(x_pmax0)
    value2_list[["Átlag"]] <- NA
  }
  
  if ("Medián" %in% metric) {
    stats_list[["Medián"]] <- "Medián"
    value1_list[["Medián"]] <- median(x_pmax0)
    value2_list[["Medián"]] <- NA
  }
  
  quantiles_needed <- c("75. percentilis", "90. percentilis",
                        "99. percentilis") %in% metric
  if (any(quantiles_needed)) {
    calculated_quantiles <- setNames(quantile(
      x, probs =  c(0.75, 0.90, 0.99)[quantiles_needed]),
      c("75. percentilis", "90. percentilis",
        "99. percentilis")[quantiles_needed])
    
    if ("75. percentilis" %in% metric) {
      stats_list[["75. percentilis"]] <- "75. percentilis"
      value1_list[["75. percentilis"]] <- calculated_quantiles["75. percentilis"]
      value2_list[["75. percentilis"]] <- NA_real_
    }
    if ("90. percentilis" %in% metric) {
      stats_list[["90. percentilis"]] <- "90. percentilis"
      value1_list[["90. percentilis"]] <- calculated_quantiles["90. percentilis"]
      value2_list[["90. percentilis"]] <- NA_real_
    }
    if ("99. percentilis" %in% metric) {
      stats_list[["99. percentilis"]] <- "99. percentilis"
      value1_list[["99. percentilis"]] <- calculated_quantiles["99. percentilis"]
      value2_list[["99. percentilis"]] <- NA_real_
    }
  }
  
  if ("75. percentilis" %in% metric) {
    stats_list[["75. percentilis"]] <- "75. percentilis"
    value1_list[["75. percentilis"]] <- quantile(x, 0.75)
    value2_list[["75. percentilis"]] <- NA
  }
  
  if ("90. percentilis" %in% metric) {
    stats_list[["90. percentilis"]] <- "90. percentilis"
    value1_list[["90. percentilis"]] <- quantile(x, 0.9)
    value2_list[["90. percentilis"]] <- NA
  }
  
  if ("99. percentilis" %in% metric) {
    stats_list[["99. percentilis"]] <- "99. percentilis"
    value1_list[["99. percentilis"]] <- quantile(x, 0.99)
    value2_list[["99. percentilis"]] <- NA
  }
  
  res <- data.table(
    stat = unlist(stats_list),
    value1 = unlist(value1_list),
    value2 = unlist(value2_list)
  )
  
  res[, formatted := fifelse(stat %in% c("-0", "0-5", "5-10", "10-15", "15-20", "20-30", "30-45", "45-60", "60-", ">5", ">20"),
                             paste0(round(value1 * 100, 1), "% (", value2, ")"),
                             fifelse(stat %in% c("Átlag", "Medián", "75. percentilis", "90. percentilis", "99. percentilis"),
                                     as.character(round(value1, 2)),
                                     fifelse(stat == "Megállások száma", as.character(value1), NA_character_)))]
  
  return(res)
}

expandlatlon <- function(dat) {
  if("Indulo" %in% colnames(dat)) dat <- merge(dat, allomaskoord[, .(Indulo = Allomas, InduloLat = lat, InduloLong = lon)], by = "Indulo", sort = FALSE)
  if("Erkezo" %in% colnames(dat)) dat <- merge(dat, allomaskoord[, .(Erkezo = Allomas, ErkezoLat = lat, ErkezoLong = lon)], by = "Erkezo", sort = FALSE)
  dat
}

keseshun <- function(metric) {
  switch(
    metric,
    "Átlag" = "Átlagos késés",
    "Medián" = "Medián késés",
    "75. percentilis" = "A késések 75. percentilise",
    "90. percentilis" = "A késések 90. percentilise",
    "99. percentilis" = "A késések 99. percentilise",
    ">5" = "5 percet meghaladó késések aránya",
    ">20" = "20 percet meghaladó késések aránya"
  )
}

ui <- navbarPage(
  theme = bslib::bs_theme(bootswatch = "default"),
  title = "Vonatkésési statisztika",
  lang = "hu",
  id = "main",
  
  header = list(
    tags$head(
      tags$meta(name = "description", content = desctext),
      tags$meta(property = "og:title",
                content = "Vonatkésési statisztika"),
      tags$meta(property = "og:type", content = "website"),
      tags$meta(property = "og:locale", content = "hu_HU"),
      tags$meta(property = "og:url", content = urlpre),
      tags$meta(property = "og:image",
                content = paste0(urlpre,
                                 "vonat-keses-image.png")),
      tags$meta(property = "og:image:width", content = 1280),
      tags$meta(property = "og:image:height", content = 640),
      tags$meta(property = "og:description", content = desctext),
      tags$meta(name = "DC.Title",
                content = "Vonatkésési statisztika"),
      tags$meta(name = "DC.Creator", content = "Ferenci Tamás"),
      tags$meta(name = "DC.Subject", content = "vasút"),
      tags$meta(name = "DC.Description", content = desctext),
      tags$meta(name = "DC.Publisher", content = urlpre),
      tags$meta(name = "DC.Contributor",
                content = "Ferenci Tamás"),
      tags$meta(name = "DC.Language", content = "hu_HU"),
      tags$meta(name = "twitter:card",
                content = "summary_large_image"),
      tags$meta(name = "twitter:title",
                content = "Vonatkésési statisztika"),
      tags$meta(name = "twitter:description", content = desctext),
      tags$meta(name = "twitter:image",
                content = paste0(urlpre,
                                 "vonat-keses-image.png")),
      
      tags$style(HTML(".share-btn {
        display: inline-block;
        color: #ffffff;
        border: none;
        padding: 0.1em 0.6em;
        outline: none;
        text-align: center;
        font-size: 0.9em;
        margin: 0 0.2em;
        text-decoration: none;
      }
      
      .share-btn:focus,
      .share-btn:hover {
        text-decoration: none;
        opacity: 0.8;
      }
      
      .share-btn:active {
        color: #e2e2e2;
      }
      
      .share-btn.twitter     { background: #55acee; }
      .share-btn.google-plus { background: #dd4b39; }
      .share-btn.facebook    { background: #3B5998; }
      .share-btn.stumbleupon { background: #EB4823; }
      .share-btn.reddit      { background: #ff5700; }
      .share-btn.hackernews  { background: #ff6600; }
      .share-btn.linkedin    { background: #4875B4; }
      .share-btn.email       { background: #444444; }"
      ))
    ),
    
    p("A program használatát részletesen bemutató súgó, valamint a technikai részletek",
      a("itt", href = "https://github.com/ferenci-tamas/vonat-keses",
        target = "_blank"), "olvashatóak el.")
  ),
  footer = list(
    hr(),
    p("Írta: ", a("Ferenci Tamás", href = "http://www.medstat.hu/", target = "_blank",
                  .noWS = "outside"), ", v1.00"),
    
    tags$script(HTML("
      var sc_project=11601191;
      var sc_invisible=1;
      var sc_security=\"5a06c22d\";
                     "),
                type = "text/javascript"),
    tags$script(type = "text/javascript",
                src = "https://www.statcounter.com/counter/counter.js", async = NA),
    tags$noscript(div(class = "statcounter",
                      a(title = "ingyen webstatisztika", href = "https://www.statcounter.hu/",
                        target = "_blank",
                        img(class = "statcounter",
                            src = "https://c.statcounter.com/11601191/0/5a06c22d/1/",
                            alt = "ingyen webstatisztika",
                            referrerPolicy = "no-referrer-when-downgrade"))))
  ),
  
  tabPanel(
    "Nyitóoldal",
    h2("Milyen funkció vannak az oldalnak?"),
    # p("A weboldalon a következő funkciók érhetőek el:"),
    tags$ul(
      tags$li("A", actionLink("gotoStat", "Táblázatos statisztikák"), " pontban lekérdezhető az összes adat számszerűen.",
              "Lekérdezhetőek a késések különféle statisztikái, egy vagy több napra, összesítve és naponként lebontva is. ",
              "Az adatok szűrhetőek adott vonattípusra vagy adott állomásra, illetve ezek szerint le is bonthatóak. ",
              "A kapott eredmények sorbarendezhetőek és lementhetőek."),
      tags$li("Az", actionLink("gotoTrend", "Időbeli trendek"), " pontban megtekinthetőek a késések hosszú távú trendjei. ",
              "Kiválaszthatóak különféle statisztikák, az eredmények szűrhetőek vonattípusra vagy állomásra, az előbbi szerint ",
              "le is bonthatóak. Az ábrák interaktívak."),
      tags$li("A", actionLink("gotoSpatial", "Területi összehasonlítás"), " pontban térképek rajzolhatóak a különféle típusú ",
              "késésekből, állomásokra és vonalakra vonatkoztatva is. A térképek színezettek, interaktívak, szabadon nagyíthatóak."),
      tags$li("Az egyéb elemzések között elérhető az", actionLink("gotoDatabase", "Adatbázis"), " pont, ahol megtekinthető és ",
              "szűrhető a statisztikák mögött lévő teljes adatbázis, a ", actionLink("gotoDistr", "Napi eloszlások"), " pont,",
              "ahol egy vagy néhány nap eloszlása vizualizálható különböző ábrázolási módszerekkel, és a ",
              actionLink("gotoWeek", "Heti mintázat"), " pont, ahol a különböző késési statisztikák esetleges heti mintázatai ",
              "vizsgálhatóak.")
    ),
    h2("Miért született ez az oldal?"),
    p(paste0(
      "A MÁV honlapján nyilvánosan elérhető minden egyes vonat minden egyes megállójánál a vonat menetrend szerinti és a ",
      "tényleges érkezési ideje, ebből fakadóan esetleges késése is, de erről nincsen semmilyen folyamatosan frissülő, grafikonon vagy ",
      "térképen kirajzolható, urambocsá' interaktívan lekérdezhető statisztika. Ez azért probléma, mert bár a vonatok késése gyakran tárgya ",
      "a közbeszédnek, rendszeresen hivatkoznak rá politikusok, szakértők, nem beszélve az utazókról, de épp az előbbi hiányosságból ",
      "fakadóan ez a közbeszéd sokszor kaotikus: a MÁV gyárt egyféle statisztikát, aztán azt eltünteti, aztán gyárt egy másikat, aztán ",
      "arra a szakértő azt mondja, hogy nem is úgy van, az egyik szerint ez pontos, a másik szerint az hiányos, az egyik szerint 7 perc, ",
      "a másik szerint nem is, mert 28%... Ez így szörnyű helyzet nekem, mint egyszeri, mezei állampolgárnak, meg valószínűleg az összes ",
      "többi mezei állampolgárnak is, ha tájékozódni kíván. Pláne megnehezíti ez a helyzet a konsktruktív eszmecserét a kérdésről, hiszen ",
      "annak elemi feltétele a közös, elfogadott információs bázis.")),
    p(paste0(
      "Az oldal célja a vonatok késéséről szóló közbeszéd színvonalának javítása: azt szerettem volna, hogy ahelyett, hogy ",
      "a különféle szereplők által előrántott és egymás fejéhez vágott, egymással összehasonlíthatatlan adattartalmú, kinézetű és ",
      "módszertanú statisztikák helyett legyen egy egységes, objektív, összehasonlítható, teljesen transzparens módszertannal készült, ",
      "hosszú távon is fenntartható, az adatokat mindenki számára elérhető és értelmezhető formában tartalmazó oldal.")),
    h2("Mit csinál az oldal?"),
    p(paste0(
      "A weboldal mögött lévő program minden éjjel letölti a MÁV honlapjáról a menetrend szerinti és tényleges érkezési időpontokat, ebből ",
      "kiszámolja a késéseket, és utána elkészíti belőle a legkülönfélébb statisztikákat, melyeket egy interaktív lekérdező felületen, ",
      "azaz ezen a weboldalon, elérhetővé tesz.")),
    p("A megvalósítás R programnyelv alatt készült, a felület a Shiny-t használja. Minden további részlet elérhető a ",
      a("https://github.com/ferenci-tamas/vonat-keses", href = "https://github.com/ferenci-tamas/vonat-keses", target = "_blank"),
      "oldalon."),
    p(paste0(
      "Külön is kiemelném, hogy ezen a Github-oldalon megtalálható mind az adatokat letöltő, mind az azokat feldolgozó program, valamint a ",
      "weboldal teljes forráskódja, így a munkám tökéletesen transzparens.")),
    h2("Milyen fontos megjegyzések tartoznak az oldalhoz?"),
    p(paste0(
      "Az oldalt hobbiból, szabadidőmben fejlesztettem, így az teljes mértékben nem hivatalos, a MÁV-hoz nincsen semmilyen köze. ",
      "Ebből az is következik, hogy a helyességére nézve nincsen semmiféle garancia, pláne hivatalos pecsét – igyekeztem ",
      "mindenben gondosan eljárni, de hibák előfordulhatnak, így minden kritikát, továbbfejlesztési javaslatot, ötlet a ",
      "legnagyobb örömmel veszek!")),
    p(paste0(
      "Fontosnak tartom még megjegyezni, hogy az oldal egy vonat adatait egyetlen egyszer tölti le egy nap (ráadásul azt is az ",
      "éjszaka közepén), így a MÁV informatikai rendszerére nézve vélhetően semmilyen érzékelhető terhelés-növekedést nem jelent. ",
      "Az oldal kizárólag nyilvános, bárki számára elérhető információkat használ fel.")),
    h2("Ki készítette az oldalt?"),
    p("Ferenci Tamás vagyok, szakmámat tekintve biostatisztikus, így a statisztikai elemzés kézre esett, ez némi vasút iránti ",
      "érdeklődéssel kombinálva ide vezetett... Minden további részlet rólam, beleértve az elérhetőségeimet, megtalálható a ",
      "honlapomon: ", a("https://www.medstat.hu/", href = "https://www.medstat.hu/", target = "_blank", .noWS = "outside"), "."),
    
    a("Facebook", href = "https://www.facebook.com/sharer.php?u=https%3A%2F%2Fwww.vonat-keses.hu",
      target = "_blank", rel = "noopener", class="share-btn facebook"),
    a("X/Twitter", href = "https://twitter.com/intent/tweet?url=https%3A%2F%2Fwww.vonat-keses.hu",
      target = "_blank", rel = "noopener", class="share-btn twitter"),
    a("E-mail", href = "mailto:?subject=vonat-keses.hu&body=vonat-keses.hu", class = "share-btn email")
  ),
  
  tabPanel("Táblázatos statisztikák", value = "stat",
           uiOutput("statContent")),
  
  tabPanel("Időbeli trendek", value = "trend",
           uiOutput("trendContent")),
  
  tabPanel("Területi összehasonlítás", value = "spatial",
           uiOutput("spatialContent")),
  
  navbarMenu(
    "Egyéb elemzések",
    
    tabPanel("Adatbázis", value = "database",
             uiOutput("databaseContent")),
    
    tabPanel("Napi eloszlások", value = "distr",
             uiOutput("distrContent")),
    
    tabPanel("Heti mintázat", value = "week",
             uiOutput("weekContent"))
  )
)

server <- function(input, output, session) {
  
  # updateSelectizeInput(session, "statTraintypeSel", choices = choicesVonatJelleg, selected = "Személyvonat", server = TRUE)
  # updateSelectizeInput(session, "statStationSel", choices = choicesAllomasErkezo, selected = "Budapest-Keleti", server = TRUE)
  # updateSelectizeInput(session, "databaseVonat", choices = choicesVonatSzam, selected = choicesVonatSzam, server = TRUE)
  # updateSelectizeInput(session, "databaseAllomas", choices = choicesAllomasErkezoIndulo, selected = choicesAllomasErkezoIndulo, server = TRUE)
  
  observeEvent(input$gotoStat, updateNavbarPage(session, "main", selected = "stat"))
  observeEvent(input$gotoTrend, updateNavbarPage(session, "main", selected = "trend"))
  observeEvent(input$gotoSpatial, updateNavbarPage(session, "main", selected = "spatial"))
  observeEvent(input$gotoDatabase, updateNavbarPage(session, "main", selected = "database"))
  observeEvent(input$gotoDistr, updateNavbarPage(session, "main", selected = "distr"))
  observeEvent(input$gotoWeek, updateNavbarPage(session, "main", selected = "week"))
  
  prev_slider_val <- reactiveVal(NULL)
  
  observeEvent(input$distrDate, {
    old_val <- prev_slider_val()
    new_val <- input$distrDate
    
    if(!is.null(old_val[1]) && !is.null(new_val[]) && any(old_val != new_val) && diff(range(new_val)) > 14) {
      if(old_val[1] != new_val[1]) updateSliderInput(session, "distrDate", value = c(new_val[2] - 14, new_val[2])) else
        updateSliderInput(session, "distrDate", value = c(new_val[1], new_val[1] + 14))
    }
    
    prev_slider_val(new_val)
  })
  
  renderedTabs <- reactiveVal(character(0))
  
  observeEvent(input$main, {
    currentTab <- input$main
    
    if (!currentTab %in% renderedTabs()) {
      if (currentTab == "stat") {
        output$statContent <- renderUI({
          sidebarLayout(
            sidebarPanel(
              selectInput("timeTime", "Időpont kiválasztása",
                          c("Utolsó nap", "Utolsó hét", "Utolsó hónap",
                            "Egyéni nap", "Egyéni intervallum"),
                          "Utolsó hét"),
              conditionalPanel("input.timeTime == 'Egyéni nap'",
                               sliderInput("timeTableCustomDate", "Dátum",
                                           min(ProcData$Datum),
                                           max(ProcData$Datum),
                                           max(ProcData$Datum))),
              conditionalPanel("input.timeTime == 'Egyéni intervallum'",
                               sliderInput("timeTableCustomInterval",
                                           "Intervallum",
                                           min(ProcData$Datum),
                                           max(ProcData$Datum),
                                           c(max(ProcData$Datum) - 7,
                                             max(ProcData$Datum)))),
              radioButtons("timeTableStratTime", "Megjelenítés módja",
                           c("Naponként", "Egyben")),
              radioButtons("statTraintype", "Vonattípus",
                           c("Összes egyben", "Lebontás", "Kiválasztott")),
              conditionalPanel(
                "input.statTraintype == 'Kiválasztott'",
                shinyWidgets::virtualSelectInput(
                  "statTraintypeSel", "Kiválasztott vonattípus",
                  choices$VonatJelleg, "Személyvonat", multiple = TRUE,
                  search = TRUE,
                  placeholder = "Válasszon",
                  allOptionsSelectedText = "Mindegyik",
                  searchPlaceholderText = "Keresés")
              ),
              radioButtons("statStation", "Vasútállomás",
                           c("Összes egyben", "Lebontás", "Kiválasztott")),
              conditionalPanel(
                "input.statStation == 'Kiválasztott'",
                shinyWidgets::virtualSelectInput(
                  "statStationSel", "Kiválasztott vasútállomás",
                  choices$AllomasErkezo, "Budapest-Keleti",
                  multiple = TRUE, search = TRUE,
                  placeholder = "Válasszon",
                  allOptionsSelectedText = "Mindegyik",
                  searchPlaceholderText = "Keresés")
              ),
              checkboxGroupInput("statMetric", "Megjelenített statisztikák",
                                 c("Megoszlás", ">5", ">20", "Átlag", "Medián",
                                   "75. percentilis", "90. percentilis", "99. percentilis"),
                                 c("Megoszlás", ">5", ">20", "Átlag")),
              width = 2
            ),
            
            mainPanel(
              shinycssloaders::withSpinner(DT::DTOutput("statOutput")),
              width = 10
            )
          )
        })
      } else if (currentTab == "trend") {
        output$trendContent <- renderUI({
          sidebarLayout(
            sidebarPanel(
              radioButtons("trendMode", "Megjelenítés módja",
                           c("Megoszlások", "Idők",
                             "Összetétel (diszkrét)")),
              conditionalPanel(
                "input.trendMode == 'Megoszlások' | input.trendMode == 'Idők'",
                radioButtons("trendTraintype", "Vonattípus",
                             c("Összes egyben", "Lebontás", "Kiválasztott")),
                conditionalPanel(
                  "input.trendTraintype == 'Kiválasztott'",
                  shinyWidgets::virtualSelectInput(
                    "trendTraintypeSel", "Kiválasztott vonattípus",
                    choices$VonatJelleg, "Személyvonat",
                    multiple = TRUE, search = TRUE,
                    placeholder = "Válasszon",
                    allOptionsSelectedText = "Mindegyik",
                    searchPlaceholderText = "Keresés")
                ),
                radioButtons("trendStation", "Vasútállomás",
                             c("Összes egyben", "Kiválasztott")),
                conditionalPanel(
                  "input.trendStation == 'Kiválasztott'",
                  shinyWidgets::virtualSelectInput(
                    "trendStationSel", "Kiválasztott vasútállomás",
                    choices$AllomasErkezo, "Budapest-Keleti",
                    multiple = TRUE, search = TRUE,
                    placeholder = "Válasszon",
                    allOptionsSelectedText = "Mindegyik",
                    searchPlaceholderText = "Keresés")
                ),
              ),
              conditionalPanel(
                "input.trendMode == 'Megoszlások' & input.trendTraintype != 'Lebontás'",
                checkboxGroupInput("trendStatsFreq",
                                   "Megjelenített statisztikák",
                                   c(">5", ">20"), c(">5", ">20"))
              ),
              conditionalPanel(
                "input.trendMode == 'Megoszlások' & input.trendTraintype == 'Lebontás'",
                radioButtons("trendStatsFreqSingle",
                             "Megjelenített statisztikák",
                             c(">5", ">20"))
              ),
              conditionalPanel(
                "input.trendMode == 'Idők' & input.trendTraintype != 'Lebontás'",
                checkboxGroupInput("trendStatsTime", "Megjelenített statisztikák",
                                   c("Átlag", "Medián", "75. percentilis", "90. percentilis", "99. percentilis"),
                                   "Átlag")
              ),
              conditionalPanel(
                "input.trendMode == 'Idők' & input.trendTraintype == 'Lebontás'",
                radioButtons("trendStatsTimeSingle", "Megjelenített statisztikák",
                             c("Átlag", "Medián", "75. percentilis", "90. percentilis", "99. percentilis"))
              ),
              conditionalPanel(
                "input.trendMode == 'Idők'",
                checkboxInput("trendLog", "Logaritmikus késési idő")
              ),
              width = 2
            ),
            
            mainPanel(
              shinycssloaders::withSpinner(highchartOutput("trendOutput")),
              width = 10
            )
          )
        })
      } else if (currentTab == "spatial") {
        output$spatialContent <- renderUI({
          sidebarLayout(
            sidebarPanel(
              radioButtons("spatialMode",
                           div("Ábrázolt jellemző",
                               bslib::tooltip(
                                 bsicons::bs_icon("question-circle"),
                                 HTML(kesesExplanation),
                                 placement = "left"
                               )),
                           c("Teljes késés", "Indulási késés",
                             "Állomási késés", "Nyíltvonali késés")),
              width = 2
            ),
            
            mainPanel(
              shinycssloaders::withSpinner(highchartOutput("spatialOutput")),
              sliderInput("spatialTimerange",
                          div("Vizsgált időpont vagy időszak",
                              bslib::tooltip(
                                bsicons::bs_icon("question-circle"),
                                "A csúszka két végét ugyanoda húzva egyetlen nap választható ki.",
                                placement = "left"
                              )),
                          min(ProcData$Datum), max(ProcData$Datum),
                          c(max(ProcData$Datum) - 7, max(ProcData$Datum)), timeFormat = "%m. %d.", width = "100%"),
              width = 10
            )
          )
        })
      } else if (currentTab == "database") {
        output$databaseContent <- renderUI({
          sidebarLayout(
            sidebarPanel(
              radioButtons("databaseMode",
                           div("Ábrázolt jellemző",
                               bslib::tooltip(
                                 bsicons::bs_icon("question-circle"),
                                 HTML(kesesExplanation),
                                 placement = "left"
                               )),
                           c("Teljes késés", "Indulási késés",
                             "Állomási késés", "Nyíltvonali késés")),
              shinyWidgets::airDatepickerInput(
                "databaseDate", "Dátum",
                c(min(ProcData$Datum), max(ProcData$Datum)),
                minDate = min(ProcData$Datum),
                maxDate = max(ProcData$Datum),
                range = TRUE),
              shinyWidgets::virtualSelectInput(
                "databaseVonattipus", "Vonattípus",
                choices$VonatJelleg, choices$VonatJelleg,
                multiple = TRUE, search = TRUE,
                placeholder = "Válasszon",
                allOptionsSelectedText = "Mindegyik",
                searchPlaceholderText = "Keresés"),
              shinyWidgets::virtualSelectInput(
                "databaseVonat", "Vonat", choices$VonatSzam,
                choices$VonatSzam, multiple = TRUE, search = TRUE,
                placeholder = "Válasszon",
                allOptionsSelectedText = "Mindegyik",
                searchPlaceholderText = "Keresés"),
              shinyWidgets::virtualSelectInput(
                "databaseAllomas", "Állomás",
                choices$AllomasErkezoIndulo,
                choices$AllomasErkezoIndulo,
                multiple = TRUE, search = TRUE,
                placeholder = "Válasszon",
                allOptionsSelectedText = "Mindegyik",
                searchPlaceholderText = "Keresés"),
              width = 2
            ),
            
            mainPanel(
              shinycssloaders::withSpinner(DT::DTOutput("databaseOutput")),
              width = 10
            )
          )
        })
      } else if (currentTab == "distr") {
        output$distrContent <- renderUI({
          sidebarLayout(
            sidebarPanel(
              sliderInput("distrDate",
                          div("Dátum vagy intervallum",
                              bslib::tooltip(
                                bsicons::bs_icon("question-circle"),
                                "Legfeljebb 14 nap választható az áttekinthetőség érdekében.",
                                placement = "left"
                              )),
                          min(ProcData$Datum),
                          max(ProcData$Datum),
                          c(max(ProcData$Datum) - 7,
                            max(ProcData$Datum))),
              radioButtons("distrMode",
                           "Megjelenítés módja",
                           c("Hisztogram", "Magfüggvényes sűrűségbecslés", "Boxplot")),
              checkboxInput("distrLog", "Logaritmikus késési idő"),
              width = 2
            ),
            mainPanel(
              shinycssloaders::withSpinner(highchartOutput("distrOutput")),
              width = 10
            )
          )
        })
      } else if (currentTab == "week") {
        output$weekContent <- renderUI({
          sidebarLayout(
            sidebarPanel(
              selectInput(
                "weekMetric", "Használt mutató",
                c(">5", ">20", "Átlag",
                  "Medián", "75. percentilis", "90. percentilis",
                  "99. percentilis")),
              radioButtons("weekTraintype", "Vonattípus",
                           c("Összes egyben", "Kiválasztott")),
              conditionalPanel(
                "input.weekTraintype == 'Kiválasztott'",
                shinyWidgets::virtualSelectInput(
                  "weekTraintypeSel", "Kiválasztott vonattípus",
                  choices$VonatJelleg, "Személyvonat",
                  multiple = TRUE, search = TRUE,
                  placeholder = "Válasszon",
                  allOptionsSelectedText = "Mindegyik",
                  searchPlaceholderText = "Keresés")
              ),
              radioButtons("weekStation", "Vasútállomás",
                           c("Összes egyben", "Kiválasztott")),
              conditionalPanel(
                "input.weekStation == 'Kiválasztott'",
                shinyWidgets::virtualSelectInput(
                  "weekStationSel", "Kiválasztott vasútállomás",
                  choices$AllomasErkezo, "Budapest-Keleti",
                  multiple = TRUE, search = TRUE,
                  placeholder = "Válasszon",
                  allOptionsSelectedText = "Mindegyik",
                  searchPlaceholderText = "Keresés")
              ),
              width = 2
            ),
            
            mainPanel(
              shinycssloaders::withSpinner(highchartOutput("weekOutput")),
              width = 10
            )
          )
        })
      }
    }
    
    renderedTabs(unique(c(renderedTabs(), currentTab)))
  }
  )
  
  output$statOutput <- DT::renderDT({
    pd <- ProcData[Tipus %in% c("Szakasz", "ZaroSzakasz")]
    pd <- switch(input$timeTime,
                 "Utolsó nap" = pd[Datum == max(pd$Datum)],
                 "Utolsó hét" = pd[Datum >= max(pd$Datum) - 7],
                 "Utolsó hónap" = pd[Datum >= max(pd$Datum) - 30],
                 "Egyéni nap" = pd[Datum == input$timeTableCustomDate],
                 "Egyéni intervallum" = pd[Datum >= input$timeTableCustomInterval[1] &
                                             Datum <= input$timeTableCustomInterval[2]])
    daterange <- range(pd$Datum)
    if(input$statTraintype == "Kiválasztott") pd <- pd[VonatJelleg %in% input$statTraintypeSel]
    if(input$statStation == "Kiválasztott") pd <- pd[Erkezo %in% input$statStationSel]
    
    if(nrow(pd) == 0) return(NULL)
    
    byvars <- character()
    if(input$timeTableStratTime == "Naponként") byvars <- c(byvars, c("Dátum" = "Datum"))
    if(input$statTraintype != "Összes egyben") byvars <- c(byvars, c("Vonat jellege" = "VonatJelleg"))
    if(input$statStation != "Összes egyben") byvars <- c(byvars, c("Állomás" = "Erkezo"))
    
    pd <- pd[, kesesstat(KumKeses, c("N", input$statMetric)), byvars]
    
    if(!"Dátum" %in% colnames(pd)) pd$`Dátum` <- paste0(daterange, collapse = " - ")
    byvars <- union("Dátum", names(byvars))
    
    pd <- dcast(pd, as.formula(paste0("`", paste0(byvars, collapse = "`+`"), "`~ factor(stat, levels = unique(pd$stat))")),
                value.var = c("formatted", "value1"))[order(`Dátum`, decreasing = TRUE)]
    names(pd) <- gsub("formatted_", "", names(pd))
    
    statcolnumber <- (ncol(pd) - length(byvars)) / 2
    
    DT::datatable(
      pd, rownames = FALSE, #filter = "top",
      extensions = "Buttons", selection = "single",
      options = list(
        language = list(url = dt18nurl),
        dom = "lfrtipB", pageLength = 20,
        buttons = c("copy", "csv", "excel", "print"),
        columnDefs =
          c(lapply(1:statcolnumber, function(i)
            list(targets = i + length(byvars) - 1,
                 orderData = i + length(byvars) - 1 + statcolnumber)),
            lapply(1:statcolnumber, function(i)
              list(targets = i + length(byvars) - 1 + statcolnumber,
                   visible = FALSE)))))
  })
  
  output$trendOutput <- renderHighchart({
    pd <- ProcData
    
    if(input$trendMode %in% c("Megoszlások", "Idők") &&
       input$trendTraintype == "Kiválasztott") pd <- pd[VonatJelleg %in% input$trendTraintypeSel]
    if(input$trendMode %in% c("Megoszlások", "Idők") &&
       input$trendStation == "Kiválasztott") pd <- pd[Erkezo %in% input$trendStationSel]
    if(nrow(pd) == 0) return(NULL)
    
    metricsel <- if(input$trendMode == "Megoszlások") {
      if(input$trendTraintype == "Lebontás") input$trendStatsFreqSingle else input$trendStatsFreq
    } else if(input$trendMode == "Idők") {
      if(input$trendTraintype == "Lebontás") input$trendStatsTimeSingle else input$trendStatsTime
    } else if(input$trendMode == "Összetétel (diszkrét)") {
      "Megoszlás"
    }
    byvars <- if(input$trendMode %in% c("Megoszlások", "Idők") &&
                 input$trendTraintype == "Lebontás") c("Datum", "VonatJelleg") else "Datum"
    
    pd <- pd[Tipus %in% c("Szakasz", "ZaroSzakasz"), kesesstat(KumKeses, metricsel), byvars]
    
    if(input$trendMode == "Megoszlások") {
      p <- if(input$trendTraintype == "Lebontás")
        hchart(pd, "line", hcaes(x = Datum, y = value1 * 100, group = VonatJelleg)) else
          hchart(pd, "line", hcaes(x = Datum, y = value1 * 100, group = stat))
      
      p <- p |>
        hc_tooltip(valueDecimals = 1, valueSuffix = "%") |>
        hc_yAxis(title = list(text = "Arány [%]")) |>
        hc_legend(title = list(text = if(input$trendTraintype == "Lebontás") "Vonattípus" else "Késési idő [perc]")) |>
        hc_title(text = paste0(
          if(input$trendTraintype == "Lebontás") keseshun(input$trendStatsFreqSingle) else "Késések időbeli trendjei",
          if(input$trendTraintype == "Kiválasztott") paste0(", ", paste0(input$trendTraintypeSel, collapse = ", ")) else "",
          if(input$trendStation == "Kiválasztott") paste0(", ", paste0(input$trendStationSel, collapse = ", ")) else ""))
    } else if(input$trendMode == "Idők") {
      p <- if(input$trendTraintype == "Lebontás")
        hchart(pd, "line", hcaes(x = Datum, y = value1, group = VonatJelleg)) else
          hchart(pd, "line", hcaes(x = Datum, y = value1, group = stat))
      
      p <- p |>
        hc_tooltip(valueDecimals = 2, valueSuffix = " perc") |>
        hc_yAxis(title = list(text = "Késési idő [perc]")) |>
        hc_legend(title = list(text = if(input$trendTraintype == "Lebontás") "Vonattípus" else "Statisztika")) |>
        hc_title(text = paste0(
          if(input$trendTraintype == "Lebontás") keseshun(input$trendStatsTimeSingle) else "Késések időbeli trendjei",
          if(input$trendTraintype == "Kiválasztott") paste0(", ", paste0(input$trendTraintypeSel, collapse = ", ")) else "",
          if(input$trendStation == "Kiválasztott") paste0(", ", paste0(input$trendStationSel, collapse = ", ")) else ""))
      
      if(input$trendLog) p <- p |> hc_yAxis(type = "logarithmic")
    } else if(input$trendMode == "Összetétel (diszkrét)") {
      p <- hchart(pd, "column",
                  hcaes(x = Datum, y = value1 * 100, group = factor(stat, levels = c("-0", "0-5", "5-10", "10-15", "15-20", "20-30", "30-45", "45-60", "60-")))) |>
        hc_plotOptions(series = list(stacking = "normal")) |>
        hc_tooltip(valueDecimals = 1, valueSuffix = "%") |>
        hc_yAxis(title = list(text = "Megoszlás [%]"), reversedStacks = FALSE, min = 0, max = 100) |>
        hc_legend(title = list(text = "Késési idő [perc]"))
    }
    
    p |>
      hc_xAxis(title = list(text = "Dátum")) |>
      hc_add_theme(hc_theme(chart = list(backgroundColor = "white"))) |>
      hc_caption(text = figcap) |>
      hc_credits(enabled = TRUE) |>
      hc_exporting(enabled = TRUE)
  })
  
  output$spatialOutput <- renderHighchart({
    pd <- ProcData[Datum >= input$spatialTimerange[1] &
                     Datum <= input$spatialTimerange[2]]
    
    p <- highchart(type = "map") |>
      hc_add_series(mapData = mapdata, showInLegend = FALSE)
    
    if(input$spatialMode == "Nyíltvonali késés") {
      pd <- expandlatlon(pd[Tipus %in% c("Szakasz", "ZaroSzakasz"),
                            .(Keses = mean(pmax(0, Keses), na.rm = TRUE)),
                            .(Indulo, Erkezo)])
      dat <- lapply(1:nrow(pd), function(i) {
        list(
          Keses = pd$Keses[i],
          Indulo = pd$Indulo[i],
          Erkezo = pd$Erkezo[i],
          geometry = list(
            type = "LineString",
            coordinates = list(
              c(pd$InduloLong[i], pd$InduloLat[i]),
              c(pd$ErkezoLong[i], pd$ErkezoLat[i])
            )
          )
        )
      })
      
      p <- p |>
        hc_add_series(data = dat,
                      type = "mapline", colorKey = "Keses",
                      showInLegend = FALSE) |>
        hc_colorAxis(min = min(pd$Keses, na.rm = TRUE),
                     max = max(pd$Keses, na.rm = TRUE),
                     minColor = "blue", maxColor = "red",
                     stops = colstops) |>
        hc_tooltip(headerFormat = "<b>{point.Indulo}</b> - <b>{point.Erkezo}</b><br>",
                   pointFormat = "Átlagos késés: {point.Keses:.1f} perc") |>
        hc_plotOptions(mapline = list(lineWidth = 2))
    } else if(input$spatialMode == "Indulási késés") {
      pd <- expandlatlon(pd[Tipus == "InduloAllomas",
                            .(Keses = mean(pmax(0, Keses), na.rm = TRUE)),
                            .(Indulo)])
      p <- p |>
        hc_add_series(data = pd[, .(Indulo, Keses, lat = InduloLat,
                                    lon = InduloLong)],
                      type = "mappoint", colorKey = "Keses",
                      showInLegend = FALSE) |>
        hc_colorAxis(min = min(pd$Keses, na.rm = TRUE),
                     max = max(pd$Keses, na.rm = TRUE),
                     minColor = "blue", maxColor = "red",
                     stops = colstops) |>
        hc_tooltip(headerFormat = "<b>{point.point.Indulo}</b><br>",
                   pointFormat = "Átlagos késés: {point.Keses:.1f} perc")
    } else if(input$spatialMode == "Teljes késés") {
      pd <- expandlatlon(pd[Tipus %in% c("Szakasz", "ZaroSzakasz"),
                            .(Keses = mean(pmax(0, KumKeses), na.rm = TRUE)),
                            .(Erkezo)])
      p <- p |>
        hc_add_series(data = pd[, .(Erkezo, Keses, lat = ErkezoLat,
                                    lon = ErkezoLong)],
                      type = "mappoint", colorKey = "Keses",
                      showInLegend = FALSE) |>
        hc_colorAxis(min = min(pd$Keses, na.rm = TRUE),
                     max = max(pd$Keses, na.rm = TRUE),
                     minColor = "blue", maxColor = "red",
                     stops = colstops) |>
        hc_tooltip(headerFormat = "<b>{point.point.Erkezo}</b><br>",
                   pointFormat = "Átlagos késés: {point.Keses:.1f} perc")
    } else if(input$spatialMode == "Állomási késés") {
      pd <- expandlatlon(pd[Tipus == "KozbensoAllomas",
                            .(Keses = mean(pmax(0, Keses), na.rm = TRUE)),
                            .(Erkezo)])
      p <- p |>
        hc_add_series(data = pd[, .(Erkezo, Keses, lat = ErkezoLat,
                                    lon = ErkezoLong)],
                      type = "mappoint", colorKey = "Keses",
                      showInLegend = FALSE) |>
        hc_colorAxis(min = min(pd$Keses, na.rm = TRUE),
                     max = max(pd$Keses, na.rm = TRUE),
                     minColor = "blue", maxColor = "red",
                     stops = colstops) |>
        hc_tooltip(headerFormat = "<b>{point.point.Erkezo}</b><br>",
                   pointFormat = "Átlagos késés: {point.Keses:.1f} perc")
    }
    
    p |>
      hc_chart(panning = list(enabled = TRUE)) |>
      hc_mapNavigation(
        enabled = TRUE, enableMouseWheelZoom = TRUE,
        enableDoubleClickZoom = TRUE,
        mouseWheelSensitivity = 1.3) |>
      hc_add_theme(hc_theme(chart = list(backgroundColor = "white"))) |>
      hc_title(text = paste0(input$spatialMode, ", ",
                             if(input$spatialTimerange[1] == input$spatialTimerange[2]) input$spatialTimerange[1] else
                               paste0(range(input$spatialTimerange), collapse = " - "))) |>
      hc_caption(text = figcap) |>
      hc_credits(enabled = TRUE) |>
      hc_exporting(enabled = TRUE)
  })
  
  output$distrOutput <- renderHighchart({
    dat <- ProcData[Tipus %in% c("Szakasz", "ZaroSzakasz") & Datum >= input$distrDate[1] &
                      Datum <= input$distrDate[2] & !is.na(KumKeses)]
    if(input$distrLog) dat <- dat[KumKeses > 0]

    if(input$distrMode == "Hisztogram") {
      p <- hchart(hist(dat$KumKeses, breaks = 30, plot = FALSE)) |>
        hc_xAxis(title = list(text = "Késési idő [perc]")) |>
        hc_yAxis(title = list(text = "Gyakoriság [darab]"))
      # p <- htmltools::browsable(hw_grid(lapply(unique(dat$Datum), function(d) hchart(hist(dat[Datum == d]$KumKeses, plot = FALSE)))))
    } else if(input$distrMode == "Magfüggvényes sűrűségbecslés") {
      p <- highchart() |>
        hc_add_series_list(lapply(unique(dat$Datum), function(d)
          list(data = list_parse2(as.data.frame(density(dat[Datum == d]$KumKeses)[1:2])), name = d)))
      if(input$distrLog) p <- p |> hc_xAxis(type = "logarithmic")
    } else if(input$distrMode == "Boxplot") {
      p <- highchart() |>
        hc_xAxis(type = "category") |>
        hc_add_series_list(data_to_boxplot(dat, KumKeses, Datum)) |>
        hc_legend(enabled = FALSE) |>
        hc_tooltip(enabled = FALSE)
      if(input$distrLog) p <- p |> hc_yAxis(type = "logarithmic")
    }
    p |>
      hc_add_theme(hc_theme(chart = list(backgroundColor = "white"))) |>
      hc_caption(text = figcap) |>
      hc_credits(enabled = TRUE) |>
      hc_exporting(enabled = TRUE)
  })
  
  output$databaseOutput <- DT::renderDT({
    pd <- if(length(input$databaseDate) == 2)
      ProcData[Datum >= input$databaseDate[1] &
                 Datum <= input$databaseDate[2]] else
                   ProcData[Datum == input$databaseDate]
    pd <- pd[VonatSzam %in% input$databaseVonat]
    pd <- pd[VonatJelleg %in% input$databaseVonattipus]
    
    if(input$databaseMode == "Nyíltvonali késés") {
      pd <- pd[Indulo %in% input$databaseAllomas |
                 Erkezo %in% input$databaseAllomas]
      pd <- pd[Tipus %in% c("Szakasz", "ZaroSzakasz"),
               .(`Dátum` = Datum, Vonat = VonatSzam,
                 `Vonat jellege` = VonatJelleg,
                 `Induló állomás` = Indulo,
                 `Érkező állomás` = Erkezo, `Késés` = Keses)]
    } else if(input$databaseMode == "Indulási késés") {
      pd <- pd[Indulo %in% input$databaseAllomas]
      pd <- pd[Tipus == "InduloAllomas",
               .(`Dátum` = Datum, Vonat = VonatSzam,
                 `Vonat jellege` = VonatJelleg,
                 `Állomás` = Indulo, `Késés` = Keses)]
    } else if(input$databaseMode == "Teljes késés") {
      pd <- pd[Erkezo %in% input$databaseAllomas]
      pd <- pd[Tipus %in% c("Szakasz", "ZaroSzakasz"),
               .(`Dátum` = Datum, Vonat = VonatSzam,
                 `Vonat jellege` = VonatJelleg,
                 `Állomás` = Erkezo, `Késés` = KumKeses)]
    } else if(input$databaseMode == "Állomási késés") {
      pd <- pd[Erkezo %in% input$databaseAllomas]
      pd <- pd[Tipus == "KozbensoAllomas",
               .(`Dátum` = Datum, Vonat = VonatSzam,
                 `Vonat jellege` = VonatJelleg,
                 `Állomás` = Erkezo, `Késés` = Keses)]
    }
    DT::datatable(pd, rownames = FALSE, #filter = "top",
                  extensions = "Buttons", selection = "single",
                  options = list(
                    language = list(url = dt18nurl),
                    dom = "lfrtipB", #pageLength = 20,
                    buttons = c("copy", "csv", "excel", "print")))
  })
  
  output$weekOutput <- renderHighchart({
    pd <- ProcData
    
    if(input$weekTraintype == "Kiválasztott") pd <- pd[VonatJelleg %in% input$weekTraintypeSel]
    if(input$weekStation == "Kiválasztott") pd <- pd[Erkezo %in% input$weekStationSel]
    if(nrow(pd) == 0) return(NULL)
    
    pd$day <- lubridate::wday(pd$Datum)
    pd$yearweek <- paste0(lubridate::year(pd$Datum), " - ",
                          lubridate::week(pd$Datum))
    
    pd <- pd[Tipus %in% c("Szakasz", "ZaroSzakasz"), kesesstat(KumKeses, input$weekMetric), .(yearweek, day)][order(yearweek, day)]
    
    p <- hchart(pd, "line", hcaes(x = day, y = value1, group = yearweek)) |>
      hc_tooltip(valueDecimals = 2, valueSuffix = " perc") |>
      hc_title(text = paste0(
        keseshun(input$weekMetric),
        if(input$weekTraintype == "Kiválasztott") paste0(", ", paste0(input$weekTraintypeSel, collapse = ", ")) else "",
        if(input$weekStation == "Kiválasztott") paste0(", ", paste0(input$weekStationSel, collapse = ", ")) else "")) |>
      hc_xAxis(title = list(text = "Hét napja"), allowDecimals = FALSE) |>
      hc_yAxis(title = list(text = "Késési idő [perc]")) |>
      hc_legend(title = list(text = "Hét")) |>
      hc_add_theme(hc_theme(chart = list(backgroundColor = "white"))) |>
      hc_caption(text = figcap) |>
      hc_credits(enabled = TRUE) |>
      hc_exporting(enabled = TRUE)
  })
}

shinyApp(ui = ui, server = server)