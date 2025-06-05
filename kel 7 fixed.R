library(shiny)
library(leaflet)
library(shinyjs)
library(shinyWidgets)
library(bslib)
library(officer)

# Setup storage files
users_file     <- "users.rds"
responses_file <- "responses.rds"
if (!file.exists(users_file)) saveRDS(data.frame(user="admin", password="Admin@123", stringsAsFactors=FALSE), users_file)
if (!file.exists(responses_file)) saveRDS(data.frame(user=character(), type=character(), detail=character(), timestamp=character(), stringsAsFactors=FALSE), responses_file)
users     <- readRDS(users_file)
responses <- readRDS(responses_file)

# Contact experts
expert_contacts <- list(
  "Psikolog - drh. Ana Psiko" = list(
    Nama         = "drh. Ana Psiko",
    Telepon      = "0812-3456-7890",
    Email        = "ana.psiko@klinikjiwa.id",
    Sosmed       = "@kliniksoulcare (IG)",
    Tempat       = "Klinik Jiwa Sejahtera",
    Spesialisasi = "Penyakit Mental, Neurologi",
    Lokasi       = c(-6.200, 106.816)
  ),
  "Psikolog - dr. Siti Rina" = list(
    Nama         = "dr. Siti Rina",
    Telepon      = "0813-1111-2222",
    Email        = "siti.rina@psikologid.org",
    Sosmed       = "@psikologsitirina",
    Tempat       = "Klinik MindCare",
    Spesialisasi = "Psikolog Anak",
    Lokasi       = c(-7.250, 112.750)
  )
)

# PSS-10 setup
test_title       <- "Tes Deteksi Dini (PSS-10)"
test_description <- "Silakan jawab setiap pernyataan berikut berdasarkan pengalaman Anda dalam sebulan terakhir."
test_questions   <- c(
  "1. Seberapa sering anda merasa gugup dan stres?",
  "2. Saya merasa tidak dapat mengendalikan hal-hal penting.",
  "3. Seberapa sering Anda merasa bahwa Anda marah karena hal-hal yang berada di luar kendali Anda?",
  "4. Seberapa sering Anda merasa bahwa Anda mengendalikan hal-hal penting dalam hidup Anda?",
  "5. Seberapa sering Anda merasa bahwa segala sesuatu berjalan sesuai keinginan Anda?",
  "6. Seberapa sering Anda merasa kesulitan mengatasi semua hal yang harus Anda lakukan?",
  "7. Seberapa sering Anda merasa mampu mengatasi gangguan yang muncul dalam hidup Anda?",
  "8. Seberapa sering Anda merasa bahwa Anda memiliki kendali penuh terhadap cara Anda mengatur waktu Anda?",
  "9. Seberapa sering Anda merasa bahwa kesulitan-kesulitan menumpuk begitu banyak sehingga Anda tidak dapat mengatasinya?",
  "10. Seberapa sering Anda merasa marah karena sesuatu yang terjadi secara tiba-tiba dan tidak terduga?"
)
reverse_idx <- c(4,5,7,8)

# Theme
app_theme <- bs_theme(
  bg = "#f0f8ff", fg = "#000000",
  base_font = font_google("Poppins"),
  heading_font = font_google("Rubik"),
  heading_font_scale = 1.4
)

ui <- fluidPage(
  theme = app_theme,
  useShinyjs(),
  tags$head(tags$style(HTML(
    ".header{background:linear-gradient(to right,#6a0dad,#00008b);color:white;padding:20px;text-align:center;font-family:'Rubik';}
     h1{font-size:2.5rem;font-family:'Rubik';}
     h2{font-size:2rem;font-family:'Rubik';}
     h3,h4{font-family:'Poppins';}
     p,li,.shiny-input-container{font-family:'Poppins';font-size:1rem;}
     body{background:#f0f8ff;}
     .article{background:#f7f7f0;border-radius:8px;padding:20px;margin-bottom:20px;}
     .sidebar{background:#f5f5dc;padding:15px;border-radius:8px;}
     .btn{font-family:'Poppins';font-weight:600;}
     .right{float:right;}
     .spacer{margin-bottom:20px;}"
  ))),
  div(class="header", h1("🧠 MoodGuard")),
  uiOutput("uiLogin"),
  uiOutput("uiApp"),
  div(style="text-align:center;color:#666;margin-top:20px;", p("© 2025 Mental Health Companion"))
)

server <- function(input, output, session) {
  creds <- reactiveValues(logged=FALSE, user=NULL)
  
  # Password validation function
  valid_pw <- function(pw) grepl("(?=.*[a-z])(?=.*[A-Z])(?=.*[^A-Za-z0-9])", pw, perl=TRUE)
  
  # Login / Sign Up UI
  output$uiLogin <- renderUI({
    if(!creds$logged) fluidRow(
      column(4, class="sidebar", wellPanel(
        h2("🔒 Login / Sign Up"),
        textInput("user","Username"),
        passwordInput("pw","Password"),
        actionButton("btnSign","Sign up", class="btn-success"), br(),
        actionButton("btnLogin","Log in", class="btn-primary"), br(),
        textOutput("msgLogin")
      )),
      column(8, class="article",
             h2("Artikel Edukasi: Menangani Stres"),
             p("Stres merupakan respons psikologis dan fisiologis terhadap tekanan eksternal atau internal yang dirasakan mengancam keseimbangan seseorang. Menurut Lazarus dan Folkman (1984), stres terjadi ketika individu menilai bahwa tuntutan yang dihadapi melebihi kemampuan kopingnya. Salah satu strategi efektif untuk mengatasi stres adalah dengan coping berbasis kognitif, yaitu mengubah cara pandang terhadap stresor. Penelitian dalam Journal of Health Psychology menunjukkan bahwa pendekatan kognitif, seperti cognitive reappraisal, mampu mengurangi tingkat kortisol dalam tubuh, hormon yang berperan dalam respons stres (Troy et al., 2010). Terapi Kognitif-Perilaku (CBT), yang banyak digunakan di praktik klinis, berfokus pada pengidentifikasian pikiran negatif yang irasional dan menggantinya dengan pola pikir yang lebih adaptif. CBT terbukti efektif dalam mengurangi gejala stres kronis dan kecemasan pada kelompok dewasa muda menurut meta-analisis oleh Hofmann et al. (2012). "),
             p("Teknik relaksasi juga memainkan peran krusial dalam manajemen stres. Salah satu metode yang paling banyak diteliti adalah Mindfulness-Based Stress Reduction (MBSR), yang dikembangkan oleh Jon Kabat-Zinn. MBSR menggabungkan meditasi kesadaran, pernapasan sadar, dan latihan tubuh seperti yoga ringan. Penelitian dalam Psychosomatic Medicine oleh Davidson et al. (2003) menunjukkan bahwa partisipan yang mengikuti program MBSR selama 8 minggu mengalami peningkatan aktivitas otak pada area prefrontal cortex yang terkait dengan emosi positif. Selain itu, studi oleh Chiesa dan Serretti (2009) menemukan bahwa MBSR mampu menurunkan gejala stres, depresi, dan kecemasan secara signifikan, baik pada populasi klinis maupun non-klinis. Praktik mindfulness membantu individu untuk tidak larut dalam pikiran negatif berulang, yang sering menjadi pemicu stres, dan menggantinya dengan kesadaran penuh terhadap momen saat ini secara non-reaktif."),
             p("Intervensi sosial juga terbukti menjadi faktor pelindung yang kuat dalam mengatasi stres. Dukungan sosial dari keluarga, teman, dan komunitas dapat meningkatkan ketahanan psikologis seseorang terhadap tekanan hidup. Menurut penelitian oleh Cohen dan Wills (1985) dalam Psychological Bulletin, dukungan sosial dapat berfungsi sebagai buffer terhadap efek stres, terutama dalam situasi penuh tekanan seperti kehilangan pekerjaan atau masalah kesehatan kronis. Program berbasis komunitas, seperti kelompok dukungan sebaya (peer support), juga telah terbukti meningkatkan kesehatan mental dan mengurangi isolasi sosial. Selain itu, aktivitas fisik teratur seperti jalan kaki, berenang, atau bersepeda tidak hanya meningkatkan kesehatan fisik tetapi juga meningkatkan pelepasan endorfin, hormon yang meningkatkan mood positif. World Health Organization (WHO) menyarankan minimal 150 menit aktivitas fisik intensitas sedang per minggu sebagai bagian dari manajemen stres dan kesehatan mental yang optimal."),
             p("Daftar Pustaka"),
             tags$ul(
               tags$li(a("Lazarus, R. S., & Folkman, S. (1984). Stress, appraisal, and coping. Springer Publishing.", href="#")),
               tags$li(a("Troy, A. S., Wilhelm, F. H., Shallcross, A. J., & Mauss, I. B. (2010). Journal of Health Psychology, 15(6), 775–785.", href="#")),
               tags$li(a("Hofmann, S. G., Asnaani, A., Vonk, I. J., Sawyer, A. T., & Fang, A. (2012). Cognitive Therapy and Research, 36(5), 427–440.", href="#")),
               tags$li(a("Davidson, R. J., et al. (2003). Psychosomatic Medicine, 65(4), 564–570.", href="#")),
               tags$li(a("Chiesa, A., & Serretti, A. (2009). Journal of Alternative and Complementary Medicine, 15(5), 593–600.", href="#")),
               tags$li(a("Cohen, S., & Wills, T. A. (1985). Psychological Bulletin, 98(2), 310–357.", href="#")),
               tags$li(a("World Health Organization. (2020). Guidelines on physical activity and sedentary behaviour.", href="https://www.who.int", target="_blank"))
             )
      )
    )
  })
  
  # Sign Up logic
  observeEvent(input$btnSign, {
    req(input$user, input$pw)
    if(!valid_pw(input$pw)) {
      output$msgLogin <- renderText("Password harus memuat huruf besar, huruf kecil, dan angka/simbol.")
    } else if(input$user %in% users$user) {
      output$msgLogin <- renderText("Username sudah terdaftar.")
    } else {
      users <<- rbind(users, data.frame(user=input$user, password=input$pw, stringsAsFactors=FALSE))
      saveRDS(users, users_file)
      output$msgLogin <- renderText("Sign up berhasil. Silakan login.")
    }
  })
  
  # Log in logic
  observeEvent(input$btnLogin, {
    idx <- which(users$user==input$user & users$password==input$pw)
    if(length(idx)==1) {
      creds$logged <- TRUE
      creds$user   <- input$user
      output$msgLogin <- renderText(NULL)
    } else {
      output$msgLogin <- renderText("Login gagal: username/password salah.")
    }
  })
  
  # Main App UI
  output$uiApp <- renderUI({ req(creds$logged)
    fluidRow(
      column(3, class="sidebar",
             radioButtons("menu","Menu",choices=c(
               "Pengamatan Kualitas Istirahat"="obs",
               "Tes Deteksi Dini"="test",
               "Konsultasi pada Ahli"="konsul",
               "Kritik & Saran"="feedback",
               "Unduh Hasil (CSV)"="export"
             ))
      ),
      column(9, class="content", uiOutput("content"))
    )
  })
  
  # Render content
  output$content <- renderUI({ req(input$menu)
    switch(input$menu,
           obs = div(class="article",
                     h3("🛏️ Pengamatan Kualitas Istirahat"),
                     dateInput("d_obs","Tanggal"),
                     numericInput("tidur","Waktu tidur (jam)",7,0,24),
                     textAreaInput("act","Jumlah aktivitas fisik yang dilakukan",rows=3,placeholder="Misal: jogging, yoga"),
                     p(em("Pisahkan aktivitas dengan koma atau titik koma.")),
                     numericInput("act_dur","Durasi aktivitas (menit)",75,0,300),
                     sliderInput("phone","Penggunaan smartphone (jam/hari)",0,24,2),
                     actionButton("go_obs","Submit",class="btn-primary"),
                     tableOutput("out_obs")
           ),
           test = div(class="article",
                      h3("🔬 ",test_title), p(test_description),
                      lapply(seq_along(test_questions), function(i)
                        sliderInput(paste0("q",i), test_questions[i],1,5,3)
                      ),
                      actionButton("go_test","Submit",class="btn-primary"),
                      verbatimTextOutput("out_test"), uiOutput("rekom_test"), hr(),
                      h4("Daftar Pustaka PSS-10"),
                      tags$ul(
                        tags$li(a("Cohen et al. 1983",href="https://doi.org/10.2307/2136404",target="_blank")),
                        tags$li(a("Lee 2012",href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3564396/",target="_blank"))
                      )
           ),
           konsul = div(class="article",
                        h3("👨‍⚕️ Konsultasi pada Ahli"),
                        selectInput("which_expert","Pilih Ahli",names(expert_contacts)),
                        actionButton("go_konsul","Tampilkan Kontak",class="btn-primary"), p(),
                        uiOutput("out_konsul"), leafletOutput("map_konsul",height=250)
           ),
           feedback = div(class="article",
                          h3("✍️ Kritik & Saran"),
                          radioButtons("star","Rating:",choices=1:5,inline=TRUE),
                          textAreaInput("kritik","Kritik & Saran Anda:",rows=5),
                          actionButton("submit_fb","Kirim",class="btn-primary"),
                          verbatimTextOutput("fb_out"),
                          p("Kalian juga bisa isi form di sini: ", tags$a("Formulir Kritik & Saran", href="https://forms.gle/GfaNzURUHjRfgV838", target="_blank"))
           ),
           export = div(class="article",
                        h3("📥 Unduh Hasil Anda"),
                        downloadButton("download_csv","Download CSV Hasil",class="btn-info right"),
                        p("Klik tombol di atas untuk unduh CSV."),
                        p(tags$a("Butuh PDF? Klik di sini untuk konversi.", href="https://www.freeconvert.com/csv-to-pdf", target="_blank"))
           )
    )
  })
  
  # Observasi logic
  observeEvent(input$go_obs, {  
    status <- ifelse(input$tidur>=7 & input$tidur<=10 & input$phone<=3,"Baik","Perlu Perbaikan")
    acts <- strsplit(input$act,"[;,]")[[1]]
    df <- data.frame(
      Tanggal      = as.character(input$d_obs),
      TidurJam     = input$tidur,
      Activities   = length(acts),
      ActivityList = paste(acts,collapse=", "),
      DurasiMenit  = input$act_dur,
      Smartphone   = input$phone,
      Status       = status,
      stringsAsFactors=FALSE
    )
    output$out_obs <- renderTable(df)
    detail <- paste0("T=",input$d_obs,",Tdur=",input$tidur,",ActCount=",length(acts),",ActList=",paste(acts,collapse=","),",Dur=",input$act_dur,",Phone=",input$phone,",Stat=",status)
    responses <<- rbind(responses,data.frame(user=creds$user,type="obs",detail=detail,timestamp=Sys.time(),stringsAsFactors=FALSE))
    saveRDS(responses,responses_file)
  })
  
  # Tes logic
  observeEvent(input$go_test, {
    scores <- sapply(seq_along(test_questions), function(i){v<-input[[paste0("q",i)]]; if(i%in%reverse_idx)6-v else v})
    total <- sum(scores)
    kategori <- if(total<=13)"Stress rendah"else if(total<=26)"Stress sedang"else"Stress tinggi"
    output$out_test <- renderPrint(cat("Skor PSS-10:",total,"| Kategori:",kategori))
    output$rekom_test <- renderUI(if(total<=26)tags$ul(tags$li("Relaksasi"),tags$li("Olahraga"),tags$li("Tidur cukup")) else p("Silakan konsultasi profesional."))
    detail <- paste0("Scores=",paste(scores,collapse=","),",Total=",total)
    responses <<- rbind(responses,data.frame(user=creds$user,type="test",detail=detail,timestamp=Sys.time(),stringsAsFactors=FALSE))
    saveRDS(responses,responses_file)
  })
  
  # Konsultasi logic
  observeEvent(input$go_konsul, {
    req(input$which_expert)
    info <- expert_contacts[[input$which_expert]]
    output$out_konsul <- renderUI(tagList(
      h4(info$Nama),
      p(strong("Telepon:"),info$Telepon),
      p(strong("Email:"),info$Email),
      p(strong("Sosial Media:"),info$Sosmed),
      p(strong("Tempat Praktik:"),info$Tempat),
      p(strong("Spesialisasi:"),info$Spesialisasi),
      p(tags$a("Lihat di Google Maps", href=paste0("https://www.google.com/maps/search/?api=1&query=", info$Lokasi[1], ",", info$Lokasi[2]), target="_blank"))
    ))
    output$map_konsul <- renderLeaflet(leaflet() %>% addTiles() %>% addMarkers(lng=info$Lokasi[2], lat=info$Lokasi[1], popup=info$Tempat))
    responses <<- rbind(responses,data.frame(user=creds$user,type="konsul",detail=input$which_expert,timestamp=Sys.time(),stringsAsFactors=FALSE))
    saveRDS(responses,responses_file)
  })
  
  # Feedback logic
  observeEvent(input$submit_fb, {
    feedback_detail <- paste("Rating:", input$star, "| Kritik & Saran:", input$kritik)
    responses <<- rbind(responses, data.frame(user=creds$user, type="feedback", detail=feedback_detail, timestamp=Sys.time(), stringsAsFactors=FALSE))
    saveRDS(responses, responses_file)
    output$fb_out <- renderPrint("Terima kasih atas kritik dan saran Anda!")
  })
  
  # Handler CSV download
  output$download_csv <- downloadHandler(
    filename = function() paste0("hasil_", creds$user, ".csv"),
    content = function(file) {
      write.csv(responses[responses$user == creds$user, ], file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)