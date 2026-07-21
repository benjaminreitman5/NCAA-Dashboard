server <- function(input, output, session) {

  active_years <- reactive({
    req(input$year_range)
    input$year_range
  })

  filtered_data <- reactive({
    yr <- active_years()
    data <- dashboard_data %>% filter(Year >= yr[1], Year <= yr[2])

    if (!is.null(input$sport_filter) && input$sport_filter != "All") data <- data %>% filter(Sport == input$sport_filter)
    if (!is.null(input$conference_filter) && input$conference_filter != "All") data <- data %>% filter(Conference == input$conference_filter)
    if (!is.null(input$state_filter) && input$state_filter != "All") data <- data %>% filter(State == input$state_filter)
    if (!is.null(input$school_filter) && input$school_filter != "All") data <- data %>% filter(Champion == input$school_filter)

    data
  })

  output$total_titles <- renderText(format(nrow(filtered_data()), big.mark = ","))
  output$unique_schools <- renderText(n_distinct(filtered_data()$Champion))
  output$unique_sports <- renderText(n_distinct(filtered_data()$Sport))
  output$years_covered <- renderText(paste(active_years()[1], active_years()[2], sep = "–"))

  output$top_schools <- renderPlotly({
    d <- filtered_data() %>% count(Champion, PrimaryHex, sort = TRUE) %>% slice_head(n = 15)
    validate(need(nrow(d) > 0, "No data available."))
    colors <- d %>% distinct(Champion, PrimaryHex) %>% deframe()

    p <- ggplot(d, aes(reorder(Champion, n), n, fill = Champion,
                       text = paste0("School: ", Champion, "<br>Titles: ", n))) +
      geom_col(width = .68) + coord_flip() +
      scale_fill_manual(values = colors) +
      labs(x = NULL, y = "Titles") + theme_minimal(base_size = 14) +
      theme(legend.position = "none", panel.grid.major.y = element_blank())

    ggplotly(p, tooltip = "text") %>%
      layout(margin = list(l = 125, r = 20, t = 10, b = 45)) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$titles_by_decade <- renderPlotly({
    d <- filtered_data() %>%
      mutate(DecadeNum = floor(Year / 10) * 10,
             Decade = paste0(DecadeNum, "s")) %>%
      count(DecadeNum, Decade, name = "Titles") %>%
      arrange(DecadeNum) %>%
      mutate(Decade = factor(Decade, levels = Decade))

    p <- ggplot(d, aes(Decade, Titles, text = paste0(Decade, "<br>Titles: ", Titles))) +
      geom_col(fill = "#00274C", width = .7) +
      labs(x = NULL, y = "Titles") + theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major.x = element_blank())

    ggplotly(p, tooltip = "text") %>%
      layout(margin = list(l = 55, r = 20, t = 10, b = 95)) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$heatmap <- renderPlotly({
    d <- filtered_data() %>%
      mutate(DecadeNum = floor(Year / 10) * 10, Decade = paste0(DecadeNum, "s")) %>%
      group_by(DecadeNum, Decade, Sport, Champion, PrimaryHex) %>%
      summarise(Titles = n(), .groups = "drop") %>%
      group_by(DecadeNum, Decade, Sport) %>%
      slice_max(Titles, n = 1, with_ties = FALSE) %>% ungroup() %>%
      mutate(Decade = factor(Decade, levels = unique(Decade[order(DecadeNum)])),
             hover = paste0("Decade: ", Decade, "<br>Sport: ", Sport,
                            "<br>Top champion: ", Champion, "<br>Titles: ", Titles))

    validate(need(nrow(d) > 0, "No data available."))
    colors <- d %>% distinct(Champion, PrimaryHex) %>% deframe()

    p <- ggplot(d, aes(Sport, Decade, fill = Champion, text = hover)) +
      geom_tile(color = "white", linewidth = .45) +
      scale_fill_manual(values = colors) +
      labs(x = NULL, y = NULL) + theme_minimal(base_size = 13) +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())

    ggplotly(p, tooltip = "text") %>%
      layout(margin = list(l = 65, r = 20, t = 10, b = 155)) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$map <- renderLeaflet({
    d <- filtered_data() %>%
      filter(!is.na(Latitude), !is.na(Longitude), Latitude >= 18, Latitude <= 50,
             Longitude >= -125, Longitude <= -66) %>%
      count(Champion, Nickname, City, State, Conference, Latitude, Longitude, PrimaryHex,
            sort = TRUE, name = "TotalTitles")

    validate(need(nrow(d) > 0, "No school map data available."))

    leaflet(d) %>% addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -96, lat = 39, zoom = 4) %>%
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude,
        radius = ~pmax(4, log2(TotalTitles + 1) * 1.9),
        color = ~PrimaryHex, fillColor = ~PrimaryHex,
        fillOpacity = .82, weight = 1,
        label = ~paste0(Champion, " — ", TotalTitles, " titles"),
        popup = ~paste0("<strong>", Champion, "</strong>",
                       ifelse(Nickname == "", "", paste0(" ", Nickname)),
                       "<br>", Conference, "<br>", City, ", ", State,
                       "<br><br><strong>Total Championships: ", TotalTitles, "</strong>")
      )
  })

  profile_data <- reactive({
    req(input$profile_school)
    dashboard_data %>% filter(Champion == input$profile_school)
  })

  output$profile_hero <- renderUI({
    d <- profile_data(); validate(need(nrow(d) > 0, "No profile data."))
    primary <- d$PrimaryHex[1]
    total <- nrow(d); sports <- n_distinct(d$Sport)
    first <- min(d$Year); recent <- max(d$Year)
    best <- d %>% count(Sport, sort = TRUE) %>% slice(1) %>% pull(Sport)

    div(class = "profile-hero", style = paste0("--school-color:", primary, ";"),
        div(class = "profile-identity",
            div(class = "profile-school", paste(input$profile_school, d$Nickname[1])),
            div(class = "profile-subtitle", paste0(d$City[1], ", ", d$State[1], " • ", d$Conference[1])),
            div(class = "profile-total", paste(total, "Total Championships"))),
        div(class = "profile-stat", span("Sports Won"), strong(sports)),
        div(class = "profile-stat", span("Best Sport"), strong(best)),
        div(class = "profile-stat", span("Title Span"), strong(paste(first, recent, sep = "–"))))
  })

  output$profile_sport_chart <- renderPlotly({
    d <- profile_data() %>% count(Sport, PrimaryHex, sort = TRUE)
    p <- ggplot(d, aes(reorder(Sport, n), n, text = paste0(Sport, "<br>Titles: ", n))) +
      geom_col(fill = d$PrimaryHex[1], width = .7) + coord_flip() +
      labs(x = NULL, y = "Titles") + theme_minimal(base_size = 13) +
      theme(panel.grid.major.y = element_blank())
    ggplotly(p, tooltip = "text") %>% layout(margin = list(l = 135, r = 20, t = 10, b = 45)) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$profile_decade_chart <- renderPlotly({
    d <- profile_data() %>% mutate(DecadeNum = floor(Year/10)*10, Decade = paste0(DecadeNum,"s")) %>%
      count(DecadeNum, Decade, PrimaryHex, name = "Titles") %>% arrange(DecadeNum) %>%
      mutate(Decade = factor(Decade, levels = Decade))
    p <- ggplot(d, aes(Decade, Titles, text = paste0(Decade, "<br>Titles: ", Titles))) +
      geom_col(fill = d$PrimaryHex[1], width = .7) + labs(x = NULL, y = "Titles") +
      theme_minimal(base_size = 13) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = "text") %>% layout(margin = list(l = 55, r = 20, t = 10, b = 95)) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$profile_table <- renderDT({
    profile_data() %>% select(Year, Sport, Champion, Conference, City, State, Region) %>%
      arrange(desc(Year), Sport) %>%
      datatable(rownames = FALSE, extensions = "Scroller",
                options = list(deferRender = TRUE, scrollY = 430, scrollX = TRUE,
                               scroller = TRUE, pageLength = 20, dom = "frtip"))
  }, server = FALSE)

  compare_data <- reactive({
    req(input$compare_school_1, input$compare_school_2)
    yr <- active_years()
    dashboard_data %>% filter(Champion %in% c(input$compare_school_1, input$compare_school_2),
                              Year >= yr[1], Year <= yr[2])
  })

  output$compare_hero <- renderUI({
    d <- compare_data()
    school_card <- function(school) {
      x <- d %>% filter(Champion == school)
      if (!nrow(x)) return(div(class = "compare-school-card", h2(school), p("No titles in selected years.")))
      div(class = "compare-school-card", style = paste0("--school-color:", x$PrimaryHex[1], ";"),
          h2(paste(school, x$Nickname[1])),
          div(class = "compare-big-number", nrow(x)),
          div(class = "compare-caption", "Total Championships"),
          div(class = "compare-detail", paste(n_distinct(x$Sport), "sports •", min(x$Year), "–", max(x$Year))))
    }
    s1 <- d %>% filter(Champion == input$compare_school_1) %>% distinct(Sport) %>% pull()
    s2 <- d %>% filter(Champion == input$compare_school_2) %>% distinct(Sport) %>% pull()
    shared <- length(intersect(s1, s2))

    layout_columns(
      school_card(input$compare_school_1),
      div(class = "compare-vs", div(class = "vs-text", "VS"), div(class = "shared-text", paste(shared, "shared sports"))),
      school_card(input$compare_school_2),
      col_widths = c(5, 2, 5)
    )
  })

  output$compare_sports <- renderPlotly({
    d <- compare_data() %>% count(Champion, Sport, PrimaryHex, sort = TRUE)
    validate(need(nrow(d) > 0, "No championships found."))
    colors <- d %>% distinct(Champion, PrimaryHex) %>% deframe()
    p <- ggplot(d, aes(reorder(Sport, n), n, fill = Champion,
                       text = paste0(Champion, "<br>", Sport, "<br>Titles: ", n))) +
      geom_col(position = position_dodge(.8), width = .65) + coord_flip() +
      scale_fill_manual(values = colors) + labs(x = NULL, y = "Titles", fill = NULL) +
      theme_minimal(base_size = 13) + theme(legend.position = "bottom", panel.grid.major.y = element_blank())
    ggplotly(p, tooltip = "text") %>%
      layout(margin = list(l = 140, r = 20, t = 10, b = 75),
             legend = list(orientation = "h", x = .5, xanchor = "center")) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$compare_decades <- renderPlotly({
    d <- compare_data() %>% mutate(DecadeNum=floor(Year/10)*10, Decade=paste0(DecadeNum,"s")) %>%
      count(Champion, DecadeNum, Decade, PrimaryHex, name="Titles") %>% arrange(DecadeNum) %>%
      mutate(Decade=factor(Decade, levels=unique(Decade)))
    colors <- d %>% distinct(Champion, PrimaryHex) %>% deframe()
    p <- ggplot(d, aes(Decade, Titles, fill=Champion,
                       text=paste0(Champion,"<br>",Decade,"<br>Titles: ",Titles))) +
      geom_col(position=position_dodge(.8), width=.65) + scale_fill_manual(values=colors) +
      labs(x=NULL,y="Titles",fill=NULL) + theme_minimal(base_size=13) +
      theme(legend.position="bottom", axis.text.x=element_text(angle=45,hjust=1))
    ggplotly(p, tooltip="text") %>%
      layout(margin=list(l=55,r=20,t=10,b=105), legend=list(orientation="h",x=.5,xanchor="center")) %>%
      config(displayModeBar=FALSE,responsive=TRUE)
  })

  output$compare_table <- renderDT({
    compare_data() %>% select(Year,Sport,Champion,Conference,City,State) %>%
      arrange(desc(Year),Champion,Sport) %>%
      datatable(rownames=FALSE, extensions="Scroller",
                options=list(deferRender=TRUE,scrollY=430,scrollX=TRUE,scroller=TRUE,pageLength=20,dom="frtip"))
  }, server=FALSE)

  output$table <- renderDT({
    filtered_data() %>% select(Year,Sport,Champion,Conference,City,State,Region) %>%
      arrange(desc(Year),Sport,Champion) %>%
      datatable(rownames=FALSE, filter="top", extensions="Scroller",
                options=list(deferRender=TRUE,scrollY=650,scrollX=TRUE,scroller=TRUE,pageLength=25,dom="frtip"))
  }, server=FALSE)
}
