ui <- page_sidebar(
  title = div(class = "app-title", "NCAA Championships Explorer"),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#00274C",
    base_font = font_google("Inter")
  ),
  tags$head(includeCSS("www/custom.css")),

  sidebar = sidebar(
    width = 310,
    div(class = "sidebar-heading", "Filters"),

    conditionalPanel(
      condition = "input.main_nav == 'compare'",
      selectInput(
        "compare_school_1", "School 1",
        choices = school_choices,
        selected = default_school("Maryland")
      ),
      selectInput(
        "compare_school_2", "School 2",
        choices = school_choices,
        selected = default_school("Virginia")
      )
    ),

    conditionalPanel(
      condition = "input.main_nav == 'profile'",
      selectInput(
        "profile_school", "School",
        choices = school_choices,
        selected = default_school("Stanford")
      )
    ),

    conditionalPanel(
      condition = "input.main_nav == 'table' || input.main_nav == 'heatmap' || input.main_nav == 'geography'",
      selectInput(
        "sport_filter", "Sport",
        choices = c("All", sport_choices),
        selected = "All"
      )
    ),

    conditionalPanel(
      condition = "input.main_nav == 'table' || input.main_nav == 'geography'",
      selectInput(
        "conference_filter", "Conference",
        choices = c("All", conference_choices),
        selected = "All"
      ),
      selectInput(
        "state_filter", "State",
        choices = c("All", state_choices),
        selected = "All"
      )
    ),

    conditionalPanel(
      condition = "input.main_nav == 'table' || input.main_nav == 'heatmap' || input.main_nav == 'geography'",
      selectInput(
        "school_filter", "School",
        choices = c("All", school_choices),
        selected = "All"
      )
    ),

    sliderInput(
      "year_range", "Years",
      min = min(dashboard_data$Year, na.rm = TRUE),
      max = max(dashboard_data$Year, na.rm = TRUE),
      value = range(dashboard_data$Year, na.rm = TRUE),
      step = 1,
      sep = ""
    )
  ),

  navset_card_tab(
    id = "main_nav",

    nav_panel(
      "Overview", value = "overview",
      div(class = "page-intro",
          h1("Dashboard Overview"),
          p("A high-level view of championship success across schools, sports, and decades.")),

      layout_columns(
        div(class = "kpi-card", div(class = "kpi-label", "Championships"), div(class = "kpi-number", textOutput("total_titles"))),
        div(class = "kpi-card", div(class = "kpi-label", "Champion Schools"), div(class = "kpi-number", textOutput("unique_schools"))),
        div(class = "kpi-card", div(class = "kpi-label", "Sports"), div(class = "kpi-number", textOutput("unique_sports"))),
        div(class = "kpi-card", div(class = "kpi-label", "Years Covered"), div(class = "kpi-number small-kpi", textOutput("years_covered"))),
        col_widths = c(3, 3, 3, 3)
      ),

      layout_columns(
        card(class = "panel-card", card_header("Top Championship Programs"), plotlyOutput("top_schools", height = 560)),
        card(class = "panel-card", card_header("Championships by Decade"), plotlyOutput("titles_by_decade", height = 560)),
        col_widths = c(7, 5)
      )
    ),

    nav_panel(
      "Geography", value = "geography",
      div(class = "page-intro", h1("Championship Geography"), p("One circle per school; hover to view total championships.")),
      card(class = "panel-card", leafletOutput("map", height = 760))
    ),

    nav_panel(
      "Heatmap", value = "heatmap",
      div(class = "page-intro", h1("Championship Heatmap"), p("The leading program in each sport and decade, colored by school.")),
      card(class = "panel-card", plotlyOutput("heatmap", height = 760))
    ),

    nav_panel(
      "School Profile", value = "profile",
      div(class = "page-intro", h1("School Profile"), p("A focused look at one program's championship history.")),
      uiOutput("profile_hero"),
      layout_columns(
        card(class = "panel-card", card_header("Titles by Sport"), plotlyOutput("profile_sport_chart", height = 520)),
        card(class = "panel-card", card_header("Titles by Decade"), plotlyOutput("profile_decade_chart", height = 520)),
        col_widths = c(7, 5)
      ),
      card(class = "panel-card table-panel", card_header("Championship History"), DTOutput("profile_table"))
    ),

    nav_panel(
      "Compare Schools", value = "compare",
      div(class = "page-intro", h1("Compare Schools"), p("Choose both programs from the filters on the left.")),
      uiOutput("compare_hero"),
      layout_columns(
        card(class = "panel-card", card_header("Titles by Sport"), plotlyOutput("compare_sports", height = 560)),
        card(class = "panel-card", card_header("Titles by Decade"), plotlyOutput("compare_decades", height = 560)),
        col_widths = c(7, 5)
      ),
      card(class = "panel-card table-panel", card_header("Championship History"), DTOutput("compare_table"))
    ),

    nav_panel(
      "Table", value = "table",
      div(class = "page-intro", h1("Championship Table"), p("Filter, search, sort, and scroll through the complete dataset.")),
      card(class = "panel-card table-panel", DTOutput("table"))
    )
  )
)
