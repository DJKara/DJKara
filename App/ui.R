shinyUI(
  pageWithSidebar(
    headerPanel("큐라레 시뮬레이션"),
    
    sidebarPanel(
      selectInput("type", "종류", 
                  choices=c("행운상자", "고급 행운상자", "프리미엄 인쇄")),
      conditionalPanel(condition = "input.type == '프리미엄 인쇄'",
                       textInput("trial", "프리미엄 인쇄 X회")),
      conditionalPanel(condition = "input.type == '행운상자'",
                       textInput("trial2", "행운상자 X회")),
      conditionalPanel(condition = "input.type == '고급 행운상자'",
                       textInput("trial3", "행운상자 X회")),
      actionButton("button", "인쇄 시작")
    ),
    
    mainPanel(
      
      tableOutput("draw")
    )
  )
)