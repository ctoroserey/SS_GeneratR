library(shiny)
ui <- fluidPage(
  
  titlePanel("Secret Santa GeneratR"),
  
  #Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Text for providing a caption ----
      # Note: Changes made to the caption in the textInput control
      # are updated in the output area immediately as you type
      textInput(inputId = "Members",
                label = "Group Members:",
                value = "A, B, C, D"),
      
      textInput(inputId = "Spouses",
                label = "Spouse grouping (e.g. A & B married: 1,1,2,3):",
                value = "Nope"),

      selectInput(inputId = "Secret",
                  label = "See the pairs?",
                  choices = c(FALSE, TRUE))

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: HTML table with requested number of observations ----
      tableOutput("view")

    )
  )
)

server <- function(input, output){
  
  # Function that does the pairings
  xmaspairs <- function(Members = 1, Spouses = 1, Secret = T) {
    
    # This crude function will pair group members for secret santa
    # If your family would like to avoid pairing significant others,
    # define Spouses to be a vector of groupings (i.e. numeric).
    # The script will then iterate over pairings until no SOs are paired.
    #
    # To avoid unreliable smtp setups to send emails, 
    # if Secret = T then .txt files will be created for each member with their secret santa.
    # (This means that you still have to send each file to each member independently..sorry)
    # Otherwise the pairings are returned as a data frame.
    
    # Make sure that members were provided..
    if (length(Members) == 1) {stop("No members indicated...")}
    
    # If spouse pairing should be avoided..
    if (length(Spouses) == length(Members)) {
      
      Affiliated <- T
      
      # Iterate over possible pairings until no SOs are paired
      while (Affiliated) {
        m <- sample(Members)
        df <- data.frame(Member = m,
                         Gift_to = c(tail(m, n = 1), m[seq(length(m)-1)])) # sure there's a better way to do this..
        Spouse1 <- Spouses[match(df$Member, Members)]
        Spouse2 <- Spouses[match(df$Gift_to, Members)]
        Affiliated <- T %in% (Spouse1 == Spouse2)
      }
      # Otherwise just produce whatever pairing comes out from a single sampling  
    } else { 
      m <- sample(Members)
      df <- data.frame(Member = m,
                       Gift_to = c(tail(m, n = 1), m[seq(length(m)-1)]))
    }
    
    # If the person running it should be blinded to the pairs, create individual txt files
    if (Secret) {
      for (i in seq(nrow(df))) {
        write.table(paste("Your secret santa is: ", df[i, 2], "!", sep = ""), 
                    file = paste(df[i,1],".txt", sep = ""),
                    row.names = F, 
                    col.names = F)
      }
      # or for groups who don't care about social subtleties
    } else {
      return(df)
    }
  }
  
  # render the resulting table
  output$view <- renderTable({
    xmaspairs(Members = unlist(strsplit(input$Members, ",")), 
              Spouses = unlist(strsplit(input$Spouses, ",")),
              Secret = input$Secret)
  })
  
}

shinyApp(ui = ui, server = server)