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
      
      p("If you want to avoid specific pairs of people from gifting each other, 
        write down some characteristic that pairs them. So, if A and B above should
        not gift each other, write down: 1, 1, 2, 3 (they are grouped by the number 1). 
        If A/B and C/D come from the same households, you can say: 
        House1, House1, House2, House2. (note: the number of entries has to match
        the number of members)"),
      
      textInput(inputId = "Spouses",
                label = "Spouse grouping:",
                value = "Nope"),
      
      p("If you want to keep it secret, a file for each member will be created telling them who they should gift"),
      
      strong("Important: make sure the download path is ok before making it secret."),
      
      textInput(inputId = "Path",
                label = "Download path:",
                value = "~/Downloads"),
      
      selectInput(inputId = "Secret",
                  label = "Make it secret?",
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
    
    # Perform the actual pairing
    # If spouse pairing should be avoided..
    if (length(Spouses) == length(Members)) {
      # Once this turns false, we are in business
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
    
    # spit out the pairs
    return(df)
    
  }
  
  # render the resulting table if Secret = F, otherwise display it
  output$view <- renderTable({
    
    # If the final pairings should be secret, then write files for each person
    if (input$Secret) {
      #Pairing
      df <- xmaspairs(Members = unlist(strsplit(input$Members, ",")), 
                Spouses = unlist(strsplit(input$Spouses, ",")))
      # Write files per person indicating to whom they have to gift
      for (i in seq(nrow(df))) {
        write.table(paste("Your secret santa is: ", df[i, 2], "!", sep = ""), 
                    file = paste(input$Path,"/",df[i,1],".txt", sep = ""),
                    row.names = F, 
                    col.names = F)
      }
      # Just so people know that something happened
      return(paste("A file for each person has been downloaded to ", input$Path, "!", sep = ""))
    # Otherwise just display the output  
    } else {
      xmaspairs(Members = unlist(strsplit(input$Members, ",")), 
                Spouses = unlist(strsplit(input$Spouses, ",")),
                Secret = input$Secret)
    }
  })
  
}

shinyApp(ui = ui, server = server)