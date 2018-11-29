library(shiny)

ui <- fluidPage(
  
  titlePanel("Secret Santa GeneratR"),

  p("Welcome to yet another secret santa generator! The advantage of this one is that it gives you the option 
    to keep the pairings secret or not, as well as avoiding pairs of people who should not gift each other! 
    Perfect if you have inter-dimensional friends that can't physically interact with each other."),
  
  #Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Text for providing a caption ----
      # Note: Changes made to the caption in the textInput control
      # are updated in the output area immediately as you type
      textInput(inputId = "Members",
                label = "Group member names:",
                value = "A, B, C, D, E, F"),
      
      # Explanation of spouse matching
      p("If you want to prevent specific pairs of people from gifting each other, 
        write down some characteristic that pairs them below (in the order they're written above). 
        In the example below, A/B and C/D are part of 'Couple' and 'Couple2', respectively, and won't gift within couples; 
        but E and F have their own group and can give/receive with anyone 
        (note that the number of entries must match the number of members). 
        Write 'NA' if you don't care about this."),
      
      # Set pairs to avoid
      textInput(inputId = "Spouses",
                label = "Avoidance list:",
                value = "Couple, Couple, Couple2, Couple2, S1, S2"),
      
      # Note on making it secret
      p("If you want to keep it secret, a file for each member will be created telling them who they should gift
        based on a new, unseen pairing. Just send each person their file!"),
      
      # and the respective download button for the zip file
      downloadButton("download", "Make Secret")

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      p("Here are the current santa pairs. Note that if you click on 'Make Secret' a completely new scheme will be 
        produced that you won't see here."),
      
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
  
  
  # render the resulting table 
  output$view <- renderTable({
    
      # Parse the strings
      m <- unlist(strsplit(gsub(" ", "", input$Members, fixed=T), ","))
      s <- unlist(strsplit(gsub(" ", "", input$Spouses, fixed=T), ","))
      # Make sure the groupings don't break the code by having a group be over 50% of members
      if(max(table(s)) > length(m)/2) {stop("A group can't be more than 50% of the total members...")}
      # Produce pairings to display
      xmaspairs(Members = m, 
                Spouses = m,
                Secret = input$Secret)
    
  })
  
  
  # Download a zip file with each pair 
  output$download <- downloadHandler(
    
    # Name of the download
    filename = function() {"SecretSantaPairs.zip"},
    
    # Prep the zip file 
    content = function(file) { 
      # Parse the strings
      m <- unlist(strsplit(gsub(" ", "", input$Members, fixed=T), ","))
      s <- unlist(strsplit(gsub(" ", "", input$Spouses, fixed=T), ","))
      # Make sure the groupings don't break the code by having a group be over 50% of members
      if(max(table(s)) > length(m)/2) {stop("A group can't be more than 50% of the total members...")}
      #Pairing
      df <- xmaspairs(Members = m, 
                      Spouses = s)
      # Write files per person indicating to whom they have to gift
      owd <- setwd(tempdir()) # temporary dir to store the files
      on.exit(setwd(owd))
      files <- list()      
      lapply(seq(nrow(df)), function(i) {
        write.table(paste("Your secret santa is: ", df[i, 2], "!", sep = ""), 
                    file = paste(df[i,1],".txt", sep = ""),
                    row.names = F, 
                    col.names = F)})
      zip(file, paste(unlist(strsplit(gsub(" ", "", input$Members, fixed=T), ",")), ".txt", sep = "")) # and zip
    }
    
  )
  
  
}

shinyApp(ui = ui, server = server)
