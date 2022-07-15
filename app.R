library(shiny)
library(dplyr)
library(magrittr)
library(holdem)
library(shinyWidgets)
library(tidyr)  


# UI
ui <- fluidPage(
  tags$style('.container-fluid {
                             background-color: #277714;
              }'),
  
  

  titlePanel(div("Hold `em", style = "color: white")),
  
  fluidRow(
    column(2, wellPanel(selectInput("players", "Players:",c(2:6), selected = 6))),
    column(3, wellPanel(radioButtons("stage", "Stage:",
               inline = TRUE,
               c("Pre-Flop",
                 "Flop",
                 "Turn",
                 "River"),
               selected="Pre-Flop"))),
    actionButton("reset", "Clear")
    ),

  br(), 
 
  fluidRow(
  column(2, wellPanel(htmlOutput("random_image_out1"))),  
  column(2, wellPanel(htmlOutput("random_image_out2"))),  
  column(2, wellPanel(htmlOutput("random_image_out3"))),
  column(2, wellPanel(htmlOutput("random_image_out4"))),
  column(2, wellPanel(htmlOutput("random_image_out5")))),
  
  br(),
  
  fluidRow(
    column(2, wellPanel(htmlOutput("random_image_out6"))),
    column(2, wellPanel(htmlOutput("random_image_out7"))),
    column(1, wellPanel(strong("Probability:"),textOutput("text"))),
    column(1, wellPanel(strong("Hand:"),textOutput("eval_text"))),
    column(1, actionButton('check', 'Check')),
    column(1, actionButton('fold', 'Fold')),
    column(1, actionButton('raise', 'Raise')),
    column(2, wellPanel(sliderInput("raisepct", "Raise %:",
                min = 0, max = 1,
                value = 0.0, step = 0.1))))
  
)
  

server = function(input, output, session){

  img_names = paste0("img", 1:53, ".jpg")
  
  if(!dir.exists("www")){
    dir.create("www")
  }
  
  for(i in seq_along(img_names)){
    png(file = paste0("www", img_names[i]), bg = colors()[i])
    par(mar = c(0,0,0,0))
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, img_names[i], 
         cex = 1.6, col = "black")
    dev.off()
  }
  

  www_imgs <- list.files(path = "www", pattern = "*.png$")
  deck <- as.data.frame(www_imgs)
  card_file <- read.csv('cards.csv', stringsAsFactors = FALSE)
  prob_matrix <- read.csv('probability_matrix.csv', stringsAsFactors = FALSE)
  
  prob_matrix %>%
    rename(C1 = 'X') %>%
    na.omit() %>%
    pivot_longer(!c(Stage:C1), names_to = "C2", values_to = "prob") %>%
    mutate(C1 = as.numeric(C1), C2 = as.numeric(gsub('X', "", C2))) -> df_prob
  

  deck %>%
    filter(www_imgs == 'zz_red.png') %>%
    as.data.frame() -> back
  
  deck %<>%
    filter(www_imgs != 'zz_red.png') -> deck
  
  cards <-2*2+5
  
  sample_n(deck, cards, replace = FALSE) -> dealt0
  
  
  
################################################################################
#Evaluate Flop
dealt0 %>%
    left_join(card_file, by='www_imgs') %>%
    slice(6:7, 1:5) %>%
    mutate(Card = ifelse(Card == 14, 'A',
                         ifelse(Card == 13, 'K',
                                ifelse(Card == 12, 'Q',
                                       ifelse(Card == 11, 'J', Card))))) %>%
    mutate(Suit = ifelse(Suit == 1, 's',
                         ifelse(Suit == 2, 'c',
                                ifelse(Suit == 3, 'h', 'd')))) %>%
    mutate(CS = paste(Card, Suit, sep="")) %>%
    mutate(preflop = paste(CS[1], CS[2])) %>%
    mutate(flop = paste(CS[1], CS[2], CS[3], CS[4], CS[5])) %>%
    mutate(turn = paste(CS[1], CS[2], CS[3], CS[4], CS[5], CS[6])) %>%
    mutate(river = paste(CS[1], CS[2], CS[3], CS[4], CS[5], CS[6], CS[7])) %>%
    select(ID, preflop, flop, turn, river) %>%
    tail(1) %>%
    pivot_longer(!'ID', names_to = 'stage', values_to = 'hand') -> my_hand

  
  source('Poker.R')
  
  preflop <- parse_card(my_hand$hand[1])  
  flop <- parse_card(my_hand$hand[2]) 
  turn <- parse_card(my_hand$hand[3]) 
  river <- parse_card(my_hand$hand[4]) 
  
  eval_preflop <- as.data.frame(evaluate.hand(preflop)$string)
  eval_flop <- as.data.frame(evaluate.hand(flop)$string)
  eval_turn <- as.data.frame(evaluate.hand(turn)$string)
  eval_river <- as.data.frame(evaluate.hand(river)$string)
  
  
  eval_preflop %<>%
    rename(hand = 1) %>%
    mutate(stage = 'Pre-Flop')
  
  eval_flop %<>%
    rename(hand = 1) %>%
    mutate(stage = 'Flop')

  eval_turn %<>%
    rename(hand = 1) %>%
    mutate(stage = 'Turn')
  
  eval_river %<>%
    rename(hand = 1) %>%
    mutate(stage = 'River')
  
  
  eval_preflop %>%
    bind_rows(eval_flop) %>%
    bind_rows(eval_turn) %>%
    bind_rows(eval_river) %>%
    mutate(hand = as.character(hand)) -> eval_hand
  

################################################################################   
  
  
##Pre-Flop
  random_image0 <- www_imgs[back$www_imgs[1]]
  

  output$random_image_out0 = renderUI({
    tags$img(src=random_image0, width="100%", height="100%")
    
  })
  
  
  players <- reactive({
    as.numeric(input$players)
  })
  

  my_cards <- reactive({
  

    dealt0 %>%
    as.data.frame() %>%
    mutate(id = row_number()) %>%
    mutate(Player = ifelse(id < 6, "dealer", 
                           ifelse(id < 8, "player1", 
                                  ifelse(id < 10, "player2", "player")))) %>%
      left_join(card_file, by='www_imgs') %>%
      filter(id %in% c(6, 7)) %>%
      mutate(Pair = ifelse(head(Card,1)==tail(Card,1), 1, 0)) -> my_cards
    
})
  
 
##CALCULATES PROBABILITY    
  output$text <- renderText({ 
    
    PLAYER_COUNT = as.numeric(players()) ##CHANGE BACK TO players() WHEN COMPLETE
    p_count = PLAYER_COUNT - 1
    
    my_cards() %>%
      mutate(C1 = head(Card,1), C2 = tail(Card,1)) %>%
      tail(1) %>%
      select(C1, C2) %>%
      left_join(df_prob, by=c('C1', 'C2')) -> df


    tail(df$prob,1)/p_count

    })
  
##Evaluate Hand Text  
  output$eval_text <- renderText({ 
   
    eval_hand %>%
      filter(stage == input$stage) -> hand_txt
    
    hand_txt$hand
    

  })
  
  
  
  # random_image for each new shiny-session
  #Dealer
  random_image1 <- www_imgs[dealt0$www_imgs[1]]
  random_image2 <- www_imgs[dealt0$www_imgs[2]]
  random_image3 <- www_imgs[dealt0$www_imgs[3]]
  random_image4 <- www_imgs[dealt0$www_imgs[4]]
  random_image5 <- www_imgs[dealt0$www_imgs[5]]
    
    
  #Player1
  random_image6 <- www_imgs[dealt0$www_imgs[6]]
  random_image7 <- www_imgs[dealt0$www_imgs[7]]
  
  #Player2
  random_image8 <- www_imgs[dealt0$www_imgs[8]]
  random_image9 <- www_imgs[dealt0$www_imgs[9]]

  
  
  output$random_image_out1 = renderUI({
    tags$img(src=ifelse(input$stage == 'Pre-Flop', random_image0, random_image1), width="100%", height="100%")
 
     })
    
    output$random_image_out2 = renderUI({
      tags$img(src=ifelse(input$stage == 'Pre-Flop', random_image0, random_image2), width="100%", height="100%")
      })
      
      output$random_image_out3 = renderUI({
        tags$img(src=ifelse(input$stage == 'Pre-Flop', random_image0, random_image3), width="100%", height="100%")
      })
      
      output$random_image_out4 = renderUI({
        tags$img(src=ifelse(input$stage %in% c('Turn', 'River'), random_image4, random_image0), width="100%", height="100%")
        
      })
      
      output$random_image_out5 = renderUI({
        tags$img(src=ifelse(input$stage == 'River', random_image5, random_image0), width="100%", height="100%")
      })
        
      output$random_image_out6 = renderUI({
          tags$img(src=random_image6, width="100%", height="100%")
          
      })
        
      output$random_image_out7 = renderUI({
          tags$img(src=random_image7, width="100%", height="100%")
          
      })
      
      output$random_image_out8 = renderUI({
        tags$img(src=random_image8, width="100%", height="100%")
        
      })
      
      output$random_image_out9 = renderUI({
        tags$img(src=random_image9, width="100%", height="100%")
        
      })
      
    ##Buttons

      
       
}

shinyApp(ui, server)
