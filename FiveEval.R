#!/usr/bin/littler
# encoding: utf-8

#FiveEval.R

#Created by Kenneth J. Shackleton on 14 June 2011.
#Copyright (c) 2011 Ringo Limited.
#All rights reserved.
#R Port by Zach Mayer on 4 December 2011

FiveEval <- list(
  rankArray = rep(0,MAX_FIVE_NONFLUSH_KEY_INT + 1),
  flushRankArray = rep(0,MAX_FIVE_FLUSH_KEY_INT + 1),
  deckcardsFace = rep(0,DECK_SIZE),
  deckcardsFlush = rep(0,DECK_SIZE),
  deckcardsSuit = rep(0,DECK_SIZE),
  face = c(TWO_FIVE, THREE_FIVE, FOUR_FIVE,
           FIVE_FIVE, SIX_FIVE, SEVEN_FIVE,
           EIGHT_FIVE, NINE_FIVE, TEN_FIVE,
           JACK_FIVE, QUEEN_FIVE, KING_FIVE,
           ACE_FIVE),
  faceFlush = c(TWO_FLUSH, THREE_FLUSH,
                FOUR_FLUSH, FIVE_FLUSH,
                SIX_FLUSH, SEVEN_FLUSH,
                EIGHT_FLUSH, NINE_FLUSH,
                TEN_FLUSH, JACK_FLUSH,
                QUEEN_FLUSH, KING_FLUSH,
                ACE_FLUSH)
)

for (n in seq(0,NUMBER_OF_FACES-1)) {	
  FiveEval$deckcardsSuit[4*n + 1] = SPADE
  FiveEval$deckcardsSuit[4*n + 2] = HEART
  FiveEval$deckcardsSuit[4*n + 3] = DIAMOND
  FiveEval$deckcardsSuit[4*n + 4] = CLUB
  
  FiveEval$deckcardsFace[4*n + 1] = FiveEval$face[12 - n + 1]
  FiveEval$deckcardsFace[4*n + 2] = FiveEval$face[12 - n + 1]
  FiveEval$deckcardsFace[4*n + 3] = FiveEval$face[12 - n + 1]
  FiveEval$deckcardsFace[4*n + 4] = FiveEval$face[12 - n + 1]
  
  FiveEval$deckcardsFlush[4*n + 1] = FiveEval$faceFlush[12 - n + 1]
  FiveEval$deckcardsFlush[4*n + 2] = FiveEval$faceFlush[12 - n + 1]
  FiveEval$deckcardsFlush[4*n + 3] = FiveEval$faceFlush[12 - n + 1]
  FiveEval$deckcardsFlush[4*n + 4] = FiveEval$faceFlush[12 - n + 1]
}


# n increments as rank.
n = 0

# High card.
for (i in seq(5, NUMBER_OF_FACES-1)) {
  for (j in seq(3, i-1)) {
    for (k in seq(2, j-1)) {
      for (l in seq(1, k-1)) {
        # No straights
        for (m in seq(0, l-1)) {
          if (! (i - m == 4 | (i == 12 & j == 3 & k == 2 & l == 1 & m == 0))) {
            n <- n+1
            FiveEval$rankArray[FiveEval$face[i + 1] + FiveEval$face[j + 1] + FiveEval$face[k + 1] + FiveEval$face[l + 1] + FiveEval$face[m + 1] + 1] = n
          }
        }
      }
    }
  }
}


# Pair.
for (i in seq(0, NUMBER_OF_FACES-1)) {
  for (j in seq(2, NUMBER_OF_FACES-1)) {
    for (k in seq(1, j-1)) {
      for (l in seq(0, k-1)) {
        if ((i != j & i != k & i != l)) {
          n <- n+1
          FiveEval$rankArray[(2*FiveEval$face[i + 1]) + FiveEval$face[j + 1] + FiveEval$face[k + 1] + FiveEval$face[l + 1] + 1] = n
        }
      }
    }
  }
}

# Two pair.
for (i in seq(1, NUMBER_OF_FACES-1)) {
  for (j in seq(0, i-1)) {
    for (k in seq(0, NUMBER_OF_FACES-1)) {
      # No fullhouse
      if (k != i & k != j) {
        n <- n+1
        FiveEval$rankArray[(2*FiveEval$face[i + 1]) + (2*FiveEval$face[j + 1]) + FiveEval$face[k + 1] + 1] = n
      }
    }
  }
}

# Triple.
for (i in seq(0, NUMBER_OF_FACES-1)) {
  for (j in seq(1, NUMBER_OF_FACES-1)) {
    for (k in seq(0, j-1)) {
      # No quad
      if (i != j & i != k) {
        n <- n+1
        FiveEval$rankArray[(3*FiveEval$face[i + 1]) + FiveEval$face[j + 1] + FiveEval$face[k + 1] + 1] = n
      }
    }
  }
}

# Low straight non-flush.
n <- n+1
FiveEval$rankArray[FiveEval$face[12 + 1] + FiveEval$face[0 + 1] + FiveEval$face[1 + 1] + FiveEval$face[2 + 1] + FiveEval$face[3 + 1] + 1] = n

# Usual straight non-flush.
for (i in seq(0, 9-1)) {
  n <- n+1
  FiveEval$rankArray[FiveEval$face[i + 1] + FiveEval$face[i+2] + FiveEval$face[i+3] + FiveEval$face[i+4] + FiveEval$face[i+5] + 1] = n
}

# Flush not a straight.
for (i in seq(5, NUMBER_OF_FACES-1)) {
  for (j in seq(3, i-1)) {
    for (k in seq(2, j-1)) {
      for (l in seq(1, k-1)) {
        for (m in seq(0, l-1)) {
          if (! (i - m == 4 | (i == 12 & j == 3 & k == 2 & l == 1 & m == 0))) {
            n <- n+1
            FiveEval$flushRankArray[FiveEval$faceFlush[i + 1] + FiveEval$faceFlush[j + 1] + FiveEval$faceFlush[k + 1] + FiveEval$faceFlush[l + 1] + FiveEval$faceFlush[m + 1] + 1] = n		
          }
        }
      }
    }
  }
}

# Full house.
for (i in seq(0, NUMBER_OF_FACES-1)) {
  for (j in seq(0, NUMBER_OF_FACES-1)) {
    if (i != j) {
      n <- n+1
      FiveEval$rankArray[(3*FiveEval$face[i + 1]) + (2*FiveEval$face[j + 1]) + 1] = n
    }
  }
}

# Quad.
for (i in seq(0, NUMBER_OF_FACES-1)) {
  for (j in seq(0, NUMBER_OF_FACES-1)) {
    if (i != j) {
      n <- n+1
      FiveEval$rankArray[(4*FiveEval$face[i + 1]) + FiveEval$face[j + 1] + 1] = n
    }
  }
}

# Low straight flush.
n <- n+1
FiveEval$flushRankArray[FiveEval$faceFlush[0 + 1] + FiveEval$faceFlush[1 + 1] + FiveEval$faceFlush[2 + 1] + FiveEval$faceFlush[3 + 1] + FiveEval$faceFlush[12 + 1] + 1] = n;

# Usual straight flush.
for (i in seq(0, 9-1)) {
  n <- n+1
  FiveEval$flushRankArray[FiveEval$faceFlush[i+1] + FiveEval$faceFlush[i+2] + FiveEval$faceFlush[i+3] + FiveEval$faceFlush[i+4] + FiveEval$faceFlush[i+5] + 1] = n
}

FiveEval$getRankOfFive <- function(card_1, card_2, card_3, card_4, card_5) {
  if (FiveEval$deckcardsSuit[card_1+1] == FiveEval$deckcardsSuit[card_2+1] &
      FiveEval$deckcardsSuit[card_1+1] == FiveEval$deckcardsSuit[card_3+1] &
      FiveEval$deckcardsSuit[card_1+1] == FiveEval$deckcardsSuit[card_4+1] &
      FiveEval$deckcardsSuit[card_1+1] == FiveEval$deckcardsSuit[card_5+1]) {
    
    return(FiveEval$flushRankArray[FiveEval$deckcardsFlush[card_1+1] +
                                     FiveEval$deckcardsFlush[card_2+1] +
                                     FiveEval$deckcardsFlush[card_3+1] +
                                     FiveEval$deckcardsFlush[card_4+1] +
                                     FiveEval$deckcardsFlush[card_5+1]+1])
  } else {
    
    return(FiveEval$rankArray[FiveEval$deckcardsFace[card_1+1] +
                                FiveEval$deckcardsFace[card_2+1] +
                                FiveEval$deckcardsFace[card_3+1] +
                                FiveEval$deckcardsFace[card_4+1] +
                                FiveEval$deckcardsFace[card_5+1]+1])
  }
  return(-1)
}

FiveEval$getRankOfSeven <- function(CARD1, CARD2, CARD3, CARD4, CARD5, CARD6, CARD7) {
  seven_cards = c(CARD1, CARD2, CARD3, CARD4, CARD5, CARD6, CARD7)
  five_temp = rep(0,5)
  BEST_RANK_SO_FAR = 0
  CURRENT_RANK = 0
  m = 0
  
  for (i in seq(1, 7-1)) {
    for (j in seq(0, i-1)) {
      m = 0
      for (k in seq(0, 7-1)) {
        if (k != i & k != j) {
          five_temp[m+1] = seven_cards[k+1]
          m <- m+1
        }
      }
      
      CURRENT_RANK = FiveEval$getRankOfFive(five_temp[0+1], five_temp[1+1], five_temp[2+1], five_temp[3+1], five_temp[4+1])
      if (BEST_RANK_SO_FAR < CURRENT_RANK) {
        BEST_RANK_SO_FAR = CURRENT_RANK
      }
    }
  }
  
  return(BEST_RANK_SO_FAR)
}

library(compiler)
FiveEval$getRankOfFive <- cmpfun(FiveEval$getRankOfFive)
FiveEval$getRankOfSeven <- cmpfun(FiveEval$getRankOfSeven)