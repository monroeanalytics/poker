#!/usr/bin/littler
# encoding{ utf-8

#SevenEval.R

#Created by Kenneth J. Shackleton on 15 June 2011.
#Copyright (c) 2011 Ringo Limited.
#All rights reserved.
#R Port by Zach Mayer on 4 December 2011

require(bitops)

SevenEval <- list()
SevenEval$rankArray = rep(0,CIRCUMFERENCE_SEVEN)
SevenEval$flushRankArray = rep(0,MAX_SEVEN_FLUSH_KEY_INT + 1)
SevenEval$deckcardsKey = rep(0,DECK_SIZE)
SevenEval$deckcardsFlush = rep(0,DECK_SIZE)
SevenEval$deckcardsSuit = rep(0,DECK_SIZE)
SevenEval$flushCheck = rep(0,MAX_FLUSH_CHECK_SUM + 1)

SevenEval$face = c(ACE, KING, QUEEN, JACK, TEN,
                   NINE, EIGHT, SEVEN, SIX, FIVE,
                   FOUR, THREE, TWO)

SevenEval$faceFlush = c(ACE_FLUSH, KING_FLUSH, QUEEN_FLUSH,
                        JACK_FLUSH, TEN_FLUSH, NINE_FLUSH,
                        EIGHT_FLUSH, SEVEN_FLUSH, SIX_FLUSH,
                        FIVE_FLUSH, FOUR_FLUSH, THREE_FLUSH,
                        TWO_FLUSH)

for (n in seq(0, NUMBER_OF_FACES-1)) {
  SevenEval$deckcardsKey[4*n + 1] = bitShiftL(SevenEval$face[n + 1], NON_FLUSH_BIT_SHIFT) + SPADE
  SevenEval$deckcardsKey[4*n + 2] = bitShiftL(SevenEval$face[n + 1], NON_FLUSH_BIT_SHIFT) + HEART
  SevenEval$deckcardsKey[4*n + 3]	= bitShiftL(SevenEval$face[n + 1], NON_FLUSH_BIT_SHIFT) + DIAMOND
  SevenEval$deckcardsKey[4*n + 4]	= bitShiftL(SevenEval$face[n + 1], NON_FLUSH_BIT_SHIFT) + CLUB
  
  SevenEval$deckcardsFlush[4*n + 1] = SevenEval$faceFlush[n + 1]
  SevenEval$deckcardsFlush[4*n + 2] = SevenEval$faceFlush[n + 1]
  SevenEval$deckcardsFlush[4*n + 3] = SevenEval$faceFlush[n + 1]
  SevenEval$deckcardsFlush[4*n + 4] = SevenEval$faceFlush[n + 1]
  
  SevenEval$deckcardsSuit[4*n + 1]	= SPADE
  SevenEval$deckcardsSuit[4*n + 2]	= HEART
  SevenEval$deckcardsSuit[4*n + 3]	= DIAMOND
  SevenEval$deckcardsSuit[4*n + 4]	= CLUB
}

# Track increments.
count = 0

# High card.
for (i in seq(1, NUMBER_OF_FACES-1)) {
  for (j in seq(1, i+1-1)) {
    for (k in seq(1, j+1-1)) {
      for (l in seq(0, k+1-1)) {
        for (m in seq(0, l+1-1)) {
          for (n in seq(0, m+1-1)) {
            for (p in seq(0, n+1-1)) {
              if (i != m & j != n & k != p) {
                count <- count+1
                key = SevenEval$face[i + 1] + SevenEval$face[j + 1] + SevenEval$face[k + 1] + SevenEval$face[l + 1] + SevenEval$face[m + 1] + SevenEval$face[n + 1] + SevenEval$face[p + 1]
                # The 4*i+0 and 4*m+1 trick prevents flushes.
                rank = FiveEval$getRankOfSeven(4*i, 4*j, 4*k, 4*l, 4*m+1, 4*n+1, 4*p+1)
                SevenEval$rankArray[(key %% CIRCUMFERENCE_SEVEN) + 1] = rank
              }
            }
          }
        }
      }
    }
  }
}

# Flush ranks.
# All 7 same suit.
for (i in seq(6, NUMBER_OF_FACES-1)) {
  for (j in seq(5, i-1)) {
    for (k in seq(4, j-1)) {
      for (l in seq(3, k-1)) {
        for (m in seq(2, l-1)) {
          for (n in seq(1, m-1)) {
            for (p in seq(0, n-1)) {
              count <- count + 1
              key = SevenEval$faceFlush[i + 1] + SevenEval$faceFlush[j + 1] + SevenEval$faceFlush[k + 1] + SevenEval$faceFlush[l + 1] + SevenEval$faceFlush[m + 1] + SevenEval$faceFlush[n + 1] + SevenEval$faceFlush[p + 1]
              rank = FiveEval$getRankOfSeven(4*i, 4*j, 4*k, 4*l, 4*m, 4*n, 4*p)
              SevenEval$flushRankArray[key + 1] = rank
            }
          }
        }
      }
    }
  }
}

# Only 6 same suit.
for (i in seq(5, NUMBER_OF_FACES-1)) {
  for (j in seq(4, i-1)) {
    for (k in seq(3, j-1)) {
      for (l in seq(2, k-1)) {
        for (m in seq(1, l-1)) {
          for (n in seq(0, m-1)) {
            count <- count + 1
            key = SevenEval$faceFlush[i + 1] + SevenEval$faceFlush[j + 1] + SevenEval$faceFlush[k + 1] + SevenEval$faceFlush[l + 1] + SevenEval$faceFlush[m + 1] + SevenEval$faceFlush[n + 1]
            
            # The Two of clubs is the card at index 51; the other six
            # cards all have the spade suit.
            rank = FiveEval$getRankOfSeven(4*i, 4*j, 4*k, 4*l, 4*m, 4*n, 51)
            SevenEval$flushRankArray[key + 1] = rank
          }
        }
      }
    }
  }
}

# Only 5 same suit.
for (i in seq(4, NUMBER_OF_FACES-1)) {
  for (j in seq(3, i-1)) {
    for (k in seq(2, j-1)) {
      for (l in seq(1, k-1)) {
        for (m in seq(0, l-1)) {
          count <- count + 1
          key = SevenEval$faceFlush[i + 1] + SevenEval$faceFlush[j + 1] + SevenEval$faceFlush[k + 1] + SevenEval$faceFlush[l + 1] + SevenEval$faceFlush[m + 1]
          rank = FiveEval$getRankOfFive(4*i, 4*j, 4*k, 4*l, 4*m);
          SevenEval$flushRankArray[key + 1] = rank
        }
      }
    }
  }
}

# Initialise flush checks.
SUIT_COUNT = 0
FLUSH_SUIT_INDEX = -1
card_S_MATCHED_SO_FAR = 0
SUIT_KEY = SPADE
suits = c(SPADE, HEART, DIAMOND, CLUB)

# Initialise all entries of flushCheck[] to UNVERIFIED, as yet unchecked.	
SevenEval$flushCheck = rep(UNVERIFIED,MAX_FLUSH_CHECK_SUM + 1)

# 7-card flush.
for (card_1 in seq(0, NUMBER_OF_SUITS-1)) {
  for (card_2 in seq(0, card_1 + 1-1)) {
    for (card_3 in seq(0, card_2 + 1-1)) {
      for (card_4 in seq(0, card_3 + 1-1)) {
        for (card_5 in seq(0, card_4 + 1-1)) {
          for (card_6 in seq(0, card_5 + 1-1)) {
            for (card_7 in seq(0, card_6 + 1-1)) {
              SUIT_COUNT = 0
              FLUSH_SUIT_INDEX = -1
              CARDS_MATCHED_SO_FAR = 0
              SUIT_KEY = suits[card_1 + 1] + suits[card_2 + 1] + suits[card_3 + 1] + 
                suits[card_4 + 1] + suits[card_5 + 1] + suits[card_6 + 1] + 
                suits[card_7 + 1]
              
              if (SevenEval$flushCheck[SUIT_KEY + 1] == UNVERIFIED) {
                while (CARDS_MATCHED_SO_FAR < 3 & FLUSH_SUIT_INDEX < 4) {
                  FLUSH_SUIT_INDEX = FLUSH_SUIT_INDEX+1
                  SUIT_COUNT = (suits[card_1 + 1] == suits[FLUSH_SUIT_INDEX + 1]) +
                    (suits[card_2 + 1] == suits[FLUSH_SUIT_INDEX + 1]) +
                    (suits[card_3 + 1] == suits[FLUSH_SUIT_INDEX + 1]) +
                    (suits[card_4 + 1] == suits[FLUSH_SUIT_INDEX + 1]) +
                    (suits[card_5 + 1] == suits[FLUSH_SUIT_INDEX + 1]) +
                    (suits[card_6 + 1] == suits[FLUSH_SUIT_INDEX + 1]) +
                    (suits[card_7 + 1] == suits[FLUSH_SUIT_INDEX + 1])
                  CARDS_MATCHED_SO_FAR = CARDS_MATCHED_SO_FAR+SUIT_COUNT
                }
              }
              # A count of 5 or more means we have a flush. We place
              # the value of the flush suit here.
              if (SUIT_COUNT > 4) {
                SevenEval$flushCheck[SUIT_KEY + 1] = suits[FLUSH_SUIT_INDEX + 1]
              } else { # Otherwise this is a non-flush hand.
                SevenEval$flushCheck[SUIT_KEY + 1] = NOT_A_FLUSH
              }
            }
          }
        }
      }
    }
  }
}

SevenEval$getRankOfSeven <- function(card_1, card_2, card_3, card_4, card_5, card_6, card_7) {
  # Create a 7-card hand key by adding up each of the card keys.
  KEY = SevenEval$deckcardsKey[card_1 + 1] + 
    SevenEval$deckcardsKey[card_2 + 1] + 
    SevenEval$deckcardsKey[card_3 + 1] + 
    SevenEval$deckcardsKey[card_4 + 1] + 
    SevenEval$deckcardsKey[card_5 + 1] + 
    SevenEval$deckcardsKey[card_6 + 1] + 
    SevenEval$deckcardsKey[card_7 + 1]
  
  # Tear off the flush check strip.
  FLUSH_CHECK_KEY = bitAnd(KEY, SUIT_BIT_MASK )
  FLUSH_SUIT = SevenEval$flushCheck[FLUSH_CHECK_KEY + 1]
  
  if (FLUSH_SUIT == NOT_A_FLUSH) {
    # Tear off the non-flush key strip, and look up the rank.
    KEY = bitShiftR(KEY, NON_FLUSH_BIT_SHIFT)
    
    # Take key modulo the circumference. A dichotomy is faster than using
    # the usual modulus operation. This is fine for us because the circumference
    # is more than half the largest SevenEval$face key we come across.
    rank = ifelse(KEY < CIRCUMFERENCE_SEVEN, SevenEval$rankArray[KEY + 1], SevenEval$rankArray[KEY - CIRCUMFERENCE_SEVEN + 1])
    
    return(rank)
  } else {
    # print "flush"
    # Generate a flush key, and look up the rank.
    FLUSH_KEY = ifelse(SevenEval$deckcardsSuit[card_1 + 1]  == FLUSH_SUIT, SevenEval$deckcardsFlush[card_1 + 1], 0) +
      ifelse(SevenEval$deckcardsSuit[card_2 + 1]  == FLUSH_SUIT, SevenEval$deckcardsFlush[card_2 + 1], 0) +
      ifelse(SevenEval$deckcardsSuit[card_3 + 1]  == FLUSH_SUIT, SevenEval$deckcardsFlush[card_3 + 1], 0) +
      ifelse(SevenEval$deckcardsSuit[card_4 + 1]  == FLUSH_SUIT, SevenEval$deckcardsFlush[card_4 + 1], 0) +
      ifelse(SevenEval$deckcardsSuit[card_5 + 1]  == FLUSH_SUIT, SevenEval$deckcardsFlush[card_5 + 1], 0) +
      ifelse(SevenEval$deckcardsSuit[card_6 + 1]  == FLUSH_SUIT, SevenEval$deckcardsFlush[card_6 + 1], 0) +
      ifelse(SevenEval$deckcardsSuit[card_7 + 1]  == FLUSH_SUIT, SevenEval$deckcardsFlush[card_7 + 1], 0)
    
    #print(FLUSH_KEY)
    return(SevenEval$flushRankArray[FLUSH_KEY + 1])
  }
  
  return(-1)
}

library(compiler)
SevenEval$getRankOfSeven <- cmpfun(SevenEval$getRankOfSeven)

#RCPP version
SevenEval$getRankOfSeven <- function(card_1, card_2, card_3, card_4, card_5, card_6, card_7) {
  # Create a 7-card hand key by adding up each of the card keys.
  KEY = SevenEval$deckcardsKey[card_1 + 1] + 
    SevenEval$deckcardsKey[card_2 + 1] + 
    SevenEval$deckcardsKey[card_3 + 1] + 
    SevenEval$deckcardsKey[card_4 + 1] + 
    SevenEval$deckcardsKey[card_5 + 1] + 
    SevenEval$deckcardsKey[card_6 + 1] + 
    SevenEval$deckcardsKey[card_7 + 1]
  
  # Tear off the flush check strip.
  FLUSH_CHECK_KEY = bitAnd(KEY, SUIT_BIT_MASK )
  FLUSH_SUIT = SevenEval$flushCheck[FLUSH_CHECK_KEY + 1]
  
  if (FLUSH_SUIT == NOT_A_FLUSH) {
    # Tear off the non-flush key strip, and look up the rank.
    KEY = bitShiftR(KEY, NON_FLUSH_BIT_SHIFT)
    
    # Take key modulo the circumference. A dichotomy is faster than using
    # the usual modulus operation. This is fine for us because the circumference
    # is more than half the largest SevenEval$face key we come across.
    rank = ifelse(KEY < CIRCUMFERENCE_SEVEN, SevenEval$rankArray[KEY + 1], SevenEval$rankArray[KEY - CIRCUMFERENCE_SEVEN + 1])
    
    return(rank)
  } else {
    # print "flush"
    # Generate a flush key, and look up the rank.
    FLUSH_KEY = ifelse(SevenEval$deckcardsSuit[card_1 + 1]  == FLUSH_SUIT, SevenEval$deckcardsFlush[card_1 + 1], 0) +
      ifelse(SevenEval$deckcardsSuit[card_2 + 1]  == FLUSH_SUIT, SevenEval$deckcardsFlush[card_2 + 1], 0) +
      ifelse(SevenEval$deckcardsSuit[card_3 + 1]  == FLUSH_SUIT, SevenEval$deckcardsFlush[card_3 + 1], 0) +
      ifelse(SevenEval$deckcardsSuit[card_4 + 1]  == FLUSH_SUIT, SevenEval$deckcardsFlush[card_4 + 1], 0) +
      ifelse(SevenEval$deckcardsSuit[card_5 + 1]  == FLUSH_SUIT, SevenEval$deckcardsFlush[card_5 + 1], 0) +
      ifelse(SevenEval$deckcardsSuit[card_6 + 1]  == FLUSH_SUIT, SevenEval$deckcardsFlush[card_6 + 1], 0) +
      ifelse(SevenEval$deckcardsSuit[card_7 + 1]  == FLUSH_SUIT, SevenEval$deckcardsFlush[card_7 + 1], 0)
    
    #print(FLUSH_KEY)
    return(SevenEval$flushRankArray[FLUSH_KEY + 1])
  }
  
  return(-1)
}