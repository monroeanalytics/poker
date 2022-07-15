## Poker.R
## Evaluate poker hands
##
## by: Christopher Bare
############################################################

## define suits and ranks
suits <- c('c','d','h','s')
ranks <- c(2:10,"J","K","Q","A")
suit_names <- c(c="clubs", d="diamonds", h="hearts", s="spades")
rank_names <- c(2:10, "Jack", "Queen", "King", "Ace")

new.deck <- function() {
  deck <- list()
  i <- 1
  for (r in 2:14) {
    for (s in suits) {
      deck[[i]] <- list(rank=r, suit=s)
      class(deck[[i]]) <- 'card'
      i <- i + 1
    }
  }
  class(deck) <- 'cardList'
  return(deck)
}

deal <- function(deck,n) {
  hand <- sample(deck,n)
  hand <- hand[order(rank(hand),suit(hand), decreasing=TRUE)]
  class(hand) <- "cardList"
  return(hand)
}

is.card <- function(x) {
  return(class(x)=='card')
}

suit <- function(c) {
  if (is.card(c))
    c$suit
  else if (is.list(c))
    sapply(c, suit)
}

rank <- function(c) {
  if (is.card(c))
    c$rank
  else if (is.list(c))
    sapply(c, rank)
}

toString.rank <- function(rank, short=TRUE, plural=FALSE) {
  if (short)
    result <- ranks[rank-1]
  else
    result <- rank_names[rank-1]
  if (plural)
    result <- paste(result,"s",sep="")
  return(result)
}

as.rank <- function(s) {
  sapply(s, function(x) which(ranks==toupper(x))) + 1
}

parse_card <- function(string) {
  card_strings <- strsplit(string,"\\s+")[[1]]
  
  # extract rank and suit from each card into parallel vectors
  card_ranks <- as.rank(sub(
    pattern="(\\d+|[AKQJ])[cdhs]",
    replacement="\\1",
    card_strings, ignore.case=TRUE))
  card_suits <- sub(
    pattern="(\\d+|[AKQJ])([cdhs])",
    replacement="\\2",
    card_strings, ignore.case=TRUE)
  
  # zip together ranks and suits into cards
  cards <- mapply(function(r,s) {
    card <- list(rank=r,suit=s)
    class(card) <- "card"
    card
  }, card_ranks, card_suits, SIMPLIFY=FALSE)
  class(cards) <- "cardList"
  return(cards)
}

toString.card <- function(card) {
  paste(toString.rank(rank(card)), suit(card), sep="")
}

toString.cardList <- function(cards) {
  paste(toString.card(cards), collapse=" ")
}

toString.pokerHandEvaluation <- function(ev) {
  ev$string
}

print.cardList <- function(cards) {
  cat("cards:\n")
  print(toString.cardList(cards))
}

print.card <- function(card) {
  print(toString.card(card))
}

print.pokerHandEvaluation <- function(ev) {
  print(ev$string)
}

evaluate.hand <- function(hand) {
  runs <- table(rank(hand))
  runs <- runs[order(runs, names(runs), decreasing=TRUE)]
  run.ranks <- as.numeric(names(runs))
  
  flush.suit <- unique(suit(hand))
  is.flush <- (length(flush.suit) == 1)
  
  highest.rank <- max(rank(hand))
  lowest.rank <- min(rank(hand))
  is.straight <- all(sort(rank(hand))==seq(lowest.rank, lowest.rank+4, 1))
  
  ev <-list(runs=runs,
            run.ranks=run.ranks,
            flush.suit=flush.suit,
            is.flush=is.flush,
            highest.rank=highest.rank,
            lowest.rank=lowest.rank,
            is.straight=is.straight)
  class(ev) <- "pokerHandEvaluation"
  
  ## straight flush
  if (is.straight && is.flush) {
    ev$type <- "Straight flush"
    if (lowest.rank==10)
      ev$string <- paste("Royal flush in", suit_names[flush.suit])
    else
      ev$string <- paste("Straight flush",
                         toString.rank(highest.rank), "high",
                         "in", suit_names[flush.suit])
  }
  
  ## four of a kind
  else if (length(runs)==2 && all(runs==c(4,1))) {
    ev$type <- "Four of a kind"
    ev$string <- paste("4", toString.rank(run.ranks[1], plural=T))
  }
  
  ## full house
  else if (length(runs)==2 && all(runs==c(3,2))) {
    ev$type <- "Full house"
    ev$string <- paste("Full house",
                       toString.rank(run.ranks[1], plural=T), "and",
                       toString.rank(run.ranks[2], plural=T))
  }
  
  ## flush
  else if (is.flush) {
    ev$type <- "Flush"
    ev$string <- paste("Flush in", suit_names[flush.suit])
  }
  
  ## straight
  else if (is.straight) {
    ev$type <- "Straight"
    ev$string <- paste("Straight", toString.rank(highest.rank), "high")
  }
  
  ## three of a kind
  else if (length(runs)==3 && all(runs==c(3,1,1))) {
    ev$type <- "Three of a kind"
    ev$string <- paste("3", toString.rank(run.ranks[1], plural=T))
  }
  
  ## two pairs
  else if (length(runs)==3 && all(runs==c(2,2,1))) {
    ev$type <- "Two pairs"
    ev$string <- paste("two pairs",
                       toString.rank(run.ranks[1], plural=T), "and",
                       toString.rank(run.ranks[2], plural=T))
  }
  
  ## pair
  else if (length(runs)==4 && all(runs==c(2,1,1,1))) {
    ev$type <- "Pair"
    ev$string <- paste("pair of", toString.rank(run.ranks[1], plural=T))
  }
  
  else {
    ev$type <- "Nothing"
    ev$string <- paste("Nothing:", toString(hand))
  }
  
  return(ev)
}

## deal a bunch of hands and evaluate them
count.hands <- function(n=10) {
  d <- new.deck()
  counts <- c(`Straight flush`=0,
              `Four of a kind`=0,
              `Full house`=0,
              `Flush`=0,
              `Straight`=0,
              `Three of a kind`=0,
              `Two pairs`=0,
              `Pair`=0,
              `Nothing`=0)
  for (i in 1:n) {
    hand <- deal(d,5)
    ev <- evaluate.hand(hand)
    counts[[ev$type]] <- counts[[ev$type]] + 1
    #print(paste(toString(hand), " - ", ev$string))
  }
  return(counts)
}