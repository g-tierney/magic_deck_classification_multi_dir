################################################################################
# Author(s):    Graham Tierney
# Written:      5/16/2018
#
# Description: Functions to classify CCG decks using a Multinomial-Dirichlet 
#   distribution. 
#
################################################################################

##########################
### Build Traning Data ###
##########################

#' For a given deck name, output the frequency of each card in that archetype. 
#' Note that for cards never seen in the archetype, a pseudo count is added. The
#' sum of the counts for all unobserved cards is 1. 
#' 
#' @param data is a long dataset of deck-card observations
#' @param deck_name is a string containing a deck name 
#' @param alpha is the parameter for a Dirichlet prior on the frequencies. 
#' Set to NA to use 1/(number of unique cards). 
#' 
#' @return is a dataset of card names and the number of times they appeared in deck_name in the data
compute_counts <- function(data,deck_name,alpha = 0){
  #alpha is the parameter for the Dirichlet prior
  if(is.na(alpha)){
    alpha <- 1/length(card_list)
  }
  
  #subset to only the relevant deck
  deck_data <- data[Deck == deck_name,.(count = sum(number)+alpha),by=card]

  #need to add counts for cards never seen in training data
  card_list <- levels(data$card)
  unobserved_cards <- card_list[!(card_list %in% deck_data$card)]
  num_unobserved <- length(unobserved_cards)
  
  rbindlist(list(deck_data, #observed cards
                 list(card = unobserved_cards,count = rep(alpha,num_unobserved))))
}

#' Turn a long dataset into a list of datasets where each element is
#' named for a deck archetype and the dataset is the total card counts 
#' for the deck. 
#' 
#' @param data is a dataset unique on the deck-card level
#' @param alpha is an optional parameter for a Dirichlet 
#' prior on the frequencies. It is added to each card count. 
#' 
#' @return a list of deck archetypes and the number of times 
#' each card was observed in the archetype plus alpha. 
make_training_list <- function(data,alpha=0){
  decks <- levels(data$Deck)
  cards <- levels(data$card)
  
  train <- lapply(X = decks,FUN = function(x){
    counts <-  compute_counts(data,x,alpha = alpha)
    counts[,prop := normalize(count)]
  })
  names(train) <- decks
  train
}

#used to turn counts into frequencies
normalize <- function(x){x/sum(x)}

#######################
### Classify sample ###
#######################

#' Return the log likelihood of observing new_deck if the 
#' deck type was deck_name from a Multinomial distribution. 
#' This function is used when no prior is specified. 
log_likelihood <- function(deck_name,new_deck,train){
  #merge deck_name's frequencies onto new_deck
  deck_train_freqs <- train[[deck_name]]
  if(!is.data.table(new_deck)){
    new_deck <- data.table(new_deck)
  }
  new_deck[deck_train_freqs,on="card",prop := i.prop]
  
  #return log probabilities to prevent R from rounding to zero
  log_total_prob <- sum(new_deck$number*log(new_deck$prop),na.rm=T)
  log_total_prob
}

#' Return the log likelihood of observing new_deck if the 
#' deck type was deck_name from a Multinomial-Dirichlet 
#' distribution. 
log_multidir_like <- function(deck_name,new_deck,train,method){
  deck_train_freqs <- train[[deck_name]]
  if(!is.data.table(new_deck)){
    new_deck <- data.table(new_deck)
  }
  
  new_deck[deck_train_freqs,on="card",c("prop","count") := .(i.prop,i.count)]
  
  if(method == "MnDr"){
    lgamma(sum(deck_train_freqs$count))-lgamma(60+sum(deck_train_freqs$count))+
      sum(lgamma(new_deck$number + new_deck$count)-lgamma(new_deck$number+1)-lgamma(new_deck$count))
  } else if(method == "MLE"){
    sum(new_deck$number*log(new_deck$prop),na.rm=T)
  }
}

#turn a vector of log probabilities into standard probabilities
logs_to_probs <- function(x){
  max_log <- max(x)
  ps <- exp(x-max_log)/sum(exp(x-max_log))
}

#' This is the workhorse function. It will take a sample of new decks, a set of
#' training decks, and an alpha prior and output the classification of each deck. 
#' 
#' @param sample a nested dataframe of decks to classify
#' @param data the training decks
#' @param alpha the alpha for a Dirichlet prior (zero returns a MLE with no prior)
#' @param deck_names a vector of deck names
#' @param file a filepath to output classified decks to
#' 
#' @return the sample parameter with the probability of each classification, and 
#' other statistics computed off those probabilities
classify_sample <- function(sample,data,alpha,deck_names=decks,file = ""){
  train <- make_training_list(data,alpha)
  if(alpha!=0){
    method = "MnDr"
  } else if(alpha == 0){
    method = "MLE"
  }
  result <- sample %>% mutate(probs = purrr::map(data,~sapply(deck_names,log_multidir_like,new_deck = .,train = train,
                                                              method = method)))
  result <- result %>% mutate(probs = purrr::map(probs,~logs_to_probs(.))) %>% 
    rowwise %>% mutate(best_guess = names(probs)[which.max(unlist(probs))],
                       max_prob = max(probs),
                       correct = best_guess == Deck)
  
  if(file != ""){
    write_csv(result %>% dplyr::select(-data,-probs),path = file)
  }
  return(result)
}

#######################
### Analyze Results ###
#######################

#' Print accuracy results to the screen and return a one-row dataframe of the 
#' printed results. 
#' 
#' @param classification_results an object output from classify_sample, must contain
#' true classifications, suggested classification, and probability of classification
#' @param very_conf_cutoff probability cutoff for "very confident" results
#' @param print option to turn off printing of results
#' 
#' @return one-row dataframe of printed results
print_accuracy <- function(classification_results,very_conf_cutoff,print=T){
  n_correct <- sum(classification_results$correct)
  n <- nrow(classification_results)
  prop <- n_correct/n
  
  vconf <- classification_results$max_prob > very_conf_cutoff
  n_correct_vconf <- sum(classification_results$correct[vconf])
  n_vconf <- nrow(classification_results[vconf,])
  prop_vconf <- n_correct_vconf/n_vconf
  
  if(print){
    print(str_c(n_correct," correct out of ",n," (",round(prop,2),")"))
    print(str_c("For very confident classifications (archetype probability > ",very_conf_cutoff,"):"))
    #print(str_c(n_correct_vconf)," correct out of ",n_vconf," (",round(prop_vconf,2),")")
  }
  
  return(data.frame(Correct=n_correct,Size=n,Rate=prop,
                    correct_confident=n_correct_vconf,size_confident=n_vconf,rate_confident=prop_vconf))
}

#' Aggregate classification results by deck archetypes. Return accuracy rate,
#' most common incorrect classification, and mean, min, and max of classification 
#' probabilities
#' 
#' @param classification_results a classified list of decks output from classify_sample
#' @param file the filepath to write results to
#' 
#' @return dataframe of each deck archetype in classification_results and summary
#' statistics about the classifications of decks from that archetype. 
accuracy_by_deck <- function(classificaiton_results,file = ""){
  results_by_deck <- classificaiton_results %>% group_by(Deck) %>% summarise(total = n(),
                                                                             prop_correct = sum(correct)/total,
                                                                             mode_incorrect = get_mode(best_guess[correct == F]),
                                                                             max_prob_avg = mean(max_prob),
                                                                             max_prob_min = min(max_prob),
                                                                             max_prob_max = max(max_prob))
  #write output
  if(file != ""){
    write_csv(results_by_deck,path = file)
  }
  return(results_by_deck)
}

#return the mode of a vector
get_mode <- function(x) {
  ux <- unique(c(x,NA))
  ux[which.max(tabulate(match(x, ux)))]
}