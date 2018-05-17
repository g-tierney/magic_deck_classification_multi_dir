# magic_deck_classification_multi_dir

This repository contains the code described in my blog post here: https://g-tierney.github.io/post/magic_classification/. I apply a genetic classification algorithm to a collectible card game. Decks are classified into broader archetypes by treating card frequencies like allele frequencies in the genetics problem. 

The file magic_classificatino.Rmd is a copy of the blog post (with slight changes to the file paths) that gives an example of how to use the functions. functions.R contains all relevant functions. The code to scrape the deck data was written in Python and is not included. 

Ultimately, with very minimal data cleaning, I am able to accurately classify 86% of decks played in a testing sample. Frequently played archetypes and archetypes that share few cards with others are more accurately classified. I also describe some simple human-implementable improvements and additional features that should be included in practice. 