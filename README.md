

# Crypto SentiTomo
In the cryptocurrency market, many studies have tried to perform sentiment analysis on Twitter data in order to detect short and long term trends. However, not many studies focused on different cryptocurrencies beside the renown Bitcoin. This study proposes and evaluates different machine learning based methods which look into the public perceptions toward cryptocurrency, utilizes Twitter posts and market price data in order to detect sentiments and discover trending topics. Finally, designs an experiment machine learning pipeline to explore correlations between those factors and predict Bitcoin and non-Bitcoin cryptocurrencies prices movement against fiat currency USD and against Bitcoin price in a defined time-frame.

![Workflow](https://github.com/o0oBluePhoenixo0o/Crypto-Senti/blob/master/img/Workflow_Mar18.JPG)

# Progress
## Oct - Nov 17 
- Develop crawler for Twitter using REST API. Schedule weekly update since historical data is limited to ~ 10 days for free API
- Crawler for Reddit base on Pushshift.io API, scripts collects all submissions data looping over rate limit.
- Finalize preprocessing pipeline

## Dec 17 - Feb 18
- Implement baseline models for sentiment analysis and topic modeling
- Framework for Ensemble model and hyper parameter testing
- Granger causality test for time series analysis with market data

## Mar 18
- Restructure workflow pipeline
- Evaluation between pre-defined topics (8) and topic modeling generated topics (correlations comparisons)

## Apr 18
- Complete framework for evaluation models. Contain 9 types of models:

![Experiment Design](https://github.com/o0oBluePhoenixo0o/NextBigCrypto-Senti/blob/master/img/Experiment%20Design.JPG)

- Complete "manual dataset" construction (~3200 observations labeled)
- Finalize sentiment analysis on manual label dataset with evaluation (trained models vs packages)
- Historical Price (HP) model version 1 complete as baseline (acc 46% with Recursive Feature Eliminations)

## May 18
- Revisit pre-processing pipeline:

![Preprocessing Pipeline](https://github.com/o0oBluePhoenixo0o/NextBigCrypto-Senti/blob/master/img/Preprocessingv3.JPG)

 - - Implement "Twitter bot detection" module (utilize package "botrnot")
 - - Custom abbreviations dictionary for translating popular acronyms / slangs on social media
 - - Enhance stop-words dictionary (exclude price movements related words / tokens affiliated removal)
 - - Lemmatization before tokenization

## June - July 18

- Completed training all 17 models and tested successfully on Bitcoin, Ethereum and Litecoin priceline.
- Obtained best performer in each token cases (BTC with HP_LDA, ETH with SAP_LDA and LTC with HP_SAT_PD)
- Published packages and finalized paper for publication
