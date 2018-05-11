# Crypto SentiTomo
Previous researches have shown that securities and other financial instruments market movements can be predicted using real-time Twitter data. Not only that, multiple studies have tried to perform sentiment analysis on Twitter data in order to detect short and long term trends in the cryptocurrency market. Vast amount of information can be found in the form of unstructured text written by internet users in the cryptocurrency network. Crowd wisdom in those networks can be analyzed and proven to be a reliable indicator of major events affecting the markets.  However, not many have focused on neither other social media platforms nor different cryptocurrencies beside renown ones such as Bitcoin and Ethereum. This study proposes a machine learning based method which look into the public perceptions about cryptocurrency, utilizes not only Twitter but also data from other social media platforms such as Reddit in order to detect sentiments, topics and trends so that correlation between those factors and prices fluctuations of alternative coins can be analyzed.

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

-- Implement "Twitter bot detection" module (utilize package "botrnot")
-- Custom abbreviations dictionary for translating popular acronyms / slangs on social media
-- Enhance stop-words dictionary (exclude price movements related words / tokens affiliated removal)
-- Lemmatization before tokenization

- **HP** model with accuracy ~ 55%
- **SAT** model with accuracy ~ 70% (improvement from sentiment features from the past 14 days)
- **SAP** model with accuracy ~ 58% (not much compare to SAT)
