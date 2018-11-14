

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
- Complete "manual dataset" construction (~3200 observations labeled)
- Finalize sentiment analysis on manual label dataset with evaluation (trained models vs packages)
- Historical Price (HP) model version 1 complete as baseline (acc 46% with Recursive Feature Eliminations)
- Complete framework for evaluation models. Contain 5 types of models with 17 combinations:

![Experiment Design](https://github.com/o0oBluePhoenixo0o/NextBigCrypto-Senti/blob/master/img/Experiment%20Design.JPG)

### Models type
* **HP**: Historical Price
* **SAT**: Sentiment Analysis (Trained model)
* **SAP**: Sentiment Analysis (Packages model)
* **LDA**: Topic Modeling - Latent Dirichlet Allocation
* **PD**: Topic Modeling - Predefined Topics

### Features (labeled with respect to time-interval)
* **P<sub>t</sub>**: Price movement of the target cryptocurrency on day *t*. Categorical variable with 2 cases: up / down.
* **T<sub>t</sub>**: Sentiment of tweets regarding the target cryptocurrency on day *t* using trained model. Have 4 variables for each set: total messages count and percentage of positive / neutral / negative.
* **P<sub>kg<sub>t</sub></sub>**: Sentiment of tweets regarding the target cryptocurrency on day *t* using packages model. Have 4 variables for each set: total messages count and percentage of positive / neutral / negative.
* **LDA<sub>t</sub>**: Topics distribution of tweets regarding the target cryptocurrency on day *t* using LDA model. The number of variables depends on the number of topics created by the LDA model, each variable is a topic with a percentage which displays the tweets distribution to that topic compare to the total number of tweets in that specific time interval.
* **PD<sub>t</sub>**: Topics distribution of tweets regarding the target cryptocurrency on day *t* using predefined topics model. The number of variables are the 10 predefined topics, each variable is a topic with a percentage which displays the tweets distribution to that topic compare to the total number of tweets in that specific time interval.


## May 18
- Revisit pre-processing pipeline:

![Preprocessing Pipeline](https://github.com/o0oBluePhoenixo0o/NextBigCrypto-Senti/blob/master/img/Preprocessingv3.JPG)

 - Implement "Twitter bot detection" module (utilize package "botrnot")
 - Custom abbreviations dictionary for translating popular acronyms / slangs on social media
 - Enhance stop-words dictionary (exclude price movements related words / tokens affiliated removal)
 - Lemmatization before tokenization

## June - July 18

- Completed training all 17 models and tested successfully on Bitcoin, Ethereum and Litecoin priceline.
- Obtained best performer in each token cases (BTC with HP_LDA, ETH with SAP_LDA and LTC with HP_SAT_PD)
- Published packages and finalized paper for publication

# Results

- F1 scores best result for BTC/USD pair
![F1_BTC_USD](https://github.com/o0oBluePhoenixo0o/NextBigCrypto-Senti/blob/master/img/F1_best_BTC_USD.JPG)

- F1 scores best result for ETH/USD and ETH/BTC pairs
![F1_BTC_USD](https://github.com/o0oBluePhoenixo0o/NextBigCrypto-Senti/blob/master/img/F1_best_ETH.JPG)

- F1 scores best result for BCH/USD and BCH/BTC pairs
![F1_BTC_USD](https://github.com/o0oBluePhoenixo0o/NextBigCrypto-Senti/blob/master/img/F1_best_BCH.JPG)


