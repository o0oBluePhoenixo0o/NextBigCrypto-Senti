# Crypto SentiTomo
Previous researches have shown that securities and other financial instruments market movements can be predicted using real-time Twitter data. Not only that, multiple studies have tried to perform sentiment analysis on Twitter data in order to detect short and long term trends in the cryptocurrency market. Vast amount of information can be found in the form of unstructured text written by internet users in the cryptocurrency network. Crowd wisdom in those networks can be analyzed and proven to be a reliable indicator of major events affecting the markets.  However, not many have focused on neither other social media platforms nor different cryptocurrencies beside renown ones such as Bitcoin and Ethereum. This study proposes a machine learning based method which look into the public perceptions about cryptocurrency, utilizes not only Twitter but also data from other social media platforms such as Reddit in order to detect sentiments, topics and trends so that correlation between those factors and prices fluctuations of alternative coins can be analyzed.


# Progress
## Oct 2017 
- Develop crawler for Twitter using REST API. Schedule weekly update since historical data is limited to 1 week for free API
- Crawler for Reddit base on Pushshift.io API, scripts collects all submissions data looping over rate limit.
## Nov 2017
- Finalize preprocessing pipeline
- Implement baseline models for sentiment analysis and topic modeling
- Framework for Ensemble model and hyper parameter testing
- Granger causality test for time series analysis with market data
