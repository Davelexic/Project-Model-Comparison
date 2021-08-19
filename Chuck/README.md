<img src="https://miro.medium.com/max/1400/1*XbuW8WuRrAY5pC4t-9DZAQ.jpeg" width="600" height="250">


# Comparing Models

### Does complicating the model lead to better results?

**Thesis**

Why complicate something if it doesn't give you any added benefit. We are going to look into the use cases for models and see if complex models yield better results.  

We  also explored using a built-in Keras Tuner to explore optimizing hyperparameters and see if that can help us giving parameters that are more optimized than try tune each parameter manually.

---



---
**What's the outcome?** 

We came to find if time and money are not a factor then with the right tuning a CNN model would preform well. Most of the time those variables are limited so making a simple deep learning provides almost as much insight with out the blood, sweat, and tears required to built and prep and CNN. We will continue to learn about this are and learn from our mistakes to evolve or pipeline so that it not so cumbersome.

We also found that a tuner could find a complex hyperparameters that may not necessarily perform well. So we cannot take it at face value. But when keeping simpler layers and letting tuners find number of units and learning rate etc that did give a better result than a simple LSTM model. But the summary was to always compare and check the performance whether a tuner is used or not. That human judgement is still needed.



##### Initial Idea

Our initial idea was to create images of candlestick plot for 5 days Monday - Friday of the week of Apple stock data closing prices that will be later fed to Convolution Neural Network so it can learn to recognize the patterns based on Candlestick image for a week. We also wanted to combine this with other inputs like News sentiment, technical indicators like RSI, Volume, moving averages like 50 SMA and 100 SMA, MACD etc and see whether those additional inputs along with image recognition can help us predict the buy/sell signal. 

Also although we already had the data of the 5 days closing prices of the image the idea was to use a CNN image learning and possible future candlestick image recognitions from news/media/social media in any situations.

The idea is we also feed a signal BUY/SELL which is based on next Monday Closing price. If the Monday closing price is higher than the prior Friday closing price then its a BUY and if not its a SELL.

Our attempt here is to see if the Convolution Neural network can learn the image and predict the signal.

We see later that this Signal is not enough of an true indicator of a Buy/Sell so the Neural network results are not that great. Ideally if there was better correlation between image and signal we would have give the Neural network to learn and predict in better way.



We start with simple Convolution Neural network model to see that the results are not that great:

### deep_learn2.ipynb

Data preparation and candlestick is done in Notebook:

### candlestick02.ipynb



We then move on to use a simple LSTM model without images but regular Apple stock close prices with rolling 5 day window to predict close prices: 

### lstm_predictor_project2-2.ipynb



Fine tuning model involves working on different Hyperparameters and adjusting the values and re-running the training.

##### Hyperparameters

Hyperparameters are the variables which determines the network structure and the variables which determine how the network is trained. Hyperparameters are set before training (before optimizing the weights and bias).

###### 	Hyperparameters related to Network structure

* Number of Hidden Layers and unit

- Dropout
- Network Weight Initialization
- Activation function

###### Hyperparameters related to Training Algorithm

- Learning Rate
- Momentum
- Number of epochs
- Batch size



#### So instead of manually fine tuning and adjusting each of these and re-running the model to evaluate and compare the results i explored the Keras Tuner which you will try different parameter ranges and present us what it found is the best tuned parameters.



Here i explored a Bayesian Optimizer from Keras Tuner and then compared with regular simple LSTM that we explored in class exercises. And see how the results stack up.



####  Optimizing LSTM model by using Bayesian optimizer of Tensorflow Keras Tuner: Bayesian_lstm_predictor_project2_finetune1.ipynb

We also use colab with GPU to run this model.

We find that Bayesian optimizer's tuned parameters does a better job than simple LSTM model.







| Notebooks                                                    | Description                                   | Result                        |
| ------------------------------------------------------------ | --------------------------------------------- | ----------------------------- |
| candlestick01.ipynb & candlestick02.ipynb                    | Data preparation for candlestick images       | (n/a)                         |
| deep_learn2.ipynb (jupyter notebook local windows)           | Simple CNN model                              | Performance is not that good. |
| lstm_predictor_project2-2.ipynb (jupyter notebook local windows) | Simple LSTM model.                            | Performance is reasonable.    |
| lstm_predictor_project2_finetune1.ipynb (colab jupyter notebook run with GPU) | Keras Tuner Fine Tuned. But gets complex      | Performance is not that good. |
| Bayesian_lstm_predictor_project2_finetune1.ipynb (colab jupyter notebook run with GPU) | Keras Tuner Bayesian model but simpler layers | Performance looks better      |





