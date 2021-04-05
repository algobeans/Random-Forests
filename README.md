# Random Forests
Layman's tutorial to random forests, explained in detail at: https://algobeans.com/2021/03/29/random-forest-tutorial-predicting-goals-in-football/

<p>
Random Forest Output: Heatmap of goal probabilities based on the location where a shot was attempted. Red and orange areas indicate high probability of scoring if a shot was made at that location, whereas blue and green areas indicate low probability of scoring.
  <br>
<img src="https://annalyzin.files.wordpress.com/2021/03/random-forest-goal-prob.png">
</p>

<p>
As a reference of football data, the scatterplot below depicts a sample of shots from the Wyscout dataset. Each dot represents the location where a shot was attempted, with red dots representing successful goals.
  <br>
<img src="https://annalyzin.files.wordpress.com/2021/03/shots-goals.png">
</p>

<p>
Example of Ensemble Voting. Models 1, 2, and 3 are individual models attempting to predict 10 outputs, where Blue is the correct output and Red is the wrong output. An ensemble model is formed by majority voting, i.e. if two models predict Blue and one model predicts Red, the ensemble predicts Blue. Here, the ensemble model scored 8/10, higher than individual models, which scored at most 7/10.
  <br>
<img src="https://annalyzin.files.wordpress.com/2016/08/ensemble.gif">
</p>

<p>
Histogram showing the RMSE of 1000 decision trees. While their RMSE averages at 0.299, with the best score at 0.296, the random forest model had an RMSE of 0.288, which is best among all of its constituent decision trees.
  <br>
<img src="https://annalyzin.files.wordpress.com/2021/03/rplot.png">
</p>

<p>
Illustration: How a tree is created in a random forest.
  <br>
<img src="https://annalyzin.files.wordpress.com/2016/08/bagging.gif">
</p>

Layman's tutorial to random forests, explained in detail at: https://algobeans.com/2021/03/29/random-forest-tutorial-predicting-goals-in-football/


