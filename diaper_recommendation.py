#reference: the bulk of this code is adopted from Prof. Panos' Machine Learning 2 course teaching materials
from pyspark.ml.recommendation import ALS
from pyspark.sql import Row
import pandas as pd

df = spark.read.load("data/final1.csv", format = 'csv', header = 'true')
  
df2 = df.withColumn("tot", df["tot"].cast("double")).withColumn("cust_id", df["cust_id"].cast("integer")).withColumn("prod_id", df["prod_id"].cast("integer"))

# Train an Alternating Least Squares model
als = ALS()\
  .setMaxIter(5)\
  .setRegParam(0.01)\
  .setUserCol("cust_id")\
  .setItemCol("prod_id")\
  .setRatingCol("tot")

alsModel = als.fit(df2)

# Generate recommendations (users and then items) - output top 10 recommendations for each user or movie.
out = alsModel.recommendForAllItems(1000)\
  .selectExpr("prod_id", "explode(recommendations)")
  
#install pandas on AWS
#sudo pip install pandas
#write the recommendations into csv files and save it on AWS
out.toPandas().to_csv('mycsv.csv')

#save the file on s3
#aws s3 cp mycsv.csv s3://mybucket
#download to local and proceed to R-studio for further manipulation


