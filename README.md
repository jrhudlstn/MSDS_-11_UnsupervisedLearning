# MSDS_-411_UnsupervisedLearning
Final Project. Unsupervised learning to find hourly utilization using K-means Clustering. 
# Unsupervised K-means clustering algorithm applied to the Call Detail Records (CDRs) dataset-Telecom Italia cellular network-over the city of Milano
The dataset provides information about the telecommunication activity over the city of Milano. The dataset is the result of a computation over the Call Detail Records (CDRs) generated by the Telecom Italia cellular network over the city of Milano that telecom companies capture during Call, SMS, and Internet activity of a customer. CDRs log the user activity for billing purposes and network management. CDRs log the user activity for billing purposes and network management. There are many types of CDRs from the data spource Open Database License (ODbL) Telecommunications - SMS, Call, Internet - MI https://doi.org/10.7910/DVN/EGZHFV .

Square id: The id of the square that is part of the city GRID.
Time interval: The beginning of the time interval expressed as the number of millisecond elapsed from the Unix Epoch on January 1st, 1970 at UTC. The end of the time interval can be obtained by adding 600000 milliseconds (10 minutes) to this value.
Country code: The phone country code of a nation. Depending on the measured activity this value assumes different meanings that are explained later.
SMS-in activity: The activity in terms of received SMS inside the Square id, during the Time interval and sent from the nation identified by the Country code.
SMS-out activity: The activity in terms of sent SMS inside the Square id, during the Time interval and received by the nation identified by the Country code.
Call-in activity: The activity in terms of received calls inside the Square id, during the Time interval and issued from the nation identified by the Country code.
Call-out activity: The activity in terms of issued calls inside the Square id, during the Time interval and received by the nation identified by the Country code.
Internet traffic activity: The activity in terms of performed internet traffic inside the Square id, during the Time interval and by the nation of the users performing the connection identified by the Country code.
Call Detail Record (CDR) is the information captured by the telecom companies during the Call, SMS and Internet activity. This information’s provides insights about the customer needs when it is used with customer demographics. 

Most of the telecom companies using call detail record information in the fraud detection by clustering the user profiles, Customer churn by usage activity and targeting the profitable customers by using RFM analysis.

Data Description:
The datasource used here is daily activity file from dandelion API. These are the CDR records generated by the Telecom Italia cellular network over the city of Milano. Daily CDR activity file contains information for 10000 grids about SMS in and out, Call in and out and internet activity.  CDR activity file contains information for 10, 000 grids about SMS in and out, Call in and out, and Internet activity 

## Introduction
The telecom companies use CDR information for fraud detection by clustering the user profiles, reducing customer churn by usage activity, and targeting the profitable customers by using RFM analysis.

    Received SMS: a CDR is generated each time a user receives an SMS
    Sent SMS: a CDR is generated each time a user sends an SMS
    Incoming Calls: a CDR is generated each time a user receives a call
    Outgoing Calls: CDR is generated each time a user issues a call
    Internet: a CDR is generate each time
        a user starts an internet connection
        a user ends an internet connection
        during the same connection one of the following limits is reached:​
            15 minutes from the last generated CDR
            5 MB from the last generated CDR

By aggregating the aforementioned records it was created this dataset that provides SMSs, calls and Internet traffic activity. It measures the level of interaction of the users with the mobile phone network; for example the higher is the number of SMS sent by the users, the higher is the activity of the sent SMS. Measurements of call and SMS activity have the same scale (therefore are comparable); those referring to Internet traffic do not.
## Business need
Clustering of the customer activities for 24 hours by using unsupervised K-means clustering algorithm. It is used to understand segment of customers with respect to their usage by hours. 
The business need is to identify hours within a day when the customers are most active. 
For example, customer segment with high activity may generate more revenue. Customer segment with high activity in the night hours might be fraud ones.
## Citation

    Barlacchi, G. et al. A multi-source dataset of urban life in the city of Milan and the Province of Trentino. Sci. Data2:150055 doi: 10.1038/sdata.2015.55 (2015)
    Telecom Italia, 2015, "Telecommunications - SMS, Call, Internet - MI", https://doi.org/10.7910/DVN/EGZHFV, Harvard Dataverse, V1
    Telecom Italia, 2015, "Milano Grid", https://doi.org/10.7910/DVN/QJWLFU, Harvard Dataverse, V1


## Dataset

Telecom Italia's data. Recorded in Milano in 2013 november and september. You can download the dataset from [here](https://dandelion.eu/datamine/open-big-data/). It licensed under [ODbL](https://opendatacommons.org/licenses/odbl/).

### Description

> This dataset provides information about the telecommunication activity over the city.
>
> The dataset is the result of a computation over the Call Detail Records (CDRs) generated by the Telecom Italia cellular network over the city. CDRs log the user activity for billing purposes and network management. There are many types of CDRs, for the generation of this dataset we considered those related to the following activities:
>- Received SMS: a CDR is generated each time a user receives an SMS
>- Sent SMS: a CDR is generated each time a user sends an SMS
>- Incoming Calls: a CDR is generated each time a user receives a call
>- Outgoing Calls: CDR is generated each time a user issues a call
>- Internet: a CDR is generate each time
>	- a user starts an internet connection
>	- a user ends an internet connection
>	- during the same connection one of the following limits is reached:​
>		- 15 minutes from the last generated CDR
>		- 5 MB from the last generated CDR
>
> By aggregating the aforementioned records it was created this dataset that provides SMSs, calls and Internet traffic activity. It measures the level of interaction of the users with the mobile phone network; for example the higher is the number of SMS sent by the users, the higher is the activity of the sent SMS. Measurements of call and SMS activity have the same scale (therefore are comparable); those referring to Internet traffic do not.

#### Schema

>1. **Square id**: The id of the square that is part of the city GRID.
>2. **Time interval**: The beginning of the time interval expressed as the number of millisecond elapsed from the Unix Epoch on January 1st, 1970 at UTC. The end of the time interval can be obtained by adding 600000 milliseconds (10 minutes) to this value.
>3. **Country code**: The phone country code of a nation. Depending on the measured activity this value assumes different meanings that are explained later.
>4. **SMS-in activity**: The activity in terms of received SMS inside the Square id, during the Time interval and sent from the nation identified by the Country code.
>5. **SMS-out activity**: The activity in terms of sent SMS inside the Square id, during the Time interval and received by the nation identified by the Country code.
>6. **Call-in activity**: The activity in terms of received calls inside the Square id, during the Time interval and issued from the nation identified by the Country code.
>7. **Call-out activity**: The activity in terms of issued calls inside the Square id, during the Time interval and received by the nation identified by the Country code.
>8. **Internet traffic activity**: The activity in terms of performed internet traffic inside the Square id, during the Time interval and by the nation of the users performing the connection identified by the Country code.

#### File Format
Pick any file 
From https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/EGZHFV 

Each file in the dataset is one 24 hour period. 
## Data Prep
Convert the square ID and the county code into factor columns as part of type conversion.
Derive new fields such as “activity start date” and “activity hour” from “time interval” field.
Find the total activity, which is the sum of SMS in and out activity, call in and out activity, and Internet traffic activity.
Remove NA's from data

# Methods
The dataset is scrubbed to reduce the size of the set to a subset of the overall records to a concentrated GRID of the customers by demographic regions in the city, treating the missing values in fields with N/A’s with new values, taking the two variables “square_ID” and the “county_code” into factor columns as part of type conversion, and creating two new variables (“activity_start_date” and “activity_hour” from “time_interval” field) to get a visual of clusters for the analysis. The variable in the dataset “square_ID” is a part of the city GRID, an area that the city of Milano that is broken into segments for demographic customer attributes. Since the dataset consist of five million records, the “square_ID” is used to further reduce the data set for analysis by selection the GRID “square_ID” to those with a value (bucket) less than 500. The dataset is furthered scrubbed to include the variable “total_activity” that can be used as the sum of the SMS capturing as part of the data collection as “in” and “out” activity, “call in” and “out” activity, and Internet traffic activity. 

# Results
Interpreting the plotted data starting with two new variables “activity_start_date” and “activity_hour” from “time_interval” field is Figure 1, in the appendix, where the active time is between the 8th hour and the 20th hour of the day, called the window of opportunity. The second plot, the “The total activity from the Top 25 square grids”, seen in Figure 2, is the “GRID”, “square_id” and the derived field, “total_activity” plotted for the most active area in the City of Milano, Italy for reaching customers. This Figure shows most of the activities happened in the square grid ID 147. In addition, the highest activity in the country for the City of Milano, Italy is seen in Figure 3, “Top 10 country by total activity”, and that is country code 39. 
From the K-means clustering algorithm, seen in Figure 4, ten clusters are the optimal number of clusters since the Sum of Squared Error (SSE) decreases with minimal change after cluster number ten and there is no sudden increase in the error distance. A synopsis of CDR K-means model and its center calculated for each cluster can be viewed in Table 2 in the Appendix. This data is used for a heat map for clustering for the most revenue generating clusters. See Figure 5. The clusters 2,3,4, and 9 have the most activity in 24 hours and are the more revenue generating clusters. Cluster 2,3,4,5, and 9 have activity in the night hours. 
To Applying K-Means Clustering to other variables, the elbow method, from Figure 6, is used to find the optimum number of clusters which is 2. 
Computing the components of sms_in_activity and country_code reveals 100% of the point variability, seen in Figure 7, as call_in_activity and internet-traffic_activity, describes 100% of the point variability. 

# Conclusion
The figures in the Appendix can be used by management to focus the customer interaction for driving new revenues and business for a competitive advantage. The next steps would be to include more data sets (each data set is one 24-hour period) and use square grid and country code data to find the square grid generating more revenues and traffic to the telecom network and to target high customers based on their geolocation. 

# Appendix
Tables
Dataset Schema
![image](https://user-images.githubusercontent.com/6859309/111854458-65b63a00-88ed-11eb-8fbd-331cc8d3d9d9.png)
Table 1. Dataset Description Call Detail Records (CDRs) generated by the Telecom Italia cellular network over the city of Milano

![image](https://user-images.githubusercontent.com/6859309/111854470-71a1fc00-88ed-11eb-8d53-0fbdbafc15ff.png)
Table 2. 
Figures 

Window of Opportunity
![image](https://user-images.githubusercontent.com/6859309/111854485-8088ae80-88ed-11eb-9e0e-bd7247bb01a6.png)

					Window of Opportunity
					Total activity by activity hours
 
Figure 1. The active time is between the 8th hour and the 20th hour of the day. 

![image](https://user-images.githubusercontent.com/6859309/111854505-926a5180-88ed-11eb-9cdb-7520e5ec46fc.png)
					
                                                 The total activity from the Top 25 square grids
 
Figure 2. The most active grid “square_id” is 147. 

![image](https://user-images.githubusercontent.com/6859309/111854514-9d24e680-88ed-11eb-815f-31b2fb201035.png)

Figure 3. The top 10 country codes by total activity is country code 39. 
				The optimal number of clusters to the K-means algorithm
                                                                         The total activity and activity hours
![image](https://user-images.githubusercontent.com/6859309/111854532-a7df7b80-88ed-11eb-93ed-f59deafb22f6.png) 
Figure 4. The elbow, from the plot of K-means algorithm, ‘Cluster’ 5 to 10. 

                                               The heat map plot with cluster, activity hour, and total activity time    
                                                                      The most revenue generating clusters    
![image](https://user-images.githubusercontent.com/6859309/111854544-baf24b80-88ed-11eb-9c10-4b986a4fa5e5.png)
Figure 5. The clusters 2,3,4, and 9 have the most activity in 24 hours and are the more revenue generating clusters. Cluster 2,3,4,5, and 9 have activity in the night hours. 

![image](https://user-images.githubusercontent.com/6859309/111854552-c180c300-88ed-11eb-9c77-b7f0230ca922.png) 
Figure 6. The elbow method to more variables the optimum number of clusters is 2. Variables country_code, sms_in_activity, sms_out_activity, call_in_activity, call_out_activity, and internet_traffic_activity. 
![image](https://user-images.githubusercontent.com/6859309/111854559-ca719480-88ed-11eb-8937-f4dc14f90925.png) 
Figure 7. 

![image](https://user-images.githubusercontent.com/6859309/111854572-d52c2980-88ed-11eb-96cc-e9d220c8ed3a.png)
Figure 8. 


