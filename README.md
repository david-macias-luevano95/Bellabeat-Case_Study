#Scenario

Bellabeat it’s a compani whith a long expancion since it was fonded in 2013 quicly posisioned it self as a tech-driven wellness company for women. The company has 5 focus prosucts: bellabeat app, leaf, time,spring, and bellabeat membership. The company has a lot of potencial our team have asked to analyze smart device data to gain insight into how consimers are using their smart devices. The insights we discover will then helpguido marketing strategy fot the company. 

#About a company
Bellabeat, a high-tech manufacturer of health-focused products for women. Bellabeat is a successful small company, but they have the potential to become a larger player in the global smart device market. Urška Sršen, cofounder and Chief Creative Officer of Bellabeat, believes that analyzing smart device fitness data could help unlock new growth opportunities for the company

#Business Task:

Conduct an analysis of Bellabeat's current user data to identify trends in smart device usage and provide recommendations for improving the Bellabeat marketing strategy and identifying potential growth opportunities.

1.2 Key stakeholders
•	Urška Sršen: Bellabeat’s co-founder and Chief Creative Officer.
•	Sando Mur: Mathematician and Bellabeat’s co-founder.
•	Bellabeat marketing analytics team.


###2 Prepare

The data used in this case study is public, therefore, everyone can access it. In addition, it is provided by FitBit Fitness Tracker, a well known entity. Due to these reasons, we can conclude the data is credible.
If you want to download it, click here.
To start, let’s set up the environment by downloading and opening the necessary libraries for the analysis.



The dataset has 18 CSV files. The data also follow a ROCCC approach: 
Reliability: The data is from 30 FitBit users who consented to the submission of personal tracker data and generated by from a distributed survey via Amazon Mechanical Turk. Original: The data is from 30 FitBit users who consented to the submission of personal tracker data via Amazon Mechanical Turk. Comprehensive: Data minute-level output for physical activity, heart rate, and sleep monitoring. While the data tracks many factors in the user activity and sleep, but the sample size is small and most data is recorded during certain days of the week. Current: Data is from March 2016 to May 2016. Data is not current so the users habit may be different now. Cited: Unknown. 
The dataset has limitations: 
Only 30 user data is available. The central limit theorem general rule of n≥30 applies and we can use the t test for statstic reference. However, a larger sample size is preferred for the analysis. Upon further investigation with n_distinct() to check for unique user Id, the set has 33 user data from daily activity, 24 from sleep and only 8 from weight. There are 3 extra users and some users did not record their data for tracking daily activity and sleep. For the 8 user data for weight, 5 users manually entered their weight and 3 recorded via a connected wifi device (eg: wifi scale). Most data is recorded from Tuesday to Thursday, which may not be comprehensive enough to form an accurate analysis.
The data used in this case is very commun to use its formal and important for reserhe
The dataset has 18 CSV. The data also follow a ROCCC approach:
•	Reliability: The data is from 30 FitBit users who consented to the submission of personal tracker data and generated by from a distributed survey via Amazon Mechanical Turk.
•	Original: The data is from 30 FitBit users who consented to the submission of personal tracker data via Amazon Mechanical Turk.
•	Comprehensive: Data minute-level output for physical activity, heart rate, and sleep monitoring. While the data tracks many factors in the user activity and sleep, but the sample size is small and most data is recorded during certain days of the week.
•	Current: Data is from March 2016 to May 2016. Data is not current so the users habit may be different now.
•	Cited: Unknown.


⛔ The dataset has limitations:
•	Only 30 user data is available. The central limit theorem general rule of n≥30 applies and we can use the t test for statstic reference. However, a larger sample size is preferred for the analysis.
•	Upon further investigation with n_distinct() to check for unique user Id, the set has 33 user data from daily activity, 24 from sleep and only 8 from weight. There are 3 extra users and some users did not record their data for tracking daily activity and sleep.
•	For the 8 user data for weight, 5 users manually entered their weight and 3 recorded via a connected wifi device (eg: wifi scale).
•	Most data is recorded from Tuesday to Thursday, which may not be comprehensive enough to form an accurate analysis.


![](https://raw.githubusercontent.com/david-macias-95/Bellabeat-Case_Study/master/imagenes/Rplot.png)










Process
Examine the data, check for NA, and remove duplicates for the five main tables: daily, activities, spleep, and weight:


Convert Activity Date into date format and add a column for day of the week:
This information tells us wich days the people recover more data between the Tuesday to thurday.



There is 33 participants in the activity, calories and intensities data sets, 24 in the sleep and only 8 in the weight data set. 8 participants is not significant to make any recommendations and conclusions based on this data.
The day for more steps is the Saturday and the day whith less step is Sunday 
This information tell us how the day rest is the most day for activity and in the weelend que summary in low than the weekend
For make only one data frame merged_data.cvs for take the summary and make the prompt
Let’s have a look at summary statistics of the data sets:

Confirm if exist 30 user using function n_distinct(). The data set has 33 user from daily activity, 24 from sleep the way 
Merge the three tables:
Clean the data to prepare for analysis in 4. Analyze!
4. Analyze
Summary:
Check min, max, mean, median and any outliers. Avg weight is 135 pounds with BMI of 24 and burn 2050 calories. Avg steps is 10200, max is almost triple that 36000 steps. Users spend on avg 12 hours a day in sedentary minutes, 4 hours lightly active, only half hour in fairly+very active! Users also gets about 7 hour of sleep.
One interesting discoveries from this summary:
•	Average sedentary time is 991 minutes or 16 hours. Definately needs to be reduced!
•	The majority of the participants are lightly active.
•	On the average, participants sleep 1 time for 7 hours.
•	Average total steps per day are 7638 which a little bit less for having health benefits for according to the CDC research. They found that taking 8,000 steps per day was associated with a 51% lower risk for all-cause mortality (or death from all causes). Taking 12,000 steps per day was associated with a 65% lower risk compared with taking 4,000 steps.
 





From weekday's total asleep minutes, we can see the graph look almost same as the graph above! We can confirmed that most sleep data is also recorded 

during Tuesday to Thursday. This raised a question "how comprehensive are the data to form an accurate analysis?"

The American Heart Association and World Health Organization recommend at least 150 minutes of moderate-intensity activity or 75 minutes of vigorous activity, or a combination of both, each week. That means it needs an daily goal of 21.4 minutes of FairlyActiveMinutes or 10.7 minutes of VeryActiveMinutes.

Share
How active the users are weekly in total steps. Tuesday and Saturdays the users take the most steps.





Less sedentary minutes for day is on Saturday an the day whith the most days for steps 

Recommendations for Bellabeat App: Enhancing Women's Health and Wellness
1. Encourage Daily Step Goals
Insight: The average total steps per day for Bellabeat users is 7,638, which is below the optimal number for significant health benefits. According to CDC research, taking 8,000 steps per day is associated with a 51% lower risk of all-cause mortality, and 12,000 steps per day is associated with a 65% lower risk compared to taking 4,000 steps.
Recommendation:
•	Feature: Introduce a daily step goal feature that encourages users to reach at least 8,000 steps per day.
•	Benefit Explanation: Provide educational content within the app that explains the health benefits of reaching these step goals, motivating users to increase their physical activity.
•	Motivation: Use app notifications and rewards to encourage users to achieve and maintain their step goals.
2. Promote Healthy Eating Habits
Insight: Controlling daily calorie consumption is crucial for users who aim to lose weight.
Recommendation:
•	Feature: Add a nutritional guide that suggests low-calorie lunch and dinner ideas.
•	Integration: Partner with nutritionists to provide healthy, balanced meal plans that align with users' fitness goals.
•	Tracking: Allow users to log their meals and receive feedback on their calorie intake versus their activity levels.
3. Improve Sleep Hygiene
Insight: Many users spend a significant amount of time awake in bed before falling asleep, and improving sleep quality is a common goal.
Recommendation:
•	Feature: Implement bedtime notifications that remind users to wind down and prepare for sleep.
•	Sleep Tips: Provide tips and routines to improve sleep hygiene, such as reducing screen time before bed and establishing a consistent sleep schedule.
•	Activity-Sleep Link: Highlight the connection between daytime activity levels and sleep quality, encouraging users to stay active to improve their sleep.
4. Optimize Activity Notifications
Insight: Most activity occurs between 5 PM and 7 PM, likely when users go to the gym or for a walk after work.
Recommendation:
•	Feature: Schedule motivational reminders during peak activity times to encourage users to stay active.
•	Custom Reminders: Allow users to set personalized reminders based on their routines and preferences.
•	Activity Challenges: Introduce time-specific challenges, such as evening walks or workouts, to engage users during these hours.
5. Reduce Sedentary Behavior
Insight: A significant portion of users' active minutes is spent sedentary, with an average of 12 hours a day in sedentary activities.
Recommendation:
•	Feature: Implement a sedentary alert system that uses devices like the Leaf wellness tracker to signal when users have been inactive for too long.
•	Movement Prompts: Encourage short, frequent breaks for physical activity throughout the day to reduce sedentary time.
•	Weekend Focus: Promote more active weekends, especially on Sundays when users are the most sedentary, through special weekend challenges and activities.
Marketing Recommendations for Global Expansion
Data Collection: Encourage the use of wifi-connected scales for accurate weight tracking, minimizing manual entries. Educational Campaigns: Launch campaigns to promote short active exercises during the week and longer sessions during weekends, focusing on the most sedentary days. Incentive Systems: Introduce a point-award system for users who complete weekly exercise goals, redeemable for Bellabeat products or memberships. Product Features: Enhance devices like the Leaf wellness tracker to provide proactive health reminders, such as vibrations after prolonged inactivity or notifications to prepare for bed.
By implementing these features and marketing strategies, Bellabeat can significantly enhance the health and wellness of its users, encouraging more active, balanced, and healthy lifestyles.

