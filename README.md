# The Association between Surgical Time and Outcome

### Overview

This final project is interested in exploring what factors influence mortality and complication within 30 days after surgery. The dataset  used contains 32,001 general surgical patients and the information on the mortality and complication in 30 days. The method is logistic regression.

### Research Questions

The project goal is to find if there is an association between scheduled surgery time and the surgery outcome. It also explores how the patients’ bodily conditions (eg. BMI, age, etc.) and baseline diseases affect the surgery outcome. 

One of the key interest of this project is exploring how surgert outcome is related to scheduled surgery time. Like any other worker group, hospital staff’s performance is related to the time of a day. It is natural to assume that if surgeons work late at night, their effectiveness would decrease, and they are more likely to make mistakes, which would affect the surgery outcome. If we find surgery outcome is highly associated with the time surgery’s been done, we can infer that time might also have an association with surgeons’ performance.

### Data

The dataset comes from a previous study that tests the hypothesis that the risk of 30-day mortality increases from morning to evening and from Monday to Friday.

The dataset contains 32,001 elective general surgical patients. It records these patients’ biological information such as age, sex, race, and BMI. It also records patients’ baseline diseases and surgical risk indices. Additionally, it has columns indicating the surgery time – the hour of a day, the day of a week, month, moon phase, etc.. This data is relatively well-cleaned, except for one minor issue - there are some missing values in the BMI column.


### Project Plan

The statistical method used is logistic regression. There are two response variables in the dataset - 30-days mortality and complications. These two variables are both post-surgery information, so they are both meant to be response variables. Two models will be build for these two response variables, one to explore what factors affect 30-day mortality, the other to explore what factors affect 30-day complications. 


