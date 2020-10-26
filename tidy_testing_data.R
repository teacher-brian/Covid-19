
'''
The following are a few lines from 3 different csv files with daily data.

Confirmed = new high count of cases.  Not new cases, new total
People tested is similar.  The total number tested to date.  Must subtract from previous day to get new number of teting.  Testing rate does not appear to be positive rate

Province_State,Country_Region,Last_Update,Lat,Long_,Confirmed,Deaths,Recovered,Active,FIPS,Incident_Rate,People_Tested,People_Hospitalized,Mortality_Rate,UID,ISO3,Testing_Rate,Hospitalization_Rate

Washington,US,2020-04-12 23:18:15,47.4009,-121.4905,10609,506,,10103,53,140.5276682,93615,642,4.7695353,84000053,USA,1240.031828,6.051465737
Washington,US,2020-04-13 23:07:54,47.4009,-121.4905,10635,513,,10122.0,53,140.8720663077597,93802,527,4.823695345557122,84000053,USA,1242.5088447391138,4.955336154207805
Washington,US,2020-10-26 04:30:38,47.4009,-121.4905,102913,2296,,100617.0,53.0,1351.4700731842195,2360387.0,,2.231010659489083,84000053,USA,30996.981835463746,
'''

'''