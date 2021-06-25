The MEP 1.0 dataset contains 5 files, of which 3 contain metadata, 1
contains roll-call vote data, and 1 contains retweet info.

Files containing metadata:

* mep_info.json
Contains metadata about the Members of 8th European Parliament until 2016-03-01 as a JSON dictionary. 
Each dictionary entry represents metadata for one MEP. 
The key of the entry is the mepId. 
The values include the following information:
- fullName: the name of the MEP
- from: the date in ISO format when he/she became a MEP
- to: for MEPs which left the EP, the date in ISO format when he/she left, otherwise null
- substituting: for MEPs which substituted another MEP, the id of the subsituted MEP, otherwise null
- substitutedBy: for MEPs which left the EP, the id of the MEP which subsituted him/her, otherwise null
- twitterId: the Twitter id of the MEP if he/she has one, otherwise null
- twitterScreenName: the Twitter account of the MEP if he/she has one, otherwise null
- politicalGroupId: the id of the European political group he/she is a member of
- politicalGroupShort: the shortened name of the European political group he/she is a member of
- politicalGroupName: the full name of the European political group he/she is a member of
- countryId: the id of the country he/she is coming from
- countryShort: the shortened name of the country he/she is coming from
- countryName: the name of the country he/she is coming from
- nationalPoliticalGroupId: the id of the national political group he/she is a member of
- nationalPoliticalGroupName: the name of the national political group he/she is a member of
- rcvMepId: the id used in the original RCV XML files
- politicalGroupMostRecentShort: the most recent European political group in the RCV XML files

* rcv_info.json
Contains metadata about the Roll-call votes of the 8th European Parliament until 1.3.2016 as a JSON dictionary. 
Each dictionary entry represents metadata for one RCV. 
The key of the entry is the rcvId. 
The values include the following information:
- rcvSittingId: id of the sitting in which the RCV took place
- sittingDate: the date of the sitting in which the RCV took place
- epReference: European Parliament RCV reference
- epNumber: European Parliament RCV id
- rcvDate: the date of the RCV
- rcvTime: the time at which the RCV took place
- description: description of the RCV
- isPublic: whether it is a public vote
- hasMeta: whether it has metadata
- documentId: the id of the document which is the subject of the RCV
- url: the URL of the RCV
- procId: the id of the procedure
- procUrl: the URL of the procedure
- title: the title of the RCV
- subject: the subject into which the RCV is classified
- geographicalArea: geographical area concerned with the RCV
- procType: the procedure type
- procSubtype: the procedure subtype
- hasAttendance: whether attendance record is present for the RCV
- lpSittingId: the sitting from the attendance record
- procTypeId: id of the procedure type
- subjectId: id of subject
- subjectName: name of the subject

* action_info.csv
Contains metadata about the actions MEPs can take during an RCV in a CSV format. 
Contains two columns: 
- actionId: the id of the action
- actionName: the name of the action

Files containing data:

* mep_rcv_vote.csv
Contains roll-call vote data as a matrix with the votes of all MEPs during all RCVs in CSV format. 
The first column contains the mepIds as in mep_info.json, and the first row contains the rcvIds as in rcv_info.json. 
The values of the matrix are the actions MEPs took on RCVs. The actionsIds are as in action_info.csv.

* retweets.csv
Contains all retweets between MEPs between 1.10.2014 and 1.3.2016 in CSV format. 
Contains the following columns:
- origUserId: the Twitter id of the MEP that posted the original tweet
- origUserScreenName: the Twitter name of the MEP that posted the original tweet
- origMepId: the mepId of the MEP that posted the original tweet
- origMepName: the name of the MEP that posted the original tweet
- origMepGroupId: the European group id of the MEP that posted the original tweet
- origMepGroupShort: the shortened European group name of the MEP that posted the original tweet
- origMepCountryId: the country id of the MEP that posted the original tweet
- origMepCountryShort: the shortened country name of the MEP that posted the original tweet
- retweetUserId: the Twitter id of the MEP that posted the retweet
- retweetUserScreenName: the Twitter name of the MEP that posted the retweet
- retweetMepId: the mepId of the MEP that posted the retweet
- retweetMepName: the name of the MEP that posted the retweet
- retweetMepGroupId: the European group id of the MEP that posted the retweet
- retweetMepGroupShort: the shortened European group name of the MEP that posted the retweet
- retweetMepCountryId: the country id of the MEP that posted the retweet
- retweetMepCountryShort: the shortened country name of the MEP that posted the retweet
- origCreatedAt: the timestamp of the original tweet
- origTweetId: the Twitter id of the original tweet
- retweetCreatedAt: the timestamp of the retweet
- retweetTweetId: the Twitter id of the retweet
- lang: the language of the original tweet
