------------------------------------------------------------------------------------------------------------------------------------
						How to run this project
------------------------------------------------------------------------------------------------------------------------------------
To run this code Please follow following steps:-
1)Unzip the workspace and switch to this workspace using eclipse or any java IDE.
2)Use jre version 1.5 or above to run this project.
3)Please check if the project contains tccorpus.txt and queries.txt file to successfully run it
4)Please add apache-commons-lang.jar and commons.io-2.4.jar (apache-commons) to run this project.
5)These jars  should be present in the workspace.  
6)To run the program please access the command line argument as follows.
   1) Click on Run -> Run (not Run Last Launched)
   2)Click on the Arguments tab
   3)Then just click Apply, followed by Run. 
   Else please access the following link:-http://www.cs.colostate.edu/helpdocs/eclipseCommLineArgs.html
7)Both programs work individually(both have there main classes) to run first program (nbtrain.java) please type "//resources//train" "resources//result//output.txt"  in arguments tab of command line arguments window
  and click on apply and run.
8)For running nbtest.java please type "resources//result//output.txt" "resources//test" "resources//result//prediction.txt" in arguments window and click on apply and run.
9)All results are in results folder.
  1)prediction_dev_neg:-Prediction of dev document for negative sentiments.
  2)prediction_dev_pos:-Prediction of dev document for positive sentiments.
  3)prediction_test :-Prediction for test.
  4)top20ratio:-List of 20 terms with highest positive to negative weight and negative to positive weight. 
10)Project is found in HW6 file  
-----------------------------------------------------------------------------------------------------------------------------------
                                     		 Citations
-----------------------------------------------------------------------------------------------------------------------------------
1)I have refered to java documentation for 
1)How to sort hash map  
2)Use comparators to sort hash map 
3)Use of iterator and keyset method of hashmap
4)How to dump hashmap as object to a file
------------------------------------------------------------------------------------------------------------------------------------
											Some Stats
------------------------------------------------------------------------------------------------------------------------------------
1)For dev positive data the accuraccy is 75%
2)For dev negative data the accuraccy is 80%
3)For test data there are 90 positive and 110 negative document.
4)The above statistics were calculated manually.
------------------------------------------------------------------------------------------------------------------------------------
                                        Collabration
-----------------------------------------------------------------------------------------------------------------------------------		
1)Group partner for assignment:-Rushikesh Badami(Section 3) and Ajay Kauthale(Section 2)								