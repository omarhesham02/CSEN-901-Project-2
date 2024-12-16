This repositry is for project 2 of the CSEN 901: Introduction to Artificial Intelligence course at the German University in Cairo (GUC). 
The project revolved around the design of a logic agent that reasons using a successor state axiom to solve a simplified version of the water sort puzzle.


You can find the project description outlining the problem and the requirements of the project in the file titled **Project Description.pdf**. 
You can also read our project report describing our development process and implementation in **Report/Report.pdf**. 


If you would like to try the program, we recommend you leave KB1.pl unmodified and use KB2.pl to write a knowledge base representing an instance of the problem instead (you may use KB.pl as a reference for help). 
Just make sure to change the import at the top of the Watersort.pl file to import KB2.pl instead of KB1.pl:

`:- consult('KB2.pl').`
