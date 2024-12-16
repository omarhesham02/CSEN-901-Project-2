# CSEN 901: Introduction to Artificial Intelligence <br> Project 2: Simplified Water Sort Logic Agent

This repositry is for project 2 of the CSEN 901: Introduction to Artificial Intelligence course at the German University in Cairo (GUC). 
The project revolved around the design of a logic agent that reasons using a successor state axiom to solve a simplified version of the water sort puzzle.


You can find the project description outlining the problem and the requirements of the project in the file titled **Project Description.pdf**. 
You can also read our project report describing our development process and implementation in **Report.pdf**. 


If you would like to try the program, we recommend you leave KB.pl unmodified and use KB2.pl to write a knowledge base representing an instance of the problem instead (you may use KB.pl as a reference for help). 
Just make sure to change the import at the top of the Watersort.pl file to import KB2.pl instead of KB.pl:

`:- consult('KB2.pl').`

# Additional Solutions

Although it was not a requirement for the project (and was not submitted for evaluation to the course staff), over the course of working on the project, we have solved the simplified water sort problems in a some other ways.

You can find two additional files containing our alternative solutions. These are: 

1. Watersort_IDS.pl: Similar to Watersort.pl, except that it abandons the idea of verifying whether an initial configuration can or cannot lead to a solution state, and instead resorts to using a depth limit on the search. Also uses a succesor state axiom to reason.
2. Watersort_2.pl: Our solution for the program without making use of a successor state axiom. Probably how we would have solved the problem if it were given to us and we were requested to write a program to solve it using Prolog without any additional requirements.


Watersort_2.pl and Watersort.pl are equivalent in terms of their ability to solve the problem (produce and verify solutions). Watersort_IDS.pl suffers from the drawback of being unable to verify solutions longer than the depth limit that was discussed in section 4.1.2 of the report on the shortcoming of a depth limit.
