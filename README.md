# marmalade

--------------------------
BACKGROUND
--------------------------

Programming Languages and Translators (COMS 4115)
Professor Stephen A. Edwards
Fall 2015

The Marmalade Group, Inc.

    Cathy Jin         -   System Architect
    Raphael Norwitz   -   Manager
    Savvas Petridis   -   Language Guru
    Uzo Amuzie        -   Tester

--------------------------
Running the Compiler
--------------------------

$ make 
$ ./make_java.sh
$ ./marmac name.marm executable_name
$ ./executable_name

It’s that easy!  

marmac is a bash script which calls an executable named ‘marmlade’ created by our compiler (marmalade.ml). It takes in 2 arguments: a marmalade file and the name of the executable to be created. 

→ Sample programs are available in the “marmalade_sample_programs” directory


--------------------------
REGRESSION TEST SUITE
--------------------------

./run_tests.sh	in project TLD (marmalade)


--------------------------
DEPENDENCIES
--------------------------

jMusic JAR file


