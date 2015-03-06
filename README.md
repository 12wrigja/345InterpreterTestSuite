# 345InterpreterTestSuite

To run this test suite, edit the import at the top of testsystem.scm to be your own file.
Then, to run a series of tests, call

(runtests 1 18)

where the numbers define a range of test#.lang files in the tests folder that 
contain the input you want interpreted by the interpreter. So for example, to run the 2nd through 5th tests, call

(runtests 2 5)

