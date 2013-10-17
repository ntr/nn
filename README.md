nn
==

###Summary###

Neural network implementation with backpropagation written on F#. Logistic regression included.

===
### Structure ###
Solution consists of the following projects: 

**NNCore** - implementation of neural network itself.

**NNTrain** - console application that trains neural network. Can be considered as usage example on F

**NumbersDemo** - ui application that demonstrates handwritten numbers recognition (numbers from 0 to 4 are supported due to lack of training data). Project uses previously trained data. Use full canvas to get proper number prediction.

###Building/Starting###

Before building project you should restore Nuget packages referenced in solution. 
