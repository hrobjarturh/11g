This readme assumes that the user uses a unix terminal such as those on linux or macOS.

First, open the terminal and find the folder located from the zip file. From here, you can translate the library file by the following command:

	$ fsharpc -a preditorPrey.fs

Now it is possible to execute the file simulate.fsx by running the following command:

	$ fsharpc -r preditorPrey.dll simulate.fsx && mono simulate.exe

Now the terminal will ask you to enter the desired parameters for the simulation.
After the program has been executed, the result of the simulation is found in a .txt file called simulation.txt. The file can be found in the same folder as the source code.