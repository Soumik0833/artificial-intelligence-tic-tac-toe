## Welcome to a game of TicTacToe

### Before running this game, you need to do a couple of things. (Note that this instructions are for windows users as I have a windows platform)
1. Place the folder "TicTac" in any of your drives. For ex., in C:/Games/TicTac
2. Now go to your path variables (Windows Users - https://www.computerhope.com/issues/ch000549.htm , Mac Users - https://www.pegaxchange.com/2017/06/17/mac-os-x-path-variable/ )
3. Set the "bin" folder in swipl as your new path variable. ( For windows, an example would be setting this as your path variable C:/Games/TicTac/swipl/bin

###  Why is doing the above necessary?
1. The program uses pyswip, which in turn uses SWI-Prolog shared library to work (as seen on windows). So if you dont do that, you might find some error like [FATAL ERROR: Could not find system resources]. More info about this - http://www.swi-prolog.org/FAQ/FindResources.html 
2. If after doing all the above steps, the program still doesn't run, send an email to soumik@minerva.kgi.edu with the description of your problem and I will try to fix it.