Just some reminders:

This hierarchy has been found to simplify running Forth
applications across multiple vendors and operation system
environments. The 'platform files' provide a common environment
(available words) and the use of 'version paths' allow the
'current working directory' to be set once, to the 'make
folder.'

The batch files will need the path to the application executable
changed to match your environment:

	make.gforth.bat
	make.swiftforth.bat

  Edit the path to the application and save the file.
  Then double clicking the batch file will run the compile.

The version file will need the paths to the various root folders
changed when you change their location relative to this folder.

The load files will need the path to the platform folder changed
when you change the its location relative to this folder. They
will need the name and path to the project file changed when you
reuse this folder for other projects.

