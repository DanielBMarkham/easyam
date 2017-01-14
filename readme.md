#EasyAM
##A Cucumber-like tool for Structured Analysis.

How many times has an important business conversation happened without that information getting to the people who needed it?

Easyam allows you to enter in mostly unstructured text notes about what the business needs and why, along with open questions and to-do items, then have those notes compiled using easyam and associated tools into the rest of your DevOps pipeline.)

The goal is OHIO, Only Handle Information Once, as close to business intent as possible, then deploy, mix and match that same information everywhere else it's used.

It is a command-line system for accepting and storing all of your project information

So, for example, questions about a user story the team has for an upcoming sprint will be automatically associated with the business intent and conversations around that.

Release planning can now automatically include the consideration of bugs and feature requests around the areas being considered.

Bug reports automatically go to the people with the most expertise in that area of the system.

Static analysis tools can provide hints to which features to develop ahead of others -- the ones that address the most architectural risk and customer value.

Dashboards and Information Radiators can be created and automatically updated based on tool output.

##Usage
"easyam" in a directory of your choice that includes Analysis information. In an empty directory it creates the target directories Behavior, Structure, Supplemental, and Meta directories.

It will compile everything in that directory and subdirectories with the ".amin" extension. Consolidated files will be created with an ".amout" extension. 

Regular code tools can be used with these notes: git, CI/CD tools, etc.

Note that the compilation process enforces rules on the notes. It is possible to "break" the analysis build. Good practices would be checking out notes (or creating a new note), adding/modifying, then compiling the results to see if any conflicts were created. Future versions may add realtime compile-as-you type capability.
