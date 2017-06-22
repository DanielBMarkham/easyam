#EasyAM
##A Cucumber-like tool for Structured Analysis.

Analysis is the part of the project that you have to understand that's not telling you exactly what to do.

If the project work were filling a hole, analysis is describing everything around the hole. It begins with observations and questions, proceeds through more questions, and ends up with a series of executable tests that exactly describe the hole to be filled.

Analysis is the most important part of any project. It's the "why"

Unstructured analysis, the type any three-year-old can perform, consists of asking any questions that cross your mind until you know enough to accomplish what you want to.

Structured analysis is tagging information and questions so that dozens of people of having hundreds of conversations can organize it all so that things aren't repeated, conflicting information is easily identified, and all relevant information (and only relevant information) is available to people when they need it.

Just like cucumber lets you enter business needs in almost freeform text, easyam lets you interview, follow-up, and organize all business conversations in the same kind of "almost freeform text"

It is OHIO, Only Handle Information Once, as close to business intent as possible, then deploy, mix and match that same information everywhere else it's used.

It is a command-line, text-based, source-controlled system for accepting and storing all of your project information.

So, for example, questions about a user story the team has for an upcoming sprint will be automatically associated with the business intent and conversations around that. Conversations and answers about higher-level features that affect that team's user stories will automatically flow to where they are needed.

Release planning can now automatically include the consideration of bugs and feature requests around the areas being considered.

Bug reports automatically go to the people with the most expertise in that area of the system.

Static analysis tools provide hints to which features to develop ahead of others -- the ones that address the most architectural risk and customer value.

Done correctly, seven or eight 200-300-line structured analysis files contain the same information as 50 to 100 times that amount using traditional formats

##Usage

The easyam compiler *is* the code. The default executable takes a target directory and compiles all of the files in that directory ending in .amin

In the target directory it creates a set of files with the .amout format

These files can be reused. The system eats its own output. That is, whatever the program puts out, it can also consume.

All of these files representing conversations, work, and questions can be accessed, changed, and compiled directly alongside all of the other technology in the project, using tools like git, subversion, Maven, etc. Easyam should be part of every project's (and enterprise's) build pipeline.

Note that the compilation process enforces rules on the notes. It is possible to "break" the analysis build. For example, nobody should be allowed to add a thousand items to any team's backlog.

It is expected that dozens of new tools can use this code to take easyam formatted files and create enterprise products: team/program backlogs, release plans, feature team groupings, prioritized backlogs based on business impact, bug reports prioritized by customer value, user story cards, Lean Startup canvases, PI planning cards, etc.

