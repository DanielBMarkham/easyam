#EasyAM
##A Cucumber-like tool for Structured Analysis.
The idea is that information is entered in mostly free-text form in one place. It is then compiled and delivered to many places and target formats.

The goal is OHIO, Only Handle Information Once, as close to business intent as possible, then deploy, mix and match that same information everywhere else it's used.
The tool compiles all files from a source directory (except for its own files) This means that information can be captured by multiple people/teams on multiple days -- and can be organized however the organization wishes.
Some sample deployment scenarios:
- Domain models created in SVG format for being used online (in-progress)
- Cucumber .feature files created (planned)
- Question lists organized by either structure or domain (planned)
- Supplementals compiled into associated Cucumber feature files

##Usage
"easyam" in a directory of your choice that includes Analysis information. In an empty directory it creates the target directories Behavior, Structure, Supplemental, and Meta directories.
