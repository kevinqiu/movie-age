Sort of does what was specified, takes a txt file of freebase mIds(seperated by newlines) for movies and sometimes spits out the right answer. Thought this satisfied some of the spirit of the test without mucking about with messy data collection. Pretty unfinished still though. Will fail to process some mIds without saying why. Some things are hacky and inelegant. Oh well, perhaps I will have some spare time in a bit....

To run(in project root dir):

sbt compile
sbt run
