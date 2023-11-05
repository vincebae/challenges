
## Write Your Own JSON Parser
Coding challenge from https://codingchallenges.fyi/challenges/challenge-json-parser

This implementation will parser JSON file and convert to EDN file.

#### Requirements

* clojure
* babashka

#### Usage

`$ ./j2e.clj [OPTION]... [FILE]...`

When no file is specified, read from stdio.
Linux pipe can be used like

`$ cat [FILE] | ./j2e.clj`


#### Run Tests

`$ bb test`

