## Write Your Own wc Tool (Clojure)
Coding challenge from https://codingchallenges.fyi/challenges/challenge-wc

#### Status

Done.

#### Requirements

* [babashka](https://babashka.org/)

#### Usage

`$ ./wc [OPTION]... [FILE]...`

When no file is specified, read from stdio.
Linux pipe can be used like

`$ cat [FILE] | ./wc`


#### Run Tests

`$ bb test`

