- ~~fix shift/reduce conflict~~ conflict is as follows:
```
type:<type:<type>>
                ^^ -- rshift or gt gt?
```
  maybe raise an exception? like so:
```
test.cpp:6:30: error: a space is required between consecutive right angle brackets
      (use '> >')
  std::vector<std::vector<int>> a;
                             ^~
                             > >
1 error generated.
```
