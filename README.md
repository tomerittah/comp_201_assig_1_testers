# Compiler Principles - 2020/Fall Assignment 1 testers

### Prerequisites - HOME
- Install external system dependency M4 - `$ sudo apt install m4`
- Install oUnit using OPAM with non root user - `$ opam install ounit`
- Source opam config with non root user -  `$ eval $(opam config env)`

### Prerequisites - LAB
- Enter bash shell - `$ bash`
- Initialize opam state (if not have already been Initialized) - `$ opam init`
- Install oUnit using OPAM - `$ opam install ounit`
- Source opam config -  `$ eval $(opam config env)`

### Files required
- Clone the repo - `$ git clone https://github.com/tomerittah/comp_201_assig_1_testers.git`
- Copy reader.ml & pc.ml (and any project relevant files) to the clone directory

### Execution
- Single test - `$ ocaml <TEST_NAME.ml>`
- All tests - `$ for test in $(ls | grep -i test_); do ocaml $test; done`

### F.A.Q
- When executing test it raises "No such package: oUnit"
  - verify you are with non root user.
  - run `$ eval $(opam config env)`
- oUnit failed to install with opam - verify m4 package is installed

### Contribution
- You are welcome to contribute more tests. Ask to be added as collaborator.
- Although we have separated tests to a different units, we still calls inside to the Reader module functions.
