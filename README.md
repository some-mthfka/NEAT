# NEAT
Common Lisp Implementation of NeuroEvolution of Augmenting Topologies (NEAT)

(scroll to the bottom for demonstration runs)

The Lisp package described here was developed according to a technique devised by Stanley and Miikkulainen in a paper called Evolving Neural Networks through Augmenting Topologies. A copy of the paper can be obtained here: http://nn.cs.utexas.edu/downloads/papers/stanley.ec02.pdf 

Please note that the purpose of this documentation is not to describe how NEAT works, but rather to:
  1.	Explain how to use the package,
  2.	Point out the deviations from the original paper, and
  3.	Describe the file structure of the package.

## Usage
For an example, take a look at _xor-experiment-parameters.lisp_ and _xor-test.lisp_.

### Writing an experiment file from scratch
1. Load experiment parameters file (e.g. xor-experiment-parameters.lisp).
2. Load the package: 

  (ql:quickload :cl-neat)
  
  Loading through asdf probably works too.
3.	Define fitness function that takes a network as its only argument and returns fitness as float equal or greater than zero. 
 * If the function is written in Lisp then it must be named experiment-evaluate-network. Taking a look at xor-test.lisp, it can be seen that activate-network-with-inputs takes a network with inputs and returns a list of outputs.
 * If the fitness function is written in language other than Lisp but can be accessed through C interface, it can be loaded like this (more on this later):
 (load-shared-object "evaluator.dll")
4. Provide the starter genome off of which the whole population will be spawned. The starter genome should be minimal: the network will grow over time as needed.

### Running the experiment
Now, after loading your experiment file (like _xor-test.lisp_), it is possible to start the experiment with _(start)_. There are several optional arguments used for saving and/or loading that the function _start_ can take:

`(defun start (&optional experiment-id read-generation-file) ...)`

_experiment-id_ is the id of the experiment which will frame the save-folder's name. Not supplying anything means not saving results. Supplying a number will create a folder named `exp-<number>` and the winner organisms will be saved there. It is necessary for saving state that the global variable *save-every-generation* be specified (it is in _experiment-parameters.lisp_).

If experiment was saved to a file, it can be read back from read-generation-file. 

NOTE: _experiment-id_ has nothing to do with _read-generation-file_! _experiment-id_ is used only for saving a population. So, _read-generation-file_ needs to contain relative path to the experiment file including all the folders (absolute path will work too).

### Example calls

`(start nil "exp-5/gen-10.gen")`

will load the generation from the specified file and will save nothing nowhere.

`(start 6)`

Will start a generation from scratch and will save results to folder called "exp_6" every *save-every-generation* times.

`(start 6 "exp-5/gen-10.gen")`

will load the generation from folder exp_5 and save results in "exp_6".

`(start 5 "exp-5/gen-10.gen")`

will load the generation from "exp_5" and save results to "exp_5" while renaming the already existing population files.

## Modifications
The one and only modification (as far as my vision goes) is the aging scheme, which the paper did not address. The C++ implementation by Stanley did include a variation of aging, but this package offers a different kind. The main idea is that a species is given a fitness boost depending on its age. The younger species are given boost in terms of adjusted fitness. The older the species, the less boost it gets. The organisms in the oldest species get no boost at all. The adjusted fitness value of a species for age-boosting is defined by this relationship:

![Aging formula](https://github.com/meatich/NEAT/blob/master/aging_formula.gif "Aging formula")

where s is adjusted fitness, A is the age of the oldest species, g is age significance (*age-significance* in the package). The age of a species is reset to zero when some organism of that species beats the best record of actual fitness inside that species.

Also, ability to cap the maximum amount of species per population is implemented. So, when a cap is reached, if some new organism is not compatible with any of the existing species in the population, instead of creating a new species, it is assigned to the one it is most compatible with.

## Package File Structure
*	_xor-experiment-parameters.lisp_: contains all the global parameters needed for the XOR experiment.
*	_pln-experiment-parameters.lisp_: contains all the global parameters needed for the PLN experiment.
*	_foreign-evaluator.lisp_
Uses foreign function interface to call a fitness function written in a different language and has C interface. One function should be featured in the interface:

`float c_evalme(EncodedNeuron n[MAX_NEURONS_PER_NETWORK],int neuron_count);`

where _EncodedNeuron_ is a struct:

```
struct EncodedNeuron
{
    int id;
    int type; //0 - input, 1 - hidden, 2 - output
    int out_nodes[MAX_NEURONS_PER_NETWORK];
    float out_weights[MAX_NEURONS_PER_NETWORK];
    int out_count;
};
```
 A shared library should be provided and loaded in foreign-evaluator.lisp: 

`(load-shared-object "evaluator.dll")`

The call to such a fitness function would be quite simple after loading:
```
(defun experiment-evaluate-network (network)
  (call-evalme network))
```
*	_genome.lisp_: defines genome and manipulation functions.
*	_globals.lisp_: contains some global variables majorly used for constructing starter genome. Could be eliminated in the future. 
*	_population.lisp_: main file loading everything and defining population, organisms, species and start function.
*	_network.lisp_: neural network code.
*	_pln-test.lisp_: pln-experiment test, uses foreign function interface.
*	_xor-test.lisp_: defines XOR experiment.
*	_printers.lisp_: function for saving and printing info about population or organisms.
*	_starter.lisp_: functions for constructing intitial population or reading one from file.

## Demo
### XOR-function
A sample run that evolves a network to behave like a XOR-function (which requires at least one hidden node):

![Evolving network to be XOR-like](https://github.com/meatich/NEAT/blob/master/sshot-251.png "Evolving network to be XOR-like")

Notes to the picture:
 * Population size: 150 organisms
 * When a network's output is rounded to the nearest integer and matches real XOR outputs, the fitness of the function is set to maximum fitness (sixteen) and the experiment ends.

With the parameters picked by trial and error, the average solution takes about 35 generations to evolve, which is not far from the results in the paper: 32 generations on average. The parameters could be tailored a bit better probably.

### Evolving for a physical-based simulation
A special [simulation](https://github.com/meatich/PLN-Simulation) was written for a performance test - a ball bounces on a function and the goal is to push the ball as far as possible to the right in three seconds. Here is how NEAT performed on it (youtube video):

[![Video](http://img.youtube.com/vi/R9je0GhgaRg/0.jpg)](https://www.youtube.com/watch?v=R9je0GhgaRg)

Notes to the video:
 * Fitness starts at 2000 so that there is no way for it to be negative (though I _think_ fitness could be negative)
 * More generations would produce better results. **I will post another video with those better results (and maybe even a more interesting simulation) by 2017.**
