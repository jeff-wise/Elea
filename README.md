# Elea Language Specification (Notes)

*Preliminary notes* on the language design. Concepts are still changing, so
descriptions are purposely vague and brief.

Elea is a programming language for anyone. Like object-oriented programming, it
appeals to human intuition. It's *natural* specifications abstract away from
machine architectures and mathematics, providing a reactive framework for
defining data in terms of systems and behaviors. Its programming structures
resemble biological processes more closely than traditional computational
processes. Most importantly, it is a fully integrated, general purpose language.
All systems are automatically given web APIs. Values and systems are given
UUIDs, such that systems are automatically distributed.  All processes are
concurrent. Programs are universal by default, just like any real world
constructions. Elea is a very different, unconventional language with the ultimate
goal of enabling anyone to program and construct complex software with minimal
difficulty. Elea is a language for humans, not machines.

## How to Run

Right now there is not much to see. Most of the ideas are still mostly within
the source code.

#####Test Suite
`cabal run testsuite`

#####Paltry Example
`cabal run example-basic`


## Components


### Values

There is only one type of value in Elea (values are untyped or uni-typed). This
is similar to JSON in Javascript and the structure of values in Elea is very
similar.

The value type is intended to represent fundamental values -- the kind
of information which is of ontological value to humans. By this I mean types of
data which are part of human functioning. For example, we need both text and
numeric values. Either could be encoded into the other, but it's not possible
for someone to process each in certain contexts as one type of value. The ways
in which we think and manipulate text and numbers are fundamental different, and
in many ways unique to the human condition. This is the point of naturality.

Nevertheless, this goal has been difficult... Because of it I have eliminated
symbols as a variant of the value type, after spending *a lot* of time fully
implementing them (in a few different ways) into the language.

I am still debating on whether values will be extensible i.e. higher level
values will be possible. Examples of higher-level values: statistical
distributions, graphs (as in *graph theory*), and images. In any case, a
datetime value will be included soon as a default. Philosophical, what values
are in Elea, is one of the most difficult ideas to decide on.


### Types


### Lens

A lens is a reference to a part of a value.

### Force

A force is some type of system modification. 

#### Synthesis

For now the main force is Synthesis. Synthesis is a process of composing
multiple values into a new value. Parts of the new value are then mapped into
some systems. Values are transformed by different kinds of Transformers.


### Transformer

Transformers are functions, they take in multiple input values and return a
single output value. Transformers represent general types of computations. For
example, if one wanted to write a mathematical function, then one would use the
Equation transformer, which is just a way to specify an equation with
mathematical operations. This way, users can write domain specific data
transformations as single entities. 

The two most important transformers (these may at some point be differentiated)
are queries and templates. Templates just take the values and place them at some
points in a pre-defined structure. Queries look for values (of particles) of
some type in some system and create a value which is a collection of those
disocovered values (therefore they currently take no input values).

Transformers are intended to be used for single-purpose linear computations
performed in some domain, which are encapsulated in the synthesis. Logic,
branching, and composition of values should be handled via receptors and action
potentials, where those ideas are best represented. It would be silly to have to
define an equation with receptors and APs. It also allows for a more fluid
interface where a user will be able to write an equation with an equation editor
designed with the mathematical domain in mind. That interface would be separate
from the others and could be tuned specifically to its purpose. For purposes of
data flow, an equation just maps input to output -- it's a transformer. If parts
or intermediate results of that equation were needed by other processes, then it
would be factored out into separate equations, where those values would be
visible to receptors.


### Particle

For now particles are just wrappers around values. They represent any data in
the system.

### Receptor

Receptors represent the logic in an Elea program. They are more or less wrappers
around types. A receptor is **triggered** when a particle in the same system is
created and that particle's value is of the type of the receptor. The receptor
fires a signal, which is sent to relevant action potentials, indicating that a
value of the receptor's type was created. The created value is associated with
the signal.

Receptors are inspired by biochemical receptors.

### Action Potential

Action Potentials are similar to functions in other languages. An action
potential waits on multiple signals and then it is **activated**. When an action
potential is activated, it causes a set of forces to be evaluated. A single action
potential may have multiple concurrent instances, called events. Each event
waits on its signals before activating. Events are sorted/mapped by the values of the
signals.



Like receptors, action potentials are named after a biological mechanism. The
corresponce is loose, refering mostly to the idea of a buildup of *potential*
and eventually reaching a threshold whereby something *interesting* happens. In
this case, some forces.



### System

Systems in Elea are similar in some ways to classes in Java. They represent
some domain which stores and reacts to data, possibly encapsulating some inner
state and side effects. The systems are pre-defined entities, which are mostly
static except for their particles. When the module/package system is more
well-defined, it will be clear that systems are active processes.  They may
exist on a separate server, or function as servers or databases or processing
engines.

A system is like a cell that is part of a larger entity. It has an isolated
interior, such that signals do not propogate outward. It also has a permeable
membrane in the form of constraints (not yet implemented). A system has the
capability to transform input and send it somewhere else. These transformations
are determined by the shape/structure of received particles.



## Dynamics

![System](docs/image/system.png?raw=true)

When a particle is created in a system, the receptors of that system may react
to it. A receptor's behavior is determined by the particle's shape (type). If
the new particle has a structure that matches the receptor, then the receptor
fires a signal which may be received throughout the system.

The receptors therefore determine the logic of programs declaratively. Given
some data, one says that if this data exists and has this exact form, then this
reaction must occur. The result of a particle A in some system M, is therefore
determined by the existence of some particles A, B, C... of some types T(A),
T(B), T(C),.. in some system N. We don't worry about constructs to provide
branching or conditional evaluation in Elea, we only worry about the data. The
transformations are implicit. The existence of some data of some form *implies*
the existence of some other data of some forms.

This type of reactive programming works at the big and small level. We define
the data we want, and which signals are triggered by which data, and then we let
the system figure out how to behave. This permits complex emergent behaviors to
arise out of simple and readable sets of inference rules.

Signals are sent to the action potentials that receive them. Action potentials
at runtime function like concurrent procedures which have as parameters
synchronizing variables. When all of a procedure's parameters have been *filled*
then it is executed. Parameters are identified as signal names and are
populated by the value which triggered the signal. Therefore, each time a
procedure runs, a new pariticle exists for each parameter of the procedure.

These procedures are called events and are processed concurrent per action
potential. Events are multiplexed by a pre-defined event class, since the same
kind of event could occur for different reasons simultaneously. Suppose an event
is related to some user information. Information about multiple users could be
received concurrently, so they events should be processed concurrent. Events are
mapped by the specified class, which is just a reference to part of the signal
value.

![Event Maps](docs/image/events.png?raw=true)

The most important undecided aspect of the semantics is currently the memory
management model. Still researching some ideas for an explicit model, but one
which can automatically make the right decisions most of the time. The main
concept would be function groups, which are like miniature systems which exist
to do one thing, and all of the particles created in the process are disposed
afterwards (basically like a normal function, but Elea doesn't normally have
normal functions).


The main focus of these experimental language dynamics are to improve
scalability and maintability of systems. Because the behavior is declarative,
reactive and concurrent, it should be easy to make changes to an extant system
simply by adding receptors and tweaking action potentials, and adding any new
forces to them if necessary. Dependencies still exist and will have to be
resolved if changed, but it is easier to redirect signals and redesign receptor
forms than to modify modules and rearrange functions

I am still figuring how to program with this model, but the following diagram is
an example of the general idea.

![Reactive Programming Example](docs/image/reactive.png?raw=true)

Now imagine making modifications to this game.

In most recent programming langauges, the greatest improvement to program
maintainability has been through program verification (strong typing). Once
constraints and proofs are finalized, this should ensure both easy and correct
program modification.


## Statics/Verification



