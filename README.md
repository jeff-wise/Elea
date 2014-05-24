# Elea Language Specification (Notes)

*Preliminary notes* on the language design.


## Purpose


Talk about how all data is accessible in an application. all that matters is
invariants. Of course, invariants can prove access controls if that is
necessary.

The programming process may be deconstructed into two independent processes:

 * Specification
 * Translation

Specification is design -- all of the details of the required components,
interfaces, and properties of the program. Conceptually, the specification is
the program.

Translation is the process of transforming a specification into a machine
executable format. The typical conception of programming is translation. It is
the act of the software engineer to take a set of requirements and construct a
collection of code modules which implements those requirements. Requirements are
usually in a *human* language, often with some added structure to reflect
dependencies, priorities, etc... 

Programmers glorify translation and underestimate specification. Most often, the
people doing the specification do not do the translation, but this has been
improving with agile methods. Translation is unnecessary in an ideal world. The
person who wants a program describes the program he or she wants, and that
description is machine executable. 

As an example of how translation is over-emphasized, examine the idea that one
day we could speak to a computer about the program we want, and it would build
that program for us. This is impossible. Any program that is even a little
interesting would be far too complex to declare verbally. In fact, the act of
*declaring* any program is equivalent to constructing that program. The
difference is in the execution of that program.

If the specification is complete, then it may be executed by any means --
perhaps a person manually performs the interpretation. The program could be like
a traditional piece of software, but it could also be something like a workplace
training procedure or recipe for dinner or a plan for some project. Translation
is concerned with the execution of the program, which is fundamentally different
than the program itself. A program could be executed in different environments,
on different mediums, and with different execution properties.

The difference between specification and translation appears in the debate of
declarative languages versus imperative languages. Declarative languages,
especially lazy functional ones, focus on the specification of a program. They
admit that the difficultly in programming is not primarily in execution, but in
just writing that program out. Without a specification, there is nothing to
execute.

Because declarative languages focus in specifying what and not how, they do not
conflate how to execute a program and what the program does (as much).
Complexity becomes easier to handle. Composition is a more powerful tool.
Well-defined and elegant mathematical abstractions play a more fundamental role
in design, because they aren't forced into any particular computational model.

Consider two recipes:

 1. Chop vegetables.
 2. Fry vegetables.

 1. Pick up knife
 2. Heat a pan.
 3. Place knife over vegetables, press downwards, lift up, move the knife over.
    Repeat.
 4. Place oil in pan.
 5. Pick up vegetables.
 6. Put vegetables in pan.

Any professional chef could be given the first list, and follow the steps. The
chef would do so idiosyncratically, according to his or her personal techniques,
experience, styles, and tastes. Given the second list, most chefs might be
frustrated. It's almost insulting that it tries to tell the chef *how* to do her
job, as opposed to what to do. The job could be done many ways with similar
results.  The essense of fried vegetables is in the chopping and the pan, not
the movements of the knife, the hands, or the placement of the vegetables.

The key point is that we abstract over the execution of the cooking and focus on
the components of the result. We need vegetables and frying. How those
components are executed is a function of themselves, but is still a function, it
may vary and is determined by the components, not vice versa. When working with
computation, we should focus on the same types of general abstractions. When
designing something, worry only about the composition of the processes and data,
not how in the real world we instantiate those processes and data. The real
world is **very** important, but is necessarily a secondary concern.

A specification is a real description of some data transformation process, but
it does not transform data. In order to transform data, the specification must
be made executable. A primary goal in developing programming languages is to
make the specification form as close to the executable form as possible.

The largest risk in software development is incorrect requirements. If one could
write executable specifications, then this risk would be nearly non-existent,
because one could simply write the requirements out and then immediately see if
they are what was really desired. The programmers (or the translators) would be
only needed on occasion, when it came time to figure out the specific
requirements for the program's execution, in particular space/time usages and
the real world resources to support those.

There will always be some translation required. Execution requires one to take
into account the environment and available resources. Execution time and space
are always issues, as are the underlying data structures and algorithms. Of
course, given some specification, these execution properties could be tweaked.
Then some specification could be run in different contexts without modifying it.
In most current software, this separation is not pragmatic. But it is ideal.


### Naturality

The intent of the language is to take advantage of human intuition for reasoning
about the behavior of structures and transformations. People “program” in the
real world everyday, yet programming in the world of computing is still very
difficult. There are learning curves to understand the idioms of computing, and
in addition, the level of abstraction is still much lower than it could be.
Knowledge of computer science is still necessary to build non-trivial software,
but it doesn’t need to be. Personally, I think computer science should be a
common curriculum, but given a huge population of diverse interests and varying
backgrounds with varying qualities of education and social status, software
development should be possible independent of all of these factors. Languages
such as Haskell and Agda succeed by abstracting computation to the level of
mathematics. This enables programming design patterns to simply be
instantiations of well-founded mathematical principles, like functors or
monoids. 

Elea intends to abstract computation to the level of human thought processes. In
philosophy of the mind, these processes are often referred to with a folk
prefix, such as folk-physics are folk-psychology, to refer to a colloquial
understanding of complex processes. For the most part, these casual
understandings are very effective. Most people don’t understand the
equations/laws of physics, but they are still very adept at building things,
moving things, and making predictions about events. Here we are interested in
folk-computation, the way in which someone normally thinks about data and the
ways in which it can be modified. Elea operates at that level, to the best of
its ability.

Living is essentially a computational experience. All human constructions are
programs which transform real world input to real world output. These
constructions could be physical such as a vehicle or a drug, or even
sociological, such as a company or project. Either way, most people are adept at
constructing computational processes, without knowing it. Therefore, Elea
intends utlimately to be more than a language. It will be a framework for any
type of representational construction, that is, a structure which is intended to
operate over properties of real world entities, but not on those real world
entities directly. [Needs expanding]


### Simplicity

Simplicity plays an important role by allowing computer augmentation to play a
larger role in software development. Because both the language components and
its semantics are simple, there are opportunties for easy analysis of the
programs. Programming can only be made simple to a point, but if its designed in
a way such that programs themselves are easily subject to analysis, we can
always write procedures to assist users in understanding their own programs.
Large programs are difficult because no one can fit into their head a picture of
everything going on at once. Computers can provide summaries of a program’s
behavior, from different perspectives, to allow programmers to quickly
understand the programmable environment they are in the process of modifying.


### Scalability

[TODO]

### Verifiability

[TODO]


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
datetime value will be included soon as a default. 


### Types


### Lens

A lens is a reference to a part of a value. They are the primary mechanism for
*destructing* values. They are analog

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
from the others and could be tuned specifical to its purpose. For purposes of
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
some domain which stores and reacts to data, possible encapsulating some inner
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



