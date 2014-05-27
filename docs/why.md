

### Goal

Let's set a goal. We need to find a way to declare structures which define both
data and behaviors. The declarations should be understandable through the human
perspective, yet unambiguous and precise. They should model the real world, so
that people may construct and destroy and anlayze these structures with
intuition and cognitive instinct. We would say that the structures exhibit
naturality. They look and behave predictably, made for human hands and human
eyes.


programming about defining structures
properties of Elea structures
human intuitive but machine interpretable
universal, integrated, distributed, concurrent.
easily modifiable, minimal dependences. all information is viewale from anywhere
strongly typed. interfaced.
reactive behaviors



-- summary paragraph of language attributes

## Naturality

-- talk about what is structure
set goal, want to find structures suitable for naturality
how to interpret structures, human vs machine.
ambiguity.
more examples of trouble between human machine interps.
project management. risk of changing requirements. agile.
how does human mind work, human hands are the human mind.
refine goal: ideally, want human interpretable machine language.
that is, intutive to humans, but still prcise and unambiguous
no perfect easy solution.
that way anyone can describe program and that description can
be given to a computer without major modification.
talk about how other languages try to accomplish that. oop, functional languages
etc
models for defining structures. math vs architectural, etc..
look at biology, physics, etc..
talk about Elea's solution.
concurrenct. reactive. simple.

the challenge of the programmer is two fold. Must understand the machine
specifications, not always easy to translate human ideas into something that the
machine can interpret. very often non-trivial and difficult. And that's only
when the human ideas are correct. Often times the ambiguity can lead to
incorrect specifications, and all the work done in translating is lost because
the wrong thing was translated. biggest risk of software is these changing
requirements. can be eliminated if translation is trivial. Make the job of the
programmer more like an artist than a translator. Gets to express ideas
directly, does not have to indirectly express ideas and spend so much time on
understanding how the machine works. Spend time with human ideas.

this is reason that I created elea in first place. was working on game deve.
have many creative ideas to express, but always limited by expressing those
ideas in way computer can understand. Haskell helps but not always enough. can
do better. should also empower others to be able to do the same. Found elea by
accident, evolved a lot. took all good ideas and put into one. Is really about
finding way to define structures optimally for human expression. Elea is a
language for humans, not machines.

## Integration

Elea is an actual general purpose programming language. Programs are not
localized, just like in the real world. Values are universal by default,
processes are concurrent by default. Every system automatically has a web api.
Programs are always in context. Systems are automatically distributed. This is
why values and systems are given UUIDs. A system can exist at multiple
locations. This allows us to build real-world programs intuitively. We do not
need to differentate between code and where it exists or how it interfaces. WE
can build programs that exist across real world barriers easily.


## Purpose


A programming language provides the means to specify data and data
transformations. There are many, many ways to do this. Evidence for that
statement includes the large number of programming languages currently in
commericial and academic use, all of which enable the users to declare data and
program behaviors in different ways. In additional, the generality of the first
sentence can be interpreted to include forms not commonly denoted as
*programming languages*. All together, would-be programmers can choose from
hundreds of tools to declare data and various behaviors of that data.

Spreadsheets are the ultimate example of these alternative forms of programming.
Spreadsheets permit users to define data in the form of two-dimensional arrays
and provide the ability to manipulate data in this form in numerous ways such as
sorting, coloring, mathematical calculations, and so on. They provide a specific
model of programming which suites many applications, and most spreadsheet users
do not even know that they are programmers.

Elea is a programming language intended to be used by anyone, with or without a
degree in computer science, prior programming experience, appreciation for
mathematics, time, resources, money, education, etc... It aims to be like
spreadsheets, in the manner that it should be intuitve and accessible.
Furthermore, it aims to be more powerful that spreadsheets, bringing the ability
to construct software to anyone with an interest. Not just anyone with an
interest AND x, where x is some limiting factor that just comes with life and
should not get in the way of enabling someone to build software. A world
controlled by software that no one can program, is like a world made of toxic
materials that only a few have immunity to. It's prohibitive and potentially
dangerous.

So how does a programming language that acheives this goal allow users to
specify data and behaviors? We should start by looking at the different ways to
specify data and write transformations over data, and then the direction will be
much clearer. To do so, we will think about the structure of the universe, the
human mind, the current state of programming, and the nature of computuation. 

There are many ways to describe structures in our universe. Here's an arbitrary
list of terms: graphs, trees, lists, spreadsheets, products, sums, nesting,
order, heirachical, concurrent, parameterized, encapsulated, and so on. We might
list terms like this all day. In the end, we must consider what they mean.
Meaning of these terms can only be ascribed through interpretation. To gain
insight into how we specify data now, let's contrast two main types of
interpretation, which we shall denote as machine and human interpretations.

Human interpretations basically just means that a person inteprets it. Simply
having an understanding of some structure is an interpretation of it. A person
knows what a list is, and as such, can use a list. Machine interpretations are
those performed by a traditional computer, the kind we use everyday (yes, there
are others). The primary difference between the two modes of interpretation is
ambiguity. Humans can interpret structures which are vague or not precisely
defined. We have an understanding of the world as humans that enables us to
leave out information when we communicate. When we interpret structures, we
almost always make assumptions. These assumptions enable us to function fluidly
in the real world. Computers though do not make assumptions, they can interpret
ambiguous structures. Computations fail without information.

Modern programmers are translators, taking structures interpreted by humans and
translating them into structures capable of computer interpretation. We call
this programming. We are humans, so of course we always start with structures
that are interpreted by humans. In order to take our ideas and give them to
computers to use, we must remove the ambiguity in them by translating them into
a language which computers can understand, that which is precise and
unambiguous. When I say programmer, I still mean it in the general sense
ascribed in the initial paragraph.

Here is a real world example of this dichotomy of interpretations. I work with
healthcare software during a transitional period from paper charts to electronic
charts. Many challenges arise. They electronic system may be slower, it may be
difficult to use at times, and its use by be limited, forcing the users into
somewhat convoluted or inefficient workflows. The ultimate challenge though
comes when building the old paper-based workflows into the electronic system. By
doing so, we take structures (the workflows made of forms, procedures, etc..)
which were built only for human interpretation and must design them for computer
interpration, which means removing all of the ambiguities, of which there are
often many. When dealing with paper data, clinicians can leave out information
and make assumptions that other clinicians can figure out. The computer does not
work this way, and requires that all information be clearly defined and present
in every case. Utlimately, the act of building these workflows into the
electronic environment required going back and actually rebuilding the workflows
themselves because they were never clearly defined to begin with. People were
able to function without clear definitions. Still, assumptions and incomplete
information are never a good thing, especially in a patient care context.  The
electronic workflows are superior by being unambiguous. It was the nature of the
software and the way its interpreted that forced us to realize these errors in
the nature of humans to begin with. 

[TODO]


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

Let computers do the hard work. Gives structure to the programmer to make
programs easier to understand and more obvious how to create.

[TODO]


