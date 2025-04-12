#set text(font: "TTLivretText-Rg", size: 10pt)
#show emph: set text(font: "TTLivretText-It")
#show strong: set text(font: "TTLivretText-DmBd")
#set par(
  spacing: 1.65em,
  justify: true,
)
#set page(
    width: 7in,
    height: 9in,
    footer: context [
        #if here().page() > 2 [
            #set text(weight: "black", size: 15pt)
            #if calc.even(here().page()) [
                #set align(right)
                #counter(page).display(
                    "1",
                )
            ] else [
                #set align(left)
                #counter(page).display(
                    "1",
                )
            ]
        ]
  ],
  foreground: context
{
    // Title top left
    set text(font: "TTLivretText-DmBd", size: 18pt)
    if query(heading.where(level: 1))
        .find(h => h.location().page() == here().page()) == none {
            // Filter headers that come after the current page
            let smh = query(heading.where(level: 1)).filter(h => h.location().page() <= here().page())
            if smh.len() > 0 {
                if calc.even(here().page()) {
                    let ht = rotate(90deg, origin: bottom + right, reflow: true, upper(smh.last().body))
                    let m = measure(ht)
                    place(top + left, dx: 16.2cm, dy: 1.5cm, float: false, ht)
                } else {
                    let ht = rotate(270deg, origin: bottom + right, reflow: true, upper(smh.last().body))
                    let m = measure(ht)
                    place(top + left, dx: 1.1cm, dy: 1.5cm, float: false, ht)
                }
            } else {
                ""
            }
        } else {
            let onPageHeading = query(heading.where(level: 1)).filter(h => h.location().page() == here().page())
            if calc.even(here().page()) {
                let ht = rotate(90deg, origin: bottom + right, reflow: true, upper(onPageHeading.first().body))
                let m = measure(ht)
                place(top + left, dx: 16.2cm, dy: 1.5cm, float: false, ht)
            } else {
                let ht = rotate(270deg, origin: bottom + right, reflow: true, upper(onPageHeading.first().body))
                let m = measure(ht)
                place(top + left, dx: 1.1cm, dy: 1.5cm, float: false, ht)
            }
        }
},
)
#show heading: set text(font: "TTLivretText-DmBd", size: 1.2em)

#show raw: set text(font: "Berkeley Mono", size: 8pt)
#set raw(theme: "calydon.tmTheme")

#align(center + horizon)[
    #align(center)[
        #text(size: 15em)[λς]
        #linebreak()#v(2.75em)
        #text(size: 4em)[Lambdas and Logos]
        #linebreak()#v(0em)
        #text(size: 2.5em)[On Writing Elegant Code]
        #linebreak()#v(0em)
        #text(size: 1.7em)[Lukáš Hozda]
    ]
]
#set page(
    margin: (inside: 3cm, outside: 2.5cm, top: 1.5cm, bottom: 3cm),
    binding: right
)
#pagebreak()
#set page(numbering: "1")
#set heading(numbering: "1.1")
#show heading: it => [#if it.level != 1 [
  #it #v(0.75em)
] else {
  [#pagebreak()#it #v(0.75em)]
}]
#set quote(block: true)

#outline()

= Preface
This book is for the junior or intermediate programmer, and to any other
interested party. To be more specific, you will gain the most from this
book if you fall into one or more of the following categories:

- You are a *university student*, or *recent graduate*. You were taught how to
  program at your school, and you have little experience writing software
  that interacts with the *real world*.

- You are *self-learned*, you write practical projects, but your programming
  knowledge is *almost completely practical*, driven by need of those projects,
  and you didn't delve much into the theory of things.

- You are a *fresh junior developer*, working in a company, and you are painfully
  discovering that maybe there is more to software development than you thought,
  and you now have to grapple with writing software that goes into production,
  is read and critiqued by others, and has to be maintained over a long
  period of time, and boy, you sure as hell have no experience doing that.

- You, like me, are an eternal *pursuer of beauty and elegance* in the things
  you do, and you consider programming to be a creative act that can have aesthetic
  merits

I think that I am getting ahead of myself with the last point, so let's take it
back from the beginning.

My name is Lukáš Hozda, I am a programmer. I work in Braiins Systems s.r.o, starting as
an Embedded SW Engineer, now a SWE Methodologist. This is a role that puts me somewhere
between a software engineer, an educator/mentor, a public speaker, and a recruiter.#footnote[
    Because of the wide range of my activities, it is somewhat difficult to categorize me in
    the company structure. Right now, I am filed under HR, which lets me jokingly call
    myself the "most technologically competent HR in the world".
] I first started programming when I was six or seven years old with Borland's
Turbo Pascal version 5.5. By then, Pascal was already very much out of fashion,
I was born in 2000, and got the Turbo Pascal books as discarded hand-me-downs from
the library my mom works as.

Even before that, I apparently exhibited interest in computers and technology.
This was to the point that a doctor has speculated that I might be addicted,
and my parents should not feed my addiction. And it is true I was mesmerized
by the sheer possibility and versatility of computers.#footnote[
    Ironically, my parents' attempts to limit my computer access, and not
    buy me any then-current-gen electronics turned me into a MacGyver
    type character, and I would learn a lot being an online pirate and compensating
    for the shortcomings and lack of performance of the hardware I had access to.
]

I think we have forgotten what miracle it is. You hold a device, and it can
do almost everything. Computers have become the cornerstone of our civilization.
We made them smaller, we made them bigger, we made them more generalized, and
more specialized, we have a tendency to replace analog machinery with them,
because it perhaps requires less brains to program a rice cooker with a sensors,
than to design a computer-less mechanism to drive all of its functionality.

Increasingly, we are putting computers in appliances, smart home devices
and whatever else you can think of. Better yet, following the Inception#footnote[Great movie, by the way]
school of thought, we put computers in your computer. In your desktop or laptop,
you may have a graphics card - that's a computer in your computer. It has a processing
unit (the difference being it has a lot of somewhat specialized, weaker cores, as opposed to your CPU,
which has fewer, stronger, more generalized cores), its own RAM, its own IO, and it can even
execute code.

In your CPU, there is a tiny additional computer with its own memory, CPU and IO - for Intel,
the Intel Management Engine, for AMD, the Platform Security Processor. Being a certified
hater, and heavy classic ThinkPad fan, I am more familiar with turning off the IME.#footnote[I consider it to be a backdoor]
This computer inside your CPU is running a Unix-like operating system called MINIX#footnote[
    Secretly making MINIX one of the most widespread desktop operating systems in the world.
], and it can talk to the internet, and see absolutely everything going on in your computer.

But all of these computers are unified by one thing - they are running programs. That's why
we invented computers in the first place, so that we can design algorithms, implement them,
and run them.

And, despite the efforts of Large Language Models, someone still has to write those programs.
That's the job of us, programmers. And I love programming, and I mean the act itself. To me,
programming is one of the ultimate creative activities, and I love exploring it. Over the
years, I have experimented and used with over a hundred programming languages, tried different
approaches, and paradigms, all in pursuit of the perfect fit. Nowadays, I mostly write
Rust (being an early adopter), Common Lisp and Scheme. By the end of this book, you will
probably understand why. :)

In other words, I am on an eternal quest of finding the best ideas, and finding solutions
tailored to my opinions, and striving to write the most elegant code possible. And I am a teacher,
and I want to share what I have learned to you, dear reader, so that you may write code
that is more beautiful.

Lukáš Hozda,#linebreak()
rennaissance man

= The Art of Programming
Programming is a discipline that's almost a century old. Arguably much older if we count the
efforts of Ada Lovelace, and much much older if we consider anything resembling an algorithm
to be the origin of programming.

I think that it is fair to say that the notion of programming is much older than computers.
We have a thousand different ways of describing algorithms, and in fact, whole programs, and
the field has evolved immensely in the last century. We programmers seem to have a lot of
opinions about how programming should be done, and have successfully turned the whole discipline
into a question of clashing personal philosophies.

```pascal
program HelloWorld;
begin
 WriteLn('Hello from Lambdas and Logos :)')
end.
```
Very similar to my first Pascal program.

Very often, we cannot objectively say, which solution to a given problem is the best one,
although we can generally point out the very bad ones. Furthermore, none of us have the moral superiority
of being a flawless programmer. Show me a programmer and I will show you someone who creates
bugs.

Writing elegant code means writing code that makes it harder to create insidious bugs, by
offering clarity and structure that make it easy to navigate, while still being an effective
solution for the task at hand.

== Lambdas and Logos
A programming language is a communication medium, just like a human language. It has a grammar,
and a vocabulary, and just like you can convey a specific meaning by creating a story composed of
sentences, you can solve an issue by creating a program composed of functions.

Who are we communicating with? The most obvious answer is with the computer. Unfortunately,
the computer has no notion of humor, sarcasm, hyperbole, metaphor, implications, innuendos or any other
departure from the most literal meaning of words, and so we have to be precised in what we say.
You as a programmer might say "The computer isn't doing what it should!", but it does precisely
what you told it to do.

If it doesn't do what you want it to do, then you need to phrase it correctly. This turns
out to be difficult, especially if you are solving a difficult problem. But through grit, spit,
and a whole lot of duct tape, we can do it.

What's worse is that we communicate not just with the computer, but with other programmers as well.
You say: "This program is just for me, I wrote it by myself, for myself!" -- you in 3 months is
"other programmers".

We need to write programs that are:
- Understood by the computers
- Understood by the programmers

Here is a program in Brainfuck:

```brainfuck
[(c) 2016 Daniel B. Cristofani
http://brainfuck.org/]

>>+>>>>>,[>+>>,]>+[--[+<<<-]<[<+>-]<[<[->[<<<+>>>>+<-]<<[>>+>[->]<<[<]
<-]>]>>>+<[[-]<[>+<-]<]>[[>>>]+<<<-<[<<[<<<]>>+>[>>>]<-]<<[<<<]>[>>[>>
>]<+<<[<<<]>-]]+<<<]+[->>>]>>]>>[.>>>]
```

The computer understands this program perfectly, how about you? I would have no idea what's going on.

Would it be better if we translated to a different language? Here is the same program in C:

#block(breakable: false)[
```cpp
void jonger(int beta[], int alpha, int omega) {
    int gamma[omega - alpha + 1];
    int theta = -1;
    gamma[++theta] = alpha;
    gamma[++theta] = omega;
    while (theta >= 0) {
        omega = gamma[theta--];
        alpha = gamma[theta--];
        int kappa = beta[omega];
        int lambda = (alpha - 1);
        for (int delta = alpha; delta <= omega - 1; delta++) {
            if (beta[delta] < kappa) {
                lambda++;
                int mu = beta[lambda];
                beta[lambda] = beta[delta];
                beta[delta] = mu;
            }
        }
        int mu = beta[lambda + 1];
        beta[lambda + 1] = beta[omega];
        beta[omega] = mu;
        int zeta = lambda + 1;
        if (zeta - 1 > alpha) {
            gamma[++theta] = alpha;
            gamma[++theta] = zeta - 1;
        }
        if (zeta + 1 < omega) {
            gamma[++theta] = zeta + 1;
            gamma[++theta] = omega;
        }
    }
}
```
]

I don't know how about you, but it still hard for me to understand what this program does.
It is still something the the computer understands perfectly, but programmers, not so much.
The issue is that the names of the variables and the function are very non-descriptive.

An adept programmer can now take a minute or a few to figure out that this is an iterative
version of the *quicksort* algorithm. But the situation would be much improved if we used
more useful names:

#block(breakable: false)[
```cpp
void quickSortIterative(int arr[], int low, int high) {
    int stack[high - low + 1];
    int top = -1;
    stack[++top] = low;
    stack[++top] = high;
    while (top >= 0) {
        high = stack[top--];
        low = stack[top--];
        int pivot = arr[high];
        int i = (low - 1);
        for (int j = low; j <= high - 1; j++) {
            if (arr[j] < pivot) {
                i++;
                int temp = arr[i];
                arr[i] = arr[j];
                arr[j] = temp;
            }
        }
        int temp = arr[i + 1];
        arr[i + 1] = arr[high];
        arr[high] = temp;
        int pi = i + 1;
        if (pi - 1 > low) {
            stack[++top] = low;
            stack[++top] = pi - 1;
        }
        if (pi + 1 < high) {
            stack[++top] = pi + 1;
            stack[++top] = high;
        }
    }
}
```
]

This now resembles code that may be written by a student who is learning C
for the first time. In a way, this is correct, but it is ugly. We have two
problems to take care of:
- Code repetition and organization
- Visuals and documentation

For the first point, there are two instances, where all we are doing is swapping
two values

```cpp
int temp = arr[i];
arr[i] = arr[j];
arr[j] = temp;
// ...
int temp = arr[i + 1];
arr[i + 1] = arr[high];
arr[high] = temp;
```

We can generalize it to a function called swap:

```cpp
void swap(int* a, int* b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}
```

This clarifies the quicksort implementation by a good amount:

```cpp
void quickSortIterative(int arr[], int low, int high) {
    int stack[high - low + 1];
    int top = -1;
    stack[++top] = low;
    stack[++top] = high;
    while (top >= 0) {
        high = stack[top--];
        low = stack[top--];
        int pivot = arr[high];
        int i = (low - 1);
        for (int j = low; j <= high - 1; j++) {
            if (arr[j] < pivot) {
                i++;
                swap(&arr[i], &arr[j]);
            }
        }
        swap(&arr[i + 1], &arr[high]);
        int pi = i + 1;
        if (pi - 1 > low) {
            stack[++top] = low;
            stack[++top] = pi - 1;
        }
        if (pi + 1 < high) {
            stack[++top] = pi + 1;
            stack[++top] = high;
        }
    }
}
```

Furthemore, if we recall the logic of *quicksort*, you will note that the step 3#footnote[Per Wikipedia, at least.]
is partitioning:

#quote[
    Partition the range: reorder its elements, while determining a point of division, so that
    all elements with values less than the pivot come before the division, while all elements
    with values greater than the pivot come after it; elements that are equal to the pivot can go either way.
]

We have the partition right here:

```cpp
int pivot = arr[high];
int i = (low - 1);
for (int j = low; j <= high - 1; j++) {
    if (arr[j] < pivot) {
        i++;
        swap(&arr[i], &arr[j]);
    }
}
swap(&arr[i + 1], &arr[high]);
int pi = i + 1;
```

This is the "divide" of the divide-and-conquer strategy *quicksort* employs. We can turn this into
a function:

```cpp
int partition(int arr[], int low, int high) {
    int pivot = arr[high];
    int i = (low - 1);
    for (int j = low; j <= high - 1; j++) {
        if (arr[j] < pivot) {
            i++;
            swap(&arr[i], &arr[j]);
        }
    }
    swap(&arr[i + 1], &arr[high]);
    return (i + 1);
}
```

Which improves how our quicksort function looks:

```cpp
void quickSortIterative(int arr[], int low, int high) {
    int stack[high - low + 1];
    int top = -1;
    stack[++top] = low;
    stack[++top] = high;
    while (top >= 0) {
        // Pop high and low
        high = stack[top--];
        low = stack[top--];
        int pi = partition(arr, low, high);
        if (pi - 1 > low) {
            stack[++top] = low;
            stack[++top] = pi - 1;
        }
        if (pi + 1 < high) {
            stack[++top] = pi + 1;
            stack[++top] = high;
        }
    }
}
```

Which we can further improve by inserting some small comments and appropriate whitespace:#footnote[
    It is arguable, how much commenting we need. Often, the answer I would provide is "as little as necessary".
    Overcommenting is a newbie mistake - we need to strike a balance. Formatting is very important also.
    We will discuss this in later on in this
    book
]#footnote[
    Also, note that there is the stack data structure lurking around in this implementation. We should probably
    point it out and describe it, if we find more usecases for it in our program than just this simple
    quicksorť
]

```cpp
void quickSortIterative(int arr[], int low, int high) {
    // create an auxiliary stack
    int stack[high - low + 1];

    // initialize top of stack
    int top = -1;

    // push initial values
    stack[++top] = low;
    stack[++top] = high;

    // keep popping from stack while it's not empty
    while (top >= 0) {
        // Pop high and low
        high = stack[top--];
        low = stack[top--];

        // get pivot position
        int pi = partition(arr, low, high);

        // if elements exist on left side of pivot
        if (pi - 1 > low) {
            stack[++top] = low;
            stack[++top] = pi - 1;
        }

        // if elements exist on right side of pivot
        if (pi + 1 < high) {
            stack[++top] = pi + 1;
            stack[++top] = high;
        }
    }
}
```

Using functions with descriptive names make your code more readable. The big idea is that we build up
abstractions. These abstractions represent new actions that we can use to write program in a more
descriptive manner, without having to worry about its implementation detail at every step of the way.

Let's take a slight theoretical detour by channeling our inner Dijkstra.#footnote[
    He was a real one, no one could talk shit about programming languages (and programmers)
    quite like he did
] Unfortunately, I am unable to find this text, and so I am paraphrasing from memory, but Dijkstra
essentially says that:

- Programs are processes composed of actions

- Action is a hopefully finite happening that has a defined effect

- Many happenings can be viewed as either a process or an action, depending on our interest
  in intermediate states

- Algorithms describe patterns of behavior using actions

- Algorithms are superior to simple step descriptions because they have connectives for *sequential*,
  *conditional* and *repetitive* composition of actions#footnote[These correspond to code blocks, conditional statements and loops respectively.
      Which renditions of these are available in particular depends on your programming language of choice.]

- The main strength of algorithms is that they can concisely express what many different happenings have in common.
  That is, you can describe how an infinite set of related scenarios are similar to one another

Building, or discovering abstractions is a very important part of every programmer's job. We are creating new
primitive actions that we can compose into ever more complex processes. So, don't be shy to make functions and
abstractions.

However, it is better to take a *more reactive than proactive approach* - you should create an abstraction because
you identify something that is a general enough notion that it deserves to be described.

Preemptively creating abstractions that prove to be unnecessary increases development time,
can harm performance#footnote[
    Although for most usecases, you shouldn't sacrificity the clarity and readability of programs for performance.
    A clear and effective algorithm should always take precedence to microoptimizations.
], increase maintenance cost, and can increase cognitive load without adding value.

The last point is particularly important. Your abstractions should decrease cognitive load,
not increase it. If you create an abstraction that is harder to understand than the unabstracted code,
then it is a terrible abstraction.

Good programming follows "simplicity as a feature". The right amount of abstraction hides complexity when needed,
but poor abstractions just add complexity. To paraphrase Einstein, *everything should be made as simple as possible,
but no simpler.*

Simplicity also does not mean _stupidity_. The power of more elaborate programming languages lies in the fact
that they let you design smarter abstractions that simplify programs effectively. Some programming
languages presume that programmers are stupid#footnote[One such language's name rhymes with "No"], and take the
power of creating generalized abstractions away from them.

This leads us to a very important point: *Programming languages matter.*#footnote[From a certain point]

Programming languages matter because they significantly influence how we model problems and design solutions.
Different languages aren't just different syntaxes for expressing the same ideas - they embody different philosophies,
different trade-offs, and different ways of conceptualizing computation.

Consider how differently you might approach a problem in C (thinking in terms of memory management and pointers),
Haskell (thinking in terms of type transformations), Prolog (thinking in terms of logical relations),
or APL (thinking in terms of array operations).

This influence of language on thought reminds me of the *Sapir-Whorf hypothesis* from linguistics.
Developed in the early 20th century by Edward Sapir and later expanded by his student Benjamin Lee Whorf,
this hypothesis explores the relationship between language and cognition.

Whorf developed the idea while working as a chemical engineer and fire insurance inspector#footnote[
    Some of the greatest ideas come from unexpected places, huh? :D
], where he noticed how language affected workers' perception of hazards. For instance, empty gasoline
drums were treated carelessly because the word "empty" implied absence of danger, despite the explosive
vapor they contained.

The hypothesis has two main variants. The strong version, *linguistic determinism*, claims that language
completely determines thought, suggesting people cannot conceptualize ideas for which their language lacks words.
Under this view, speakers of languages without future tense would struggle with long-term planning, or those
without certain color terms couldn't perceive those distinctions. This strong version has been largely rejected
by modern linguistics through empirical evidence showing people can think beyond the confines of their language.

The weak version, *linguistic relativity*, suggests that language influences (but doesn't determine)
thought and perception. It proposes that language makes certain distinctions easier to notice or express.
This version has empirical support - for example, languages with different color term boundaries show slight
differences in color recognition tasks, and languages that use absolute directions (north/south) rather than
relative ones (left/right) affect how their speakers navigate space.

I believe something similar to the weak form applies to programming languages. The language you use
influences which solutions you see first, which abstractions feel natural, which patterns you reach
for instinctively, and how you decompose complex problems.

A programmer who only knows imperative languages will struggle to see elegant functional solutions.
Someone trained only in class-based object-oriented programming might overuse inheritance where
composition would be clearer. Unfortunately, many programmers tend to be narrow-minded hubristic
creatures, who need to justify their investment into a particular technology. This has led to
many snarking at and discounting programming languages that are too different to what they are already
used to.

This is why I strongly recommend experiencing and immersing yourself in multiple *very different*
programming languages. Each language teaches you new mental models that remain useful even when
programming in other languages.

Learning Lisp makes you better at *symbolic programming* - treating code
and data as the same underlying structure and manipulating programs themselves as data, and it lets you
uncover something about the nature and implementation of programming languages#footnote[
    Lisp being the programmable programming language from outer space, of course
]. Learning Rust makes you think more carefully about *ownership* and *lifetimes*, and everything that
can possibly go wrong when it comes to memory management and concurrent code.
Learning Prolog teaches you to think *declaratively* rather than procedurally.

The more diverse your language experience, the richer your conceptual toolkit becomes for solving problems elegantly in any language.
Each paradigm teaches you to see computation from a different angle, and combining these perspectives
leads to more creative and effective solutions. In the coming chapters, we will examine a number of these
paradigms and observe even the most basic good practices of writing elegant code.

Going back to *quicksort*, this algorithm can be implemented (and most often is) in a recursive
way also. In mathematics, recursion is very common, because a lot of numerical sequences are defined
in terms of previous elements. Before Lisp popularized it, many programming languages did not support
recursion at all.

This was the case for Fortran, which had no notion of recursion at all in its first version. The recursive
version of quicksort is more elegant:

```python
def quicksort(arr):
    # base case: arrays with 0 or 1 element are already sorted
    if len(arr) <= 1:
        return arr

    # choose pivot and partition around it (middle element)
    pivot = arr[len(arr) // 2]
    left = [x for x in arr if x < pivot]
    middle = [x for x in arr if x == pivot]
    right = [x for x in arr if x > pivot]

    # recursively sort subarrays and combine
    return quicksort(left) + middle + quicksort(right)
```

In comparison to the previous examples, this one is written in Python. Python is about as readable, as
programming languages of the C (or broadly speaking, imperative) pedigree can get. Recursion is often
discouraged, because most languages don't have tail-call optimizations, and even if they do, the most
elegant representation of a particular problem recursively is not a tail call.

Quicksort is fine if we choose the appropriate pivot point. Usually, we go about `log2(N)` calls deep, and
to reach the 1000 calls recursion limit Python imposes by default, we would need an array in the ballpark
of 10^307 elements. We probably can't fit such an array into memory (or anywhere else) anyway, so this
algorithm is fine to be represented recursively without paying much attention to the size of the input.

We can achieve even more readability by trying a functional programming-oriented language,
where recursion is a prefered mechanism to solve problems requiring iteration:

```haskell
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (first:rest) =
    let smaller = quicksort [a | a <- rest, a <= first]
        bigger  = quicksort [a | a <- rest, a > first]
    in smaller ++ [first] ++ bigger
```

This syntax may be a little unfamiliar to you, so let's go through it:



```haskell
quicksort :: [Int] -> [Int]
```
First, we declare a function named `quicksort` that takes a list of integers and returns a list of integers.

```haskell
quicksort [] = []
```
We define the base case: when given an empty list, return an empty list (an empty list is already sorted).

```haskell
quicksort (first:rest) =
```
This pattern matches a non-empty list, splitting it into the first element `first` (our pivot) and the rest of the list `rest`.
In languages related to Haskall, it is very common to name these bindings `(x:xs)`. However, if you aren't a Haskell
programmer, I think `(first:rest)` tells you a little bit more about what's going on.

```haskell
    let smaller = quicksort [a | a <- rest, a <= first]
```
We create and recursively sort a list containing only elements from `rest` that are less than or equal to the pivot.

```haskell
        bigger  = quicksort [a | a <- rest, a > first]
```
Similarly, this creates and sorts a list of all elements greater than the pivot.

```haskell
    in smaller ++ [first] ++ bigger
```
Finally, it concatenates the three parts: smaller elements, the pivot, and bigger elements. This solution
is far more elegant, but it is vulnerable to potentially requiring a lot of nested calls, since we do not pick
the middle element, but the first element as our starting pivot.

It is perhaps slightly less readable than the previous solution, but we can change to use a middle pivot:

```haskell
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort [x] = [x]
quicksort elements =
    let pivot = elements !! (length elements `div` 2) -- Middle as pivot
        smaller = quicksort [a | a <- elements, a < pivot]
        equal = [a | a <- elements, a == pivot]  -- Handle duplicates properly
        bigger = quicksort [a | a <- elements, a > pivot]
    in smaller ++ equal ++ bigger
```

The somewhat weird `!!` operator just does list indexing, `elements !! 2` would retrieve the third element of the list `elements`.

Haskell is a very, very powerful language. It is perhaps the one pure functional programming lanuage
that can be widely applied in practice. This means that we can express ideas fairly elegantly in it,
because it gives us a lot of tools to our disposal.

On the other hand, learning Haskell takes a bit longer, and requires a bit of a paradigm shift if you
are coming from languages where the imperative approach reigns supreme. The idea of functional programming
is powerful enough that mainstream languages are now adopting its wisdom. However, they are largely impure,
usually because they allow mutability#footnote[
    For functional programming, the biggest issue mutating values from the outside,
    that is, whatever what violates referential transparency -- a situation where we
    can replace a function call with the result of said function call and the behavior of the program will not chagne
] or make no effort to limit side-effects#footnote[
    Side-effects are once again a problem for referential transparency, and also the predictability of
    a program's execution. Haskell has solved the issue of side-effects with Monadic IO, where the
]. This is something the languages question do because functional programming is not the primary priority.

Here is a similar quicksort written in Rust:

```rs
fn partition<F>(arr: &[i32], pivot_idx: usize, pred: F) -> Vec<i32>
where
    F: Fn(i32) -> bool
{
    arr.iter()
       .enumerate()
       .filter(|&(i, &x)| i != pivot_idx && pred(x))
       .map(|(_, &x)| x)
       .collect()
}

fn quicksort(arr: &[i32]) -> Vec<i32> {
    // base case: empty or single-element slices are already sorted
    if arr.len() <= 1 {
        return arr.to_vec();
    }

    // choose middle element as pivot
    let pivot_idx = arr.len() / 2;
    let pivot = arr[pivot_idx];
    
    // Partition array using the helper function
    let smaller = partition(arr, pivot_idx, |x| x <= pivot);
    let greater = partition(arr, pivot_idx, |x| x > pivot);

    // recursively sort partitions and combine results
    let mut result = quicksort(&smaller);
    result.push(pivot);
    result.extend(quicksort(&greater));

    result
}
```

Rust is a programming language that is fundamentally imperative, but has functional leanings. These show
in two main characteristics. First, we have iterators and iterator operations as opposed to using loops#footnote[Which are still available, Rust has `loop`, `for`, `while`, and `while-let`. The `while-let` structures does not verify a boolean condition, but a pattern match.]:

```rust
arr.iter()
   .enumerate()
   .filter(|&(i, &x)| i != pivot_idx && pred(x))
   .map(|(_, &x)| x)
   .collect()
```

#pagebreak()
=== On Lambdas an Logos
== Coding != Programming

== Programming should be fun

== It's not just the code

== Elegant code and the cost of inelegant code

= Programming in the small

== Line lengths and whitespace

== Source code files

== Naming things

== Documenting code

== Taming your hubris

== Object-Oriented Programming

== Functional Programming

== Symbolic Programming

== Optimizations en route to hell

== Design patterns

= Programming in the large

== Preparation and agility

== A goodly home for programs

== Stratification

== Separation of Concern

== Locality of Behavior

== The Expression Problem

== Technical Debt - Now or Never

== Code reviews

= Conclusion

== No silver bullet

== Aesthetics are an acquired skill, and an acquired taste
