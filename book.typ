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
    #set align(right)
    #set text(weight: "black", size: 15pt)

    #if here().page() > 2 [
    #counter(page).display(
      "1",
      //both: true,
    )
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
                let ht = rotate(270deg, origin: bottom + right, reflow: true, upper(smh.last().body))
                let m = measure(ht)
                if calc.even(here().page()) {
                    place(top + left, dx: 16.2cm, dy: 1.5cm, float: false, ht)
                } else {
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

#show raw: set text(font: "Berkeley Mono", size: 10pt)
#set raw(theme: "agola.tmTheme")

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

What's worse is that we can

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
