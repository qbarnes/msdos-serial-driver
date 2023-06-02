The code when it came to my company worked great on real IBM PCs
and AT&T 6300 PCs, but had all sorts of intermittant problems
when running on IBM clone PCs my company had bought to fulfill a
contract.  They were dropping characters and sometimes wedging.

The code originally came to a couple of senior developers at the
company.  They worked on it trying to solve the problem, but then it
got punted to me so they could get back to more important work.

Being told the problems were probably just some race in the code, I
worked on it for a couple of weeks under that assumption.  I learned
the code, made some bug fixes and enhancements, but the problems
remained.

I was becoming convinced the chips themselves were buggy, and not
the driver's fault, in part because the manifestations of the bugs
were different with the different parts and revisions.

The president of the company (with an EE degree, no less) couldn't
believe the 8250s had defects.  He said something like, "Hardware
doesn't have bugs."

I suggested the fix was to replace the serial ISA boards with newer
chips that were known to work.  Of course I was informed that
that was way too expensive and would eat all the profit from the
contract.

I did get someone to crack open all the machines and go through
their serial cards writing down all the 8250 part numbers and their
revisions though.

With the list of part numbers and revisions in hand, I called up one
of their manufacturers.  I asked them if their 8250 chips had any
bugs.  They told me, "Of course they don't!"  I called the second
manufacturer and asked the same question.  They told me the same
answer.  I called the third, ask the same question, got the same
answer.  But before hanging up though, she paused and said, "Do you
mean our anomaly list?"

Not exactly sure what an anomaly list was at that time, I spat out,
"Yes!"  I gave her my USPS mail address at GIST.

Thinking "anomaly list" was their codeword for "bug list", I called
back the other manufacturers and said I wanted their anomaly list
for part blah-blah-blah with revision yada-yada.  They were all,
"Yes, sir!  Let me have your address and I'll send it right out!"

So "anomaly" was the magic word.  Hardware doesn't have "bugs" --
_they have "anomalies"_!

(I also learned about this later from someone I know working at
Intel.  Legally, having a "bug" in hardware was something verboten
to ever admit to.)

I requested that someone pull each version of the serial cards
from the clones and send them to me for my testing.

In a week or two, I got in all the anomaly lists and reviewed
them.  Boy, those old, cheap 8250s were littered with all sorts of
"anomalies".

I sorted out which anomalies would affect our driver's use cases
and studied their mitigations.  Luckily, none of their mitigations
conflicted with one another.

Sometimes the anomaly lists included example workaround code, other
times I had to come up with it myself.

I got the first workaround for the first 8250 chip and revision
coded up in the driver.  I tested it running our application
software using that chip and it now worked great!

Then I coded up workarounds for all the other chips and revisions,
and \*poof!\*, all the bad behaviors went away on all the chips and
their old revisions.  Mission accomplished!
