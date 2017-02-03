# Controllers

The purpose of a controller is automatically tweaking the parameters of a system
so that it follows certain rules. For example, a stove has a controller that
enables and disables the heating elements in a way that the desired temperature
is reached as fast as possible, and then maintained relatively constant.

The simplest controller worth the name is known as a »Schmitt Trigger« or »Bang
Bang Controller«: if the desired quantity (stove heat) is below a threshold
enable the input (current to the heater), if it’s above a threshold disable it
again. This works well enough for many cases, but not so well for others.

  - Ramping up too heavily leads to overshooting the goal. This is particularly
    annoying in the heating setting, since cooling a stove takes *much* longer
    than heating it.
  - The quantity oscillates between on/off phases; ideally it should be
    constant.

## Schmitt trigger

TODO. Until then: there are enough of these around online already.

## PID controller

The PID controller is a more sophisticated, but also much more powerful,
controller. Conceptually, it consists of three parts: a (P)roportional, an
(I)ntegral, and a (D)ifferential one. Each of these parts reads an input signal
E, which is usually a measure of how far off we are from the desired value (»E«
for »error«), and an output signal »S«. This signal is sent back to the system,
and the controller adjusts S such that E is minimized. Ideally, E is 0, meaning
the system does exactly what we want.

  - The proportional term reacts proportionally to the input error, nothing
    special about it.
  - The integral term sums up and remembers the errors made in the past; the
    longer the system is not in a desirable state, the larger the integral term
    grows.
  - The differential term extrapolates the error into the future, predicting how
    large the error is going to be based on its current slope.

### Schematic

Below is how one could build a PID controller in Factorio. The coloured upper
parts are the integral, differential and proportional units, and below them are
the cells to tune their output to achieve the desired feedback for the system.

![](img/pid.png)

  - The purple integral unit feeds back into itself, and is equivalent to a
    memory cell for the A signal. Each tick, it is altered by adding a value E
    to its internal state.

  - The orange differential unit has a 1-tick delay unit (»E+0 ⇒ C«) on the
    right hand side. The left cell then compares this delayed signal with the
    current one; the result is the change of E over the last tick, stored as B.

  - The blue proportional unit piggy-backs on part of the differential unit.
    This is theoretically unnecessary, since we might as well use E directly as
    C, but since both the integral and differential units have a 1-tick delay
    built in, this delay cell synchronizes A, B and C so that their value
    changes synchronously with E.

  - `P/Q` is the parameter for the proportional term. It is split into
    numerator/denominator due to Factorio’s limited integer-only arithmetic.
    See the section on numerics below.
  - `I/J` is the parameter for the integral term.
  - `D/F` is the parameter for the differential term.

### Tuning the parameters

A well-tuned PID controller brings a suitable destabilized system into
equilibrium quickly, and maintains it without much fluctuation. However, finding
the right parameters can be a bit tricky. It is useful to keep the following
physical interpretations in mind:

  - Considering the past errors, the integral term will make the error
    (momentarily) zero after a time `Ti = P*J/(Q*I)` (the inverse product of the
    proportional and integral factors). If `Ti` is too large we’ll quickly reach
    but overshoot it, if it is too small it takes a long time to converge.

  - `Td = D*Q/(F*P)` describes how many ticks into the future we’re
    extrapolating. This is useful if we expect drift effects that we’d like to
    counteract. `Td` large means small fluctuations destabilize the system, `Td`
    too small makes  the system more stable, but makes it less adaptive to
    disturbances.

  - `Kp = P/Q` is a constant that simply scales the output signal. Too small and
    the system will converge only slowly because it has to rely on the integral
    term a lot, too large and it might destabilize.

There are entire books about tuning controller parameters. A good rule of thumb
for the PID controller is to set `P`, `I` and `D` to zero, and `Q`, `J` and `F`
to `1000`. Now watch what happens to the system when you increase `P` in steps
of a couple of hundreds at a time; keep doing this until the system starts
oscillating noticably when distiurbed out of equilibrium. Next, increase `I`
until the oscillation either vanishes completely, or at least dies off to at
least a quarter amplitude each period. Often, having reasonable `P` and `I` is
already satisfactory. If not, increase `D` until the system converges quickly
enough.

### Numerics

Factorio’s number system is integer-based, but a PID controller usually requires
fractional numbers in order to be able to tune a system nicely; if `1` is too
small but `2` is already too large, the controller can’t do its job properly.
For this reason the parameters were split into numerator/denominator, so that we
can at least have rational coefficients. In the previous section, we set the
denominators to `1000`, which effectively means we can craft parameters up to a
precision of `1/1000`. This should be plenty of accuracy for Factorio
applications.

### Example application

It is visually pleasing to see belts not fully occupied, because lots of little
moving items are more interesting than a long queue of inactive items. On the
other hand, it is functionally pleasing to know there is plenty of material for
the consumer. Finding the middle ground by hand is practically impossible,
because supply and demand change all the time, so in order to find a good middle
ground – production just right for consumption – requires someone to tune the
system for us.

Who would have guessed, we can use a PID controller for that. Let’s see how that
looks like and then discuss it!

![](img/rate-limiter.jpg)

The part of the belt that extends to the right is gated by two belt sensors. The
lower one adds one to a counter when a blue circuit passes, the upper one
subtracts one. Combining these gives us Δ, the number of items on that part of
the belt. From this value, we can calculate how unhappy we are with the  current
count, which gives us an error quantity; this is done at the very top. This
error is then fed into a PID controller, which generates the control variable S,
whose purpose it is to influence the system in order to minimize the error, i.e.
to accomplish that only X items are on the measure belt.

On the left, we have our LCG from the other chapter. It generates random values
and feeds them to the rate limiter belt element. This element compares the
random value R with the PID output S every tick, and if R exceeds S, the belt is
activated, letting items through. Therefore, the discrepancy between R and S
controls the likelihood the belt is moving each tick – which is what a rate
limiter does.

Connecting the dots, the PID controller adjusts the likelihood with which the
belt is activated or not each tick, based on whether there are enough (or too
many) blue circuits on the measure belt. As you can see in the picture, the
desiried quantity of 10 blue circuits on the measure belt is accomplished, and
even after changing the quantity to say 15, the system quickly adapts to the new
circumstances.



# Random number generation

## Linear Congruential Generator (LCG)

The linear congruential number generator is a very simple algorithm to produce
pseudorandom numbers. The simplicity comes at a price though, because the
numbers generated are of pretty poor quality, making it unsuitable for many
real-world applications. It is however very useful to generate »some noise« in
Factorio.

The LCG has two parameters `a` and `c` (carefully chosen) and a seed value
(arbitrary). A new random number Y is generated from the last one X by
calculating

    Y = a X + c   mod m

The parameter `m` is 2^32 in Factorio, since we’ll be using its built-in 32-bit
wrapping integer overflow arithmetic. The constants `a` and `c` need to satisfy
some properties to make the output random; luckily, good values can be looked up
on the internet. We’ll be using

    a = 214013
    c = 2531011

since they’re relatively short to type in. [Wikipedia has a list of alternative
choices.](https://en.wikipedia.org/wiki/Linear_congruential_generator)

### Truncation of predictable digits

We can now generate lots of numbers using the formula; unfortunately, the less
significant the bit, the less random it is due to LCG’s properties. For our
(already ideal) choice of parameters, the i-th least significant bit has a
period length of at most 2^i, which is quite terrible for a random number
generator. Basically, the red parts of our output are not random at all,

![](img/lcg-truncation.png)

We now have to take away as many digits on the right as possible, leaving us
with good random numbers. We can do this by dividing by 2^n, where n is the
number of bits to discard. Good choices are

  - n=31: we get a pretty good RNG for a true/false value.
  - n=16: there are 65536 possible outcomes (ranging from -2^15+1 to 2^15);
    probably more than enough for Factorio.

### Circuit schematic

Putting both the generator and the truncator in Factorio gives us

![](img/lcg-circuit-schematic.png)

where the left part is our raw generator with feedback loop, and the right one
truncates the not very random least significant n bit. And simple enough, in
the game it looks like this:

![](img/lcg-circuit-ingame.png)

### Applications

Random number generators have lots of applications, even in Factorio. For
example, they can be used to rate-limit the throughput of a belt, as seen in the
example of the PID controller in the other section.
