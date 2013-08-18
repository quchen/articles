(Very brief description of how bind for Reader comes about)




Many definitions of >>= can be understood as two individual steps:
1. Getting the value out of the current monadic value
2. Creating a new monadic value using the value from 1., which is the final
   result.

(>>=) = m a -> (a -> m b) -> m b
      = (r -> a) -> (a -> r -> b) -> r b
         ^ Type of m
                     ^ Type of f
m >>= f = \environment -> let currentValue = m environment -- What's the current value stored in
                                                           -- `m a`, i.e. "the thing of type `a`"?
                                                           -- Well, apply m to the environment to
                                                           -- read it out.
                              newFunction = f currentValue -- Create a new monadic value (a new
                                                           -- thing of type `m b = r -> b`) from the
                                                           -- current value.
                          in  newFunction environment      -- We would like to return only
                                                           -- newFunction because point-free is
                                                           -- (sometimes) pretty, but we've still
                                                           -- got the "environment" lambda open,
                                                           -- since we previously needed its value.
                                                           -- Oh well, then we'll pass it explicitly.

Now let's pick shorter names for this:
      environment  -> e
      currentValue -> a  (because it has type a)
      newFunction  -> mb (because it has type m b, which is (->) e b = e -> b for Reader)

Insert this above, and you'll get

m >>= f = \e -> let a = m e
                    mb = f a
                in  mb e

We can now insert the definitions of a and mb in the `in` clause, and we'll
arrive at

m >>= f = \e -> f (m e) e

Tadaa :-)

So Reader can be seen as a shorthand way of writing the procedure described in
the wordy example in the beginning.