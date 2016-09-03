Alegria
=======
Manipulate FSMs and store them in PostgreSQL.

Alegria is a library that allows you to work with Mealy machines,
a kind of finite-state machine, in Haskell using PostgreSQL for persistence.
It is based on an idea that [Jakob Sievers](http://canned.primat.es/)
had when we both worked at a payment service provider.

The advantage of modelling your ``business logic'' as the manipulation
of FSMs is that it enforces a certain structure, encapsulates side-effects,
prevents illegal state transitions and generally makes it easier to
reason about your code.

When using Alegria a change to an FSM is applied in two steps. First,
when a `Msg` is received the new state for the FSM is calculated and
a log entry is written to the database. The log entry contains the
client-generated `MsgId`, the new `State` of the machine and the `Effects`
to be executed.

Then, the `Effects` of said state change are executed asynchronously.
Apart from performance, one advantage of this design is that state changes
are idempotent. Another one is that, since `MsgId` are client-generated,
you can have sending a message to another FSM as an `effect` and it
will be idempotent as well.
