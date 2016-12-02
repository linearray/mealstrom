Alegria [![CircleCI](https://circleci.com/gh/linearray/alegria.svg?style=svg)](https://circleci.com/gh/linearray/alegria)
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


When you want to record state changes in a process


You are writing an application for a bookstore. When a request for
`retrieveBook(id)` comes in, the book needs to be taken from storage.
You store the retrieved books in a database. You are concerned about
connections dropping, so you program the application to just retry the `retrieveBook(id)`
command until it goes through.

Now your application handles requests for `orderBook(isbn,qty)`. You store orders
in a database table. You realise you cannot just retry the query when
the connection drops, because you might order the book multiple times.
Hence you devise a scheme to attach a client-generated ID to `orderBook(isbn,qty)` entries.
You store the ID with the entry in the table. When you retry a dropped query, duplicates
can be caught and discarded by the database.


When the vendor fills an order you mark the order filled in the table using a boolean flag.
Requests `[orderBook(1316626229,10),orderBook(1316626229,15)]` come in.
The vendor sends you a shipment of `8` books. You do not know which order this shipment belongs to.
You decide it doesn not matter as long as it is deterministic, so you add a `filled` column
to your `orderBook(isbn,qty)` table, you write queries sorting orders by date and you enter `8` in the
filled column of the first order.
Tomorrow the vendor sends you 25 books. Your program enters 10 in the `filled` column of the first order,
and 23 for the second order, even though it is an order for 15. You call the vendor and she tells you
to send back the excess books. You would like to have a record of having been sent too many goods. You add
another database column to track returns. You heard of event sourcing instead of destructive updates.
Your boss tells you to cancel the later order for 15 books and send them back.
By now your code looks like a tangled mess of database query code and `if`s.

You decide you had enough of highly domain-specific database query code, unintended behaviour that goes
unnoticed every time you forget to handle a case. You heard of finite state machines to represent the state
of an entity in response to input.

newtype Ordered = Ordered {orderqty  :: Int} deriving (Show,Num)
newtype Arrived = Arrived {arriveqty :: Int} deriving (Show,Num)

data OrderState = Open (Ordered,Arrived) | Cancelled | Completed
data OrderEvent = Order Int | Delivery Int | Return Int | Cancel | Complete

orderTransition :: (OrderState,OrderEvent) -> OrderState
orderTransition (Open (o,a), Order i)  = Open (o+i,a)
orderTransition (Open (o,a), Return i) = Open (qty-i)
orderTransition (_,             Cancel)   = Cancelled
orderTransition

-- example for FSM2FSM + Effects
You now have a `retrieveBook(isbn,user)` request. Any user can request up to three books to be retrieved
from storage. When a book is marked for retrievel, you need to mark it, add it
to the number of books a user has asked for and you need to print a retrievel request on the printer
in the basement.
Since the printer cannot do much more than acknowledge the print to be successful, you realise you have
the same problem as before when ordering books. You decide not to build a new printer and leave this exercise
to someone else.
You do however want to make marking for retrieval and adding to the number of retrieved books transactional.
You heard about database transactions in college.

You do not want to stall your entire system every time the printer is down.
