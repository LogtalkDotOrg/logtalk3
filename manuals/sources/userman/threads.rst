..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. _threads_threads:

Multi-threading programming
===========================

Logtalk provides **experimental** support for multi-threading
programming on selected Prolog compilers. Logtalk makes use of the
low-level Prolog built-in predicates that implement message queues and
interface with POSIX threads and mutexes (or a suitable emulation),
providing a small set of high-level predicates and directives that
allows programmers to easily take advantage of modern multi-processor
and multi-core computers without worrying about the details of creating,
synchronizing, or communicating with threads. Logtalk multi-threading
programming integrates with object-oriented programming providing a
*threaded engines* API, enabling objects and categories to prove goals
concurrently, and supporting synchronous and asynchronous messages.

.. _threads_enabling:

Enabling multi-threading support
--------------------------------

Multi-threading support may be disabled by default. It can be enabled on
the Prolog adapter files of supported compilers by setting the read-only
:ref:`threads <flag_threads>` compiler flag to ``supported``.

.. _threads_directive:

Enabling objects to make multi-threading calls
----------------------------------------------

The :ref:`directives_threaded_0` object
directive is used to enable an object to make multi-threading calls:

::

   :- threaded.

.. _threads_predicates:

Multi-threading built-in predicates
-----------------------------------

Logtalk provides a small set of built-in predicates for multi-threading
programming. For simple tasks where you simply want to prove a set of
goals, each one in its own thread, Logtalk provides a
:ref:`predicates_threaded_1` built-in
predicate. The remaining predicates allow for fine-grained control,
including postponing retrieving of thread goal results at a later time,
supporting non-deterministic thread goals, and making *one-way*
asynchronous calls. Together, these predicates provide high-level
support for multi-threading programming, covering most common use cases.

.. _threads_threaded:

Proving goals concurrently using threads
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A set of goals may be proved concurrently by calling the Logtalk
built-in predicate :ref:`predicates_threaded_1`. Each goal in
the set runs in its own thread.

When the ``threaded/1`` predicate argument is a *conjunction* of goals,
the predicate call is akin to *and-parallelism*. For example, assume
that we want to find all the prime numbers in a given interval,
``[N, M]``. We can split the interval in two parts and then span two
threads to compute the prime numbers in each sub-interval:

::

   prime_numbers(N, M, Primes) :-
       M > N,
       N1 is N + (M - N) // 2,
       N2 is N1 + 1,
       threaded((
           prime_numbers(N2, M, [], Acc),
           prime_numbers(N, N1, Acc, Primes)
       )).

   prime_numbers(N, M, Acc, Primes) :-
       ...

The ``threaded/1`` call terminates when the two implicit threads
terminate. In a computer with two or more processors (or with a
processor with two or more cores) the code above can be expected to
provide better computation times when compared with single-threaded code
for sufficiently large intervals.

When the ``threaded/1`` predicate argument is a *disjunction* of goals,
the predicate call is akin to *or-parallelism*, here reinterpreted as a
set of goals *competing* to find a solution. For example, consider the
different methods that we can use to find the roots of real functions.
Depending on the function, some methods will faster than others. Some
methods will converge into the solution while others may diverge and
never find it. We can try all the methods simultaneously by writing:

::

   find_root(Function, A, B, Error, Zero) :-
       threaded((
           bisection::find_root(Function, A, B, Error, Zero)
       ;   newton::find_root(Function, A, B, Error, Zero)
       ;   muller::find_root(Function, A, B, Error, Zero)
       )).

The above ``threaded/1`` goal succeeds when one of the implicit threads
succeeds in finding the function root, leading to the termination of all
the remaining competing threads.

The ``threaded/1`` built-in predicate is most useful for lengthy,
independent deterministic computations where the computational costs of
each goal outweigh the overhead of the implicit thread creation and
management.

.. _threads_call:

Proving goals asynchronously using threads
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A goal may be proved asynchronously using a new thread by calling the
:ref:`predicates_threaded_call_1_2` built-in predicate .
Calls to this predicate are always true and return immediately (assuming
a callable argument). The term representing the goal is copied, not
shared with the thread. The thread computes the first solution to the
goal, posts it to the message queue of the object from where the
``threaded_call/1`` predicate was called, and suspends waiting for
either a request for an alternative solution or for the program to
commit to the current solution.

The results of proving a goal asynchronously in a new thread may be
later retrieved by calling the :ref:`predicates_threaded_exit_1_2`
built-in predicate within the same object where the call to the
``threaded_call/1`` predicate was made. The ``threaded_exit/1``
calls suspend execution until the results of the ``threaded_call/1``
calls are sent back to the object message queue.

The ``threaded_exit/1`` predicate allow us to retrieve alternative
solutions through backtracking (if you want to commit to the first
solution, you may use the :ref:`predicates_threaded_once_1_2`
predicate instead of the ``threaded_call/1`` predicate). For example,
assuming a ``lists`` object implementing the usual ``member/2``
predicate, we could write:

.. code-block:: text

   | ?- threaded_call(lists::member(X, [1,2,3])).

   X = _G189 
   yes

   | ?- threaded_exit(lists::member(X, [1,2,3])).

   X = 1 ;
   X = 2 ;
   X = 3 ;
   no

In this case, the ``threaded_call/1`` and the ``threaded_exit/1`` calls
are made within the pseudo-object *user*. The implicit thread running
the ``lists::member/2`` goal suspends itself after providing a solution,
waiting for a request to an alternative solution; the thread is
automatically terminated when the runtime engine detects that
backtracking to the ``threaded_exit/1`` call is no longer possible.

Calls to the ``threaded_exit/1`` predicate block the caller until the
object message queue receives the reply to the asynchronous call. The
predicate :ref:`predicates_threaded_peek_1_2`
may be used to check if a reply is already available without removing it
from the thread queue. The ``threaded_peek/1`` predicate call succeeds
or fails immediately without blocking the caller. However, keep in mind
that repeated use of this predicate is equivalent to polling a message
queue, which may severely hurt performance.

Be careful when using the ``threaded_exit/1`` predicate inside
failure-driven loops. When all the solutions have been found (and the
thread generating them is therefore terminated), re-calling the
predicate will generate an exception. Note that failing instead of
throwing an exception is not an acceptable solution as it could be
misinterpreted as a failure of the ``threaded_exit/1`` argument.

The example on the previous section with prime numbers could be
rewritten using the ``threaded_call/1`` and ``threaded_exit/1``
predicates:

::

   prime_numbers(N, M, Primes) :-
       M > N,
       N1 is N + (M - N) // 2,
       N2 is N1 + 1,
       threaded_call(prime_numbers(N2, M, [], Acc)),
       threaded_call(prime_numbers(N, N1, Acc, Primes)),
       threaded_exit(prime_numbers(N2, M, [], Acc)),
       threaded_exit(prime_numbers(N, N1, Acc, Primes)).

   prime_numbers(N, M, Acc, Primes) :-
       ...

When using asynchronous calls, the link between a ``threaded_exit/1``
call and the corresponding ``threaded_call/1`` call is established using
unification. If there are multiple ``threaded_call/1`` calls for a
matching ``threaded_exit/1`` call, the connection can potentially be
established with any of them. Nevertheless, you can easily use a tag the
calls by using the extended :ref:`predicates_threaded_call_1_2`
and :ref:`predicates_threaded_exit_1_2` built-in predicates. For example:

.. code-block:: text

   ?- threaded_call(member(X, [1,2,3]), Tag).

   Tag = 1
   yes

   ?- threaded_call(member(X, [1,2,3]), Tag).

   Tag = 2
   yes

   ?- threaded_exit(member(X, [1,2,3]), 2).

   X = 1 ;
   X = 2 ;
   X = 3
   yes

When using these predicates, the tags shall be considered as an opaque
term; users shall not rely on its type.

.. _threads_ignore:

One-way asynchronous calls
--------------------------

Sometimes we want to prove a goal in a new thread without caring about
the results. This may be accomplished by using the built-in predicate
:ref:`predicates_threaded_ignore_1`.
For example, assume that we are developing a multi-agent application
where an agent may send an "happy birthday" message to another agent. We
could write:

::

   ..., threaded_ignore(agent::happy_birthday), ...

The call succeeds with no reply of the goal success, failure, or even
exception ever being sent back to the object making the call. Note that
this predicate implicitly performs a deterministic call of its argument.

.. _threads_synchronized_predicates:

Asynchronous calls and synchronized predicates
----------------------------------------------

Proving a goal asynchronously using a new thread may lead to problems
when the goal results in side effects such as input/output operations or
modifications to an :term:`object database`. For example, if a new thread is
started with the same goal before the first one finished its job, we may
end up with mixed output, a corrupted database, or unexpected goal
failures. In order to solve this problem, predicates (and grammar rule
non-terminals) with side effects can be declared as *synchronized* by
using the :ref:`directives_synchronized_1`
predicate directive. Proving a query to a synchronized predicate (or
synchronized non-terminal) is internally protected by a mutex, thus
allowing for easy thread synchronization. For example:

::

   % ensure thread synchronization
   :- synchronized(db_update/1).

   db_update(Update) :-
       % predicate with side-effects
       ...

A second example: assume an object defining two predicates for writing,
respectively, even and odd numbers in a given interval to the standard
output. Given a large interval, a goal such as:

.. code-block:: text

   | ?- threaded_call(obj::odd_numbers(1,100)),
        threaded_call(obj::even_numbers(1,100)).

   1 3 2 4 6 8 5 7 10 ...
   ...

will most likely result in a mixed up output. By declaring the
``odd_numbers/2`` and ``even_numbers/2`` predicates synchronized:

::

   :- synchronized([
       odd_numbers/2,
       even_numbers/2]).

one goal will only start after the other one finished:

.. code-block:: text

   | ?- threaded_ignore(obj::odd_numbers(1,99)),
        threaded_ignore(obj::even_numbers(1,99)).

   1 3 5 7 9 11 ...
   ...
   2 4 6 8 10 12 ...
   ...

Note that, in a more realistic scenario, the two ``threaded_ignore/1``
calls would be made concurrently from different objects. Using the same
synchronized directive for a set of predicates imply that they all use
the same mutex, as required for this example.

As each Logtalk entity is independently compiled, this directive must be
included in every object or category that contains a definition for the
described predicate, even if the predicate declaration is inherited from
another entity, in order to ensure proper compilation. Note that a
synchronized predicate cannot be declared dynamic. To ensure atomic
updates of a dynamic predicate, declare as synchronized the predicate
performing the update.

Synchronized predicates may be used as wrappers to messages sent to
objects that are not multi-threading aware. For example, assume a
``random`` object defining a ``random/1`` predicate that generates
random numbers, using side effects on its implementation (e.g. for
storing the generator seed). We can specify and define e.g. a
``sync_random/1`` predicate as follows:

::

   :- synchronized(sync_random/1).

   sync_random(Random) :-
       random::random(Random).

and then always use the ``sync_random/1`` predicate instead of the
predicate ``random/1`` from multi-threaded code.

The synchronization entity and predicate directives may be used when
defining objects that may be reused in both single-threaded and
multi-threaded Logtalk applications. The directives are simply ignored
(i.e. the synchronized predicates are interpreted as normal predicates)
when the objects are used in a single-threaded application.

.. _threads_notifications:

Synchronizing threads through notifications
-------------------------------------------

Declaring a set of predicates as synchronized can only ensure that they
are not executed at the same time by different threads. Sometimes we
need to suspend a thread not on a synchronization lock but on some
condition that must hold true for a thread goal to proceed. I.e. we want
a thread goal to be suspended until a condition becomes true instead of
simply failing. The built-in predicate :ref:`predicates_threaded_wait_1`
allows us to suspend a predicate execution (running in its own thread)
until a notification is received. Notifications are posted using the
built-in predicate :ref:`predicates_threaded_notify_1`.
A notification is a Prolog term that a programmer chooses to represent
some condition becoming true. Any Prolog term can be used as a
notification argument for these predicates. Related calls to the
``threaded_wait/1`` and ``threaded_notify/1`` must be made within the
same object, *this*, as the object message queue is used internally for
posting and retrieving notifications.

Each notification posted by a call to the ``threaded_notify/1``
predicate is consumed by a single ``threaded_wait/1`` predicate call
(i.e. these predicates implement a peer-to-peer mechanism). Care should
be taken to avoid deadlocks when two (or more) threads both wait and
post notifications to each other.

.. _threads_engines:

Threaded engines
----------------

Threaded engines provide an alternative to the multi-threading
predicates described in the previous sections. An *engine* is a computing
thread whose solutions can be lazily computed and retrieved. In
addition, an engine also supports a term queue that allows passing
arbitrary terms to the engine.

An engine is created by calling the :ref:`predicates_threaded_engine_create_3`
built-in predicate. For example:

.. code-block:: text

   | ?- threaded_engine_create(X, member(X, [1,2,3]), worker).
   yes

The first argument is an *answer template* to be used for retrieving
solution bindings. The user can name the engine, as in this example
where the atom ``worker`` is used, or have the runtime generate a name,
which should be treated as an opaque term.

Engines are scoped by the object within which the
``threaded_engine_create/3`` call takes place. Thus, different objects
can create engines with the same names with no conflicts. Moreover,
engines share the visible predicates of the object creating them.

The engine computes the first solution of its goal argument and suspends
waiting for it to be retrieved. Solutions can be retrieved one at a time
using the :ref:`predicates_threaded_engine_next_2` built-in predicate:

.. code-block:: text

   | ?- threaded_engine_next(worker, X).
   X = 1
   yes

The call blocks until a solution is available and fails if there are no
solutions left. After returning a solution, this predicate signals the
engine to start computing the next one. Note that this predicate is
deterministic. In contrast with the ``threaded_exit/1-2`` built-in
predicates, retrieving the next solution requires calling the predicate
again instead of by backtracking into its call. For example: 

::

   collect_all(Engine, [X| Xs]) :-
       threaded_engine_next(Engine, X),
       !,
       collect_all(Engine, Xs).
   collect_all(_, []).

There is also a reified alternative version of the predicate,
:ref:`predicates_threaded_engine_next_reified_2`,
which returns ``the(Answer)``, ``no``, and ``exception(Error)`` terms as
answers. Using this predicate, collecting all solutions to an engine
uses a different programming pattern:

::

   ... :-
       ...,
       threaded_engine_next_reified(Engine, Answer),
       collect_all_reifeid(Answer, Engine, List0),
       ...

   collect_all_reifeid(no, _, []).
   collect_all_reifeid(the(X), Engine, [X| Xs]) :-
       threaded_engine_next_reified(Engine, Answer),
       collect_all_reifeid(Answer, Engine, Xs).


Engines must be explicitly terminated using the
:ref:`predicates_threaded_engine_destroy_1` built-in predicate:

.. code-block:: text

   | ?- threaded_engine_destroy(worker).
   yes

A common usage pattern for engines is to define a recursive predicate
that uses the engine term queue to retrieve a task to be performed. For
example, assume we define the following predicate:

::

   loop :-
       threaded_engine_fetch(Task),
       handle(Task),
       loop.

The :ref:`predicates_threaded_engine_fetch_1`
built-in predicate fetches a task for the engine term queue. The engine
clients would use the :ref:`predicates_threaded_engine_post_2`
built-in predicate to post tasks into the engine term queue. The engine
would be created using the call:

.. code-block:: text

   | ?- threaded_engine_create(none, loop, worker).

   yes

The ``handle/1`` predicate, after performing a task, can use the
:ref:`predicates_threaded_engine_yield_1`
built-in predicate to make the task results available for consumption
using the ``threaded_engine_next/2`` built-in predicate. Blocking
semantics are used by these two predicates: the
``threaded_engine_yield/1`` predicate blocks until the returned solution
is consumed while the ``threaded_engine_next/2`` predicate blocks until
a solution becomes available.

.. _threads_performance:

Multi-threading performance
---------------------------

The performance of multi-threading applications is highly dependent on
the back-end Prolog compiler, on the operating-system, and on the use of
:term:`dynamic binding` and dynamic predicates. All compatible back-end
Prolog compilers that support multi-threading features make use of POSIX
threads or *pthreads*. The performance of the underlying pthreads
implementation can exhibit significant differences between operating
systems. An important point is synchronized access to dynamic
predicates. As different threads may try to simultaneously access and
update dynamic predicates, these operations may used a lock-free algorithm
or be protected by a lock, usually implemented using a mutex. In the latter
case, poor mutex lock operating-system performance, combined with a large
number of collisions by several threads trying to acquire the same lock,
can result in severe performance penalties. Thus, whenever possible,
avoid using dynamic predicates and dynamic binding.
