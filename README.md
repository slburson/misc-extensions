# Misc-Extensions

The Misc-Extensions system provides several macros that I like to use.

## 1. Macro `nlet`

Macro `nlet` is an upward-compatible replacement for `cl:let` that generalizes
`let`, `let*`, and `multiple-value-bind`.  It can be imported as `let`,
shadowing `cl:let` — this is how I have used it for years — but if that seems
too radical, the same macro can be imported as `nlet`.  For clarity, I will
refer to it as `nlet` here.

There are two key ideas:

- `nlet` allows more than one variable in a binding clause to bind to
  additional values returned by the init-form, as with `multiple-value-bind`.
- `nlet` allows the binding clauses to be nested to indicate sequential binding:
  more deeply nested clauses are within the scopes of bindings made by less
  deeply nested clauses.  Within a level, bindings are parallel, as in `cl:let`.

A simple example first:

```common-lisp
   (nlet ((a (foo))
          ((b c (bar a))))
     ...)
```

Here, first `a` is bound to the (first) value of `(foo)`, and then `b` and `c`
are bound to the (first and second) values of `(bar a)`, where the latter `a`
refers to the binding created by the first clause.  (As with
`multiple-value-bind`, if `foo` returns more than one value, or `bar` returns
more than two, the extra values are silently ignored; if fewer values are
returned than expected, `nil` is supplied for the missing ones.)

A more complex example:

```common-lisp
  (nlet ((a b c (zot))
	 ((d (quux a c))
	  ((e f (mumble b d))
	   (g (mung a))))
	 ((h (frobozz c))
	  ((i (xyzzy h))))
	 (*print-level* 3))
    ...)
```

First `a`, `b`, and `c` are bound to the first three values of `(zot)`, and in
parallel, `*print-level*` is bound to 3; then `d` and `h` are bound; then `e`,
`f`, `g`, and `i` are bound.

As this example illustrates, all bindings at a given nesting level are done in
parallel, with all bindings at a deeper level following.  Stylistically, it is
expected that init-forms in nested clauses will refer only to variables bound in
containing clauses.  However, this is not enforced; for instance, the init-form
for `g` could refer to `h`, since the latter is bound one level out.

The macro correctly handles `declare` forms at the beginning of the body,
emitting the declarations at the correct level within the expansion so that they
apply to the binding named.

I first wrote this macro in 1980, as I was developing a personal Lisp style that
made heavier use of functional programming than was, I think, common at the
time.  I quickly found that multiple values were a great convenience for this
style, but the name `multiple-value-bind` was annoyingly long.  I think many
others have come to the same conclusion, as various other people have written
binding macros that handle multiple values less verbosely.  (Also, the `let`
construct in the Dylan language can bind multiple variables, as can tuple
assignment in Python.)

The value of using nesting to indicate sequencing may be less clear; I'm sure
some readers are thinking, "just use `let*` and be done with it".  That
certainly is a viable choice.  But I find it makes these expressions a little
more readable that I can tell exactly which init-forms are intended to reference
outer bindings, and which aren't; if they were in one big `let*`, I would have
to read all the init-forms to tell that.


## 2. GMap

GMap is an iteration macro for Common Lisp — now one of many, but actually among
the oldest; I first wrote it in 1980, in Lisp Machine Lisp.  It was conceived as
a generalization of `mapcar` (hence the name).  It is intended for cases when
`mapcar` doesn't suffice because the things you want to map over aren't in
lists, or you need to collect the results of the mapping into something other
than a list.

That is, `gmap` is probably the right thing to use when you are using iteration
to perform the same computation on each element of some collection, as opposed
to changing your state in some complicated way on every iteration of a loop.
It's conceptually reasonable to imagine all the iterations of a `gmap` as
happening in parallel, just as you might with `mapcar`.  It also supports
arbitrary reductions of the results of the mapped function; more on this below.

GMap is explicitly intended only for iterations that fit this "map-reduce"
model.  It is not trying to be a "universal" iteration construct.  People have
asked me what guarantees it offers concerning the ordering of the various
operations it performs, and the answer is none, other than those obviously
imposed by the data flow (a result can't be used until it is computed).  I think
that the question was rooted in experience of people using `loop` or other
iteration constructs and supplying variable update expressions with side
effects, so that there was "crosswise" data flow between the iterations.  I
strongly advise that such side effects be avoided in `gmap` calls.  If you find
yourself wanting to use them, either there is a better way (more on this below),
or else `gmap` simply isn't the right tool for the job.  In short, you should
think of `gmap` very much as a _functional_ iteration construct.

In general, my philosophy about iteration in Lisp is that there are many ways to
do it, for the very good reason that there are many kinds of iterations, and one
should use the tool appropriate for the particular case one is confronted with.
So, for example, I almost never write a `gmap` form with side effects.  If I'm
just iterating over a list for effect, I'll use `dolist`.  For iterations with
cross-iteration data flow ("loop-carried dependences" is the compiler
developer's term) or where the iteration space is not well defined in advance
(e.g., iterating over lines being read from a file) I might use good old `do`,
or I might even write the code tail-recursively, counting on the fact that most
CL implementations these days do tail-call optimization at least between
functions defined in a single `labels` form.

So when I do use `gmap`, it's specifically intended to convey to someone reading
the code that the function being mapped is side-effect-free, so that the calls
to it are independent of one another.  I strongly urge adherence to this rule.

Even with that constraint, I find that occasions to use `gmap` are not at all
uncommon.  It has proven handy over the years.

The difference between `mapcar` and `gmap` is that with `gmap`, you explicitly
indicate what kinds of collections the elements come from and how to combine the
successive values of the mapped function into a result.  For example, the
following two expressions are equivalent:

```common-lisp
(mapcar #'foo this-list that-list)
```
and
```common-lisp
(gmap (:result list) #'foo (:arg list this-list) (:arg list that-list))
```

[Side note to existing GMap users: this is the new, GMap 4.0 syntax.  The older
syntax is considered mildly deprecated, but will continue to be supported
indefinitely.  More on this below.]

The `(:result list)` subform indicates that `gmap` is to build a list; the
`:arg` subforms tell it that `this-list` and `that-list` are in fact lists of
elements over which `foo` is to be mapped.  Other types are known besides
`list`; for example, `string`, if used as an argument type, causes its argument
to be viewed as a string; the values it supplies to the function being mapped
are the successive characters of the string.

Like `mapcar`, `gmap` accepts any number of argument specs; each one supplies
one argument (or more, in some cases) to each call to the function being mapped.
Also like `mapcar`, `gmap` terminates its iteration when any of the arguments
runs out of elements.

For a small collection of examples, look at `test-new-syntax` in `tests.lisp`.

### 2.1. Mapped function

The mapped function is called with one or two arguments from each argument
generator (the ones that generate two arguments are noted below).  It may return
multiple values; some result collectors make use of the additional values
(again, see below).

A literal `nil` may be supplied as an abbreviation for the n-ary identity
function `#'values`.

### 2.2. Argument types

The set of argument types is extensible.  Thus you can adapt `gmap` to other
kinds of data structures over which you would like to iterate.  For details of
how to do this, see `def-arg-type` in the source file, and study the existing
definitions.

An argument type can explicitly indicate that it supplies more than one argument
to the function being mapped.  That function must have one or more additional
parameters at the corresponding point in its parameter list.  For example:

```common-lisp
(gmap (:result list) #'(lambda (x y z) (cons x (+ y z)))
      (:arg alist '((a . 47) (b . 72)))
      (:arg list '(2 6)))
==>
((A . 49) (B . 78))
```

Here `x` and `y` receive the pairs of the alist, and `z` receives the elements
of the list.

#### 2.1.1. Predefined argument types

- `constant` _value_: Yields `value` on every iteration.

- `list` _list_: Yields successive elements of `list`.

- `improper-list` _list_: Yields the successive elements of `list`, which may be
  improper; any non-cons tail terminates the iteration.

- `alist` _alist_: Yields, as two values, the successive pairs of `alist`.

- `plist` _plist_: Yields, as two values, the successive pairs of elements of
  `plist'; that is, there is one iteration for each two elements.

- `hash-table` _table_: Yields, as two values, the successive pairs of `table`.
  (Warning: the ordering of pairs is Lisp-implementation-dependent and should not
  be relied on.)

- `tails` _list_: Yields the successive tails (cdrs) of `list`, starting with
  `list` itself, which may be improper.

- `index` _start_ &optional _stop_ &key _incr fixnums?_: Yields integers in the
  interval [`start`, `stop`) if `incr` (which defaults to 1) is positive; or in
  the interval [`stop`, `start`) if `incr` is negative.  Specifically, in the
  upward case, the values begin with `start` and increase by `incr` until >=
  `stop`; in the downward case, the values begin with `start` - `incr` and
  decrease by `incr` until < `stop`.  All values are assumed to be fixnums unless
  `fixnums?` is a literal `nil`.  `stop` can be omitted or a literal `nil` to
  indicate an unbounded sequence.  `start` can be omitted to start at 0.

- `index-inc` _start stop_ &key _incr fixnums?_: ("Index, INClusive") Yields
  integers in the interval [`start`, `stop`].  Specifically, in the upward case
  (`incr` > 0), the values begin with `start` and increase by `incr` until >
  `stop`; in the downward case, the values begin with `start` and decrease by
  `incr` until < `stop`.  All values are assumed to be fixnums unless `fixnums?`
  is a literal `nil`.  `stop` can be a literal `nil` to indicate an unbounded
  sequence.

- `exp` _initial-value base_: Yields an exponential sequence whose first value
   is _initial-value_, and whose value is multiplied by _base_ on each
   iteration.

- `vector` _vec_ &key _start stop incr_: Yields elements of vector `vec`.  `start`
  and `stop` may be supplied to select a subsequence of `vec`; `incr` may be
  supplied (it must be positive) to select every second element etc.  For
  performance, you may prefer `simple-vector`.

- `simple-vector` _vec_ &key _start stop incr_: Yields elements of vector `vec`,
  which is assumed to be simple, and whose size is assumed to be a fixnum.
  `start` and `stop` may be supplied to select a subsequence of `vec`; `incr`
  may be supplied (it must be positive) to select every second element etc.

- `string` _str_ &key _start stop incr_: Yields elements of string `str`.  `start`
  and `stop` may be supplied to select a subsequence of `vec`; `incr` may be
  supplied (it must be positive) to select every second element etc.  For
  performance, you may prefer `simple-string`.

- `simple-string` _str_ &key _start stop incr_: Yields elements of string `str`,
  which is assumed to be simple, and whose size is assumed to be a fixnum.
  `start` and `stop` may be supplied to select a subsequence of `str`; `incr`
  may be supplied (it must be positive) to select every second element etc.

### 2.3. Result types

GMap, unlike `mapcar`, has the ability to perform arbitrary reductions on the
results returned by the function being mapped.  So, cases where you might have
written

```common-lisp
(reduce #'+ (mapcar ...))
```

can be replaced with a single `gmap` call, which is also more efficient in that
it doesn't materialize the intermediate result list:

```common-lisp
(gmap (:result sum) ...)
```

GMap takes the view that consing up a collection of function results is a kind
of reduction — a slightly unusual view, perhaps, but not unreasonable.  So it
treats collecting the results and summing them, for example, as instances of the
same pattern.

As with the argument types, the set of result types is extensible.  For details
of how to do this, see `def-result-type` in the source file, and study the
existing definitions.

A result type can explicitly indicate that it expects the function being mapped
to return multiple values, which it can turn into multiple arguments to a
reduction function.  Also, there is the special result type `values`, which
takes two or more result specs, and expects the function being mapped to return
the same number of values; it reduces each value according to the corresponding
result spec, then finally returns all the reduction results as multiple values.
For example:

```common-lisp
(gmap (:result values list sum) #'(lambda (x y) (values x y))
      (:arg alist '((a . 7) (b . 19))))
==>
(A B)   ; first value
26      ; second value
```

Additionally, there is a `:consume` feature that allows a single reduction to
consume multiple values from the function being mapped; see the source for
details.  — Earlier, I promised a "better way" (search for that phrase) to
handle cases where you need cross-iteration data flow.  The use of multiple
values and `:consume` can solve many of these problems without dirtying your
code with side effects.


#### 2.2.1. Predefined result types

- `list` &key _filterp_: Returns a list of the values, optionally filtered by
  `filterp`.

- `alist` &key _filterp_: Consumes two values from the mapped function; returns
  an alist of the pairs.  Note that `filterp`, if supplied, must take two
  arguments.

- `plist` &key _filterp_: Consumes two values from the mapped function; returns
  a plist of the pairs.  Note that `filterp`, if supplied, must take two
  arguments.

- `hash-table` &key _test_ _size_ _rehash-size_ _rehash-threshold_ _filterp_:
  Consumes two values from the mapped function; returns a hash-table of the
  pairs.  If any of `test`, `size`, `rehash-size`, or `rehash-threshold` are
  supplied, they are passed to `make-hash-table`.  Note that `filterp`, if
  supplied, must take two arguments.

- `append` &key _filterp_: Returns the result of `append`ing the values,
  optionally filtered by `filterp`.

- `nconc` &key _filterp_: Returns the result of `nconc`ing the values,
  optionally filtered by `filterp`.

- `and`: If one of the values is false, terminates the iteration and returns
  false; otherwise returns the last value.  Does not work as an operand of
  `values`.  (Generalizes `cl:every`.)
  
- `or`: If one of the values is true, terminates the iteration and returns it;
  otherwise, returns false.  Does not work as an operand of `:values`.
  (Generalizes `cl:some`.)

- `sum` &key _filterp_: Returns the sum of the values, optionally filtered by
  `filterp`.

- `product` &key _filterp_: Returns the product of the values, optionally
  filtered by `filterp`.

- `count-if`: Returns the number of true values.

- `max` &key _filterp_ _key_: Optionally filters the values by `filterp`, then
   returns the maximum, or if `key` is supplied, the value with the maximum key
   (if that's not unique, returns the first one); or `nil` if no values were
   supplied (or survived filtering).  Example:
   ```
   (gmap (:result max :key #'cdr) nil (:arg list alist))
   ```
   returns the (first) pair of `alist` with the maximum `cdr`.

   If `key` is `:second-value`, the second value of the mapped function is used;
   for example,
   ```
   (gmap (:result max :key :second-value) nil (:arg alist an-alist))
   ```
   returns the (first) `car` of `an-alist` with the maximum corresponding `cdr`.

- `min` &key _filterp_ _key_: Optionally filters the values by `filterp`, then
   returns the minimum, or if `key` is supplied, the value with the minimum key
   (if that's not unique, returns the first one); or `nil` if no values were
   supplied (or survived filtering).  Example:
   ```
   (gmap (:result min :key #'cdr) nil (:arg list alist))
   ```
   returns the (first) pair of `alist` with the minimum `cdr`.

   If `key` is `:second-value`, the second value of the mapped function is used;
   for example,
   ```
   (gmap (:result min :key :second-value) nil (:arg alist an-alist))
   ```
   returns the (first) `car` of `an-alist` with the minimum corresponding `cdr`.

- `vector` &key _use-vector length fill-pointer adjustable filterp_: Constructs
  a vector containing the results.  If `use-vector` is supplied, the argument
  will be filled with the results and returned; if `fill-pointer` is true and
  `adjustable` is true, it must have a fill pointer and be adjustable, and
  values will be appended to it with `vector-push-extend`; if `fill-pointer` is
  true and `adjustable` is false, it must have a fill pointer, and values will
  be appended to it with `vector-push`; otherwise, the vector is assumed to be
  simple and must be large enough to hold the results.  (Recall that
  `vector-push` has no effect if the vector is full.)

  If `use-vector` is not supplied, a vector will be constructed and returned;
  if `length` is supplied, returns a simple vector of the specified length (which
  must be sufficient to hold the results); otherwise, returns a simple vector of
  the correct length (but to do this, it must cons a temporary list).

  In any case, if `filterp` is supplied, it is a predicate of one argument,
  the value of the function being mapped, that says whether to include it in
  the result.

- `string` &key _use-string length fill-pointer adjustable filterp_: Constructs
  a string containing the results.  If `use-string` is supplied, the argument
  will be filled with the results and returned; if `fill-pointer` is true and
  `adjustable` is true, it must have a fill pointer and be adjustable, and
  values will be appended to it with `vector-push-extend`; if `fill-pointer` is
  true and `adjustable` is false, it must have a fill pointer, and values will
  be appended to it with `vector-push`; otherwise, the vector is assumed to be
  simple and must be large enough to hold the results.  (Recall that
  `vector-push` has no effect if the vector is full.)

  If `use-string` is not supplied, a string will be constructed and returned; if
  `length` is supplied, returns a simple string of the specified length (which
  must be sufficient to hold the results); otherwise, returns a simple string of
  the correct length (but to do this, it must cons a temporary list).

  In any case, if `filterp` is supplied, it is a predicate of one argument, the
  value of the function being mapped, that says whether to include it in the
  result.

### 2.4. The Old Syntax

For most of GMap's existence, it has had a slightly different syntax from that
shown above.  The `:arg` and `:result` keywords were not used; instead, the
argument and result types were defined as keywords themselves.  For instance,
the first example above would look like this:

```common-lisp
(gmap (:list) #'foo (:list this-list) (:list that-list))
;; The parens around the result are optional; you can also write:
(gmap :list #'foo (:list this-list) (:list that-list))
```

The reason I made them keywords was so that uses of them, as in this example,
wouldn't look like function calls; at least, the presence of the colons would
presumably give the reader of the code, who might not be familiar with GMap, a
clue that something out of the ordinary was going on.  The problem with this, of
course, is that it abandoned the modularity which is the entire point of the
package system: there can be only one definition of a given keyword as an
argument type or as a result type.

The 4.0 release fixes this by introducing `:arg` and `:result` to visually mark
these syntactic elements, and by changing all the predefined types to use
non-keyword symbols exported either from `cl:` or from `gmap:`.  However, it's
important not to break existing code; so here's what I have done.
`def-gmap-arg-type` and `def-gmap-res-type`, if given a name that is not a
keyword symbol, now also define the keyword symbol with the same name; but
before they do that, they check that it is not already defined by a previous
call supplying a different non-keyword name; if it is, they signal an error.

With this change, the old syntax will continue to work, but collisions, where
two systems try to define argument or result types with the same symbol-name,
will be detected; previously, the one loaded last would "win".

(Personally, I've settled on a compromise, wherein I write result types `and`
and `or` in the old syntax, as `:and` and `:or`; it's not plausible that these
could be defined as types.  For other result types, and all argument types, I
use the new syntax.)

### 2.5. Examples

To return the position of the largest value in a list `numbers`:

```
(gmap (:result max :key :second-value) nil (:arg index 0) (:arg list numbers))
```

## 3. Macro `fn`

For very small lambda expressions, the six characters taken by the word `lambda`
can be a significant fraction of the total.  If the body doesn't reference all
the parameters, moreover, you'll want to add a `(declare (ignore ...))` for the
unused ones, which adds to the verbosity.  The `fn` macro helps with both
annoyances.  Obviously, its name is quite short.  (Those willing to set foot on
the slippery slope of Unicode could use `λ`, of course, but I've been afraid to
go there yet, lest my code wind up as an unintelligible mass of obscure
mathematical symbols and cat emojis.)  And, you can use either a bare `_` or a
name beginning with `_` as a parameter name, to indicate an ignored parameter;
`fn` automatically inserts the required `ignore` declaration.

One catch, though, is that if you inadvertently write `#'(fn ...)`, you will get
a probably-unintelligible compiler error.  Just delete the `#'`.  (Lisp Machine
Lisp had something called "lambda macros" that solved this problem, but the
feature didn't make it into Common Lisp.)

## 4. Lexical contexts

I still consider these experimental and probably not terribly useful, though I do
use one occasionally.  If curiosity gets the better of you, have a look at
`contexts.text`.  Briefly, it's an alternate syntax for combinations of `let`
and `labels`.

## 5. Global lexical variables

Macro `deflex` _var_ &optional _val doc_:

Declares `var` as a global lexical variable, and if `val` is supplied and
`var` is not already bound, initializes it to `val`.  `doc`, if supplied,
is taken as a documentation string.  In some implementations (e.g. Scieneer),
locally rebinding the same name is not permitted; in most, it is permitted
but creates a new lexical variable, with no effect on the global one.

Macro `deflex-reinit` is the same except that it reinitializes the variable if
it's already bound, like `cl:defparameter`.

## 6. Interactive `setq`

Macro `isetq` &rest _var-val-pairs_:

Some implementations, notably SBCL, issue a warning if you use `setq` to set a
new global variable in the REPL.  (I guess they want you to use `defvar` first.)
`isetq` prevents this warning, using the same trick `deflex` uses to declare a
global lexical.  `isetq` is ***not recommended for use in programs***.

## 7. "Reversed" function binding forms

It's not uncommon to use `labels` to define a set of several mutually-recursive
functions, whose code can fill a screen or two, then have the body of the
`labels` kick off the recursion with a one-liner that just calls one of the
functions.  This strikes me as a little hard to read — you have to scroll all
the way to the bottom to find the initial values of the recursion variables —
and also a little wasteful of indentation space.  So I introduced a macro
`rlabels`, which puts the initial call first as a single subform, then takes the
function-binding clauses as its `&body` parameter, saving 7 columns of
indentation.

There are also `rflet` and `rmacrolet`, but I don't think I've ever used them.

## 7. Succinct class definitions with `define-class`

As everyone knows, `defclass` forms tend to be rather verbose and repetitive:

```common-lisp
(defclass frob (widget)
    ((color :initarg :color :reader frob-color
       :initform (error "A color must be specified.")
       :documentation "The color of the frob.")
     (height :initarg :height :accessor frob-height
       :initform 3.0
       :documentation "The height of the frob in cm.")
     ...)
  (:documentation "The class of all frobs.")
```

I've written a `define-class` macro to shorten them.  It is completely upward
compatible with `defclass`; you could just replace `defclass` with
`define-class` and have a valid invocation that would produce the same result.
But `define-class` provides several features to make the definition more
succinct.  Use only the ones you like!  The big change is that where `defclass`
slot descriptions have a strict alternating keyword-value syntax, `define-class`
is more flexible.

- A doc string for the class can be placed just before the slot specs (visually
  similar to where doc strings for functions go).
- The first element of a slot specifier, which is normally a slot name, can
  instead be a list `(slot-name initform)`; an `:initform` option will be
  generated.
- `:may-init` generates `:initarg :slot-name`.
- `:must-init` generates `:initarg :slot-name`, and also an `:initform` that
  signals an error if the argument is not provided to `make-instance`.
- `:readable` generates `:reader gf-name`, and `:accessible` generates
  `:accessor gf-name`, where `gf-name` is either the slot name or, if a
  `:conc-name` _class_ option is supplied, that string prepended to the slot
  name.
- A doc string can appear anywhere in the slot options; `:documentation` will be
  inserted.
- Or, you can use `:doc` as an abbreviation for `:documentation`.

So, here's the above example using `define-class`:

```common-lisp
(define-class frob (widget)
  "The class of all frobs."
  ((color :must-init :readable "The color of the frob.")
   ((height 3.0) :may-init :accessible "The height of the frob in cm."))
  (:conc-name #:frob-))
```

