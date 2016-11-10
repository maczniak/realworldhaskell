# [Real World Haskell](/) by Bryan O'Sullivan, Don Stewart, and John Goerzen

<table width="100%" summary="Navigation header"><tr><th colspan="3" align="center">Chapter 18. Monad transformers</th></tr><tr><td width="20%" align="left"><a accesskey="p" href="interfacing-with-c-the-ffi.html">Prev</a> </td><th width="60%" align="center"> </th><td width="20%" align="right"> <a accesskey="n" href="error-handling.html">Next</a></td></tr></table>

# Chapter 18. Monad transformers

<b>Table of Contents</b>

* [Motivation: boilerplate avoidance](monad-transformers.html#id656888)
* [A simple monad transformer example](monad-transformers.html#id657034)
* [Common patterns in monads and monad transformers](monad-transformers.html#id657356)
* [Stacking multiple monad transformers](monad-transformers.html#id657557)
 * [Hiding our work](monad-transformers.html#id657944)
 * [Exercises](monad-transformers.html#id658064)
* [Moving down the stack](monad-transformers.html#id658162)
 * [When explicit lifting is necessary](monad-transformers.html#id658511)
* [Understanding monad transformers by building one](monad-transformers.html#monadtrans.maybet)
 * [Creating a monad transformer](monad-transformers.html#id658976)
 * [More typeclass instances](monad-transformers.html#id659032)
 * [Replacing the Parse type with a monad stack](monad-transformers.html#id659118)
 * [Exercises](monad-transformers.html#id659161)
* [Transformer stacking order is important](monad-transformers.html#id659259)
* [Putting monads and monad transformers into
perspective](monad-transformers.html#id659474)
 * [Interference with pure code](monad-transformers.html#id659488)
 * [Overdetermined ordering](monad-transformers.html#id659632)
 * [Runtime overhead](monad-transformers.html#id659676)
 * [Unwieldy interfaces](monad-transformers.html#id659760)
 * [Pulling it all together](monad-transformers.html#id659884)

## Motivation: boilerplate avoidance

<a name="x_Ct"></a>Monads provide a powerful way to build
computations with effects.  Each of the standard monads is
specialised to do exactly one thing.  In real code, we
often need to be able to use several effects at once.

<a name="x_Dt"></a>Recall the `Parse` type that we
developed in
[Chapter 10, ######TagOpen "i" []Code case study: parsing a binary data format######TagClose "i"](code-case-study-parsing-a-binary-data-format.html), for instance.  When we introduced
monads, we mentioned that this type was a state monad in
disguise. Our monad is more complex than the standard
`State` monad, because it uses the
`Either` type to allow the possibility of a parsing
failure.  In our case, if a parse fails early on, we want to
stop parsing, not continue in some broken state.  Our monad
combines the effect of carrying state around with the effect of
early exit.

<a name="x_Et"></a>The normal `State` monad doesn't let us
escape in this way; it only carries state.  It uses the default
implementation of `fail`: this calls
`error`, which throws an exception that we
can't catch in pure code.  The `State` monad thus
*appears* to allow for failure, without that
capability actually being any use.  (Once again, we recommend
that you almost always avoid using
`fail`!)

<a name="x_Ft"></a>It would be ideal if we could somehow take the
standard `State` monad and add failure handling to
it, without resorting to the wholesale construction of custom
monads by hand. The standard monads in the `mtl`
library don't allow us to combine them.  Instead, the library
provides a set of *monad
	transformers*<sup>[<a name="id656986"></a>[37](#ftn.id656986)]</sup> to achieve the same
result.

<a name="x_Ht"></a>A monad transformer is similar to a regular monad, but it's
not a standalone entity: instead, it modifies the behaviour of
an underlying monad.  Most of the monads in the `mtl`
library have transformer equivalents.  By convention, the
transformer version of a monad has the same name, with a
`T` stuck on the end.  For example, the transformer
equivalent of `State` is `StateT`; it adds
mutable state to an underlying monad.  The `WriterT`
monad transformer makes it possible to write data when stacked
on top of another monad.

## A simple monad transformer example

<a name="x_It"></a>Before we introduce monad transformers, let's look
at a function written using techniques we are already familiar
with.  The function below recurses into a directory tree, and
returns a list of the number of entries it finds at each level
of the tree.

<a name="CountEntries.hs:countEntriesTrad"></a>
```haskell
-- file: ch18/CountEntries.hs
module CountEntries (listDirectory, countEntriesTrad) where

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Control.Monad (forM, liftM)

listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
    where notDots p = p /= "." && p /= ".."

countEntriesTrad :: FilePath -> IO [(FilePath, Int)]
countEntriesTrad path = do
  contents <- listDirectory path
  rest <- forM contents $ \name -> do
            let newName = path </> name
            isDir <- doesDirectoryExist newName
            if isDir
              then countEntriesTrad newName
              else return []
  return $ (path, length contents) : concat rest
```

<a name="x_Jt"></a>We'll now look at using the writer monad to
achieve the same goal.  Since this monad lets us record a value
wherever we want, we don't need to explicitly build up a
result.

<a name="x_Kt"></a>As our function must execute in the
`IO` monad so that it can traverse directories, we
can't use the `Writer` monad directly.  Instead, we
use `WriterT` to add the recording capability to
`IO`.  We will find the going easier if we look at
the types involved.

<a name="x_Lt"></a>The normal `Writer` monad has two type
parameters, so it's more properly written `Writer w
	a`.  The first parameter `w` is the type of the values to be
recorded, and `a` is the usual
type that the `Monad` typeclass requires. Thus
`Writer [(FilePath, Int)] a` is a writer monad that
records a list of directory names and sizes.

<a name="x_Mt"></a>The `WriterT` transformer has a similar
structure, but it adds another type parameter `m`: this is the underlying monad whose
behaviour we are augmenting. The full signature of
`WriterT` is `WriterT w m a`.

<a name="x_Nt"></a>Because we need to traverse directories, which requires
access to the `IO` monad, we'll stack our writer on
top of the `IO` monad.  Our combination of monad
transformer and underlying monad will thus have the type
`WriterT [(FilePath, Int)] IO a`.  This stack of
monad transformer and monad is itself a monad.

<a name="CountEntriesT.hs:countEntries"></a>
```haskell
-- file: ch18/CountEntriesT.hs
module CountEntriesT (listDirectory, countEntries) where

import CountEntries (listDirectory)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import Control.Monad (forM_, when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, tell)

countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
  contents <- liftIO . listDirectory $ path
  tell [(path, length contents)]
  forM_ contents $ \name -> do
    let newName = path </> name
    isDir <- liftIO . doesDirectoryExist $ newName
    when isDir $ countEntries newName
```

<a name="x_Ot"></a>This code is not terribly different from our earlier
version. We use `liftIO` to expose the
`IO` monad where necessary, and
`tell` to record a visit to a
directory.

<a name="x_Pt"></a>To run our code, we must use one of `WriterT`'s
execution functions.

<a name="countEntries.ghci:runWriterT"></a><pre>ghci> **:type runWriterT**
runWriterT :: WriterT w m a -> m (a, w)
ghci> **:type execWriterT**
execWriterT :: (Monad m) => WriterT w m a -> m w
</pre>

<a name="x_Qt"></a>These functions execute the action, then remove the
`WriterT` wrapper and give a result that is wrapped
in the underlying monad.  The `runWriterT`
function gives both the result of the action and whatever was
recorded as it ran, while `execWriterT`
throws away the result and just gives us what was
recorded.

<a name="countEntries.ghci:countEntries"></a><pre>ghci> **:type countEntries ".."**
countEntries ".." :: WriterT [(FilePath, Int)] IO ()
ghci> **:type execWriterT (countEntries "..")**
execWriterT (countEntries "..") :: IO [(FilePath, Int)]
ghci> **take 4 `liftM` execWriterT (countEntries "..")**
[("..",30),("../ch15",23),("../ch07",26),("../ch01",3)]
</pre>

<a name="x_RJ1"></a>We use a `WriterT` on top of `IO`
because there is no `IOT` monad transformer.
Whenever we use the `IO` monad with one or more monad
transformers, `IO` will always be at the bottom of
the stack.

## Common patterns in monads and monad transformers

<a name="x_Rt"></a>Most of the monads and monad transformers in the
`mtl` library follow a few common patterns around
naming and typeclasses.

<a name="x_St"></a>To illustrate these rules, we will focus on a
single straightforward monad: the reader monad.  The reader
monad's API is detailed by the `MonadReader`
typeclass.  Most `mtl` monads have similarly named
typeclasses: `MonadWriter` defines the API of the
writer monad, and so on.

<a name="Reader.hs:class"></a>
```haskell
-- file: ch18/Reader.hs
class (Monad m) => MonadReader r m | m -> r where
    ask   :: m r
    local :: (r -> r) -> m a -> m a
```

<a name="x_Ut"></a>The type variable `r`
represents the immutable state that the reader monad carries
around.  The `Reader r` monad is an instance of the
`MonadReader` class, as is the `ReaderT r
	m` monad transformer.  Again, this pattern is repeated
by other `mtl` monads: there usually exist both a
concrete monad and a transformer, each of which are instances of
the typeclass that defines the monad's API.

<a name="x_Vt"></a>Returning to the specifics of the reader monad, we haven't
touched upon the `local` function before.  It
temporarily modifies the current environment using the `r
	-> r` function, and executes its action in the
modified environment.  To make this idea more concrete, here is
a simple example.

<a name="LocalReader.hs:localExample"></a>
```haskell
-- file: ch18/LocalReader.hs
import Control.Monad.Reader

myName step = do
  name <- ask
  return (step ++ ", I am " ++ name)

localExample :: Reader String (String, String, String)
localExample = do
  a <- myName "First"
  b <- local (++"dy") (myName "Second")
  c <- myName "Third"
  return (a, b, c)
```

<a name="x_Wt"></a>If we execute the `localExample` action
in **ghci**, we can see that the effect of modifying the
environment is confined to one place.

<a name="localReader.ghci:localExample"></a><pre>ghci> **runReader localExample "Fred"**
Loading package mtl-1.1.0.0 ... linking ... done.
("First, I am Fred","Second, I am Freddy","Third, I am Fred")
</pre>

<a name="x_Xt"></a>When the underlying monad `m`
is an instance of `MonadIO`, the `mtl`
library provides an instance for `ReaderT r m`, and
also for a number of other typeclasses.  Here are a few.

<a name="Reader.hs:instances"></a>
```haskell
-- file: ch18/Reader.hs
instance (Monad m) => Functor (ReaderT r m) where
    ...

instance (MonadIO m) => MonadIO (ReaderT r m) where
    ...

instance (MonadPlus m) => MonadPlus (ReaderT r m) where
    ...
```

<a name="x_Yt"></a>Once again, most `mtl` monad transformers define
instances like these, to make it easier for us to work with
them.

## Stacking multiple monad transformers

<a name="x_Zt"></a>As we have already mentioned, when we stack a monad
transformer on a normal monad, the result is another monad. This
suggests the possibility that we can again stack a monad
transformer on top of our combined monad, to give a new monad,
and in fact this is a common thing to do.  Under what
circumstances might we want to create such a stack?

* <a name="x_at"></a>If we need to talk to the outside world, we'll
have `IO` at the base of the stack. Otherwise, we
will have some normal monad.
* <a name="x_bt"></a>If we add a `ReaderT` layer, we give
ourselves access to read-only configuration
information.
* <a name="x_ct"></a>Add a `StateT` layer, and we gain global
state that we can modify.
* <a name="x_dt"></a>Should we need the ability to log events, we can add a
`WriterT` layer.

<a name="x_et"></a>The power of this approach is that we can customise the
stack to our exact needs, specifying which kinds of effects we
want to support.

<a name="x_ft"></a>As a small example of stacked monad transformers in action,
here is a reworking of the `countEntries`
function we developed earlier.  We will modify it to recurse no
deeper into a directory tree than a given amount, and to record
the maximum depth it reaches.

<a name="UglyStack.hs:AppData"></a>
```haskell
-- file: ch18/UglyStack.hs
import System.Directory
import System.FilePath
import Control.Monad.Reader
import Control.Monad.State

data AppConfig = AppConfig {
      cfgMaxDepth :: Int
    } deriving (Show)

data AppState = AppState {
      stDeepestReached :: Int
    } deriving (Show)
```

<a name="x_gt"></a>We use `ReaderT` to store configuration data, in
the form of the maximum depth of recursion we will perform.  We
also use `StateT` to record the maximum depth we
reach during an actual traversal.

<a name="UglyStack.hs:App"></a>
```haskell
-- file: ch18/UglyStack.hs
type App = ReaderT AppConfig (StateT AppState IO)
```

<a name="x_ht"></a>Our transformer stack has `IO` on the bottom,
then `StateT`, with `ReaderT` on top.  In
this particular case, it doesn't matter whether we have
`ReaderT` or `WriterT` on top, but
`IO` must be on the bottom.

<a name="x_SJ1"></a>Even a small stack of monad transformers quickly develops an
unwieldy type name.  We can use a `type` alias to reduce the lengths
of the type signatures that we write.

<table border="0" summary="Note: Where's the missing type parameter?"><tr><td rowspan="2" align="center" valign="top" width="25">![](/support/figs/note.png)</td><th align="left">Where's the missing type parameter?</th></tr><tr><td align="left" valign="top"><a name="x_it"></a>You might have noticed that our `type` synonym doesn't
have the usual type parameter `a` that we
associate with a monadic type:

<a name="UglyStack.hs:App2"></a>
```haskell
-- file: ch18/UglyStack.hs
type App2 a = ReaderT AppConfig (StateT AppState IO) a
```

<a name="x_jt"></a>Both `App` and `App2` work fine in
normal type signatures.  The difference arises when we try to
construct another type from one of these.  Say we want to add
another monad transformer to the stack: the compiler will
allow `WriterT [String] App a`, but reject
`WriterT [String] App2 a`.

<a name="x_kt"></a>The reason for this is that Haskell does not allow us to
partially apply a type synonym.  The synonym `App`
doesn't take a type parameter, so it doesn't pose a problem.
However, because `App2` takes a type parameter, we
must supply some type for that parameter if we want to use
`App2` to create another type.

<a name="x_lt"></a>This restriction is limited to type synonyms.  When we
create a monad transformer stack, we usually wrap it with a
`newtype` (as we will see below).  As a result, we will rarely
run into this problem in practice.

</td></tr></table>

<a name="x_mt"></a>The execution function for our monad stack is simple.

<a name="UglyStack.hs:runApp"></a>
```haskell
-- file: ch18/UglyStack.hs
runApp :: App a -> Int -> IO (a, AppState)
runApp k maxDepth =
    let config = AppConfig maxDepth
        state = AppState 0
    in runStateT (runReaderT k config) state
```

<a name="x_nt"></a>Our application of `runReaderT` removes
the `ReaderT` transformer wrapper, while
`runStateT` removes the `StateT`
wrapper, leaving us with a result in the `IO`
monad.

<a name="x_ot"></a>Compared to earlier versions, the only complications we have
added to  our traversal function are slight: we track our
current depth, and record the maximum depth we reach.

<a name="UglyStack.hs:constrainedCount"></a>
```haskell
-- file: ch18/UglyStack.hs
constrainedCount :: Int -> FilePath -> App [(FilePath, Int)]
constrainedCount curDepth path = do
  contents <- liftIO . listDirectory $ path
  cfg <- ask
  rest <- forM contents $ \name -> do
            let newPath = path </> name
            isDir <- liftIO $ doesDirectoryExist newPath
            if isDir && curDepth < cfgMaxDepth cfg
              then do
                let newDepth = curDepth + 1
                st <- get
                when (stDeepestReached st < newDepth) $
                  put st { stDeepestReached = newDepth }
                constrainedCount newDepth newPath
              else return []
  return $ (path, length contents) : concat rest
```

<a name="x_pt"></a>Our use of monad transformers here is admittedly a little
contrived.  Because we're writing a single straightforward
function, we're not really winning anything.  What's useful
about this approach, though, is that it
*scales* to bigger programs.

<a name="x_qt"></a>We can write most of an application's imperative-style code
in a monad stack similar to our `App` monad.  In a
real program, we'd carry around more complex configuration data,
but we'd still use `ReaderT` to keep it read-only
and hidden except when needed.  We'd have more mutable state to
manage, but we'd still use `StateT` to encapsulate
it.

### Hiding our work

<a name="x_rt"></a>We can use the usual `newtype` technique to erect a solid
barrier between the implementation of our custom monad and its
interface.

<a name="UglyStack.hs:MyApp"></a>
```haskell
-- file: ch18/UglyStack.hs
newtype MyApp a = MyA {
      runA :: ReaderT AppConfig (StateT AppState IO) a
    } deriving (Monad, MonadIO, MonadReader AppConfig,
                MonadState AppState)

runMyApp :: MyApp a -> Int -> IO (a, AppState)
runMyApp k maxDepth =
    let config = AppConfig maxDepth
        state = AppState 0
    in runStateT (runReaderT (runA k) config) state
```

<a name="x_st"></a>If we export the `MyApp` type constructor and
the `runMyApp` execution function from a
module, client code will not be able to tell that the
internals of our monad is a stack of monad
transformers.

<a name="x_tt"></a>The large `deriving` clause requires the
`GeneralizedNewtypeDeriving` language pragma.  It
seems somehow magical that the compiler can derive all of
these instances for us.  How does this work?

<a name="x_ut"></a>Earlier, we mentioned that the `mtl` library
provides instances of a number of typeclasses for each monad
transformer.  For example, the `IO` monad
implements `MonadIO`.  If the underlying monad is
an instance of `MonadIO`, `mtl` makes
`StateT` an instance, too, and likewise for
`ReaderT`.

<a name="x_vt"></a>There is thus no magic going on: the top-level monad
transformer in the stack is an instance of all of the type
classes that we're rederiving with our `deriving`
clause. This is a consequence of `mtl` providing a
carefully coordinated set of typeclasses and instances that
fit together well.  There is nothing more going on than the
usual automatic derivation that we can perform with `newtype`
declarations.

### Exercises

1. <a name="id658074"></a><a name="id658076"></a><a name="x_TJ1"></a>Modify the `App` type synonym to swap the
order of `ReaderT` and `WriterT`.
What effect does this have on the
`runApp` execution function?
1. <a name="id658105"></a><a name="id658107"></a><a name="x_UJ1"></a>Add the `WriterT` transformer to the
`App` monad transformer stack.  Modify
`runApp` to work with this new
setup.
1. <a name="id658132"></a><a name="id658135"></a><a name="x_VJ1"></a>Rewrite the `constrainedCount`
function to record results using the
`WriterT` transformer in your new
`App` stack.

## Moving down the stack

<a name="x_wt"></a>So far, our uses of monad transformers have been simple, and
the plumbing of the `mtl` library has allowed us to
avoid the details of how a stack of monads is constructed.
Indeed, we already know enough about monad transformers to
simplify many common programming tasks.

<a name="x_xt"></a>There are a few useful ways in which we can depart from
the comfort of `mtl`.  Most often, a custom monad
sits at the bottom of the stack, or a custom monad transformer
lies somewhere within the stack.  To understand the potential
difficulty, let's look at an example.

<a name="x_yt"></a>Suppose we have a custom monad transformer,
`CustomT`.

<a name="CustomT.hs:CustomT"></a>
```haskell
-- file: ch18/CustomT.hs
newtype CustomT m a = ...
```

<a name="x_zt"></a>In the framework that `mtl` provides, each monad
transformer in the stack makes the API of a lower level
available by providing instances of a host of typeclasses.  We
could follow this pattern, and write a number of boilerplate
instances.

<a name="CustomT.hs:mtl"></a>
```haskell
-- file: ch18/CustomT.hs
instance MonadReader r m => MonadReader r (CustomT m) where
    ...

instance MonadIO m => MonadIO (CustomT m) where
    ...
```

<a name="x_Au"></a>If the underlying monad was an instance of
`MonadReader`, we would write a
`MonadReader` instance for `CustomT` in
which each function in the API passes through to the
corresponding function in the underlying instance.  This would
allow higher level code to only care that the stack as a whole
is an instance of `MonadReader`, without knowing or
caring about which layer provides the *real*
implementation.

<a name="x_Bu"></a>Instead of relying on all of these typeclass instances to
work for us behind the scenes, we can be explicit. The
`MonadTrans` typeclass defines a useful function
named `lift`.

<a name="monadTrans.ghci:MonadTrans"></a><pre>ghci> **:m +Control.Monad.Trans**
ghci> **:info MonadTrans**
class MonadTrans t where lift :: (Monad m) => m a -> t m a
  	-- Defined in Control.Monad.Trans
</pre>

<a name="x_Cu"></a>This function takes a monadic action from one layer down the
stack, and turns it—in other words,
*lifts* it—into an action in the
current monad transformer. Every monad transformer is an
instance of `MonadTrans`.

<a name="x_Du"></a>We use the name `lift` based on its
similarity of purpose to `fmap` and
`liftM`. In each case, we hoist something
from a lower level of the type system to the level we're
currently working in.

* <a name="x_Eu"></a>`fmap` elevates a pure function to
the level of functors;
* <a name="x_Fu"></a>`liftM` takes a pure function to the
level of monads;
* <a name="x_Gu"></a>and `lift` raises a monadic action
from one level beneath in the transformer stack to the
current one.

<a name="x_Hu"></a>Let's revisit the `App` monad stack we defined
earlier (before we wrapped it with a `newtype`).


```haskell
-- file: ch18/UglyStack.hs
type App = ReaderT AppConfig (StateT AppState IO)
```

<a name="x_Iu"></a>If we want to access the `AppState` carried by
the `StateT`, we would usually rely on
`mtl`'s typeclasses and instances to handle the
plumbing for us.

<a name="UglyStack.hs:implicitGet"></a>
```haskell
-- file: ch18/UglyStack.hs
implicitGet :: App AppState
implicitGet = get
```

<a name="x_Ju"></a>The `lift` function lets us achieve the
same effect, by lifting `get` from
`StateT` into `ReaderT`.

<a name="UglyStack.hs:explicitGet"></a>
```haskell
-- file: ch18/UglyStack.hs
explicitGet :: App AppState
explicitGet = lift get
```

<a name="x_Ku"></a>Obviously, when we can let `mtl` do this work for
us, we end up with cleaner code, but this is not always
possible.

### When explicit lifting is necessary

<a name="x_Lu"></a>One case in which we *must* use
`lift` is when we create a monad
transformer stack in which instances of the same typeclass
appear at multiple levels.

<a name="StackStack.hs:Foo"></a>
```haskell
-- file: ch18/StackStack.hs
type Foo = StateT Int (State String)
```

<a name="x_Mu"></a>If we try to use the `put`
action of the `MonadState` typeclass, the instance
we will get is that of `StateT Int`, because it's
at the top of the stack.

<a name="StackStack.hs:outerPut"></a>
```haskell
-- file: ch18/StackStack.hs
outerPut :: Int -> Foo ()
outerPut = put
```

<a name="x_Nu"></a>In this case, the only way we can access the
underlying `State` monad's `put`
is through use of `lift`.

<a name="StackStack.hs:innerPut"></a>
```haskell
-- file: ch18/StackStack.hs
innerPut :: String -> Foo ()
innerPut = lift . put
```

<a name="x_Ou"></a>Sometimes, we need to access a monad more than one level
down the stack, in which case we must compose calls to
`lift`.  Each composed use of
`lift` gives us access to one deeper
level.

<a name="StackStack.hs:Bar"></a>
```haskell
-- file: ch18/StackStack.hs
type Bar = ReaderT Bool Foo

barPut :: String -> Bar ()
barPut = lift . lift . put
```

<a name="x_Pu"></a>When we need to use `lift`,
it can be good style to write wrapper functions that do the
lifting for us, as above, and to use those.  The alternative
of sprinkling explicit uses of `lift`
throughout our code tends to look messy.  Worse, it hard-wires
the details of the layout of our monad stack into our code,
which will complicate any subsequent modifications.

## Understanding monad transformers by building one

<a name="x_Qu"></a>To give ourselves some insight into how monad
transformers in general work, we will create one and describe
its machinery as we go.  Our target is simple and useful.
Surprisingly, though, it is missing from the `mtl`
library: `MaybeT`.

<a name="x_Ru"></a>This monad transformer modifies the behaviour of an
underlying monad `m a` by wrapping its type parameter
with `Maybe`, to give `m (Maybe a)`.   As
with the `Maybe` monad, if we call
`fail` in the `MaybeT` monad
transformer, execution terminates early.

<a name="x_Su"></a>In order to turn `m (Maybe a)` into a
`Monad` instance, we must make it a distinct type,
via a `newtype` declaration.

<a name="MaybeT.hs:newtype"></a>
```haskell
-- file: ch18/MaybeT.hs
newtype MaybeT m a = MaybeT {
      runMaybeT :: m (Maybe a)
    }
```

<a name="x_Tu"></a>We now need to define the three standard monad functions.
The most complex is `(>>=)`, and its innards shed the most light
on what we are actually doing.  Before we delve into its
operation, let us first take a look at its type.

<a name="MaybeT.hs:bindMT.type"></a>
```haskell
-- file: ch18/MaybeT.hs
bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
```

<a name="x_Uu"></a>To understand this type signature, hark back to our
discussion of multi-parameter typeclasses in [the section called “Multi-parameter typeclasses”](programming-with-monads.html#monadcase.mptc).  The thing that we intend to make
a `Monad` instance is the *partial
	type* `MaybeT m`: this has the usual
single type parameter, `a`, that satisfies the
requirements of the `Monad` typeclass.

<a name="x_Vu"></a>The trick to understanding the body of our `(>>=)`
implementation is that everything inside the `do` block executes
in the *underlying* monad `m`,
whatever that is.

<a name="MaybeT.hs:bindMT"></a>
```haskell
-- file: ch18/MaybeT.hs
x `bindMT` f = MaybeT $ do
                 unwrapped <- runMaybeT x
                 case unwrapped of
                   Nothing -> return Nothing
                   Just y -> runMaybeT (f y)
```

<a name="x_Wu"></a>Our `runMaybeT` function unwraps the
result contained in `x`.  Next, recall that the
`<-` symbol desugars to `(>>=)`: a monad
transformer's `(>>=)` must use the underlying monad's `(>>=)`. The
final bit of case analysis determines whether we short circuit
or chain our computation.  Finally, look back at the top of the
body: here, we must wrap the result with the `MaybeT`
constructor, to once again hide the underlying monad.

<a name="x_Xu"></a>The `do` notation above might be pleasant to read, but it
hides the fact that we are relying on the underlying monad's
`(>>=)` implementation.  Here is a more idiomatic version of
`(>>=)` for `MaybeT` that makes this clearer.

<a name="MaybeT.hs:altBindMT"></a>
```haskell
-- file: ch18/MaybeT.hs
x `altBindMT` f =
    MaybeT $ runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)
```

<a name="x_Yu"></a>Now that we understand what `(>>=)` is doing, our
implementations of `return` and
`fail` need no explanation, and neither does
our `Monad` instance.

<a name="MaybeT.hs:Monad"></a>
```haskell
-- file: ch18/MaybeT.hs
returnMT :: (Monad m) => a -> MaybeT m a
returnMT a = MaybeT $ return (Just a)

failMT :: (Monad m) => t -> MaybeT m a
failMT _ = MaybeT $ return Nothing
 
instance (Monad m) => Monad (MaybeT m) where
  return = returnMT
  (>>=) = bindMT
  fail = failMT
```

### Creating a monad transformer

<a name="x_Zu"></a>To turn our type into a monad transformer, we must provide
an instance of the `MonadTrans` class, so that a
user can access the underlying monad.

<a name="MaybeT.hs:MonadTrans"></a>
```haskell
-- file: ch18/MaybeT.hs
instance MonadTrans MaybeT where
    lift m = MaybeT (Just `liftM` m)
```

<a name="x_au"></a>The underlying monad starts out with a type parameter of
`a`: we “inject” the `Just`
constructor so it will acquire the type that we need,
`Maybe a`.  We then hide the monad with our
`MaybeT` constructor.

### More typeclass instances

<a name="x_bu"></a>Once we have an instance for `MonadTrans`
defined, we can use it to define instances for the umpteen
other `mtl` typeclasses.

<a name="MaybeT.hs:mtl"></a>
```haskell
-- file: ch18/MaybeT.hs
instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO m = lift (liftIO m)

instance (MonadState s m) => MonadState s (MaybeT m) where
  get = lift get
  put k = lift (put k)

-- ... and so on for MonadReader, MonadWriter, etc ...
```

<a name="x_cu"></a>Because several of the `mtl` typeclasses use
functional dependencies, some of our instance declarations
require us to considerably relax GHC's usual strict type
checking rules. (If we were to forget any of these directives,
the compiler would helpfully advise us which ones we needed in
its error messages.)

<a name="MaybeT.hs:LANGUAGE"></a>
```haskell
-- file: ch18/MaybeT.hs
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
             UndecidableInstances #-}
```

<a name="x_du"></a>Is it better to use `lift`
explicitly, or to spend time writing these boilerplate
instances?  That depends on what we expect to do with our
monad transformer. If we're going to use it in just a few
restricted situations, we can get away with providing an
instance for `MonadTrans` alone.  In this case, a
few more instances might still make sense, such as
`MonadIO`.  On the other hand, if our transformer
is going to pop up in diverse situations throughout a body of
code, spending a dull hour to write those instances might be a
good investment.

### Replacing the Parse type with a monad stack

<a name="x_eu"></a>Now that we have developed a monad transformer that can
exit early, we can use it to bail if, for example, a parse
fails partway through. We could thus replace the
`Parse` type that we developed in [the section called “Implicit state”](code-case-study-parsing-a-binary-data-format.html#binary.implicit) with a monad customised to our
needs.

<a name="MaybeTParse.hs:Parse"></a>
```haskell
-- file: ch18/MaybeTParse.hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MaybeTParse
    (
      Parse
    , evalParse
    ) where

import MaybeT
import Control.Monad.State
import Data.Int (Int64)
import qualified Data.ByteString.Lazy as L

data ParseState = ParseState {
      string :: L.ByteString
    , offset :: Int64
    } deriving (Show)

newtype Parse a = P {
      runP :: MaybeT (State ParseState) a
    } deriving (Monad, MonadState ParseState)

evalParse :: Parse a -> L.ByteString -> Maybe a
evalParse m s = evalState (runMaybeT (runP m)) (ParseState s 0)
```

### Exercises

1. <a name="id659169"></a><a name="id659171"></a><a name="x_fu"></a>Our `Parse` monad is not a perfect
replacement for its earlier counterpart.  Because we are
using `Maybe` instead of `Either`
to represent a result, we can't report any useful
information if a parse fails.<a name="x_gu"></a>Create an `EitherT sometype` monad
transformer, and use it to implement a more capable
`Parse` monad that can report an error
message if parsing fails.<table border="0" summary="Tip"><tr><td rowspan="2" align="center" valign="top" width="25">![](/support/figs/tip.png)</td><th align="left">Tip</th></tr><tr><td align="left" valign="top"><a name="x_hu"></a>If you like to explore the Haskell
libraries for fun, you may have run across an existing
`Monad` instance for the
`Either` type in the
`Control.Monad.Error` module.  We suggest
that you do not use that as a guide. Its design is
too restrictive: it turns `Either String`
into a monad, when you could use a type parameter
instead of `String`.

<a name="x_T11"></a>*Hint*: If you
follow this suggestion, you'll probably need to use
the `FlexibleInstances` language extension
in your definition.

</td></tr></table>



## Transformer stacking order is important

<a name="x_iu"></a>From our early examples using monad transformers like
`ReaderT` and `StateT`, it might be easy
to conclude that the order in which we stack monad transformers
doesn't matter.

<a name="x_ju"></a>When we stack `StateT` on top of
`State`, it should be clearer that order can indeed
make a difference.  The types `StateT Int (State
	String)` and `StateT String (State Int)`
might carry around the same information, but we can't use them
interchangeably.  The ordering determines when we need to use
`lift` to get at one or the other piece of
state.

<a name="x_ku"></a>Here's a case that more dramatically demonstrates the
importance of ordering.  Suppose we have a computation that
might fail, and we want to log the circumstances under which it
does so.

<a name="MTComposition.hs:problem"></a>
```haskell
-- file: ch18/MTComposition.hs
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Writer
import MaybeT

problem :: MonadWriter [String] m => m ()
problem = do
  tell ["this is where i fail"]
  fail "oops"
```

<a name="x_lu"></a>Which of these monad stacks will give us the information we
need?

<a name="MTComposition.hs:types"></a>
```haskell
-- file: ch18/MTComposition.hs
type A = WriterT [String] Maybe

type B = MaybeT (Writer [String])

a :: A ()
a = problem

b :: B ()
b = problem
```

<a name="x_mu"></a>Let's try the alternatives in **ghci**.

<a name="mtComposition.ghci:problem"></a><pre>ghci> **runWriterT a**
Loading package mtl-1.1.0.0 ... linking ... done.
Nothing
ghci> **runWriter $ runMaybeT b**
(Nothing,["this is where i fail"])
</pre>

<a name="x_nu"></a>This difference in results should not come as a surprise:
just look at the signatures of the execution functions.

<a name="mtComposition.ghci:runWriterT"></a><pre>ghci> **:t runWriterT**
runWriterT :: WriterT w m a -> m (a, w)
ghci> **:t runWriter . runMaybeT**
runWriter . runMaybeT :: MaybeT (Writer w) a -> (Maybe a, w)
</pre>

<a name="x_ou"></a>Our `WriterT`-on-`Maybe` stack has
`Maybe` as the underlying monad, so
`runWriterT` must give us back a result of
type `Maybe`.  In our test case, we only get to see
the log of what happened if nothing actually went wrong!

<a name="x_pu"></a>Stacking monad transformers is analogous to
composing functions.  If we change the order in which we apply
functions, and we then get different results, we are not
surprised.  So it is with monad transformers, too.

## Putting monads and monad transformers into
      perspective

<a name="x_qu"></a>It's useful to step back from details for a few moments, and
look at the weaknesses and strengths of programming with monads
and monad transformers.

### Interference with pure code

<a name="x_ru"></a>Probably the biggest practical irritation of working with
monads is that a monad's type constructor often gets in our
way when we'd like to use pure code.  Many useful pure
functions need monadic counterparts, simply to tack on a
placeholder parameter `m` for some monadic
type constructor.

<a name="monadProblems.ghci:filter"></a><pre>ghci> **:t filter**
filter :: (a -> Bool) -> [a] -> [a]
ghci> **:i filterM**
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
  	-- Defined in Control.Monad
</pre>

<a name="x_su"></a>However, the coverage is incomplete: the standard
libraries don't always provide monadic versions of pure
functions.

<a name="x_tu"></a>The reason for this lies in history.  Eugenio
Moggi introduced the idea of using monads for programming in
1988, around the time the Haskell 1.0 standard was being
developed. Many of the functions in today's
`Prelude` date back to Haskell 1.0, which was
released in 1990.  In 1991, Philip Wadler started writing for
a wider functional programming audience about the potential of
monads, at which point they began to see some use.

<a name="x_uu"></a>Not until 1996, and the release of Haskell 1.3, did the
standard acquire support for monads.  By this time, the
language designers were already constrained by backwards
compatibility: they couldn't change the signatures of
functions in the `Prelude`, because it would have
broken existing code.

<a name="x_vu"></a>Since then, the Haskell community has learned a lot about
creating suitable abstractions, so that we can write code that
is less affected by the pure/monadic divide. You can find
modern distillations of these ideas in the
`Data.Traversable` and `Data.Foldable`
modules.  As appealing as those modules are, we do not cover
them in this book. This is in part for want of space, but also
because if you're still following our book at this point, you
won't have trouble figuring them out for yourself.

<a name="x_wu"></a>In an ideal world, would we make a break from the past,
and switch over `Prelude` to use
`Traversable` and `Foldable` types?
Probably not.  Learning Haskell is already a stimulating
enough adventure for newcomers.  The `Foldable` and
`Traversable` abstractions are easy to pick up when
we already understand functors and monads, but they would put
early learners on too pure a diet of abstraction.  For
teaching the language, it's *good* that
`map` operates on lists, not on
functors.

### Overdetermined ordering

<a name="x_xu"></a>One of the principal reasons that we use monads is that
they let us specify an ordering for effects.  Look again at a
small snippet of code we wrote earlier.


```haskell
-- file: ch18/MTComposition.hs
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Writer
import MaybeT

problem :: MonadWriter [String] m => m ()
problem = do
  tell ["this is where i fail"]
  fail "oops"
```

<a name="x_yu"></a>Because we are executing in a monad, we are guaranteed
that the effect of the `tell` will occur
before the effect of `fail`.  The problem
is that we get this guarantee of ordering even when we don't
necessarily want it: the compiler is not free to rearrange
monadic code, even if doing so would make it more
efficient.

### Runtime overhead

<a name="x_zu"></a>Finally, when we use monads and monad transformers, we
can pay an efficiency tax.  For instance, the `State`
monad carries its state around in a closure.  Closures might
be cheap in a Haskell implementation, but they're not
free.

<a name="x_Av"></a>A monad transformer adds its own overhead to that of
whatever is underneath.   Our `MaybeT` transformer
has to wrap and unwrap `Maybe` values every time we
use `(>>=)`. A stack of `MaybeT` on top of
`StateT` over `ReaderT` thus has a lot
of book-keeping to do for each `(>>=)`.

<a name="x_Bv"></a>A sufficiently smart compiler might make some or all of
these costs vanish, but that degree of sophistication is not
yet widely available.

<a name="x_WJ1"></a>There are relatively simple techniques to avoid some of
these costs, though we lack space to do more than mention them
by name. For instance, by using a continuation monad, we can
avoid the constant wrapping and unwrapping in `(>>=)`, only
paying for effects when we use them.  Much of the complexity
of this approach has already been packaged up in libraries.
This area of work is still under lively development as we
write.   If you want to make your use of monad transformers
more efficient, we recommend looking on Hackage, or asking for
directions on a mailing list or IRC.

### Unwieldy interfaces

<a name="x_Cv"></a>If we use the `mtl` library as a black box, all
of its components mesh quite nicely.  However, once we start
developing our own monads and monad transformers, and using
them with those provided by `mtl`, some
deficiencies start to show.

<a name="x_Dv"></a>For example, if we create a new monad transformer
`FooT` and want to follow the same pattern as
`mtl`, we'll have it implement a typeclass
`MonadFoo`.  If we really want to integrate it
cleanly into the `mtl`, we'll have to provide
instances for each of the dozen or so `mtl` type
classes.

<a name="x_Ev"></a>On top of that, we'll have to declare instances of
`MonadFoo` for each of the
`mtl` transformers.  Most of those instances will
be almost identical, and quite dull to write.  If we
want to keep integrating new monad transformers into the
`mtl` framework, the number of moving parts we must
deal with increases with the *square* of
the number of new transformers!

<a name="x_Fv"></a>In fairness, this problem only matters to a tiny number of
people.  Most users of `mtl` don't need to develop
new transformers at all, so they are not affected.

<a name="x_Gv"></a>This weakness of `mtl`'s design lies with the
fact that it was the first library of monad transformers that
was developed.  Given that its designers were plunging into
the unknown, they did a remarkable job of producing a powerful
library that is easy for most users to understand and work
with.

<a name="x_Hv"></a>A newer library of monads and transformers,
`monadLib`, corrects many of the design flaws in
`mtl`.  If at some point you turn into a hard core
hacker of monad transformers, it is well worth looking
at.

<a name="x_XJ1"></a>The quadratic instances definition is actually a problem
with the approach of using monad transformers. There have been
many other approaches put forward for composing monads that
don't have this problem, but none of them seem as convenient
to the end user as monad transformers.  Fortunately, there
simply aren't that many foundational, generically useful monad
transformers.

### Pulling it all together

<a name="x_Iv"></a>Monads are not by any means the end of the road when it
comes to working with effects and types.  What they are is the
most practical resting point we have reached so far.  Language
researchers are always working on systems that try to provide
similar advantages, without the same compromises.

<a name="x_Jv"></a>Although we must make compromises when we use them, monads
and monad transformers still offer a degree of flexibility and
control that has no precedent in an imperative language.  With
just a few declarations, we can rewire something as fundamental
as the semicolon to give it a new meaning.

<br><hr width="100" align="left">
<sup>[<a name="ftn.id656986"></a>[37](#id656986)] </sup>The name
`mtl` stands for “monad transformer
library”.

![](/support/figs/rss.png) Want to stay up to date? Subscribe to the comment feed for [this chapter](http://book.realworldhaskell.org/feeds/comments/), or the [entire book](http://book.realworldhaskell.org/feeds/comments/).

Copyright 2007, 2008 Bryan O'Sullivan, Don Stewart, and
John Goerzen. This work is licensed under a [Creative
Commons Attribution-Noncommercial 3.0 License](http://creativecommons.org/licenses/by-nc/3.0/). Icons by [Paul Davey](mailto:mattahan@gmail.com) aka [Mattahan](http://mattahan.deviantart.com/).

<table width="100%" summary="Navigation footer"><tr><td width="40%" align="left"><a accesskey="p" href="interfacing-with-c-the-ffi.html">Prev</a> </td><td width="20%" align="center"> </td><td width="40%" align="right"> <a accesskey="n" href="error-handling.html">Next</a></td></tr><tr><td width="40%" align="left" valign="top">Chapter 17. Interfacing with C: the FFI </td><td width="20%" align="center"><a accesskey="h" href="index.html">Home</a></td><td width="40%" align="right" valign="top"> Chapter 19. Error handling</td></tr></table>

