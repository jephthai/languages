;; ------------------------------------------------------------------------
;;         Sloth - the slow, but adorable programming language
;;
;;                by Joshua Stone (yakovdk@gmail.com)
;; ------------------------------------------------------------------------
;;
;; In this literate program, I will set out to create a small
;; concatenative programming language.  For lack of a better name, and
;; frankly, it's pretty hard to name languages, I'll call it Sloth.
;; This is a pedagogical interpreter; it's not efficient, but the code
;; should be easy to read.  As of my first testing, it comes in about
;; 6x slower than Ruby (which is saying something) in a completely
;; unrealistic benchmark battle.  Nevertheless, it's a functioning
;; language that could serve as the basis for experimentation.
;;
;; Most basic "interpreter" tutorials build a calculator and call it
;; quits.  My intent is to make this a complete programming language,
;; if light on built-in functionality.  The intent is to illustrate
;; how a small language interpreter can be constructed, as well as
;; some of the conveniences that come with the concatenative
;; programming paradigm.
;;
;; In a concatenative language, code is built on the concept of
;; composition, rather than function application.  While in most
;; languages, we're used to seeing functions with various types,
;; concatenative functions always have compatible type signatures --
;; usually identical, in fact.
;;
;; In a typical applicative language, functions can take any number of
;; arguments, and return values.  We think of programming in these
;; languages in terms of applying functions to arguments, they get
;; names, and the compiler / interpreter has to manage tracking the
;; values.
;;
;; Most languages are applicative.  In contrast, a concatenative
;; language is characterized by using the exact same type signature
;; for all functions.  By doing this, the naming and passing of
;; arguments by value goes away, and the program becomes less about
;; applying functions to values, and more about denoting a sequence of
;; actions.  
;;
;; I don't want to make a whole essay on concatenative languages, but
;; I'll give you just a quick picture of the difference.  Whereas in
;; an applicative language, we would write this:
;;
;;    foo(bar(baz(x)));
;;
;; This is actually written "backwards" when we consider the order of
;; execution.  In a concatenative language, it would look like this:
;;
;;    x baz bar foo
;;
;; Which means, "Put x on the stack, then do 'baz', then do 'bar',
;; then do 'foo'."  While this is perhaps strange compared to other
;; language paradigms, there are a number of places where we see code
;; that at least looks concatenative in other places.  For example, in
;; most object oriented languages, it's not unusual to see code like
;; this:
;;
;;    x.baz().bar().foo();
;;
;; Or, perhaps even more obviously analogous to concatenative
;; programming, many of us happy UNIX users are accustomed to shell
;; pipelines.  This is probably the most common form of concatenative
;; programming:
;;
;;   cat x | baz | bar | foo
;;
;; Instead of using pipes to redirect input and output streams between
;; named actions, like the shell does, a concatenative program uses a
;; shared data structure for implicit data passing.  I said before
;; that concatenative language functions all have the same "type".
;; Below, on the left you see a typical function for an applicative
;; language.  On the right is the "analogous" definition for a
;; concatenative function.
;;
;;    int double(x) {           stack double(stack s) {
;;      return x + x;             s.dup();
;;    }                           s.add();
;;                                return s;
;;                              }
;;
;; If you can get your mind to think in this way, you can use
;; concatenative languages to do general purpose programming.  These
;; languages have the potential to be very pretty, and lead to
;; expressive code.  
;;
;; It's not all beds of roses, though -- concatenative code can also
;; be very difficult to read when written poorly.  The implicit hiding
;; of data in the compositional chain of function calls requires the
;; programmer to express code well so that the data flow can still be
;; understood.  Finally, "stack juggling" can make some code difficult
;; to follow.
;;
;; Classic concatenative languages like Forth can seem anachronistic,
;; and terse function names and a cultural focus on conciseness can
;; make it seem like concatenative languages are inaccessibly arcane.
;; Today, we'll make a little concatenative language that isn't quite
;; so challenging, and is a little easier to use and read.
;;
;; As a concatenative language, we need to to create the implicit data
;; structure that functions can use to receive and store data.  We'll
;; just use a linked list, which starts empty.  It turns out that the
;; Common Lisp functions #'push and #'pop make it super easy to use a
;; list like this as a stack.

(defparameter *stack* '())

;; We'll need to know when the interpreter is running, and have a way
;; for error conditions or exits to signal that execution should stop.

(defparameter *running* nil)

;; The next thing our language needs is a way to store the definition
;; of functions.  The interpreter must be able to look up functions
;; and obtain the code to execute their behavior as needed.  A most
;; convenient way to do this is with a hash table:

(defparameter *dictionary* (make-hash-table))

;; We will need to create functions in the dictionary, so we'll decide
;; on there basic representation.  There will be two types -- built-in
;; functions that will form a layer of primitive operations, and
;; functions defined in the language itself.  So functions have three
;; things:
;;
;;    +------+    +------+------+
;;    | name |--->| kind | code |
;;    +------+    +------+------+
;;
;; The dictionary will index functions by name, and store a "kind"
;; code that indicates whether the code is a primitive (i.e., it is
;; implemented in Lisp) or defined in the language.  Here's how we
;; create a primitive:

(defmacro defprimitive (name &body code)
  `(setf (gethash ',name *dictionary*)
	 (list :primitive (lambda () ,@code t))))

;; And to show how it works, we'll define a couple primitives for
;; managing objects on the stack.  These will be handy later when we
;; need to do some of that undesirable "stack juggling."

(defprimitive dup  (push (first *stack*) *stack*))
(defprimitive drop (pop *stack*))
(defprimitive over (push (second *stack*) *stack*))

(defprimitive swap
  (let ((a (pop *stack*))
	(b (pop *stack*)))
    (push a *stack*)
    (push b *stack*)))

;; And, of course, there are going to be other functions that can be
;; defined in terms of those primitives.  These "defined" functions
;; will either be defined in the source code processed by the
;; interpreter or here, in the following macro.

(defmacro defword (name &body code)
  `(setf (gethash ',name *dictionary*)
	 (list :defined ',code)))

;; The difference between this macro and the primitive macro, other
;; than the "kind" symbol, is subtle, so don't miss it.  A defined
;; function is simply a list of function calls and literals, which in
;; this interpreter will be represented as a Lisp list.  Here's an
;; example of a defined function based on the primitives above:

(defword nip  swap drop)

;; Since we can now define our functions in the dictionary, we turn
;; our attention to executing them as code.  This is an interpreter,
;; so we're not compiling these to machine code (not even for a
;; bytecode VM or some such).  Executing code will depend on the kind
;; of function.  Primitives can simply be executed when called.
;; Defined functions will need to be processed in sequence, executing
;; each literal or function call as encountered.
;;
;; To support these operations, we'll need a few utilities for
;; managing function bindings in the dictionary:

(defun lookup (function) (gethash function *dictionary*))
(defun kind   (binding)  (first binding))
(defun code   (binding)  (second binding))

;; Also, as we are processing code (particularly in defined
;; functions), we'll encounter literals.  Now, literals are an
;; interesting thing in concatenative languages.  We can imagine that
;; a literal is actually a function.  For example, the function "1"
;; pushes the number 1 on the stack.
;;
;;    (defprimitive 1 (push 1 *stack*))
;;
;; While conceptually beautiful, this won't make any sense for our
;; interpreter, since we can't afford to fill up memory with functions
;; for all the numbers.  So we can make a concession -- while
;; theoretically, a literal is a function that pushes itself on the
;; stack, we'll need to be able to detect literals and handle them as
;; a special case.
;;
;; Since we're going to lean on the Lisp reader as our parser, we will
;; generally use Common Lisp syntax for literals.  This is pretty
;; standard fare for things like numbers and strings, but some
;; literals (like characters) are a little weird if you're not a Lisp
;; fan.  For now, all we need to do is detect the literals that we
;; support:

(defun literalp (token)
  (or (numberp token)
      (stringp token)
      (characterp token)))

;; For reasons that will become apparent later (in brief, we will need
;; to implement functions that can access the token stream), we will
;; create a global variable used by the interpreter to represent its
;; current location in the code.  This will actually be a "code
;; stack", where the top of the stack represents the current code
;; sequence being executed.  Items lower on the stack will be code to
;; be "returned" to when the current sequence is done.

(defparameter *code* nil)

;; And any time we need to execute a token, this is the function that
;; will do it.  Note again the special case for literals -- this is
;; baked in here, but it still behaves like the theoretical literals
;; mentioned above.

(declaim (ftype function execute)) ; make SBCL happy

(defun execute-token (token)
  (let ((binding (lookup token)))
    (cond
      ((literalp token) 
       (push token *stack*))

      ((eq :primitive (kind binding))
       (funcall (code binding)))

      ((eq :defined (kind binding))
       (push (code binding) *code*)
       (execute)))))

;; Note that last clause, which executes code for a defined function.
;; All we do is process the list of tokens in the code for the
;; function, passing them to (execute).  This passing, however, is
;; through the *code* variable (this way we can do some
;; metaprogramming that manipulates the code later).

(defun execute ()
  (cond
    ((null *code*) nil)
    ((null (first *code*)) (pop *code*))
    (t (let ((token (pop (first *code*))))
	 (execute-token token)
	 (execute))))) 

;; (note, hopefully your Lisp does tail call optimization!)

(defun execute-code (code)
  (push code *code*)
  (execute))

;; So now we should actually have enough infrastructure to run some
;; very basic programs.  I've run these while writing this code, but
;; I'll leave these commented out so as to ensure this remains a
;; usable literate program.
;;
;; Example 1: push two literals and run 'over' to copy the first one
;;
;;   (defword test 1 2 over)
;;   (execute-token 'test) -> stack: '(1 2 1)
;;
;; So this is pretty neat -- we can define primitive functions,
;; defined functions, and execute them.  But doing this all in Lisp
;; doesn't make it a useful language.  We need the ability to read
;; code from a string and execute it -- from there, the next step is
;; to read code from a file to a string, et voila, we will have the
;; beginning of a real programming language.

(defun parse (string)
  (with-input-from-string (stream string)
    (loop for token = (read stream nil nil)
	 while token collect token)))

;; Now example 1 looks more like this:
;;
;;   (execute-code (parse "1 2 over"))
;;
;; To do some real work, we need to be able to do more than just push
;; literals to the stack and manipulate the stack's contents.  Now
;; it's time to define a bunch of basic literals for math functions,
;; string functions, etc.

(defmacro defoperator (name op)
  `(setf (gethash ',name *dictionary*)
	 (list :primitive 
	       (lambda ()
		 (let ((b (pop *stack*))
		       (a (pop *stack*)))
		   (push (,op a b) *stack*)
		   t)))))

(defoperator +   +)
(defoperator -   -)
(defoperator /   /)
(defoperator *   *)
(defoperator % mod)
(defoperator <   <)
(defoperator >   >)
(defoperator >= >=)
(defoperator <= <=)
(defoperator =   =)

(defprimitive sqrt (push (sqrt (pop *stack*)) *stack*))

(defprimitive print  (format t "~a" (pop *stack*)))
(defprimitive emit   (write-char (pop *stack*)))
(defprimitive char   (code-char (pop *stack*)))
(defprimitive unchar (char-code (pop *stack*)))

(defword newline #\Newline emit)
(defword space   #\Space emit)
(defword tab     #\Tab emit)

;; So now we can do some math.  Here's an example that does math for
;; us:
;;
;;   (execute-code (parse "3 dup * 4 dup * + sqrt print newline"))
;;
;; So if we needed a (very complicated) calculator, we'd be all set.
;; But for a real programming language, we need to be able to create
;; bindings from our code.  We haven't treated how a programmer is
;; supposed to define a new function.  Somehow, we need to expose the
;; functionality we're currently expressing using our defword function
;; as a mechanism available in the language itself.
;;
;; For once, we have to define some syntax.  How about we do it like
;; this?
;;
;;   define <name> <tokens>* end
;;
;; The "define" function will read ahead in the token stream,
;; collecting everything up to the next "end" token.  This list is
;; then saved in the dictionary as a function with the indicated name.
;; While I'm only using this concept very briefly here, there's a lot
;; of metaprogramming magic you can do with this idea.

(defprimitive define
  (setf (gethash (pop (first *code*)) *dictionary*)
	(list :defined
	      (loop
		 for token = (pop (first *code*))
		 until (eq token 'end)
		 collect token))))

;; This is pretty fun -- now we can define functions within the
;; language, they'll join the dictionary, and we can refer to them.  A
;; little program like this can now be written:
;;
;;   (execute-code (parse "
;;     define square 
;;       dup *
;;     end
;;     
;;     define hyp
;;       square swap square + sqrt
;;     end
;;     
;;     3 4 hyp print newline
;;     "))
;;
;; If you pull that out of these comments and run it, you'll see that
;; the hypotenuse of a triangle with two sides of length 3 and 4 is 5
;; ("hyp" implements the Pythagorean theorem).  The only thing our
;; language is missing, now, for general computation is conditional
;; execution and loops.
;;
;; If you look at our definition of "define" above, you can probably
;; imagine how we might create loops the same way.  We could create
;; more syntax for keywords that mark where sections of code are
;; conditionally executed or passed over, or iterated multiple times
;; in a loop.
;;
;; Instead of going that route, I'll introduce another concept that is
;; popular in concatenative languages.  It is the idea of a
;; "quotation" or "quoted program."  If you come from other language
;; paradigms that have a "lambda" construct or "anonymous function",
;; you will see some similarities here.
;;
;; The special thing about quotations in a concatenative language is
;; that since we are not operating in an applicative mode, we don't
;; have to worry about defining the arguments and return values.  A
;; quotation is, itself, just a concatenative program (hence the
;; sometimes popular term "quoted program").  In fact, here is where
;; we can see the reason these are called concatenative languages --
;; you can take any sequence of words in the language and break it out
;; and it becomes its own program.  Programs can be concatenated to
;; compose their functionality, and this becomes very interesting with
;; quotations.
;;
;; First, how do we define a quotation?  Owing to our representation
;; of a "defined" function as a list of tokens, we can just make a
;; quotation put a list of tokens on the stack.  This can be
;; accomplished with this definition of the "[" word":

(defprimitive [
  (let ((count 0)
	(quote '()))
    (loop for word = (pop (first *code*))
	 until (and (eq word '])
		    (zerop count))
	 do
	 (when (eq word '[) (incf count))
	 (when (eq word ']) (decf count))
	 (push word quote))
    (push (nreverse quote) *stack*)))

;; OK, this is a little hairy.  The interesting thing that we have to
;; be careful about with quotations is that a quotation may, itself,
;; contain a quotation.  We could just decide that's not allowed in
;; the language (if you want to do that, define a function for the
;; inner quotation, and call it, for example).  But we don't want to
;; inconvenience the programmer too much just for the sake of simple
;; interpreter development.
;;
;; So in the code above, we process tokens until we get to the end of
;; the quotation, but every time we run into a nested quotation
;; (another '[ symbol), we have to account for it.  The "count"
;; variable lets us keep track of nesting, and we only terminate when
;; we get to the outermost '].
;;
;; Once we have these beautiful quotations on our stack, what do we do
;; to run them?  We need a primitive that can call them:

(defprimitive call 
  (push (pop *stack*) *code*) 
  (execute))

;; Now that we can create quotations, we have the building blocks for
;; conditionals.  Since we can imagine that quotations are little
;; anonymous functions, then we can think of the functions we'll
;; create that manipulate those quotations as combinators.  If you
;; hail from functional programming, you'll recognize what that means.
;; If not, just think of it as functions that manipulate other
;; functions.  Here's "if":

(defprimitive true  (push t *stack*))
(defprimitive false (push nil *stack*))

(defprimitive if
  (let ((false-quote (pop *stack*))
	(true-quote  (pop *stack*)))
    (if (pop *stack*)
	(push true-quote *stack*)
	(push false-quote *stack*))
    (push 'call (first *code*))))

;; That last line is a little nifty.  What we're doing is pulling a
;; true- and a false-quotation from the stack, deciding which one to
;; put back, and then adding a new token to the input stream ('call).
;; You could likely implement some neat capabilities with this, since
;; it's possible to create functions that write new ad-hoc code at run
;; time.  By pushing this 'call function, when we return from running
;; "if", the interpreter will now see this function call that wasn't
;; there before.  Here's an example of using "if":
;;
;;   5 2 % =            ; this puts a boolean on the stack
;;   [ "Even!" print ]  ; if true, it's an even number
;;   [ "Odd!"  print ]  ; if false, it's an odd number
;;   if
;;
;; Hopefully, here you can see how the "if" combinator just pops the 
;; top three items from the stack, and then determines which of the
;; quotations to execute.  
;;
;; And here is an implementation of a simple loop: "times"

(defprimitive times
  (let ((quote (pop *stack*))
	(count (pop *stack*)))
    (loop for i below count do
	 (push quote *code*)
	 (execute))))    

;; Now, extending the above example of conditionals, here's a little
;; code that will print out which numbers are even and odd from 0 to
;; 5:
;; 
;; 0 6                                Output:
;; [ dup print " is " print             0 is Even!
;;   dup 2 % 0 =			1 is Odd! 
;;   [ "Even!" print newline ]		2 is Even!
;;   [ "Odd!"  print newline ]		3 is Odd! 
;;   if					4 is Even!
;;   1 + 				5 is Odd! 
;; ] times
;;
;; Here are a few more reasonable primitives that may come in handy;;
;; Very useful for debugging -- just put it anywhere and see what the
;; stack looks like.

(defprimitive debug
  (format t "Stack: ")
  (loop for nodes = *stack* then (cdr nodes)
       for i below 5
       while nodes
       do (format t "~s " (first nodes)))
  (fresh-line))

(defprimitive exit
  (setf *running* nil) 
  (setf *code* nil))

(defprimitive reset
  (setf *stack* '()))

(defprimitive words
  (loop
     for name being the hash-keys in *dictionary*
     for (kind code) = (gethash name *dictionary*)
     do (format t "~10a function: ~20a~%" kind name)))

;; With that, we actually have a good foundation for a usable
;; programming language.  We'll now make the interpreter's user
;; interface -- this will just be a function to load a program from a
;; disk file and execute it.

(defun slurp (file)
  (with-open-file (stream file)
    (let* ((len (file-length stream))
	   (str (make-string len)))
      (read-sequence str stream :end len)
      str)))

(defun run (file)
  (setf *stack* nil)
  (setf *code* nil)
  (setf *running* t)
  (execute-code (parse (slurp file))))

(defun banner ()
  (format t "~%------------------------------------------------------------~%")
  (format t "      Sloth - A slow, but adorable programming language~%")
  (format t "------------------------------------------------------------~%~%")
  (format t "usage: sloth.lisp <file>~%~%"))
  
(if (= (length sb-ext:*posix-argv*) 2)
    (run (second sb-ext:*posix-argv*))
    (banner))

(quit)

;;
;; EPILOGUE and CONCLUSION:
;;
;; I've prepared a sample program that uses all of the features we've
;; built into the language so far.  It calculates and prints out the
;; first 10 factorials (starting at 0).  You can look at the code in
;; the adjoining file.  Here's an example of running it:
;;   
;;   $ ./sloth factorial.sloth
;;   
;;   Listing first 10 factorials:
;;   
;;   0! = 1
;;   1! = 1
;;   2! = 2
;;   3! = 6
;;   4! = 24
;;   5! = 120
;;   6! = 720
;;   7! = 5040
;;   8! = 40320
;;   9! = 362880
;; 
;; So, is it done?  Not really -- not by a long shot!  But it is
;; enough core functionality on which to build a full-size language,
;; if that was your goal.  It's obviously lacking some conveniences,
;; and the API is very small.  If you are interested in playing with
;; it, I would recommend the following ideas as opportunities to
;; enhance it:
;;
;;   (1)  Support defining, reading, and updating global variables
;;   (2)  Build an I/O layer for reading and writing files
;;   (3)  Build a string manipulation library
;;   (4)  Define more combinators to get cool loops and abstractions
;;   (5)  Make the dictionary a stack with modules to create namespaces
;;   (6)  Create an interface to Lisp lists in Sloth
;;   (7)  Build a built-in stack library based on lists 
;;   (8)  Add in basic error checking and nice messages / stack traces
;;   (9)  Create "compiled" functions that don't require #'lookup to run
;;   (10) Make dictionary access and advance token reads available to
;;        user-defined functions (e.g., Forth's "defining" words, etc.)
;;
;; How similar is this to the popular programming languages?  Well,
;; not very -- this interpreter treats code as a linked list and looks
;; up function bindings every time they're executed.  It makes the
;; code very clean, but it's incredibly inefficient.  Depending on
;; your Common Lisp environment, if you don't have tail call
;; optimization, you might not even be able to run long programs
;; because of stack space (#'execute is recursive, after all).
;;
;; That said, there's nothing fundamental about the design that
;; couldn't be mapped to a more efficient implementation.  Instead of
;; storing code as linked lists, create sequences of pointers to code
;; segments.  Functions could be "compiled" so that dynamic execution
;; doesn't have to lookup bindings all the time.  The sky's the limit!
;;
;; I hope this has been a fun ride -- it took me a few hours or so to
;; put together this little literate program.  I've enjoyed the
;; process, and this has helped solidify some of my ideas for more
;; serious attempts at a more complete, efficient concatenative
;; language.  I hope you, the reader, have gotten something from it
;; too!
;;
;; -Josh Stone-
;; yakovdk@gmail.com
;;
;;
