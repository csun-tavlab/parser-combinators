package nondet

case class ~[+A, +B](_1: A, _2: B)

package proc_states {
  // the iterator protocol stipulates that things *should* be called as follows
  // while (it.hasNext) {
  //   val elem = it.next()
  // }
  // With this in mind, the actual call is actually on hasNext - this will
  // be the first point where one enters the iterator.  This is important
  // in cases where the iterator is empty, as we would otherwise never see it's
  // output if we put the instrumentation on next.
  //
  // hasNext is supposed to be safe to call twice, even if next wasn't called in
  // between.
  //
  // Since hasNext and next are two separate methods, this complicates the logic
  // quite a bit in terms of what we should print out for debugging.  A state machine
  // is used to manage this logic.
  sealed trait TraceState {
    def hasNext[A](
      // wrapped is the iterator we are wrapping around
      wrapped: Iterator[A],
      // printLevel takes the kind of thing we are printing, and a suffix to stick at the end
      printLevel: (String, String) => Unit): (Boolean, TraceState)
    def next[A](
      wrapped: Iterator[A],
      printLevel: (String, String) => Unit): (A, TraceState) = {
      directNext(wrapped, printLevel)
    }
    def entry(printLevel: (String, String) => Unit): Unit = {}

    // used when next is called directly, without calling hasNext first
    def directNext[A](
      wrapped: Iterator[A],
      printLevel: (String, String) => Unit): (A, TraceState) = {
      if (hasNext(wrapped, printLevel)._1) {
        val a = wrapped.next()
        printLevel("Return", s" - $a")
        (a, Yielded)
      } else {
        throw new NoSuchElementException()
      }
    }
  }
  case object Init extends TraceState {
    // Initial state.  No calls to hasNext or next have been made yet.
    def hasNext[A](
      wrapped: Iterator[A],
      printLevel: (String, String) => Unit): (Boolean, TraceState) = {
      printLevel("Call", "")
      val res = wrapped.hasNext
      (res, if (res) HasSoln else PrintFail)
    }
  }
  case object HasSoln extends TraceState {
    // initial call to hasNext returned true
    def hasNext[A](
      wrapped: Iterator[A],
      printLevel: (String, String) => Unit): (Boolean, TraceState) = {
      (true, HasSoln)
    }
  }
  case object Yielded extends TraceState {
    // we have yielded at least one solution, and the user
    // just asked for another via hasNext
    def hasNext[A](
      wrapped: Iterator[A],
      printLevel: (String, String) => Unit): (Boolean, TraceState) = {
      printLevel("Redo", "")
      val res = wrapped.hasNext
      (res, if (res) SpamTrue else PrintFail)
    }
  }
  case object SpamTrue extends TraceState {
    // we reported there is another solution, and are waiting for next
    def hasNext[A](
      wrapped: Iterator[A],
      printLevel: (String, String) => Unit): (Boolean, TraceState) = {
      (true, SpamTrue)
    }
  }
  case object PrintFail extends TraceState {
    // No more solutions are left, but we haven't reported it yet
    def hasNext[A](
      wrapped: Iterator[A],
      printLevel: (String, String) => Unit): (Boolean, TraceState) = {
      (false, SpamFail)
    }
    override def entry(printLevel: (String, String) => Unit): Unit = {
      printLevel("Fail", "")
    }
  }
  case object SpamFail extends TraceState {
    // We reported no solutions are left, but the user keeps
    // calling hasNext
    def hasNext[A](
      wrapped: Iterator[A],
      printLevel: (String, String) => Unit): (Boolean, TraceState) = {
      (false, SpamFail)
    }
  }
} // proc_states

trait Combinators {
  import scala.language.implicitConversions

  type Elem

  // deterministic
  // type Parser[A] = (List[Elem]) => Either[String, (A, List[Elem])]

  // nondeterministic
  // type Parser[A] = (List[Elem]) => Iterator[(A, List[Elem])]

  object ParseInput {
    def apply(tokens: List[Elem]): ParseInput = {
      ParseInput(tokens, TraceInformation(0, Set()))
    }
  }

  case class TraceInformation(
    stackLevel: Int,
    procs: Set[String]) // procs we track

  case class ParseInput(
    tokens: List[Elem],
    traceInfo: TraceInformation
  ) {
    def incrementStackLevel: ParseInput = {
      copy(traceInfo = traceInfo.copy(stackLevel = traceInfo.stackLevel + 1))
    }
    def withTrace(procs: Set[String]): ParseInput = {
      copy(traceInfo = traceInfo.copy(procs = procs))
    }
    def withTokens(newTokens: List[Elem]): ParseInput = {
      copy(tokens = newTokens)
    }
  }

  trait Parser[+A] extends (ParseInput => Iterator[(A, List[Elem])]) {
    self =>

    def phrase(tokens: List[Elem]): Iterator[A] = {
      phrase(ParseInput(tokens))
    }

    def phrase(input: ParseInput): Iterator[A] = {
      self.apply(input).collect({ case (a, Nil) => a})
    }

    def ~[B](pRaw: => Parser[B]): Parser[~[A, B]] = {
      input1 => {
        lazy val p = pRaw
        for {
          (a, tokens2) <- self.apply(input1)
          (b, tokens3) <- p.apply(input1.withTokens(tokens2))
        } yield (new ~(a, b), tokens3)
      }
    }

    def |[B >: A](pRaw: => Parser[B]): Parser[B] = {
      input => {
        // ++ for iterators already uses call-by-name...nice
        self.apply(input) ++ pRaw.apply(input)
      }
    }

    def map[B](f: A => B): Parser[B] = {
      input1 => {
        self.apply(input1).map({ case (a, input2) =>
          (f(a), input2)
        })
      }
    }

    def ^^[B](f: A => B): Parser[B] = map(f)

    def ^^^[B](b: B): Parser[B] = map(_ => b)

    def flatMap[B](f: A => Parser[B]): Parser[B] = {
      input1 => {
        self.apply(input1).flatMap({ case (a, tokens2) =>
          f(a).apply(input1.withTokens(tokens2))
        })
      }
    }

    def filter(p: A => Boolean): Parser[A] = {
      input => {
        self.apply(input).filter(pair => p(pair._1))
      }
    }

    def ~>[B](pRaw: => Parser[B]): Parser[B] = {
      (self ~ pRaw) ^^ (_._2)
    }

    def <~[B](pRaw: => Parser[B]): Parser[A] = {
      (self ~ pRaw) ^^ (_._1)
    }

    def withTrace(procs: Set[String]): Parser[A] = {
      input => {
        self(input.withTrace(procs))
      }
    }
  }

  def success[A](a: A): Parser[A] = {
    input => {
      Iterator((a, input.tokens))
    }
  }

  def failure: Parser[Nothing] = {
    _ => Iterator()
  }

  def accept[A](pf: PartialFunction[Elem, A]): Parser[A] = {
    input => {
      input.tokens match {
        case head :: tail if pf.isDefinedAt(head) => Iterator((pf(head), tail))
        case _ => Iterator()
      }
    }
  }

  implicit def elem(e: Elem): Parser[Elem] = {
    input => {
      input.tokens match {
        case (`e` :: tail) => Iterator((e, tail))
        case _ => Iterator()
      }
    }
  }

  // defines a parser that will appear in trace's output
  def proc[A](name: String)(p: => Parser[A]): Parser[A] = {
    input => {
      if (input.traceInfo.procs(name)) {
        val stackLevel = input.traceInfo.stackLevel

        def printLevel(kind: String, suffix: String): Unit = {
          val spacer = "  "
          println(s"${spacer * stackLevel}[$stackLevel] ${kind}: ${name}(${input.tokens})$suffix")
        }

        new Iterator[(A, List[Elem])] {
          lazy val wrapped = p.apply(input.incrementStackLevel)
          var state: proc_states.TraceState = proc_states.Init

          def hasNext: Boolean = {
            val (retval, newState) = state.hasNext(wrapped, printLevel _)
            newState.entry(printLevel _)
            state = newState
            retval
          }
          def next(): (A, List[Elem]) = {
            val (retval, newState) = state.next(wrapped, printLevel _)
            newState.entry(printLevel _)
            state = newState
            retval
          }
        }
      } else {
        // this isn't one of the procs we are supposed to trace - just pass
        // it on
        p(input)
      }
    }
  }

  def opt[A](p: => Parser[A]): Parser[Option[A]] = {
    p.map(a => Some(a)) | success(None)
  }

  // In a nondeterministic sense, this will return
  // an iterator over all lists, instead of being greedy.
  // This will likely harm performance, since the usual intended
  // behavior is to be greedy.  As an optimization, this will
  // return matches in order of largest to smallest, under the assumption
  // that the greedy behavior is what is intended.
  //
  // If this turns out to be a huge problem, it might be possible to
  // combine these with traditional deterministic parser combinators.
  def rep[A](pRaw: => Parser[A]): Parser[List[A]] = {
    lazy val p = pRaw

    // to avoid reevaluating p
    def inner: Parser[List[A]] = {
      p ~ inner ^^ { case a ~ rest => a :: rest } |
      success(Nil)
    }
    inner
  }

  def repsep[A](pRaw: => Parser[A], delimRaw: => Parser[Any]): Parser[List[A]] = {
    // exp ::= epsilon | nonempty
    // nonempty ::= term | term delim nonempty
    lazy val p = pRaw
    lazy val delim = delimRaw

    def nonempty: Parser[List[A]] = {
      p ~ delim ~ nonempty ^^ { case a ~ _ ~ rest => a :: rest } |
      p ^^ (a => List(a))
    }

    opt(nonempty) ^^ (op => op.getOrElse(Nil))
  }

  def rep1[A](pRaw: => Parser[A]): Parser[List[A]] = {
    rep(pRaw).filter(_.nonEmpty)
  }

  def rep1sep[A](p: => Parser[A], delim: => Parser[Any]): Parser[List[A]] = {
    repsep(p, delim).filter(_.nonEmpty)
  }

  // mimics Prolog's once (https://www.swi-prolog.org/pldoc/man?predicate=once/1),
  // so that only the first solution from the given parser will be considered.
  // Intended as a performance optimization, particularly if greedy behavior
  // is desired from rep and friends.
  def once[A](p: => Parser[A]): Parser[A] = {
    input => {
      p(input).take(1)
    }
  }
}
