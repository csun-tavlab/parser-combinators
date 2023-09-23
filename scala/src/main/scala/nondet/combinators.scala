package nondet

case class ~[+A, +B](_1: A, _2: B)

trait Combinators {
  import scala.language.implicitConversions

  type Elem

  // deterministic
  // type Parser[A] = (List[Elem]) => Either[String, (A, List[Elem])]

  // nondeterministic
  // type Parser[A] = (List[Elem]) => Iterator[(A, List[Elem])]

  trait Parser[+A] extends (List[Elem] => Iterator[(A, List[Elem])]) {
    self =>

    def phrase(tokens: List[Elem]): Iterator[A] = {
      self.apply(tokens).collect({ case (a, Nil) => a})
    }

    def ~[B](pRaw: => Parser[B]): Parser[~[A, B]] = {
      tokens1 => {
        lazy val p = pRaw
        for {
          (a, tokens2) <- self.apply(tokens1)
          (b, tokens3) <- p.apply(tokens2)
        } yield (new ~(a, b), tokens3)
      }
    }

    def |[B >: A](pRaw: => Parser[B]): Parser[B] = {
      tokens => {
        // ++ for iterators already uses call-by-name...nice
        self.apply(tokens) ++ pRaw.apply(tokens)
      }
    }

    def map[B](f: A => B): Parser[B] = {
      tokens1 => {
        self.apply(tokens1).map({ case (a, tokens2) =>
          (f(a), tokens2)
        })
      }
    }

    def ^^[B](f: A => B): Parser[B] = map(f)

    def ^^^[B](b: B): Parser[B] = map(_ => b)

    def flatMap[B](f: A => Parser[B]): Parser[B] = {
      tokens1 => {
        self.apply(tokens1).flatMap({ case (a, tokens2) =>
          f(a).apply(tokens2)
        })
      }
    }

    def filter(p: A => Boolean): Parser[A] = {
      tokens => {
        self.apply(tokens).filter(pair => p(pair._1))
      }
    }

    def ~>[B](pRaw: => Parser[B]): Parser[B] = {
      (self ~ pRaw) ^^ (_._2)
    }

    def <~[B](pRaw: => Parser[B]): Parser[A] = {
      (self ~ pRaw) ^^ (_._1)
    }
  }

  def success[A](a: A): Parser[A] = {
    tokens => {
      Iterator((a, tokens))
    }
  }

  def failure: Parser[Nothing] = {
    _ => Iterator()
  }

  def accept[A](pf: PartialFunction[Elem, A]): Parser[A] = {
    tokens => {
      tokens match {
        case head :: tail if pf.isDefinedAt(head) => Iterator((pf(head), tail))
        case _ => Iterator()
      }
    }
  }

  implicit def elem(e: Elem): Parser[Elem] = {
    tokens => {
      tokens match {
        case (`e` :: tail) => Iterator((e, tail))
        case _ => Iterator()
      }
    }
  }

  def opt[A](p: => Parser[A]): Parser[Option[A]] = {
    p.map(a => Some(a)) | success(None)
  }

  def phrase[A](p: Parser[A]): Parser[A] = {
    tokens => p.phrase(tokens).map(a => (a, Nil))
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
    tokens => {
      p(tokens).take(1)
    }
  }
}