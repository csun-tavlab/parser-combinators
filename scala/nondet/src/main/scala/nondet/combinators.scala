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

    def completeParses(tokens: List[Elem]): Iterator[A] = {
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

    def flatMap[B](f: A => Parser[B]): Parser[B] = {
      tokens1 => {
        self.apply(tokens1).flatMap({ case (a, tokens2) =>
          f(a).apply(tokens2)
        })
      }
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
}
