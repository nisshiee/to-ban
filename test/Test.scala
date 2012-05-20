import org.specs2._

class Test extends Specification { def is =

  "1 + 1 = 2" ! (1 + 1 must_== 2)
  end
}
