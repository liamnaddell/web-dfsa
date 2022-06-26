package tutorial.webapp

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TutorialAppTests extends AnyFlatSpec with Matchers {
  "The Dfsa object" should "make new initial" in {
    val dfsa  = Dfsa(0,0,Array(0,1),Array(0),Array((0,0,0)))
    val dfsa2 = dfsa.initial(1);

    dfsa.initial shouldEqual  0
    dfsa2.initial shouldEqual 1
  }
  "The Dfsa object" should "add new state" in {
    val dfsa  = Dfsa(0,0,Array(0,1),Array(0),Array((0,0,0)))
    val dfsa2 = dfsa.add_state(2,true);

    dfsa.initial shouldEqual 0
    dfsa.states shouldEqual Array(0,1)
    dfsa.final_states shouldEqual Array(0)

    dfsa2.initial shouldEqual 0
    dfsa2.states shouldEqual Array(0,1,2)
    dfsa2.final_states shouldEqual Array(0,2)
  }
  "The Dfsa object" should "find transitions" in {
    val dfsa = Dfsa(0,0,Array(0),Array(0),Array(
    //  (0,1,2),
      (0,1,2),
      (3,4,5),
      (6,7,8),
      (9,1,2),
      (4,5,6),
      (7,8,9)
      ));

    dfsa.find_transition(0,Zero()) shouldEqual Some(1)
    dfsa.find_transition(9,One()) shouldEqual Some(2)
    dfsa.find_transition(8,One()) shouldEqual None
    dfsa.find_transition(0,One()) shouldEqual Some(2)
    dfsa.find_transition(10,Zero()) shouldEqual None
    dfsa.find_transition(10,One()) shouldEqual None
  }
  "The Dfsa object" should "run transitions" in {
    var dfsa = Dfsa(0,0,Array(0,1,2,3,4,5,6,7,8,9),Array(0),Array(
      (0,1,2),
      (1,3,9),
      (2,4,5),
      (3,4,5),
      (6,7,8),
      (9,1,2),
      (4,5,6),
      (7,8,9)
      ));

    dfsa = dfsa.run_transition(Zero())
    dfsa.current_state shouldEqual 1
    dfsa = dfsa.run_transition(One())
    dfsa.current_state shouldEqual 9
    dfsa = dfsa.run_transition(One())
    dfsa.current_state shouldEqual 2
    dfsa = dfsa.run_transition(One())
    dfsa.current_state shouldEqual 5
  }
  "The Dfsa object" should "run strings" in {
    var dfsa = Dfsa(0,0,Array(0,1,2,3,4,5,6,7,8,9),Array(0),Array(
      (0,1,2),
      (1,3,9),
      (2,4,5),
      (3,4,5),
      (6,7,8),
      (9,1,2),
      (4,5,6),
      (7,8,9)
    ));

    val arr: Array[BinDig] = Array(Zero(),One(),One(),One());
    val binstr:BinStr = new BinStr(arr)
    dfsa = dfsa.run_string(binstr)
    dfsa.current_state shouldEqual 5
  }
}
