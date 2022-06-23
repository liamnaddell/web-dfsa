package tutorial.webapp

import org.scalajs.dom
import org.scalajs.dom.document
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.annotation.JSGlobalScope
import org.scalajs.dom.html
import scala.scalajs.js.annotation.JSImport.Namespace
import scala.scalajs.js

@JSGlobalScope
@js.native
object RenderDot extends js.Object {
  def renderDot(dot: String): Unit = js.native
}

trait BinDig
case class Zero() extends BinDig
case class One() extends BinDig

case class BinStr(digs: Array[BinDig]) 



class Dfsa(var initial: Int, var current_state: Int, var states: Array[Int], var final_states: Array[Int], var transitions: Array[(Int,Int,Int)]) {
  override def toString() : String = {
      return "initial: " + initial.toString() + " current_state: " + current_state.toString() + " states: " + states.mkString(" ") + " final_states: " + final_states.mkString(" ") + " transitions: " + transitions.mkString(" ");
  }
  def initial(i: Int) : Dfsa = {
    val dfsa2 = this;
    dfsa2.initial = i;
    return dfsa2;
  }
  def set_current_state(new_state: Int) : Dfsa = {
    var dfsa2 = this;
    dfsa2.current_state = new_state;
    return dfsa2;
  }
  def add_state(state: Int,is_final: Boolean) : Dfsa = {
    var dfsa2 = this;
    dfsa2.states = this.states.concat(Array(state));
    if (is_final == true) {
      println("adding final state");
      dfsa2=dfsa2.add_final_state(state);
    }
    if (dfsa2.states.length == 1) {
      dfsa2.initial = state;
      dfsa2.current_state = state;
    }
    return dfsa2;
  }
  def add_final_state(fs: Int) : Dfsa = {
    val dfsa2 = this;
    dfsa2.final_states = dfsa2.final_states.concat(Array(fs));
    return dfsa2;
  }
  def add_transition(transitions: (Int,Int,Int)) : Dfsa = {
    val dfsa2 = this;
    dfsa2.transitions = this.transitions.concat(Array(transitions));
    return dfsa2;
  }
  
  def find_transition(state: Int, dig: BinDig): Option[Int] = {
    val all_tns = this.transitions.filter((o => o._1 == state));
    if (all_tns.length == 0) {
      return None;
    } else {
      dig match {
        case Zero() => { return Some(all_tns(0)._1);}
        case One() => { return Some(all_tns(0)._2);}
      }
    }
  }
  def run_transition(dig: BinDig): Dfsa = {
    val tns = find_transition(this.current_state,dig);
    tns match {
      case Some(new_state) => {
        return this.set_current_state(new_state);
      }
      case None => {
        return this;
      }
    }
  }
  def run_string(str: BinStr): Dfsa = {
    str.digs.length match {
      case 0 => return this;
      case n => return this.run_transition(str.digs(0)).run_string(BinStr(str.digs.tail));
    }
  }
  def is_accepted(str: BinStr): Boolean = {
    val final_dfsa = this.run_string(str);
    print(final_dfsa)
    val cstate = final_dfsa.current_state;
    print(cstate)
    return this.is_final(cstate);
  }
  def is_final(s: Int): Boolean = {
    return this.final_states.contains(s);
  }
  def gen_dot(): String = {
    var res = """
digraph g{
  rankdir=LR;
"""
    for (state <- this.states) {
      //"1" [color="red"] only if final 
      if (this.is_final(state)) {
        val tocat0 = s"""   "$state" [color="red"]\n"""
        res=res.concat(tocat0);
      } else {
        val tocat0 = s"""   "$state" [color="black"]\n"""
        res=res.concat(tocat0);
      }
    }

    for ((state,zero,one) <- this.transitions) {
      //"1" -> "2" [label="0"]
      val tocat0 = s"""   "$state" -> "$zero" [label=0]\n"""
      val tocat1 = s"""   "$state" -> "$one" [label=1]\n"""
      res=res.concat(tocat0);
      res=res.concat(tocat1);
    }
    res=res.concat("}");
    return res;
  }
}

object TutorialApp {
  var dfsa: Dfsa = new Dfsa(1,1,Array(),Array(),Array());

  @JSExportTopLevel("web_add_state")
  def web_add_state(): Unit = {
    val state_to_add: Int = document.getElementById("state-to-add").asInstanceOf[html.Input].value.toInt;
    val is_final: Boolean = document.getElementById("state-is-final").asInstanceOf[html.Input].checked;

    println("is_final", is_final);

    println(this.dfsa);
    this.dfsa = dfsa.add_state(state_to_add,is_final)
    println(this.dfsa);
    val dot = this.dfsa.gen_dot();
    println(dot);
    RenderDot.renderDot(dot)
  }
  @JSExportTopLevel("web_add_transition")
  def web_add_transition(): Unit = {
    val state_from: Int = document.getElementById("state-from").asInstanceOf[html.Input].value.toInt;
    val state_to_zero: Int = document.getElementById("state-to-zero").asInstanceOf[html.Input].value.toInt;
    val state_to_one: Int = document.getElementById("state-to-one").asInstanceOf[html.Input].value.toInt;
    this.dfsa = this.dfsa.add_transition((state_from,state_to_zero,state_to_one))
    println(this.dfsa)
    val dot = this.dfsa.gen_dot();
    println(dot);
    RenderDot.renderDot(dot)
  }
  @JSExportTopLevel("web_validate_string")
  def web_validate_string(): Unit = {
    println("validooting");
    val string_to_validate: String = document.getElementById("string-to-validate").asInstanceOf[html.Input].value;
    def c_to_bindig(c: Char): BinDig = {
      if (c == '0')
        Zero()
      else 
        One()
    };
    val binstr = new BinStr(string_to_validate.map(c_to_bindig).toArray);
    print(binstr.toString())
    if (dfsa.is_accepted(binstr)) {
      appendPar(document.body,"Was accepted")
    } else {
      appendPar(document.body,"Was not accepted")
    }

  }
  def appendPar(targetNode: dom.Node, text: String): Unit = {
    val parNode = document.createElement("p")
    parNode.textContent = text
    targetNode.appendChild(parNode)
  }
  def main(args: Array[String]): Unit = {
  }
}
