package tutorial.webapp

import org.scalajs.dom
import org.scalajs.dom.document
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.annotation.JSGlobalScope
import org.scalajs.dom.html
import scala.scalajs.js.annotation.JSImport.Namespace
import scala.scalajs.js
import scala.util.Try

@JSGlobalScope
@js.native
object RenderDot extends js.Object {
  def renderDot(dot: String): Unit = js.native
}

abstract class BinDig
case class Zero() extends BinDig {
  override def toString(): String = {
    "0";
  }
}
case class One() extends BinDig {
  override def toString(): String = {
    "1";
  }
}

case class BinStr(digs: Array[BinDig])  {
  override def toString(): String = {
    digs.mkString("")
  }
}



case class Dfsa(val initial: Int, val current_state: Int, val states: Array[Int], val final_states: Array[Int], val transitions: Array[(Int,Int,BinDig)], val dead: Boolean) {
  override def toString() : String = {
      return "initial: " + initial.toString() + " current_state: " + current_state.toString() + " states: " + states.mkString(" ") + " final_states: " + final_states.mkString(" ") + " transitions: " + transitions.mkString(" ");
  }
  def initial(i: Int) : Dfsa = {
    val dfsa2 = this.copy(initial=i);
    return dfsa2;
  }
  def make_dead(): Dfsa = {
    val dfsa2 = this.copy(dead=true);
    return dfsa2;
  }
  def set_current_state(new_state: Int) : Dfsa = {
    var dfsa2 = this.copy(current_state=new_state);
    return dfsa2;
  }
  def add_state(state: Int,is_final: Boolean) : Dfsa = {
    val new_states = this.states.concat(Array(state));
    val new_final_states = {
      if (is_final == false) {
        this.final_states;
      } else {
        this.final_states.concat(Array(state));
      }
    }
    val dfsa2 = { 
      if (this.states.length == 0) {
        this.copy(initial=state,current_state=state,states=new_states,final_states=new_final_states);
      } else {
        this.copy(states=new_states,final_states=new_final_states);
      }
    }
    return dfsa2;
  }
  def add_final_state(fs: Int) : Dfsa = {
    val dfsa2 = this.copy(final_states = this.final_states.concat(Array(fs)));
    return dfsa2;
  }
  def add_transition(transitions: (Int,Int,BinDig)) : Dfsa = {
    val dfsa2 = this.copy(transitions = this.transitions.concat(Array(transitions)));
    return dfsa2;
  }
  
  def find_transition(state: Int, dig: BinDig): Option[Int] = {
    val all_tns = this.transitions.filter((o => o._1 == state && o._3 == dig));
    if (all_tns.length == 0) {
      return None;
    } else {
        return Some(all_tns(0)._2);
    }
  }
  def run_transition(dig: BinDig): Dfsa = {
    val tns = find_transition(this.current_state,dig);
    tns match {
      case Some(new_state) => {
        return this.set_current_state(new_state);
      }
      case None => {
        //if (!this.is_final(this.current_state)) {
          return this.make_dead();
        //} else {
         // return this;
        //}
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
    if (final_dfsa.dead == true) {
      return false;
    } else {
      val cstate = final_dfsa.current_state;
      print(cstate)
      return this.is_final(cstate);
    }
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
    


      if (this.initial == state && this.is_final(state)) {
        val tocat0 = s"""   "$state" [color="red",label="ini: $state"]\n"""
        res=res.concat(tocat0);
      } else if (this.initial == state && !this.is_final(state)) {
        val tocat0 = s"""   "$state" [color="yellow",label="ini: $state"]\n"""
        res=res.concat(tocat0);
      } else if (this.is_final(state)) {
        val tocat0 = s"""   "$state" [color="red"]\n"""
        res=res.concat(tocat0);
      } else {
        val tocat0 = s"""   "$state" [color="black"]\n"""
        res=res.concat(tocat0);
      }
    }

    for ((from,to,dig) <- this.transitions) {
      //"1" -> "2" [label="0"]
      val strdig = dig.toString()
      val tocat0 = s"""   "$from" -> "$to" [label=$strdig]\n"""
      res=res.concat(tocat0);
    }
    res=res.concat("}");
    return res;
  }
}

object TutorialApp {
  var dfsa: Dfsa = new Dfsa(1,1,Array(),Array(),Array(),false);
  var num_calcs: Int = 0;

  def rerender_dfsa(): Unit = {
    println(this.dfsa);
    val dot = this.dfsa.gen_dot();
    println(dot);
    RenderDot.renderDot(dot)
  }

  @JSExportTopLevel("web_reset_dfsa")
  def web_reset_dfsa(): Unit = {
    this.dfsa = new Dfsa(1,1,Array(),Array(),Array(),false);
    rerender_dfsa();
  }

  @JSExportTopLevel("web_add_state")
  def web_add_state(): Unit = {
    val state_to_add: Int = document.getElementById("state-to-add").asInstanceOf[html.Input].value.toInt;
    val is_final: Boolean = document.getElementById("state-is-final").asInstanceOf[html.Input].checked;

    this.dfsa = dfsa.add_state(state_to_add,is_final)
    rerender_dfsa();
    document.getElementById("state-to-add").asInstanceOf[html.Input].select();
    document.getElementById("state-is-final").asInstanceOf[html.Input].checked = false;
  }
  def handle_int_parse(s: String): Option[Int] = {
    val maybe_int:Option[Int] = Try(s.toInt).toOption
    if (maybe_int.isEmpty) {
      appendPar("Please input a valid int")
    }
    return maybe_int;
  }
  @JSExportTopLevel("web_add_transition")
  def web_add_transition(): Unit = {
    val optup: Option[(Int,Int,Int)] = for {
      state_from <- handle_int_parse(document.getElementById("state-from").asInstanceOf[html.Input].value)
      state_to <- handle_int_parse(document.getElementById("state-to-zero").asInstanceOf[html.Input].value)
      state_dig <-  handle_int_parse(document.getElementById("state-to-one").asInstanceOf[html.Input].value)
    } yield (state_from,state_to,state_dig)

    val (state_from,state_to,state_dig): (Int,Int,Int) = {
      if (optup.isEmpty ) {
        return;
      } else {
        optup.get;
      }
    }

    val dig: BinDig = {
      if (state_dig == 0) {
        Zero();
      } else {
        One();
      }
    }

    this.dfsa = this.dfsa.add_transition((state_from,state_to,dig))

    rerender_dfsa();
    document.getElementById("state-from").asInstanceOf[html.Input].select();
    if (dig == Zero()) {
      document.getElementById("state-to-one").asInstanceOf[html.Input].value="1";
    } else {
      document.getElementById("state-to-one").asInstanceOf[html.Input].value="0";
    }
  }
  @JSExportTopLevel("web_validate_string")
  def web_validate_string(): Unit = {
    println("validating")
    val string_to_validate: String = document.getElementById("string-to-validate").asInstanceOf[html.Input].value;
    def c_to_bindig(c: Char): BinDig = {
      if (c == '0')
        Zero()
      else 
        One()
    };
    val binstr = new BinStr(string_to_validate.map(c_to_bindig).toArray);
    println(binstr.toString())

    val num=this.num_calcs;
    if (dfsa.is_accepted(binstr)) {
      println("appooonding")
      appendPar(f"$num: $string_to_validate was accepted")
    } else {
      println("appooonding")
      appendPar(f"$num: $string_to_validate was not accepted")
    }
    this.num_calcs+=1;
    document.getElementById("string-to-validate").asInstanceOf[html.Input].select();
  }
  def appendPar(text: String): Unit = {
    val targetElement: html.Div = document.getElementById("results_displayer").asInstanceOf[html.Div];

    val parNode = document.createElement("span").asInstanceOf[html.Span]
    parNode.style.display = "block";
    parNode.textContent=text
    targetElement.appendChild(parNode)
  }
  def main(args: Array[String]): Unit = {
  }
}
