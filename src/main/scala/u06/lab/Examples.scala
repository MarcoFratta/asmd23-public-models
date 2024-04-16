package scala.u06.lab
import scala.u06.lab.PetriNetFacade.CPN

object Examples:
  import CPN.*
  import PetriNetApi.*

  enum ComPlace:
    case SEND, RECEIVED, NS, NR,
    A, B, C, D
  import ComPlace.*
  def comProtocol: PetriNet[ComPlace] =
    type Packet = (Int, String)
    type Counter = Int

    given stringSum: Composable[String] = _ + _

    // Tells how a packet should be expanded to its components
    val packet = ~+((p: Packet) => >(p._1 -> "n", p._2 -> "p"))

    val net = CPN[ComPlace](
      >(SEND <-- packet, NS <-- <>[Int]("n")) ~~> "Send packet"
        ~~> >(<>[Packet] --> A, <>[Packet] --> SEND, <>[Int]("n") --> NS),
      >(A <-- <>[Packet]) ~~> "Transmit packet" ~~> >(<>[Packet] --> B),
      >(B <-- packet, NR <-- <>[Int]("n"), RECEIVED <-- <>[String]("str"))
        ~~> "Receive packet" ~~> >(
        :~:[Int]("n")(_ + 1) --> C,
        :~:[Int]("n")(_ + 1) --> NR,
        >>[String](<>[String]("str"), <>[String]("p")) % (_ => "str") --> RECEIVED),
      >(C <-- <>[Int]) ~~> "Transmit ack" ~~> >(<>[Int] --> D),
      >(D <-- <>[Int], NS <-- :~[Int]("k")) ~~> "Receive ack" ~~> >(<>[Int]("n") --> NS),
    )
    net