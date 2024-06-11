package scala.u07.examples
import scala.u07.lab.CSPN

object SPn:
  object Repressilator:
    import CSPN.*
    type Gene = Int

    enum Place:
      case GENE, PROTEINE, BLOCKED

    import Place.*

    def net =
      val n = CSPN[Place](
      >(GENE <-- <>[Gene]) ~~> "generate" ~~> >(<>[Gene] --> PROTEINE, <>[Gene] --> GENE),
      >(PROTEINE <-- <>[Gene]) ~~> "degrade" ~~> >(),
      >(BLOCKED <-- <>[Gene]) ~~> "unblock" ~~> >(<>[Gene] --> GENE),
      >(GENE <-- :~[Gene]("x"), PROTEINE <-- :~[Gene]("-x"))
        ~~> "block" ~~>
        >(:~:[Gene]("x")(_ - 1) --> PROTEINE, <>[Gene]("x") --> BLOCKED) ??
        {><[Gene]("x") == ><[Gene]("-x") + 1})
      val r = rates[Place](
        "generate" -> (0.1 * _(GENE)),
        "block" -> (1.0 * _(PROTEINE)),
        "unblock" -> (0.0001 * _(BLOCKED)),
        "degrade" -> (0.001 * _(PROTEINE))
      )
      n.toSpn(r)

  object MembraneSystems:
    enum Place:
      case CELL, RNA, PROTEIN_C
    enum Token:
      case EXT, EXT_INT, EXT_ENV_CAP,
      EXT_INT_VES_ENV_CAP, EXT_INT_CAP
    import Place.*
    import CSPN.*
    import Token.*


    def net  = CSPN[Place](
      >(CELL <-- EXT, RNA <-- EXT_ENV_CAP) ~~> "r1 Enter" ~~> >(EXT_INT_VES_ENV_CAP --> RNA) ** 1,
      >(RNA <-- EXT_INT_VES_ENV_CAP) ~~> "r2 Dissolve" ~~> >(EXT_INT_CAP --> RNA) ** 1,
      >(RNA <-- EXT_INT_CAP) ~~> "r3 Disassemble" ~~> >((5 <> EXT_INT) --> PROTEIN_C, EXT_INT --> RNA) ** 1,
      >(RNA <-- EXT_INT) ~~> "r4 Translate" ~~> >(EXT_INT --> PROTEIN_C, EXT_INT --> RNA) ** 1,
      >(RNA <-- EXT_INT) ~~> "r5 Replicate" ~~> >( (2 <> EXT_INT) --> RNA) ** 1,
      (>(RNA <-- EXT_INT) ++ (5 </> (PROTEIN_C <-- EXT_INT))) ~~> "r6 Assemble" ~~> >(EXT_INT_CAP --> RNA) ** 1,
      >(RNA <-- EXT_INT_CAP) ~~> "r7 Release" ~~> >(EXT_ENV_CAP --> RNA) ** 1
    )




