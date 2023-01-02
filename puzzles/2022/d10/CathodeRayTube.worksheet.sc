val rawCommands = scala.io.Source
   .fromFile("./puzzles/2022/d10/input")
   .getLines
   .toList

type Log = Map[Int, Int]

case class CPU(regX: Int, clock: Int, log: Log)

rawCommands
  .foldLeft(CPU(1, 1, Map(1 -> 1))) { (cpu, instruction) =>
    instruction match
      case s"addx $n"=>
        CPU(
          cpu.regX + n.toInt,
          cpu.clock + 2,
          cpu.log + (cpu.clock + 1 -> cpu.regX) + (cpu.clock + 2 -> (cpu.regX + n.toInt)))
      case "noop"=>
        CPU(cpu.regX, cpu.clock + 1, cpu.log + (cpu.clock + 1 -> cpu.regX))
  }
  .log
  .filter((idx, _) => ((idx - 20) % 40 == 0))
  .map((idx, value) => idx * value)
  .sum