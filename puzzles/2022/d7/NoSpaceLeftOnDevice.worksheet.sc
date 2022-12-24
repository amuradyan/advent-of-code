val projectPath = new java.io.File(".").getCanonicalPath

val commands = scala.io.Source
   .fromFile(projectPath + "/puzzles/2022/d7/input")
   .getLines
   .toList

opaque type Path = String

object Path:
  def apply(s: String): Path = s

extension (p: Path)
  def up: Path = p match
    case "/" => "/"
    case _ => p.substring(0, p.lastIndexOf('/'))

  def /(s: Path): Path = p match
    case "/" => p + s
    case _ => p + "/" + s

  def name: Path = p match
    case "/" => "/"
    case _ => p.substring(p.lastIndexOf('/') + 1)

case class Info(isDirectory: Boolean, size: Option[Int]):
  override def toString: String = isDirectory match
    case true => "(dir)"
    case false => s"(file, ${size.get})"

class FS(fs: Map[Path, Info] = Map.empty):
  def mkdir(path: Path) = FS(fs + (path -> Info(true, None)))

  def touch(path: Path, size: Int) = FS(fs + (path -> Info(false, Some(size))))

  def directorySizes: Map[Path, Int] =
    fs.filter(_._2.isDirectory)
      .map((path, info) => (path, directorySize(path)))
      .toMap

  def directorySize(path: Path): Int = fs.filter( (p, f) => p.startsWith(path) && !f.isDirectory)
    .map {_._2.size.get}
    .sum

  def freeSpace: Int = TOTAL_FS_SIZE - directorySize("/")

  override def toString: String = fs.map((path, info) => {
      val depth = path.split("/").length - 1
      val indentaion = "  " * depth

      s"$indentaion- $path : $info"
    })
    .mkString("\n")

object FS:
  type Commands = List[String]

  def empty = FS(Map.empty[Path, Info])

  def recover(commands: Commands) = commands
    .foldLeft(InitialState: State) { (state, command) =>
      command match {
        case "$ cd /" => State(state.fs.mkdir("/"), "/")
        case "$ cd .." => State(state.fs, state.currentDir.up)
        case s"$$ cd $dir" => State(state.fs, state.currentDir / dir)
        case "$ ls" => state
        case s"dir $name" => State(state.fs.mkdir(state.currentDir / name), state.currentDir)
        case s"$size $name" => State(state.fs.touch(state.currentDir / name, size.toInt), state.currentDir)
      }
    }
    .fs

case class State(fs: FS, currentDir: Path)

object InitialState
extends State(FS.empty, "")

val fileSystem = FS.recover(commands)

// Part 1: No Space Left On Device

fileSystem
  .directorySizes
  .filter(_._2 < 100000)
  .map(_._2)
  .sum

// Part 2: Freeing up space

val TOTAL_FS_SIZE = 70000000
val MINIMUM_FREE_SPACE = 30000000

val unusedSpace = fileSystem.freeSpace
val minimumSpaceToFree = Math.max(MINIMUM_FREE_SPACE - unusedSpace, 0)

fileSystem
  .directorySizes
  .map(_._2)
  .toList
  .filter(_ > minimumSpaceToFree)
  .sorted
  .head