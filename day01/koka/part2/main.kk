import std/os/file
import std/os/path

val digit-words = [
  "one",
  "two",
  "three",
  "four",
  "five",
  "six",
  "seven",
  "eight",
  "nine"
]

fun reverse(s : string) : string
  s.list.reverse.string

val rev-digit-words = digit-words.map(reverse)

fun first-digit'(s : string, digit-words : list<string>) : <div, exn> int
  val d = digit-words.index-of(fn(pre) { s.starts-with(pre).bool }) + 1
  if d != 0 then
    return d
  match s.head-char.unjust.string.parse-int
    Just(d') -> d'
    Nothing -> s.tail.first-digit'(digit-words)

fun first-digit(s : string) { first-digit'(s, digit-words) }

fun last-digit(s : string)
  first-digit'(s.list.reverse.string, rev-digit-words)

fun calibration-val(s : string) : <div, exn> int
  s.first-digit * 10 + s.last-digit

fun main()
  val input = read-text-file(realpath("../../input"))
  println(input.lines.init.map(calibration-val).sum)
