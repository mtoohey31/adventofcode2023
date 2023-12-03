import std/os/file
import std/os/path

fun calibration-val(s : string) : exn int
  val tens = s.list.filter-map(fn(c) { c.string.parse-int }).head.unjust
  val ones = s.list.reverse.filter-map(fn(c) { c.string.parse-int }).head.unjust
  tens * 10 + ones

fun main()
  val input = read-text-file(realpath("../../input"))
  println(input.lines.init.map(calibration-val).sum)
