val res = formatArgs(Array("zero", "one", "two"))

def printArgs(args: Array[String]): Unit = {
  var i = 0
  while (i < args.length) {
    println(args(i))
    i += 1
  }
}

def printArgs2(args: Array[String]): Unit = {
  for (arg <- args)
    println(arg)
}

def printArgs3(args: Array[String]): Unit = {
  args.foreach(println)
}

println(formatArgs(args))

def formatArgs(args: Array[String]) = args.mkString("\n")
assert(res == "zero\none\ntwo")