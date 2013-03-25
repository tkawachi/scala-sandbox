package net.pikot

import collection.mutable

// ref. http://d.hatena.ne.jp/jetbead/20110923/1316769132
// download test data:
// http://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary.html#a1a
// in sbt console:
// run a1a a1a.t 0.0001
object PassiveAggressive extends App {
  def parseInt(s: String): Int =
    if (s == "+1") {
      1
    } else if (s == "-1") {
      -1
    } else {
      throw new NumberFormatException
    }


  val trainFile = args(0)
  val testFile = args(1)
  val C = args(2).toDouble

  val loop = 10
  val w = collection.mutable.Map[String, Double]()
  val tList = collection.mutable.Buffer[Int]()
  val xList = collection.mutable.Buffer[collection.mutable.Map[String, Int]]()

  for (line <- io.Source.fromFile(trainFile).getLines) {
    val list = line.split("\\s+")
    tList += parseInt(list(0))
    val hash = collection.mutable.Map[String, Int]()

    for (elem <- list.drop(1)) {
      val ary = elem.split(":")
      val a = ary(0)
      val b = ary(1).toInt
      hash(a) = b
      w(a) = 0
    }
    xList += hash
  }

  def loss(w: mutable.Map[String, Double], x: mutable.Map[String, Int], t: Int): Double = {
    var y = 0.0
    x.keys.foreach {
      (f) =>
        if (w.get(f).isDefined) {
          y += w(f) * x(f)
        }
    }
    if (1.0 - t * y <= 0) {
      0.0
    } else {
      1.0 - t * y
    }
  }

  def train(w: mutable.Map[String, Double], x: mutable.Map[String, Int], t: Int) {
    val l = loss(w, x, t)
    var sqX = 0.0
    x.foreach {
      case (key, value) =>
        sqX += value * value
    }
    val tau = l / (sqX + 1.0 / (2 * C))
    x.foreach {
      case (key, value) =>
        w(key) += (tau * t * value)
    }
  }

  for (l <- 1 to loop) {
    for (i <- 1 to tList.size) {
      train(w, xList(i - 1), tList(i - 1))
    }
  }

  var num = 0
  var success = 0

  def predict(w: mutable.Map[String, Double], x: mutable.Map[String, Int]): Double = {
    var y = 0.0
    x.foreach {
      case (key, value) =>
        if (w.get(key).isDefined) {
          y += w(key) * value
        }
    }
    y
  }

  for (line <- io.Source.fromFile(testFile).getLines) {
    val list = line.split("\\s+")
    val hash = collection.mutable.Map[String, Int]()
    list.drop(1).foreach {
      (elem) =>
        val ary = elem.split(":")
        val a = ary(0)
        val b = ary(1).toInt
        hash(a) = b
    }

    val t = predict(w, hash)
    num += 1
    if (t * parseInt(list(0)) >= 0) {
      success += 1
    }
  }

  println("Result: %f (%d/%d)".format(success.toDouble / num, success, num))
}
