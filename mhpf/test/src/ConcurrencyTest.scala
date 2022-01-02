package com.scilari.particlefilter.mhpf

import com.scilari.particlefilter.utils.{MinimalParticleFilter, Process}
import com.scilari.particlefilter.utils.MinimalParticleFilter._
import org.scalatest._
import flatspec._
import matchers._
import scala.util.Random
import scala.collection.parallel.CollectionConverters._

class ConcurrencyTest extends AnyFlatSpec with should.Matchers {
  def slowLikelihood(d: Double): Double = {
    val randomNanos = 5000 + Random.nextInt(5000)
    val t0 = System.nanoTime
    while (System.nanoTime - t0 < randomNanos) { /* no-op */ }
    randomNanos.toDouble
  }

  def randomCondition(d: Double): Boolean = Random.nextInt(2) == 0

  def createLikelihoodsAndConditions = IndexedSeq(
    (slowLikelihood(_), randomCondition(_)),
    (slowLikelihood(_), randomCondition(_)),
    (slowLikelihood(_), randomCondition(_))
  )

  def runWithConditions(
      likelihoodsAndConditions: IndexedSeq[(Double => Double, Double => Boolean)],
      parallel: Boolean
  ): Long = {
    val t0 = System.currentTimeMillis
    val particleCount = 10000
    val mhpf =
      if (parallel)
        ParallelMHPF(particleCount, likelihoodsAndConditions, particleThreadCount = 4)
      else
        MHPF(particleCount, likelihoodsAndConditions)
    val descr = if (parallel) "parallel" else "serial"

    for t <- 0 until 50 do
      if t % 10 == 0 then println(s"Updating iteration $t ($descr)")
      mhpf.computeWeights(IndexedSeq.fill(particleCount)(Random.nextDouble()))

    System.currentTimeMillis - t0
  }

  "MHPF" should "work faster on parallel collections" in {
    val sequentialTime = runWithConditions(createLikelihoodsAndConditions, parallel = false)

    val parallelTime = runWithConditions(createLikelihoodsAndConditions, parallel = true)

    info(s"Serial computation took $sequentialTime ms")
    info(s"Parallel computation took $parallelTime ms")

    parallelTime should be < (sequentialTime / 2)

  }

}
