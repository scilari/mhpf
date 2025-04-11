package com.scilari.particlefilter.mhpf

import scala.collection.parallel.CollectionConverters._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.collection.parallel.ForkJoinTaskSupport
import java.util.concurrent.ForkJoinPool

/** Parallel version of MHPF. Parallelizes over the likelihood functions and particles.
  */
class ParallelMHPF[ParticleT](
    particleCount: Int,
    functionsAndConditions: IndexedSeq[(ParticleT => Double, ParticleT => Boolean)],
    particleThreadCount: Int = 16
) extends MHPF(particleCount, functionsAndConditions) {

  val parallelIndices = (0 until particleCount)
    .grouped(math.ceil(particleCount.toDouble / particleThreadCount).toInt)
    .toArray
    .par

  parallelIndices.tasksupport = ForkJoinTaskSupport(ForkJoinPool(particleThreadCount))

  override def updateLogLikelihoods(fi: Int, particles: IndexedSeq[ParticleT]): Unit = {
    val (f, c) = functionsAndConditions(fi)
    parallelIndices.foreach { ixs =>
      ixs.foreach { pi =>
        val p = particles(pi)
        // Computing and updating the likelihood values if the corresponding condition is met
        if (c(p)) {
          val logL = f(p)
          val data = states(fi)(pi)
          val newEvalCount = data.evalCount + 1
          val newLogL = (data.logL * data.evalCount + logL) / newEvalCount
          states(fi)(pi) = State(newLogL, newEvalCount)
        }
      }
    }
  }

}
