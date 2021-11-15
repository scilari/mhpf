package com.scilari.particlefilter.mhpf

import scala.collection.parallel.CollectionConverters._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.collection.parallel.ForkJoinTaskSupport

/** Parallel version of MHPF. Parallelizes over the likelihood functions and particles.
  */
class ParallelMHPF[ParticleT](
    particleCount: Int,
    functionsAndConditions: IndexedSeq[(ParticleT => Double, ParticleT => Boolean)],
    particleThreadCount: Int = 4
) extends MHPF(particleCount, functionsAndConditions) {

  override def functionwiseUpdates(particles: IndexedSeq[ParticleT]): Unit = {
    val results = functionsAndConditions.indices.map { fi =>
      Future {
        updateLogLikelihoods(fi, particles)
        normalizeSubsetWeights(fi)
      }
    }

    Await.ready(Future.sequence(results), Duration.Inf)
  }

  override def updateLogLikelihoods(fi: Int, particles: IndexedSeq[ParticleT]): Unit = {
    val indices = particles.indices.par
    indices.tasksupport = new ForkJoinTaskSupport(
      new java.util.concurrent.ForkJoinPool(particleThreadCount)
    )

    val (f, c) = functionsAndConditions(fi)
    indices.foreach { pi =>
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
