package com.scilari.particlefilter.mhpf

import scala.collection.parallel.CollectionConverters._

/** Parallel version of MHPF. Parallelizes over the likelihood functions.
  */
class ParallelMHPF[ParticleT](
    particleCount: Int,
    functionsAndConditions: IndexedSeq[(ParticleT => Double, ParticleT => Boolean)]
) extends MHPF(particleCount, functionsAndConditions) {

  override def computeWeights(particles: IndexedSeq[ParticleT]): IndexedSeq[Double] = {
    for (fi <- functionsAndConditions.indices.par) {
      updateLogLikelihoods(fi, particles)
      normalizeSubsetWeights(fi)
    }

    combinedWeights = combineWeights()
    combinedWeights
  }
}
