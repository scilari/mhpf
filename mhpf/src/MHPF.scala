package com.scilari.particlefilter.mhpf

/** Computes weights and keeps track of the needed variables according to Monty Hall Particle Filter
  * (Vallivaara 2013).
  *
  * @param particleCount
  *   Number of particles
  * @param functionsAndConditions
  *   Likelihood functions returning likelihoods as logarithmic values and conditions corresponding
  *   to the functions
  * @tparam ParticleT
  *   Particle parameter type
  */
class MHPF[-ParticleT](
    val particleCount: Int,
    val functionsAndConditions: IndexedSeq[(ParticleT => Double, ParticleT => Boolean)]
) {

  // Class for keeping track of geometric mean of log-likelihood and the number of its evaluations
  private class State(val logL: Double = 0.0, val evalCount: Int = 0)

  // States for every function-condition pair
  private var states: Array[Array[State]] = _

  // Weights for every function-condition pair
  private var allWeights: Array[Array[Double]] = _

  // Precomputed reciprocal
  private val invParticleCount = 1.0 / particleCount

  // Weight combined from all function-condition pair weights
  protected var combinedWeights = IndexedSeq.fill[Double](particleCount)(invParticleCount)

  reset()

  def weights: IndexedSeq[Double] = combinedWeights

  def reset(): Unit = {
    states = Array.fill[State](functionsAndConditions.size, particleCount)(new State())
    allWeights = Array.fill[Double](functionsAndConditions.size, particleCount)(invParticleCount)
    combinedWeights = IndexedSeq.fill[Double](particleCount)(invParticleCount)
  }

  private def reset(fi: Int): Unit = {
    states(fi) = Array.fill[State](particleCount)(new State())
    allWeights(fi) = Array.fill[Double](particleCount)(invParticleCount)
    combinedWeights = IndexedSeq.fill[Double](particleCount)(invParticleCount)
  }

  def computeWeights(particles: IndexedSeq[ParticleT]): IndexedSeq[Double] = {
    for (fi <- functionsAndConditions.indices) {
      updateLogLikelihoods(fi, particles)
      normalizeSubsetWeights(fi)
    }

    combinedWeights = combineWeights()
    combinedWeights
  }

  protected def updateLogLikelihoods(fi: Int, particles: IndexedSeq[ParticleT]): Unit = {
    val (f, c) = functionsAndConditions(fi)
    for (pi <- particles.indices) {
      val p = particles(pi)
      // Computing and updating the likelihood values if the corresponding condition is met
      if (c(p)) {
        val logL = f(p)
        val data = states(fi)(pi)
        val newEvalCount = data.evalCount + 1
        val newLogL = (data.logL * data.evalCount + logL) / newEvalCount
        states(fi)(pi) = new State(newLogL, newEvalCount)
      }
    }
  }

  protected def normalizeSubsetWeights(fi: Int): Unit = {
    // Finding the subset with its indices
    val state = states(fi)
    val (s, si) = state.zipWithIndex.filter { case (d, _) => d.evalCount > 0 }.unzip
    val sN = s.length.toDouble

    if (sN > 0) {
      // Mean of likelihood counts used to estimate "missing" values
      val m = s.map { _.evalCount }.sum / sN
      val logL = s.map { _.logL * m }

      // Normalizing log likelihoods
      val maxLogL = logL.max
      val normLogL = logL.map { _ - maxLogL }

      // Mapping back to likelihood space
      val weights = normLogL.map { math.exp }

      // Normalize inside the subset
      val Ws = weights.sum
      val normFactor = sN * invParticleCount * 1 / Ws

      if (Ws > 0) {
        // Update subset weights with normalized values
        for (i <- si.indices) {
          val ix = si(i)
          this.allWeights(fi)(ix) = weights(i) * normFactor
        }
      } else {
        // Subset weights sums to zero - fixing by resetting the corresponding weights
        failInfo = Some(
          s"\nNormalizing subset ($fi) weights ($sN) has failed (sums to zero). Resetting to 1/N."
        )
        reset(fi)
      }
    }
  }

  protected def combineWeights(): IndexedSeq[Double] = {
    // multiply over function indices
    val combinedWeights: IndexedSeq[Double] = for {
      pi <- 0 until particleCount
    } yield {
      val particleWeights = functionsAndConditions.indices.map { allWeights(_)(pi) }
      particleWeights.product
    }

    val sum = combinedWeights.sum
    if (sum > 0.0) {
      // normalizing the final weights
      val invSum = 1.0 / sum
      combinedWeights.map { _ * invSum }
    } else {
      // Combined weights sums to zero - fixing by resetting the filter
      failInfo = Some(
        s"\n Normalizing weights has failed (sums to zero). Resetting to everything to 1/N."
      )
      reset()
      IndexedSeq.fill[Double](particleCount)(invParticleCount)
    }
  }

  var failInfo: Option[String] = None

  override def toString: String = {
    val name = "Monty Hall Particle filter \n"
    val condStrings = for (fi <- functionsAndConditions.indices) yield {
      val data = states(fi)
      val s = data.filter { _.evalCount > 0 }
      val sN = s.length
      val N = particleCount
      s"Condition/likelihood no $fi. Ratio w/ measurements: ${sN.toDouble / N}"
    }
    val output = name + condStrings.mkString("\n") + failInfo.mkString("")
    failInfo = None
    output
  }

}
