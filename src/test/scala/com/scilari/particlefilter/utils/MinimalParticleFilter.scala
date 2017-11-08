package com.scilari.particlefilter.utils

import com.scilari.particlefilter.mhpf.MHPF

class MinimalParticleFilter(val n: Int = 50, val process: NonLinearRandomProcess, val resampleThreshold: Double = 0.5) {
  import MinimalParticleFilter._

  var currentMeasurement: Double = 0

  val invN = 1.0/n

  val mhpf = new MHPF[Double](
    n,
    Seq(
      (process.logLikelihoodSmall(_, currentMeasurement) , process.smallDevMeasurementCondition(_)),
      (process.logLikelihoodLarge(_, currentMeasurement), !process.smallDevMeasurementCondition(_))
    )
  )

  var particles: Array[Double] = Array.fill[Double](n)( process.initialState )

  var weights: Array[Double] = Array.fill[Double](n)(invN)

  def updateWeights(): Unit ={
    weights = mhpf.computeWeights(particles).toArray
  }

  def moveParticles(k: Int): Unit ={
    particles = particles.map{ p =>
      process.randomFunction(p, k, process.devMotion)
    }
  }

  def resample(): Unit ={
    if(nEff(weights) <= resampleThreshold*n ){
      val childIx = systematicSampling(weights)
      particles = childIx.map{ i => particles(i) }
      resetWeights()
      mhpf.reset()
    }
  }

  def resetWeights(): Unit ={
    weights = Array.fill[Double](n)(invN)
  }

  def update(measurement: Double, k: Int): Unit ={
    currentMeasurement = measurement
    moveParticles(k)
    updateWeights()
    resample()
  }

  def mean(): Double ={
    weights.zip(particles).map{case (w, p) => w*p}.sum
  }


}

object MinimalParticleFilter{

  def systematicSampling(w: Array[Double]): Array[Int] = {
    val sampleCount = w.length
    val cdf = w.map{var s = 0.0; d => {s += d; s}}
    val IX = new Array[Int](sampleCount)
    val step = 1.0/sampleCount
    var r0 = scala.util.Random.nextDouble()*step
    var j = 0
    for(i <- cdf.indices){
      while(cdf(i) >= r0 && j < sampleCount){
        IX(j) = i
        r0 += step
        j += 1
      }
    }
    IX
  }

  def nEff(w: Array[Double]): Double ={
    1.0/w.map(x => x*x).sum
  }

  def runForRMSE(pf: MinimalParticleFilter, pr: NonLinearRandomProcess): Double ={
    val errors = for(i <- pr.states.indices) yield {
      pf.update(pr.measurements(i), i)
      val s = pr.states(i)
      val m = pf.mean()
      s - m
    }

    math.sqrt(errors.map(e => e*e).sum/errors.size)
  }

  // This is the error measure used e.g. in Hol
  def runForRMSEIgnoringSign(pf: MinimalParticleFilter, pr: NonLinearRandomProcess): Double ={
    val errors = for(i <- pr.states.indices) yield {
      pf.update(pr.measurements(i), i)
      val s = pr.states(i)
      val m = pf.mean()
      math.abs(s) - math.abs(m)
    }

    math.sqrt(errors.map(e => e*e).sum/errors.size)
  }



}
