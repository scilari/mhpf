package com.scilari.particlefilter.utils

/**
 * A random process that defaults to the one presented e.g. in Arulampalam Particle Filter Tutorial
 * @param stepCount Number of steps in the process
 * @param initialState Initial state
 * @param devMotion Standard deviation for the motion
 * @param devMeasurement Standard deviation for the measurement
 * @param devMeasurementLarge Larger standard deviation for the measurement, if applicable
 * @param smallDevMeasurementCondition Condition when to use the normal measurement deviation
 * @param randomFunction (state, timestep, devMotion) => nextState
 */
case class Process(
  stepCount: Int = 100,
  initialState: Double = 0,
  devMotion: Double = Math.sqrt(10),
  devMeasurement: Double = Math.sqrt(1),
  devMeasurementLarge: Double = Math.sqrt(100),
  smallDevMeasurementCondition: Double => Boolean = (_: Double) => true,
  randomFunction: Process.RandomFunction = Process.nonLinearRandomFunction
) {
  import Process._
  def measurementFunction(x: Double, dev: Double = 0): Double = measurement(x, dev)
  val states = new Array[Double](stepCount) // ground truth
  val measurements = new Array[Double](stepCount) // measurements

  for(i <- 0 until stepCount){
    val state =
      randomFunction(
        if(i == 0) initialState else states(i-1),
        i, devMotion
      )

    measurements(i) = measurementFunction(
      state,
      if(smallDevMeasurementCondition(state)) devMeasurement else devMeasurementLarge
    )

    states(i) = state
  }

  def logLikelihoodSmall(x: Double, z: Double): Double = logLikelihood(z, measurementFunction(x), devMeasurement)
  def logLikelihoodLarge(x: Double, z: Double): Double = logLikelihood(z, measurementFunction(x), devMeasurementLarge)

}

object Process{
  type RandomFunction = (Double, Int, Double) => Double

  /**
   * Non-linear random process defined e.g. in Arulampalam et al
   */
  def nonLinearRandomFunction(x: Double, k: Int, dev: Double = math.sqrt(10)): Double = {
    val next = x/2 + 25*x/(1+x*x) + 8*math.cos(1.2*k)
    val noise = dev*scala.util.Random.nextGaussian()
    next + noise
  }

  /**
   * Non-linear random process that converges to either positive or negative side.
   * The transition model is a bit different on both sides, so identifying the side is possible
   */
  def nonLinearRandomFunctionGrowing(x: Double, k: Int, dev: Double = math.sqrt(10)): Double = {
    val transitionFactor = if(x < 0) 20 else 25
    val next = 4*x/9 + transitionFactor*x/(1+x*x) + 8*math.cos(1.2*k) + math.signum(x)*(k/4) // convergence to either side
    val noise = dev*scala.util.Random.nextGaussian()
    next + noise
  }

  def measurement(x: Double, dev: Double = 0.0): Double ={
    x*x/20.0 + dev*scala.util.Random.nextGaussian()
  }

  def logLikelihood(z: Double, expected: Double, dev: Double): Double = {
    val d = expected - z
    -(d*d)/(2*dev*dev)
  }

  def main(args: Array[String]): Unit = {
    val process = new Process()
    println("states (standard): " + process.states.mkString(" "))
    println("measurements (standard): " + process.measurements.mkString(" "))

    val process2 = new Process(randomFunction = nonLinearRandomFunctionGrowing)
    println("states (growing) = " + process2.states.mkString("[", ",", "]"))
    println("measurements (growing): " + process2.measurements.mkString(" "))
  }

}
