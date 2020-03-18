package com.scilari.particlefilter.utils

/**
  * This is the non-linear random process presented e.g. in Arulampalam Particle Filter Tutorial
  * @param stepCount Number of steps in the process
  * @param initialState Initial state
  * @param devMotion Standard deviation for the motion
  * @param devMeasurement Standard deviation for the measurement
  * @param devMeasurementLarge Larger standard deviation for the measurement, if applicable
  * @param smallDevMeasurementCondition Condition when to use the normal measurement deviation
  */
class NonLinearRandomProcess(
  val stepCount: Int = 100,
  val initialState: Double = 0,
  val devMotion: Double = Math.sqrt(10),
  val devMeasurement: Double = Math.sqrt(1),
  val devMeasurementLarge: Double = Math.sqrt(100),
  val smallDevMeasurementCondition: Double => Boolean = (_: Double) => true
) {
  import NonLinearRandomProcess._
  def randomFunction(x: Double, k: Int, dev: Double): Double = nonLinearRandomFunction(x, k, dev)
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

class NonLinearRandomProcessGrowing(
  stepCount: Int = 100,
  initialState: Double = 0,
  devMotion: Double = Math.sqrt(10),
  devMeasurement: Double = Math.sqrt(1),
  devMeasurementLarge: Double = Math.sqrt(100),
  smallDevMeasurementCondition: Double => Boolean = (_: Double) => true
) extends NonLinearRandomProcess(stepCount, initialState, devMotion, devMeasurement, devMeasurementLarge, smallDevMeasurementCondition){

  import NonLinearRandomProcess._
  override def randomFunction(x: Double, k: Int, dev: Double): Double = nonLinearRandomFunctionGrowing(x, k, dev)
}

object  NonLinearRandomProcess{

  def nonLinearRandomFunction(x: Double, k: Int, dev: Double = math.sqrt(10)): Double = {
    val next = x/2 + 25*x/(1+x*x) + 8*math.cos(1.2*k)
    val noise = dev*scala.util.Random.nextGaussian()
    next + noise
  }

  def nonLinearRandomFunctionGrowing(x: Double, k: Int, dev: Double = math.sqrt(10)): Double = {
    val next = 4*x/9 + 25*x/(1+x*x) + 0.1 + 8*math.cos(1.2*k) + math.signum(x)*(k/4)
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
    val process = new NonLinearRandomProcess()
    //println("states: " + process.states.mkString(" "))
    //println("measurements: " + process.measurements.mkString(" "))

    val process2 = new NonLinearRandomProcessGrowing(stepCount = 100)
    println("states = " + process2.states.mkString("[", ",", "]"))
    //println("measurements: " + process2.measurements.mkString(" "))

  }



}
