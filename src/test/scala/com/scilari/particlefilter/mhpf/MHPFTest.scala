package com.scilari.particlefilter.mhpf

import com.scilari.particlefilter.utils.{MinimalParticleFilter, NonLinearRandomProcess, NonLinearRandomProcessGrowing}
import com.scilari.particlefilter.utils.MinimalParticleFilter._
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class MHPFTest extends FlatSpec{

  "MHPF" should "give similar results to standard PF estimating the non-linear process (see Arulampalam tutorial on PF)" in {
    val errors = for(run <- 0 until 100) yield {
      val pr = new NonLinearRandomProcess(stepCount = 100)
      val pf = new MinimalParticleFilter(50, pr, resampleThreshold = 1.0)
      // Using RMSE as a sanity check to compare to Arulampalam, although it is not necessarily meaningful in multimodal
      // situations
      val rmse = runForRMSE(pf, pr)
      rmse
    }

    val meanError = errors.sum/errors.size
    info("Mean error: " + meanError)
    meanError should be (5.54 +- 1)

  }

  it should "give better results than fixed-likelihood PF with changing uncertainty " +
    "especially when motion model uncertainty is small compared to measurement model" in {
    val stepCount = 100
    val particleCount = 100
    val devMotion = 0.5 // small value here
    val devMeasurementSmall = 2.0
    val devMeasurementLarge = 30.0
    val conditionSmallDev = (x: Double) => math.abs(x % 10.0) < 5.0

    val runCount = 100

    type Process = NonLinearRandomProcess
    // TODO: although usually better MHPF diverges on this case more often than fixed dev => investigate why
    // type Process = NonLinearRandomProcessGrowing

    def errorFunction(pf: MinimalParticleFilter, pr: NonLinearRandomProcess) = runForRMSE(pf, pr)

    def prSmall = new Process(
      stepCount = stepCount, devMeasurement = devMeasurementSmall, devMotion = devMotion
    )

    def prLarge = new Process(
      stepCount = stepCount, devMeasurement = devMeasurementLarge, devMotion = devMotion
    )

    def prCombined = new Process(
      stepCount = stepCount, devMeasurement = devMeasurementSmall, devMeasurementLarge = devMeasurementLarge,
      smallDevMeasurementCondition = conditionSmallDev, devMotion = devMotion
    )

    def errorsForDev(devs: Seq[Double], prToRunAgainst: => NonLinearRandomProcess): Seq[Double] = {
      val errors = devs.map { dev =>
        val prForPf = new Process(
          stepCount = stepCount, devMeasurement = dev, devMotion = devMotion)
        val pf = new MinimalParticleFilter(n = particleCount, prForPf)

        val fixedErrors = for (run <- 0 until runCount) yield {
          errorFunction(pf, prToRunAgainst)
        }
        // remove 5% top and bottom values as outliers
        val k = fixedErrors.size/20
        val filteredErrors = fixedErrors.sorted.drop(k).dropRight(k)
        filteredErrors.sum/filteredErrors.size

      }
      errors
    }

    val (optimalSmallDev, optimalSmallError) = {
      val devs = 0.5 until devMeasurementSmall + 2.0 by 0.1
      val errors = errorsForDev(devs, prSmall)
      (devs zip errors).minBy{_._2}
    }

    val (optimalLargeDev, optimalLargeError) = {
      val devs = devMeasurementLarge/2 until devMeasurementLarge + 5.0 by 1.0
      val errors = errorsForDev(devs, prLarge)
      (devs zip errors).minBy{_._2}
    }

    val (optimalCombinedDev, optimalCombinedError) = {
      val devs = (0.1 until devMeasurementSmall + 2.0 by 0.1) ++ (devMeasurementSmall + 2.0 until devMeasurementLarge + 5 by 1.0)
      val errors = errorsForDev(devs, prCombined)
      (devs zip errors).minBy{_._2}
    }

    println("Optimal small dev: " + optimalSmallDev + " " + optimalSmallError)
    println("Optimal large dev: " + optimalLargeDev + " " + optimalLargeError)
    println("Optimal combined dev: " + optimalCombinedDev + " " + optimalCombinedError)

    val errors: Seq[(Double, Double)] = for(run <- 0 until runCount) yield {
      val prActual = new Process(
        stepCount = stepCount, devMeasurement = devMeasurementSmall, devMeasurementLarge = devMeasurementLarge,
        smallDevMeasurementCondition = conditionSmallDev, devMotion = devMotion
      )

      val prMhpf = new Process(
        stepCount = stepCount, devMeasurement = optimalSmallDev, devMeasurementLarge = optimalLargeDev,
        smallDevMeasurementCondition = conditionSmallDev, devMotion = devMotion
      )

      val prFixed = new Process(
        stepCount = stepCount, devMeasurement = optimalCombinedDev, devMotion = devMotion)

      val mhpf = new MinimalParticleFilter(particleCount, prMhpf)
      val pfFixed = new MinimalParticleFilter(particleCount, prFixed)

      val errorMhpf = errorFunction(mhpf, prActual)
      val errorFixed = errorFunction(pfFixed, prActual)

      println("mhpf: " + errorMhpf +" fixed: " + errorFixed)
      (errorMhpf, errorFixed)
    }

    val (errorsMhpf, errorsFixed) = errors.unzip
    val meanErrorMhpf = errorsMhpf.sum/errorsMhpf.size
    val meanErrorFixed = errorsFixed.sum/errorsFixed.size
    println("Mean error mhpf: " + meanErrorMhpf + " mean error fixed: " + meanErrorFixed)

    meanErrorMhpf should be < meanErrorFixed

    assert(false, "Testing github actions")

  }

  it should "implicitly handle outliers assuming they are detected (often possible)" in {
    val steps = 100
    val particleCount = 100
    val runCount = 100

    // 20% of totally broken points (outliers)
    val devLarge = 1000.0
    val conditionSmall = (x: Double) => math.abs(x % 10.0) < 8.0

    // Give 10% chance for false positives in outlier detection
    val conditionSmallForMhpf = (x: Double) => conditionSmall(x) && math.random() < 0.9

    val devMotion = 0.5

    val errors: Seq[(Double, Double)] = for(run <- 0 until runCount) yield {

      val prActual = new NonLinearRandomProcess(
        stepCount = steps,
        devMeasurementLarge = devLarge,
        smallDevMeasurementCondition = conditionSmall,
        devMotion = devMotion
      )

      val prMhpf = new NonLinearRandomProcess(
        stepCount = steps,
        devMeasurementLarge = devLarge,
        smallDevMeasurementCondition = conditionSmallForMhpf,
        devMotion = devMotion
      )

      val prFixed = new NonLinearRandomProcess(
        stepCount = steps,
        devMotion = devMotion
      )

      val mhpf = new MinimalParticleFilter(particleCount, prMhpf, resampleThreshold = 0.5)
      val pfFixed = new MinimalParticleFilter(particleCount, prFixed, resampleThreshold = 0.5)

      val errorMhpf = runForRMSE(mhpf, prActual)
      val errorFixed = runForRMSE(pfFixed, prActual)

      println("mhpf: " + errorMhpf + " fixed: " + errorFixed)
      (errorMhpf, errorFixed)
    }

    val (errorsMhpf, errorsFixed) = errors.unzip
    val meanErrorMhpf = errorsMhpf.sum/errorsMhpf.size
    val meanErrorFixed = errorsFixed.sum/errorsFixed.size
    println("Mean error mhpf: " + meanErrorMhpf + " mean error fixed: " + meanErrorFixed)

    meanErrorMhpf should be < meanErrorFixed
  }

}
