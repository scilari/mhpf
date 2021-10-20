package com.scilari.particlefilter.mhpf

import com.scilari.particlefilter.utils.{MinimalParticleFilter, Process}
import com.scilari.particlefilter.utils.MinimalParticleFilter._
import org.scalatest._
import flatspec._
import matchers._

class MHPFTest extends AnyFlatSpec with should.Matchers {

  "MHPF" should "give similar results to standard PF estimating the non-linear process (see Arulampalam tutorial on PF)" in {
    val errors = for (run <- 0 until 100) yield {
      val pr = new Process(stepCount = 100)
      val pf = new MinimalParticleFilter(n = 50, process = pr, resampleThreshold = 1.0)
      // Using RMSE as a sanity check to compare to Arulampalam, although it is not necessarily meaningful in multimodal
      // situations
      val rmse = runForRMSE(pf, pr)
      rmse
    }

    val meanError = errors.sum / errors.size
    info("Mean error: " + meanError)
    meanError should be(5.54 +- 1)
  }

  def testWithChangingUncertainty(
      stepCount: Int = 100,
      particleCount: Int = 100,
      devMotion: Double = 1,
      devMeasurementSmall: Double = 2.0,
      devMeasurementLarge: Double = 15.0,
      conditionSmallDev: Double => Boolean = (x: Double) => math.abs(x % 10.0) < 7.5,
      resampleThreshold: Double = 0.5,
      runCount: Int = 100,
      randomFunction: Process.RandomFunction = Process.nonLinearRandomFunction
  ): Unit = {
    def errorFunction(pf: MinimalParticleFilter, pr: Process) = runForRMSE(pf, pr)

    def createMixedProcess = new Process(
      stepCount = stepCount,
      devMeasurement = devMeasurementSmall,
      devMeasurementLarge = devMeasurementLarge,
      smallDevMeasurementCondition = conditionSmallDev,
      devMotion = devMotion,
      randomFunction = randomFunction
    )

    def createFixedProcess(dev: Double) = new Process(
      stepCount = stepCount,
      devMeasurement = dev,
      devMotion = devMotion,
      randomFunction = randomFunction
    )

    def createFixedPf(process: Process) = new MinimalParticleFilter(
      n = particleCount,
      process = process,
      resampleThreshold = resampleThreshold
    )

    def errorsForDevs(devs: Seq[Double]): Seq[Double] = {
      val errors = devs.map { dev =>

        val fixedErrors = for (_ <- 0 until runCount) yield {
          val prForPf = createFixedProcess(dev)
          val pf = createFixedPf(prForPf)
          val prToRunAgainst = createMixedProcess
          errorFunction(pf, prToRunAgainst)
        }
        fixedErrors.sum / fixedErrors.size
      }
      errors
    }

    val (optimalCombinedDev: Double, optimalCombinedError: Double) = {
      val devs = (BigDecimal(0.5) until devMeasurementLarge + 5.0 by 0.5).map { _.toDouble }
      val errors = errorsForDevs(devs)
      info("ERRORS: " + errors.mkString(" "))
      (devs zip errors).minBy { _._2 }
    }

    info("Optimal combined dev: " + optimalCombinedDev + " " + optimalCombinedError)

    var mhpfBetterCount = 0

    val errors: Seq[(Double, Double)] = for (_ <- 0 until runCount) yield {
      val prActual = createMixedProcess

      val prMhpf = createMixedProcess

      val prFixed = createFixedProcess(optimalCombinedDev)

      val mhpf =
        new MinimalParticleFilter(particleCount, prMhpf, resampleThreshold = resampleThreshold)
      val pfFixed = createFixedPf(prFixed)

      val errorMhpf = errorFunction(mhpf, prActual)
      val errorFixed = errorFunction(pfFixed, prActual)

      info("mhpf: " + errorMhpf + " fixed: " + errorFixed)
      if (errorMhpf < errorFixed) mhpfBetterCount += 1

      (errorMhpf, errorFixed)
    }

    val (errorsMhpf, errorsFixed) = errors.unzip
    val meanErrorMhpf = errorsMhpf.sum / errorsMhpf.size
    val meanErrorFixed = errorsFixed.sum / errorsFixed.size
    val mhpfBetterRatio = mhpfBetterCount.toDouble / runCount
    info("Mean error mhpf: " + meanErrorMhpf + " mean error fixed: " + meanErrorFixed)
    info(s"Mhpf better ratio: $mhpfBetterRatio")

    meanErrorMhpf should be < meanErrorFixed
    mhpfBetterRatio should be > 0.55
  }

  it should "give better results than fixed-likelihood PF with changing uncertainty (standard process)" in {
    info("Testing changing uncertainty with standard process")
    testWithChangingUncertainty(
      randomFunction = Process.nonLinearRandomFunction
    )
  }

  it should "give better results than fixed-likelihood PF with changing uncertainty (growing process)" in {
    info("Testing changing uncertainty with growing process")
    testWithChangingUncertainty(
      randomFunction = Process.nonLinearRandomFunctionGrowing,
      particleCount = 500 // More particles to prevent random divergence
    )
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

    val errors: Seq[(Double, Double)] = for (run <- 0 until runCount) yield {

      val prActual = new Process(
        stepCount = steps,
        devMeasurementLarge = devLarge,
        smallDevMeasurementCondition = conditionSmall,
        devMotion = devMotion
      )

      val prMhpf = new Process(
        stepCount = steps,
        devMeasurementLarge = devLarge,
        smallDevMeasurementCondition = conditionSmallForMhpf,
        devMotion = devMotion
      )

      val prFixed = new Process(
        stepCount = steps,
        devMotion = devMotion
      )

      val mhpf = new MinimalParticleFilter(particleCount, prMhpf, resampleThreshold = 0.5)
      val pfFixed = new MinimalParticleFilter(particleCount, prFixed, resampleThreshold = 0.5)

      val errorMhpf = runForRMSE(mhpf, prActual)
      val errorFixed = runForRMSE(pfFixed, prActual)

      info("mhpf: " + errorMhpf + " fixed: " + errorFixed)
      (errorMhpf, errorFixed)
    }

    val (errorsMhpf, errorsFixed) = errors.unzip
    val meanErrorMhpf = errorsMhpf.sum / errorsMhpf.size
    val meanErrorFixed = errorsFixed.sum / errorsFixed.size
    info("Mean error mhpf: " + meanErrorMhpf + " mean error fixed: " + meanErrorFixed)

    meanErrorMhpf should be < meanErrorFixed
  }

}
