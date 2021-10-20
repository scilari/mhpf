Monty Hall Particle Filter
==========================
Scala implementation of Monty Hall Particle Filter (MHPF) described
fully in paper

[Vallivaara, Ilari, et al. 
*"Monty hall particle filter: A new method to tackle predictive model uncertainties." 
Advanced Robotics (ICAR), 2013 16th International Conference on. IEEE, 2013.*](http://ieeexplore.ieee.org/document/6766512/)


In short, the method allows defining multiple likelihood functions
that each correspond to a condition, which defines if the function
should be used to evaluate the particle. 

This allows, e.g., using predictive model's (spatial) uncertainty as a condition. That is, particle's weight should not
be updated, if the predictive model is too uncertain in the particle's position. 
A simple example of this is a robot possibly entering an unmapped area (e.g. part of the cloud entering the unmapped
area). In this case, it makes sense to continue updating the particles in the mapped area with the normal (narrow) likelihood
function, and on the other hand use a more wide likelihood function where the map is unknown or uncertain.

Likewise, the conditions can be used to decide that we should use a very discriminative likelihood function, if
the model's uncertainty is low. 

By defining multiple conditions and likelihoods for different
sources of information, MHPF offers machinery to combine the weights so that only reliable-enough or otherwise
suitable sources are used.

## Usage
The tests contain a simple example on how to use MHPF
in a non-linear toy context, where it is able to reduce the RMSE.
The code snippet below describes the high-level usage. 


```scala
import com.scilari.particlefilter.mhpf._

// Replace with your stuff
class MyParticle{ def move() = ??? }
class MyMap{ def uncertainty(p: MyParticle): Double = ??? }
val particles = Seq.fill[MyParticle](1000)(new MyParticle())
val map = new MyMap()
def doResample(ps: Seq[MyParticle]): Boolean = ???

// Define the functions and conditions (your stuff also)
def isUncertain(p: MyParticle): Boolean = map.uncertainty(p) > 10.0
def isCertain(p: MyParticle): Boolean = !isUncertain(p)
def uncertainLogL(p: MyParticle): Double = ???
def certainLogL(p: MyParticle): Double = ???
val functionConditionPairs = Seq((uncertainLogL(_), isUncertain(_)), (certainLogL(_), isCertain(_)))

// Create MHPF
val mhpf = new MHPF[MyParticle](1000, functionConditionPairs)

// Using inside a loop
for(t <- 0 until 100){
  particles.foreach(_.move())
  val weights = mhpf.computeWeights(particles)
  if(doResample(particles)){
   /* resampling etc */
   mhpf.reset()
  } 
}
```

![Scala CI](https://github.com/scilari/mhpf/workflows/Scala%20CI/badge.svg)
