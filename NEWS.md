# MoveR release v0.2.0

* Add readAnimalTA function to import tracking data from AnimalTA software.
* Add MSD to compute mean square displacement of particles according to the equations derived from Kareiva & Shigesada (1983) or Turchin (2015).
* Modify the Dcoef function to properly use the equations derived from Kareiva & Shigesada (1983) or Turchin (2015) to compute net square displacement index.
* Improve the speed of the slidWindow function by using stats::filter instead of a 'for loop' within the function's body.
NB: while it increases the speed of the slidWindow function, it also make it less flexible, it is not possible to perform a custom computation over the sliding window anymore. Instead, the 'statistic' argument allows to choose some already implemented computation methods.
* some minor modifications in readTrackR function


# MoveR dev. release v0.1.0

* This is the development release of the MoveR package.
* This version is still under active development and might be unstable.
