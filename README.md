# Models to evaluate the consequences of IPC decisions
Evaluation of consequences of keeping a hospital bay open vs closing a bay for different quarantine time periods

The simulation code can be found in Simulation_model_code folder - a specific README file can be found within the folder

## Running the tests
The tests use the [testthat](https://testthat.r-lib.org/) package (`install.packages("testthat")`). From the repository root, run:

```r
testthat::test_file("tests/test_simulation.R")
```
