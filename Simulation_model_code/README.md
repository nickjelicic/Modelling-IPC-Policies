The model is organized into $5$ separate R scripts, each responsible for a specific stage of the simulation. 
For a detailed description of the inputs to each function - including their types and ranges - see the preamble at the top of the file.
For an example run of each file, see 'Example runs.R'.

#Simulation Metrics.R
This file contains the main function of the simulation model. 
It starts by setting up the ward and initial patient structure, based on inputted configurations. It then randomly samples the current infection state of patients in the focus bay, based on an inputted distribution. Following this, the following files are run, in order:
 1. 'Initial Patient Attributes.R' is run to determine the attributes of patients in the ward at the start of the simulation
 2. 'Infection Times.R' is run to determine the time at which patients currently in the ward contract an infection
 3. 'Arrival Patient Attributes.R' is run to determine the arrival times and attributes of new patients that arrive during the simulation
 4. 'Ward Simulation.R' is run to determine the output metrics, given the initial and arrival patient attributes and infection times.

The function returns a matrix, where each row corresponds to one of the specified stopping times. The $i$th row of the matrix corresponds to the simulated metric values after the $i$th stopping time.

For an example of how to run the simulation model, see the final example in 'Examples runs.R'


#Arrival Patient Attributes.R
This file determines the attributes of the patients that will arrive at the ward during the simulation.
Specifically, it randomly generates the number of arrivals and the corresponding attributes and arrival times for each patient, using the specified rate parameters and distributions.

The function outputs a list of attributes for each patient as a list of positive numeric vectors. A particular index corresponds to the same patient across all vectors. The vectors correspond to, in order: patients' arrival times, time from arrival to transfer, time from arrival to discharge, and the durations of their latent and each of the three infectious periods if they were to become infected. The duration in the second and third infectious periods is infinity for all patients if there are fewer than two or three infectious states, respectively.

#Infection Times.R
This file contains a function that samples the infection times for patients in a ward during the simulation. Given the current state of the ward and its infection parameters, it randomly generates the time at which each susceptible patient will contract an infection according to the infection rates and infection status of the patients in the ward.

The function outputs a matrix of positive numbers representing the time at which each patient will contract an infection. The matrix is of size $n \times m$ where $n$ is the number of bays in the ward (including side rooms) and $m$ is the largest size of any of these bays.  A row of the matrix represents the infection times of the beds in that bay, where row $1$ always represents side rooms.
If the bed cannot become infected (either it is empty, does not exist, or the patient is already infected), then the time is set to infinity.

#Initial Patient Attributes.R
This file initialises the attributes of all patients present in the ward at the start of the simulation, as in the second half of Algorithm \ref{Initial patient attributes Algoirthm}. It randomly generates the transfer and discharge times for each patient and simulates the durations of their latent and infectious periods according to the specified distributions.

The function outputs a list of matrices of positive numbers, representing the simulated patient attributes. Specifically, it returns the patients' transfer times, discharge times, and the durations of their latent and infectious periods if they were to become infected. Any empty bed is assigned $\infty$ for all these attributes. The matrices are of size $n \times m$ where $n$ is the number of bays in the ward (including side rooms) and $m$ is the largest size of any of these bays.  A row of the matrix represents the infection status of beds in that bay, where row $1$ always represents side rooms.

#Ward Simulation.R
This file contains a function that simulates the infection dynamics and the movement of the patient within the ward, as in the Algorithm \ref{Base Model Algoirthm}. It models scheduled arrivals and departures, bay closures, and the transmission process across infectious states, given the ward structure, initial and arrival patient attributes, infection parameters, and specified control measures.

The function outputs a matrix of positive numbers, where the $i$th row represents the metrics for the $i$th stopping time.
