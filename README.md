# Matthees et al.
## title tba 

---
This 
code was written and used for analysis and visualisation of data contained in Matthees et al.
---

### Classification of responding and non-responding β-arrestin conformational change sensors ###
A flowchart describing this process can be found in **Suppl. Figure X**. 
The donut plot in this figure was created 
[**in donut_plot.R**](https://github.com/mo-yoda/Matthees_fingerprints/blob/master/donut_plot.R).

Conformational change data was acquired in a concentration-dependent manner as described in Matthees et al.
Data was formated and normalised to signal measured in the lowest ligand concentration for further processing
in 
[**data_format.py**](https://github.com/mo-yoda/Matthees_fingerprints/blob/master/data_format.py).

Data was fitted using a four-parameter non-linear model and several fitting parameters were collected in 
[**fingerprint_fits.R**](https://github.com/mo-yoda/Matthees_fingerprints/blob/master/fingerprint_fits.R).

Collected fit parameters were plotted to explore the possibility of certain populations separating
concentration-dependent responses from signals which were not concentration-dependent in 
[**fit_categorization.R**](https://github.com/mo-yoda/Matthees_fingerprints/blob/master/fit_categorization.R).

According to filter constrains choosen according to the previous results, responder and non-responder conditions
were assigned in 
[**responsive_sensors_categorization.R**](https://github.com/mo-yoda/Matthees_fingerprints/blob/master/responsive_sensors_categorization.R). 
Additionally, after manual revision of the fitted plots 4x conditions were assigned as non-responsive.

For visualisation of the β-arrestin fingerprints, heatmaps were created in 
[**heatmap_generation.R**](https://github.com/mo-yoda/Matthees_fingerprints/blob/master/heatmap_generation.R).

---

### Quantification of tail- and core-transferability ###
A flowchart describing this process can be found in **Suppl. Figure X**.

Tail- and core-transferability coefficient was calculated in 
[**coefficient_calculation.R**](https://github.com/mo-yoda/Matthees_fingerprints/blob/master/coefficient_calculation.R)
and bubble plots illustrating the results were created in 
[**tail_core_coeff_plots.R**](https://github.com/mo-yoda/Matthees_fingerprints/blob/master/tail_core_coeff_plots.R).