# VoiceGender
The project consisted in building a model to identify a voice as male or female, based upon acoustic properties of the voice and speech. The dataset consists of a number of recorded voice samples, collected from male and female speakers.

The following acoustic properties of each voice are measured and included within the CSV:  
•	meanfreq: mean frequency (in kHz)  
•	sd: standard deviation of frequency  
•	median: median frequency (in kHz)  
•	Q25: first quantile (in kHz)  
•	Q75: third quantile (in kHz)  
•	IQR: interquantile range (in kHz)  
•	skew: skewness
•	kurt: kurtosis
•	sp.ent: spectral entropy  
•	sfm: spectral flatness  
•	mode: mode frequency  
•	centroid: frequency centroid  
•	peakf: peak frequency (frequency with highest energy)  
•	meanfun: average of fundamental frequency measured across acoustic signal  
•	minfun: minimum fundamental frequency measured across acoustic signal  
•	maxfun: maximum fundamental frequency measured across acoustic signal  
•	meandom: average of dominant frequency measured across acoustic signal  
•	mindom: minimum of dominant frequency measured across acoustic signal  
•	maxdom: maximum of dominant frequency measured across acoustic signal  
•	dfrange: range of dominant frequency measured across acoustic signal  
•	modindx: modulation index. Calculated as the accumulated absolute difference between adjacent measurements of fundamental frequencies divided by the frequency range  
