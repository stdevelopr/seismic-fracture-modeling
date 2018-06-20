# seismic-fracture-modeling

A software to model seismic reflection data from fractured medium
____

With it you can model the expected seismic reflection data in a fractured medium for the P and S waves in different azimuths.
<p>
  
The files have in their names the types of conversions:<p>
- **fracturePP.py** : calculates the P wave reflection from an incident P wave.
- **fractureP_S.py** : calculates the S wave reflection from an incident P wave.
- **fractureSH.py** : calculates the SH wave reflection from an incident SH wave.
- **fractureSV.py** : calculates the SV wave reflection from an incident SV wave.
<p><p>
 
### INSTALL

These files assume that you have ANRAY software installed on your computer.<br>
<a>You can find instructions here: http://w3d.cz/software/sw3dcd21/anray/anray.htm <p>
To run the files just copy them to the anray folder and execute with python. **The _gui files must be in the same folder and are resposible to generate the graphical user interface.
Add the files of the anray folder into your instalation folder, replacing the original anray.for.

### Usage
Here you enter the parameters(layer 1 and layer 2, fracture and bulk properties) and get an aproximation of the reflection coeficient for differents azimuths. In the split box, you can choose the azimuths in which a minimum of 10% split occurs. It will then be plotted on the graph where the difference occurs.
ngeo, d_min, d_step refers to the acquisition parameters (number of geophones, minimun distance, and step).
![alt tag](/images/P_formula_split_min.png)
<p>
In this tab, click on generate model, to use the chosen parameters as data to Anray. After the calculation the result will be plotted (The total component, wich is composed of the vertical and the radial). This program use B-spline approximation.

![alt tag](/images/P_anray.png)

In this tab you see the expected seismograms, and the option to plot the reflection from the soil, to compare the amplitudes.
![alt tag](/images/P_anray_sismo_reflec.png)
The last tab compare the amplitudes for differents azimuths.
![alt tag](/images/P_sismo_azm_vertical.png)
