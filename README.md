# mimic_data_analysis
personal practice of advanced r skills

# rshiny app for mimic data
Visualizing a patient's encounters in a health care system is a common task in clinical data analysis. This tool enables us to visualize a patient's ADT (admission-discharge-transfer) history and ICU vitals in the MIMIC-IV data.

A patient's ADT history records the time of admission, discharge, and transfer in the hospital. The first figure shows the ADT history of the patient. The x-axis is the calendar time, and the y-axis is the type of event (ADT, lab, procedure). The color of the line segment represents the care unit. The size of the line segment represents whether the care unit is an ICU/CCU. The crosses represent lab events, and the shape of the dots represents the type of procedure. The title of the figure shows the patient's demographic information and the subtitle shows top 3 diagnoses.

ICU stays are a subset of ADT history. For the second figure, it shows the vitals of the patient you inputed during ICU stays. The x-axis is the calendar time, and the y-axis is the value of the vital. The color of the line represents the type of vital. The facet grid shows the abbreviation of the vital and the stay ID.
